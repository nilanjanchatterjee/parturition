library(move2)
library(zoo)
library(tidyverse)
library(sf)
library(units)
library(magic)
library(geosphere)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(grid)
library(elevatr)
library(terra)
library(viridisLite)
library(tidyterra)

# Okabe-Ito colorblind-friendly palette
get_r4_colors <- function() {
  return(c(
    "#000000",  # Black
    "#E69F00",  # Orange
    "#56B4E9",  # Sky blue
    "#009E73",  # Bluish green
    "#F0E442",  # Yellow
    "#0072B2",  # Blue
    "#D55E00",  # Vermillion
    "#CC79A7",  # Reddish purple
    "#999999"   # Grey
  ))
}

#' Get elevation raster for track extent
#' @param track_data processed track data
#' @return SpatRaster object for ggplot
get_elevation_raster <- function(track_data) {
  tryCatch({
    # Get elevation data - returns a SpatRaster
    elevation <- elevatr::get_elev_raster(
      locations = data.frame(
        x = track_data$location_long,
        y = track_data$location_lat
      ), z = 6, prj = st_crs(4326), src = "aws",
      override_size_check = T, clip = "locations",
      expand = 0.02)
    
    return(terra::rast(elevation))
  }, error = function(e) {
    warning("Could not retrieve elevation data: ", e$message)
    return(NULL)
  })
}

PLOTS_PER_ROW <- 3  # Speed, Location, NSD
PDF_WIDTH <- 8.5   # Portrait orientation
PDF_HEIGHT <- 11

# Data Processing Functions ---------------------------------------------------

#' Extract spatial coordinates and add track information
#' @param data move2 object
#' @return data.frame with coordinates and track info
prepare_movement_data <- function(data) {
  coords <- sf::st_coordinates(data)
  
  result <- data |>
    mutate(
      location_long = coords[, 1],
      location_lat = coords[, 2],
      distance = as.numeric(mt_distance(data))
    ) |>
    as.data.frame()
  
  return(result)
}

#' Calculate movement metrics for a single track
#' @param track_data data.frame for single track
#' @param window numeric, rolling window size in hours
#' @return processed data.frame with movement metrics
calculate_movement_metrics <- function(track_data, window, track_id) {
  
  result <- track_data |>
    mutate(
      timediff = magic::shift(
        as.numeric(
          as.POSIXct(timestamp) - as.POSIXct(lag(timestamp)),
          units = "hours"
        ), -1
      )
    ) |>
    filter(timediff != 0) |>
    mutate(
      nsd_km = distVincentyEllipsoid(
        cbind(location_long, location_lat),
        cbind(first(location_long), first(location_lat))
      ) / 1000,
      speed = distance / timediff,
      speed_rolling_mean = NA,
      nsd_km_rolling_mean = NA
    )
  
  # validate rollmean parameters
  median_timediff <- median(result$timediff, na.rm = TRUE)
  valid_window <- TRUE
  
  if (window < median_timediff) {
    valid_window <- FALSE
    logger.error(str_interp("Window ${window} less than median time between locations ${median_timediff} for track ${track_id}. Increase your window size."))
  } else if ((window / median_timediff) > nrow(result)) {
    valid_window <- FALSE
    logger.error(str_interp("Window ${window} too large for median time between locations ${median_timediff} for track ${track_id}. Decrease window size."))
  } else {
    result <- result |>
      mutate(
        speed_rolling_mean = rollmean(
          speed,
          window / median(timediff, na.rm = TRUE),
          fill = NA
        ),
        nsd_km_rolling_mean = rollmean(
          nsd_km,
          window / median(timediff, na.rm = TRUE),
          fill = NA
        )
      )
  }
  
  return(list(df = result |> mutate(valid_window = valid_window),
              valid_window = valid_window))
}

#' Identify parturition events based on movement patterns
#' @param track_data processed track data
#' @param working_threshold speed threshold
#' @param window rolling window size
#' @return track data with parturition indicators
identify_parturition_events <- function(track_data, working_threshold, window) {
  cutoff <- floor(window / median(track_data$timediff, na.rm = TRUE))
  
  # Initial processing
  result <- track_data |>
    mutate(
      is_below_threshold = ifelse(speed < working_threshold & !is.na(speed), 1, 0),
      run_length = sequence(rle(is_below_threshold)$lengths),
      run_positive = ifelse(is_below_threshold == 0, 0, as.numeric(run_length)),
      run_change = abs(run_positive - lag(run_positive, default = 0)),
      parturition_event = 0
    ) |>
    mutate(
      run_change = ifelse(row_number() == n(),
                          run_positive[n() - 1],
                          run_change)
    )
  
  # Mark parturition events
  event_indices <- which(result$run_change >= cutoff - 1)
  
  for (idx in event_indices) {
    start_idx <- idx - result$run_positive[idx - 1]
    end_idx <- idx

    if (!identical(start_idx, numeric(0)) && start_idx > 0 && !is.na(start_idx)) {
      result$parturition_event[start_idx:end_idx] <- 1
    }
  }
  
  return(result)
}

#' Create summary data for detected parturition events
#' @param track_data processed track data with events
#' @param track_id track identifier
#' @param working_threshold speed threshold used
#' @param window window size
#' @param original_track_id_column name of track ID column
#' @return data.frame with event summaries
create_event_summary <- function(track_data, track_id, working_threshold,
                                 window, original_track_id_column) {
  cutoff <- floor(window / median(track_data$timediff, na.rm = TRUE))
  event_indices <- which(track_data$run_change >= cutoff - 1)
  
  if (length(event_indices) == 0) {
    return(data.frame())
  }
  
  n_events <- ifelse(is.na(tabulate(track_data$run_positive)[cutoff + 1]), 1,
                     tabulate(track_data$run_positive)[cutoff + 1])
  
  event_summary <- tibble()
  
  for (j in seq_len(n_events)) {
    if (j <= length(event_indices)) {
      idx <- event_indices[j]
      start_idx <- idx - track_data$run_positive[idx - 1]
      end_idx <- idx
      
      event_data <- tibble(
        !!original_track_id_column := track_id,
        number_max_reloc = ifelse(is.na(start_idx), NA, track_data$run_positive[idx - 1]),
        threshold_speed_meters_per_hour = working_threshold,
        start_date = ifelse(is.na(start_idx), NA, track_data$timestamp[start_idx]) |>
          as.POSIXct(origin = "1970-01-01"),
        end_date = ifelse(is.na(end_idx), NA, track_data$timestamp[end_idx]) |>
          as.POSIXct(origin = "1970-01-01"),
        number_detected_events = n_events,
        location_long = ifelse(is.na(start_idx), NA,
                               mean(track_data$location_long[start_idx:end_idx], na.rm = TRUE)),
        location_lat = ifelse(is.na(start_idx), NA,
                              mean(track_data$location_lat[start_idx:end_idx], na.rm = TRUE))
      )
      
      event_summary <- bind_rows(event_summary, event_data)
    }
  }
  
  return(event_summary)
}

#' Load and process known calving events
#' @param events_file path to events file
#' @return processed events data or NULL
load_known_events <- function(events_file) {
  if (is.null(events_file)) {
    return(NULL)
  }
  
  result <- tryCatch({
    data <- read_csv(events_file,
                     col_types = cols(.default = "c"),
                     na = c("NA", "n/a", "NaN", "")) |>
      mutate(
        known_birthdate = as.POSIXct(birthdate, tz = "UTC", format = "%Y-%m-%d")
      ) |>
      select(track_id, known_birthdate)
    
    return(data)
  }, error = function(e) {
    warning("Could not load known events file: ", e$message)
    return(NULL)
  })
  
  return(result)
}

# Plotting Functions ----------------------------------------------------------

#' Create speed plot without title (for combined display)
#' @param track_data processed track data
#' @param event_summary event summary data
#' @param working_threshold speed threshold
#' @param y_limit y-axis limit
#' @return ggplot object
create_speed_plot_no_title <- function(track_data, event_summary, working_threshold, y_limit) {
  # Get colors
  colors <- get_r4_colors()
  
  # Color mapping:
  # colors[1] = "#000000" (Black) - main data
  # colors[3] = "#56B4E9" (Sky blue) - event end
  # colors[4] = "#009E73" (Bluish green) - event start  
  # colors[6] = "#0072B2" (Blue) - known parturition
  # colors[8] = "#CC79A7" (Reddish purple) - rolling mean
  
  p <- ggplot(track_data, aes(x = timestamp)) +
    geom_point(aes(y = speed), color = colors[1], size = 0.4, alpha = 0.7) +
    geom_line(aes(y = speed), color = colors[1], alpha = 0.5) +
    labs(x = "Time", y = expression(paste("Speed (Distance/", Delta, "t)"))) +
    ylim(0, y_limit) +
    scale_x_datetime(date_labels = "%m/%d/%y", date_breaks = "3 months") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      axis.title = element_text(size = 9),
      plot.margin = margin(5, 5, 5, 5),
      panel.spacing = unit(0, "lines"),
      aspect.ratio = NULL
    )
  
  if (any(!is.na(track_data$speed_rolling_mean))) {
    p <- p + geom_line(aes(y = speed_rolling_mean), color = colors[8])
  }
  
  # Add known parturition events
  if ("known_birthdate" %in% names(event_summary) && !all(is.na(event_summary$known_birthdate))) {
    known_dates <- event_summary$known_birthdate[!is.na(event_summary$known_birthdate)]
    if (!inherits(known_dates, "POSIXct")) {
      known_dates <- as.POSIXct(known_dates)
    }
    p <- p + geom_vline(xintercept = known_dates, linetype = "solid", color = colors[6])
  }
  
  # Add detected events
  if (nrow(event_summary) > 0) {
    for (i in seq_len(nrow(event_summary))) {
      if (!is.na(event_summary$start_date[i]) && !is.na(event_summary$end_date[i])) {
        start_date <- event_summary$start_date[i]
        end_date <- event_summary$end_date[i]
        
        if (!inherits(start_date, "POSIXct")) {
          start_date <- as.POSIXct(start_date)
        }
        if (!inherits(end_date, "POSIXct")) {
          end_date <- as.POSIXct(end_date)
        }
        
        p <- p + annotate("rect", xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf, fill = "grey50", alpha = 0.3) +
          geom_vline(xintercept = start_date, linetype = "dashed", color = colors[4]) +
          geom_vline(xintercept = end_date, linetype = "dotdash", color = colors[3])
      }
    }
  }
  
  return(p)
}

#' Create location plot without title
#' @param track_data processed track data
#' @param event_summary event summary data
#' @param include_elevation whether to include elevation background
#' @return ggplot object
create_location_plot_no_title <- function(track_data, event_summary, include_elevation = FALSE) {
  # Get colors
  colors <- get_r4_colors()
  
  p <- ggplot()
  
  if (include_elevation) {
    # Get elevation raster
    elev_raster <- get_elevation_raster(track_data)
    # Add elevation background if available
    if (!is.null(elev_raster)) {
      p <- p + 
        tidyterra::geom_spatraster(data = elev_raster, alpha = 0.6) +
        scale_fill_whitebox_c(name = "Elevation (m)",
                             #option = "terrain",
                             na.value = "transparent")
    }
  }
  
  # Add track data
  p <- p + 
    geom_point(data = track_data, aes(x = location_long, y = location_lat), 
               size = 0.4, alpha = 0.8, color = colors[1]) +
    geom_path(data = track_data, aes(x = location_long, y = location_lat), 
              alpha = 0.7, color = colors[1]) +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      plot.margin = margin(5, 5, 5, 5),
      panel.spacing = unit(0, "lines"),
      aspect.ratio = NULL,
      legend.position = "bottom",
      legend.key.width = unit(0.8, "cm"),
      legend.key.height = unit(0.2, "cm"),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 6),
      legend.margin = margin(t = 2, b = 2)
    )
  
  # Add parturition locations
  if (nrow(event_summary) > 0) {
    valid_locations <- event_summary |>
      filter(!is.na(location_long), !is.na(location_lat))
    
    if (nrow(valid_locations) > 0) {
      p <- p + 
        geom_point(data = valid_locations, aes(x = location_long, y = location_lat), 
                   shape = 4, size = 4, color = colors[4], stroke = 1.5) +
        geom_point(data = valid_locations, aes(x = location_long, y = location_lat), 
                   shape = 19, size = 2, color = colors[3])
    }
  }
  
  return(p)
}

#' Create NSD plot without title
#' @param track_data processed track data
#' @param event_summary event summary data
#' @return ggplot object
create_nsd_plot_no_title <- function(track_data, event_summary) {
  # Get colors
  colors <- get_r4_colors()
  
  p <- ggplot(track_data, aes(x = timestamp)) +
    geom_line(aes(y = nsd_km), color = colors[1], alpha = 0.7) +
    labs(x = "Time", y = "Net Squared Displacement (km)") +
    scale_x_datetime(date_labels = "%m/%d/%y", date_breaks = "3 months") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      axis.title = element_text(size = 9),
      plot.margin = margin(5, 5, 5, 5),
      panel.spacing = unit(0, "lines"),
      aspect.ratio = NULL
    )
  
  if(any(!is.na(track_data$nsd_km_rolling_mean))) {
    p <- p + geom_line(aes(y = nsd_km_rolling_mean), color = colors[8])
  }
  
  # Add known parturition events
  if ("known_birthdate" %in% names(event_summary) && !all(is.na(event_summary$known_birthdate))) {
    known_dates <- event_summary$known_birthdate[!is.na(event_summary$known_birthdate)]
    if (!inherits(known_dates, "POSIXct")) {
      known_dates <- as.POSIXct(known_dates)
    }
    p <- p + geom_vline(xintercept = known_dates, linetype = "solid", color = colors[6])
  }
  
  # Add detected events
  if (nrow(event_summary) > 0) {
    for (i in seq_len(nrow(event_summary))) {
      if (!is.na(event_summary$start_date[i]) && !is.na(event_summary$end_date[i])) {
        start_date <- event_summary$start_date[i]
        end_date <- event_summary$end_date[i]
        
        if (!inherits(start_date, "POSIXct")) {
          start_date <- as.POSIXct(start_date)
        }
        if (!inherits(end_date, "POSIXct")) {
          end_date <- as.POSIXct(end_date)
        }
        
        p <- p + annotate("rect", xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf, fill = "grey50", alpha = 0.3) +
          geom_vline(xintercept = start_date, linetype = "dashed", color = colors[4]) +
          geom_vline(xintercept = end_date, linetype = "dotdash", color = colors[3])
      }
    }
  }
  
  return(p)
}

#' Create a combined plot row for a single track
#' @param track_data processed track data
#' @param event_summary event summary data
#' @param track_id track identifier
#' @param working_threshold speed threshold
#' @param y_limit y-axis limit
#' @param include_elevation whether to include elevation background
#' @return combined plot object
create_track_row <- function(track_data, event_summary, track_id, working_threshold, y_limit, include_elevation = FALSE) {
  year_label <- year(track_data$timestamp[1])
  base_title <- paste(track_id, year_label, sep = "_")
  
  # Check if analysis worked by looking at valid_window column
  analysis_failed <- FALSE
  if ("valid_window" %in% names(track_data)) {
    analysis_failed <- !all(track_data$valid_window, na.rm = TRUE)
  }
  
  # Create title with status indicator
  if (analysis_failed) {
    title <- paste(base_title, "ANALYSIS FAILED - Check logs")
    title_color <- "red"
  } else {
    title <- base_title
    title_color <- "darkgreen"
  }
  
  # Create individual plots without titles - NEW ORDER: Location, Speed, NSD
  location_plot <- create_location_plot_no_title(track_data, event_summary, include_elevation)
  speed_plot <- create_speed_plot_no_title(track_data, event_summary, working_threshold, y_limit)
  nsd_plot <- create_nsd_plot_no_title(track_data, event_summary)
  
  # Create shared title as a text grob with status color
  title_grob <- grid::textGrob(
    title, 
    gp = grid::gpar(fontsize = 12, fontface = "bold", col = title_color),
    hjust = 0.5
  )
  
  # Combine plots horizontally with shared title - NEW ORDER
  combined_plot <- gridExtra::arrangeGrob(
    title_grob,
    gridExtra::arrangeGrob(location_plot, speed_plot, nsd_plot, ncol = 3),
    ncol = 1,
    heights = c(0.08, 0.92)  # Title takes 8% of height
  )
  
  return(combined_plot)
}

#' Create all plots for multiple tracks (improved version)
#' @param all_processed_data list of processed track data
#' @param all_event_summaries list of event summaries
#' @param track_ids vector of track identifiers
#' @param working_thresholds vector of working thresholds
#' @param y_limit y-axis limit
#' @param include_elevation whether to include elevation background
#' @return list of combined plot objects
create_all_track_plots <- function(all_processed_data, all_event_summaries, 
                                   track_ids, working_thresholds, y_limit, include_elevation = FALSE) {
  plot_rows <- list()
  
  for (i in seq_along(all_processed_data)) {
    if (!is.null(all_processed_data[[i]]) && nrow(all_processed_data[[i]]) > 0) {
      plot_rows[[i]] <- create_track_row(
        all_processed_data[[i]], 
        all_event_summaries[[i]], 
        track_ids[i], 
        working_thresholds[i], 
        y_limit,
        include_elevation
      )
    }
  }
  
  # Remove NULL elements
  plot_rows <- plot_rows[!sapply(plot_rows, is.null)]
  return(plot_rows)
}

#' Render improved plots to PDF
#' @param plot_rows list of combined plot row objects
#' @param window window size for filename
#' @param threshold threshold value for filename
#' @param tracks_per_page number of track rows per page
render_improved_plots_to_pdf <- function(plot_rows, window, threshold = NULL, tracks_per_page = 3) {
  if (length(plot_rows) == 0) {
    logger.info("No plots to render")
    return(invisible(NULL))
  }
  
  # Create filename with threshold info
  threshold_text <- ifelse(is.null(threshold), "average", threshold)
  pdf_path <- appArtifactPath(str_interp("parturition_analysis_threshold_${threshold_text}mh_window_${window}h.pdf"))

  n_pages <- ceiling(length(plot_rows) / tracks_per_page)
  
  # Start PDF device
  pdf(pdf_path, width = PDF_WIDTH, height = PDF_HEIGHT)
  
  # Create legend page first
  grid::grid.newpage()
  
  # Get colors for legend
  colors <- get_r4_colors()
  
  # Create legend with colored text
  grid::grid.text("Parturition Analysis Results", x = 0.05, y = 0.95, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 14, fontface = "bold"))
  
  grid::grid.text("TITLE STATUS INDICATORS:", x = 0.05, y = 0.90, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 11, fontface = "bold"))
  grid::grid.text("Green text: Analysis completed successfully", x = 0.05, y = 0.87, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10, col = "darkgreen"))
  grid::grid.text("Red text: ANALYSIS FAILED - Window size issues - check logs for details", 
                  x = 0.05, y = 0.84, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10, col = "red"))
  
  grid::grid.text("PLOT LEGEND:", x = 0.05, y = 0.79, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 11, fontface = "bold"))
  
  grid::grid.text("Location Plot (Left):", x = 0.05, y = 0.75, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10, fontface = "bold"))
  grid::grid.text("Background: Elevation raster (terrain colors)", x = 0.05, y = 0.72, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10))
  grid::grid.text("Black points/line: Animal track", x = 0.05, y = 0.69, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10, col = colors[1]))
  grid::grid.text("Bluish green X + Sky blue dot: Detected parturition location", 
                  x = 0.05, y = 0.66, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10, col = colors[4]))
  
  grid::grid.text("Speed Plot (Center):", x = 0.05, y = 0.61, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10, fontface = "bold"))
  grid::grid.text("Black points/line: Raw speed data", x = 0.05, y = 0.58, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10, col = colors[1]))
  grid::grid.text("Reddish purple line: Rolling mean speed", x = 0.05, y = 0.55, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10, col = colors[8]))
  grid::grid.text("Grey shaded area: Detected parturition event", x = 0.05, y = 0.49, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10, col = "grey50"))
  grid::grid.text("Bluish green dashed line: Event start", x = 0.05, y = 0.46, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10, col = colors[4]))
  grid::grid.text("Sky blue dot-dash line: Event end", x = 0.05, y = 0.43, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10, col = colors[3]))
  grid::grid.text("Blue solid line: Known parturition date (if available)", 
                  x = 0.05, y = 0.40, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10, col = colors[6]))
  
  grid::grid.text("NSD Plot (Right):", x = 0.05, y = 0.35, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10, fontface = "bold"))
  grid::grid.text("Black line: Net squared displacement", x = 0.05, y = 0.32, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10, col = colors[1]))
  grid::grid.text("Reddish purple line: Rolling mean NSD", x = 0.05, y = 0.29, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10, col = colors[8]))
  grid::grid.text("Event markers same as speed plot", x = 0.05, y = 0.26, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10))
  
  grid::grid.text("Date Format: mm/dd/yy (marks show exact date)", x = 0.05, y = 0.21, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10))
  
  grid::grid.text(paste("Analysis window:", window, "hours"), x = 0.05, y = 0.17, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10))
  grid::grid.text(paste("Threshold:", if (is.null(threshold)) "Rolling average" else threshold), 
                  x = 0.05, y = 0.14, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10))
  grid::grid.text(paste("Generated on:", Sys.time()), x = 0.05, y = 0.11, just = c("left", "top"), 
                  gp = grid::gpar(fontsize = 10))
  
  # Create data pages - all with same layout regardless of number of plots
  for (page in seq_len(n_pages)) {
    start_idx <- (page - 1) * tracks_per_page + 1
    end_idx <- min(page * tracks_per_page, length(plot_rows))
    
    page_plots <- plot_rows[start_idx:end_idx]
    
    if (length(page_plots) > 0) {
      # Create new page
      grid::grid.newpage()
      
      # Always use the same layout (3 equal sections) regardless of actual plot count
      heights <- c(1/3, 1/3, 1/3)
      
      # Create empty grobs for missing plots to maintain consistent layout
      all_plots <- list()
      for (i in 1:3) {
        if (i <= length(page_plots)) {
          all_plots[[i]] <- page_plots[[i]]
        } else {
          all_plots[[i]] <- grid::nullGrob()  # Empty space
        }
      }
      
      # Arrange plots with consistent spacing
      combined_page <- gridExtra::arrangeGrob(
        grobs = all_plots,
        ncol = 1,
        heights = heights
      )
      
      grid::grid.draw(combined_page)
    }
  }
  
  dev.off()
  logger.info(paste("Enhanced plots with legend saved to:", pdf_path, sep = " "))
  return(invisible(pdf_path))
}

# Utility Functions -----------------------------------------------------------

#' Filter data for specific track
#' @param data_df full dataset
#' @param track_id track identifier
#' @param original_track_id_column track ID column name
#' @return filtered data
get_track_data <- function(data_df, track_id, original_track_id_column) {
  filtered_data <- data_df |>
    filter(!!sym(original_track_id_column) == track_id)
  
  return(filtered_data)
}

#' Check if track has sufficient data
#' @param track_data track data
#' @param window minimum window size
#' @return logical
has_sufficient_data <- function(track_data, window) {
  if (nrow(track_data) <= 10) {
    return(FALSE)
  }
  
  total_time <- as.numeric(
    as.POSIXct(max(track_data$timestamp)) - as.POSIXct(min(track_data$timestamp)),
    units = "hours"
  )
  
  sufficient <- total_time > window
  return(sufficient)
}

# Main Function ---------------------------------------------------------------

#' Main parturition analysis function
#' @param data move2 object containing movement data
#' @param threshold speed threshold for identifying parturition events
#' @param window rolling window size in hours
#' @param events_file path to known events file
#' @param yaxis_limit y-axis limit for speed plots
#' @param include_elevation whether to include elevation background in location plots
#' @return updated move2 object with parturition indicators
rFunction <- function(data, threshold = NULL, window = 72,
                      events_file = NULL, yaxis_limit = 1000, 
                      include_elevation = FALSE) {
  
  # Setup
  original_track_id_column <- mt_track_id_column(data)

  # Prepare data
  data_df <- prepare_movement_data(data)
  track_ids <- unique(mt_track_id(data))
  
  # Load known events if provided
  known_events <- load_known_events(events_file)
  
  # Initialize storage
  all_processed_data <- list()
  all_event_summaries <- list()
  all_working_thresholds <- numeric(length(track_ids))
  
  # Process each track
  for (i in seq_along(track_ids)) {
    track_id <- track_ids[i]

    logger.info(paste("Processing track", track_id, sep = ": "))
    
    track_data <- get_track_data(data_df, track_id, original_track_id_column)
    
    # Skip tracks with insufficient data
    if (!has_sufficient_data(track_data, window)) {
      logger.info("Skipping track ", track_id, " - insufficient data")
      next
    }
    
    # Calculate movement metrics
    processed_data_result <- calculate_movement_metrics(track_data, window, track_id)
    processed_data <- processed_data_result$df
    
    if (!processed_data_result$valid_window) {
      event_summary <- tibble()
      working_threshold <- threshold %||% 0  # Default for failed analysis
    } else {
      
      # Determine threshold
      working_threshold <- threshold %||% mean(processed_data$speed_rolling_mean, na.rm = TRUE)
      
      # Identify parturition events
      processed_data <- identify_parturition_events(processed_data, working_threshold, window)
      
      # Create event summary
      event_summary <- create_event_summary(
        processed_data, track_id, working_threshold, 
        window, original_track_id_column
      )
    }
    
    # Store working threshold
    all_working_thresholds[i] <- working_threshold
    
    # Add known events if available
    if (!is.null(known_events)) {
      event_summary <- event_summary |>
        left_join(known_events, by = setNames("track_id", original_track_id_column))
    }
    
    # Store results
    all_processed_data[[i]] <- processed_data
    all_event_summaries[[i]] <- event_summary
  }
  
  # Combine results
  final_processed_data <- bind_rows(all_processed_data)
  final_event_summaries <- bind_rows(all_event_summaries)
  
  # Generate outputs
  if (!is.null(final_event_summaries) && nrow(final_event_summaries) > 0) {
    # Save CSV with threshold in filename
    threshold_text <-   threshold_text <- ifelse(is.null(threshold), "average", threshold)
    csv_path <- appArtifactPath(str_interp("parturition_output_threshold_${threshold_text}mh_window_${window}h.csv"))
    
    final_event_summaries |>
      filter(!is.na(start_date)) |>
      mutate(
        start_date = format(start_date, "%Y-%m-%d %H:%M:%S"),
        end_date = format(end_date, "%Y-%m-%d %H:%M:%S")
      ) |>
      write_csv(csv_path)
    
    logger.info(paste("Results saved to:", csv_path, sep = " "))
  }
  
  # Generate enhanced plots
  if (length(all_processed_data) > 0) {
    # Filter out NULL processed data and corresponding elements
    valid_indices <- !sapply(all_processed_data, is.null)
    
    if (any(valid_indices)) {
      valid_processed_data <- all_processed_data[valid_indices]
      valid_event_summaries <- all_event_summaries[valid_indices]
      valid_track_ids <- track_ids[valid_indices]
      valid_thresholds <- all_working_thresholds[valid_indices]
      
      # Create all plot rows with shared titles and status indicators
      plot_rows <- create_all_track_plots(
        valid_processed_data, 
        valid_event_summaries, 
        valid_track_ids, 
        valid_thresholds, 
        yaxis_limit,
        include_elevation
      )
      
      # Render to PDF with legend page
      if (length(plot_rows) > 0) {
        render_improved_plots_to_pdf(plot_rows, window, threshold)
      }
    }
  }
  
  # Return updated move2 object
  if (!is.null(final_processed_data) && nrow(final_processed_data) > 0) {
    track_attributes <- mt_track_data(data)
    
    updated_data <- final_processed_data |>
      left_join(track_attributes, by = original_track_id_column)
    
    return(mt_as_move2(
      updated_data,
      coords = c("location_long", "location_lat"),
      time_column = "timestamp",
      crs = 4326,
      track_id_column = original_track_id_column,
      track_attributes = names(track_attributes)
    ))
  }
  
  return(data)
}