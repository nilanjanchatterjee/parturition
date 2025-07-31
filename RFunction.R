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

# Configuration and Constants ------------------------------------------------

PLOTS_PER_PAGE <- 4
PDF_WIDTH <- 8
PDF_HEIGHT <- 12
PLOT_COLS <- 3

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
  
  track_data_timediff <- track_data |>
    mutate(
      timediff = magic::shift(
        as.numeric(
          as.POSIXct(timestamp) - as.POSIXct(lag(timestamp)),
          units = "hours"
        ), -1
      )
    ) |>
    filter(timediff != 0)
  
  # validate rollmean parameters
  
  median_timediff <- median(track_data_timediff$timediff, na.rm = TRUE)
  
  if (window < median_timediff) {
    logger.error(str_interp("Window ${window} less than median time between locations ${median_timediff} for track ${track_id}. Increase your window size."))
    stop()
  } 
  
  if ((window / median_timediff) > nrow(track_data_timediff)) {
    logger.error(str_interp("Window ${window} too large for median time between locations ${median_timediff} for track ${track_id}. Decrease window size."))
    stop()
  }
  
  result <- track_data_timediff |>
    mutate(
      nsd_km = distVincentyEllipsoid(
        cbind(location_long, location_lat),
        cbind(first(location_long), first(location_lat))
      ) / 1000,
      speed = distance / timediff,
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
  
  return(result)
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
    if (!is.na(start_idx) && start_idx > 0) {
      result$parturition_event[start_idx:end_idx] <- 1
    }
  }
  
  return(result)
}

#' Create summary data for detected parturition events
#' @param track_data processed track data with events
#' @param track_id track identifier
#' @param animal_id animal identifier
#' @param working_threshold speed threshold used
#' @param window window size
#' @param original_track_id_column name of track ID column
#' @return data.frame with event summaries
create_event_summary <- function(track_data, track_id, animal_id, working_threshold, 
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
        individual_local_identifier = animal_id,
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

#' Create speed plot with parturition events
#' @param track_data processed track data
#' @param event_summary event summary data
#' @param track_id track identifier
#' @param working_threshold speed threshold
#' @param y_limit y-axis limit
#' @return ggplot object
create_speed_plot <- function(track_data, event_summary, track_id, working_threshold, y_limit) {
  year_label <- year(track_data$timestamp[1])
  
  p <- ggplot(track_data, aes(x = timestamp)) +
    geom_point(aes(y = speed), color = "grey30", size = 0.4, alpha = 0.7) +
    geom_line(aes(y = speed), color = "grey30", alpha = 0.5) +
    geom_line(aes(y = speed_rolling_mean), color = "brown4", linewidth = 1.5) +
    geom_hline(yintercept = working_threshold, 
               linetype = "dotted", linewidth = 2, color = "coral") +
    labs(
      title = paste(track_id, year_label, sep = "_"),
      x = "Time",
      y = expression(paste("Distance /", Delta, "t"))
    ) +
    ylim(0, y_limit) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
  
  # Add known calving events
  if ("known_birthdate" %in% names(event_summary) && !all(is.na(event_summary$known_birthdate))) {
    known_dates <- event_summary$known_birthdate[!is.na(event_summary$known_birthdate)]
    p <- p + geom_vline(xintercept = as.numeric(known_dates),
                        linetype = "solid", linewidth = 2, 
                        color = alpha("grey50", 0.5))
  }
  
  # Add detected events
  if (nrow(event_summary) > 0) {
    for (i in seq_len(nrow(event_summary))) {
      if (!is.na(event_summary$start_date[i]) && !is.na(event_summary$end_date[i])) {
        # Shaded region
        p <- p + annotate("rect",
                          xmin = as.numeric(event_summary$start_date[i]),
                          xmax = as.numeric(event_summary$end_date[i]),
                          ymin = -Inf, ymax = Inf,
                          fill = "grey50", alpha = 0.3)
        
        # Start and end lines
        p <- p + geom_vline(xintercept = as.numeric(event_summary$start_date[i]),
                            linetype = "dashed", linewidth = 1.5, color = "green4")
        p <- p + geom_vline(xintercept = as.numeric(event_summary$end_date[i]),
                            linetype = "dotdash", linewidth = 1.5, color = "royalblue")
      }
    }
  }
  
  p
}

#' Create location plot with parturition events
#' @param track_data processed track data
#' @param event_summary event summary data
#' @param track_id track identifier
#' @return ggplot object
create_location_plot <- function(track_data, event_summary, track_id) {
  year_label <- year(track_data$timestamp[1])
  
  p <- ggplot(track_data, aes(x = location_long, y = location_lat)) +
    geom_point(size = 0.4, alpha = 0.7) +
    geom_path() +
    labs(
      title = paste(track_id, year_label, sep = "_"),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
  
  # Add parturition locations
  if (nrow(event_summary) > 0) {
    valid_locations <- event_summary |>
      filter(!is.na(location_long), !is.na(location_lat))
    
    if (nrow(valid_locations) > 0) {
      p <- p +
        geom_point(data = valid_locations, 
                   aes(x = location_long, y = location_lat),
                   shape = 4, size = 3, color = "green4") +
        geom_point(data = valid_locations, 
                   aes(x = location_long, y = location_lat),
                   shape = 19, size = 1.5, color = "royalblue")
    }
  }
  
  p
}

#' Create net squared displacement plot
#' @param track_data processed track data
#' @param event_summary event summary data
#' @param track_id track identifier
#' @return ggplot object
create_nsd_plot <- function(track_data, event_summary, track_id) {
  year_label <- year(track_data$timestamp[1])
  
  p <- ggplot(track_data, aes(x = timestamp)) +
    geom_line(aes(y = nsd_km), color = "black") +
    geom_line(aes(y = nsd_km_rolling_mean), color = "brown4", linewidth = 1) +
    labs(
      title = paste(track_id, year_label, sep = "_"),
      x = "Time",
      y = "Net squared displacement (km)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
  
  # Add known calving events
  if ("known_birthdate" %in% names(event_summary) && !all(is.na(event_summary$known_birthdate))) {
    known_dates <- event_summary$known_birthdate[!is.na(event_summary$known_birthdate)]
    p <- p + geom_vline(xintercept = as.numeric(known_dates),
                        linetype = "solid", linewidth = 2, 
                        color = alpha("grey50", 0.5))
  }
  
  # Add detected events (same logic as speed plot)
  if (nrow(event_summary) > 0) {
    for (i in seq_len(nrow(event_summary))) {
      if (!is.na(event_summary$start_date[i]) && !is.na(event_summary$end_date[i])) {
        p <- p + annotate("rect",
                          xmin = as.numeric(event_summary$start_date[i]),
                          xmax = as.numeric(event_summary$end_date[i]),
                          ymin = -Inf, ymax = Inf,
                          fill = "grey50", alpha = 0.3)
        
        p <- p + geom_vline(xintercept = as.numeric(event_summary$start_date[i]),
                            linetype = "dashed", linewidth = 1.5, color = "green4")
        p <- p + geom_vline(xintercept = as.numeric(event_summary$end_date[i]),
                            linetype = "dotdash", linewidth = 1.5, color = "royalblue")
      }
    }
  }
  
  p
}

#' Create all plots for a single track
#' @param track_data processed track data
#' @param event_summary event summary data
#' @param track_id track identifier
#' @param working_threshold speed threshold
#' @param y_limit y-axis limit
#' @return list of ggplot objects
create_track_plots <- function(track_data, event_summary, track_id, working_threshold, y_limit) {
  plots <- list(
    speed = create_speed_plot(track_data, event_summary, track_id, working_threshold, y_limit),
    location = create_location_plot(track_data, event_summary, track_id),
    nsd = create_nsd_plot(track_data, event_summary, track_id)
  )
  
  return(plots)
}

#' Render plots to PDF
#' @param plot_list list of plot objects
#' @param output_path path for PDF output
#' @param window window size for filename
render_plots_to_pdf <- function(plot_list, output_path, window) {
  if (length(plot_list) == 0) {
    return(invisible(NULL))
  }
  
  pdf_path <- str_interp("${output_path}parturition_velocity_${window}h.pdf")
  n_pages <- ceiling(length(plot_list) / PLOTS_PER_PAGE)
  
  pdf(pdf_path, width = PDF_WIDTH, height = PDF_HEIGHT)
  
  for (page in seq_len(n_pages)) {
    start_idx <- (page - 1) * PLOTS_PER_PAGE + 1
    end_idx <- min(page * PLOTS_PER_PAGE, length(plot_list))
    
    page_plots <- plot_list[start_idx:end_idx] |>
      map(~ list(.x$speed, .x$location, .x$nsd)) |>
      flatten() |>
      compact()
    
    browser()
    
    if (length(page_plots) > 0) {
      grid.arrange(grobs = page_plots, ncol = PLOT_COLS)
    }
  }
  
  dev.off()
  
  message("Plots saved to: ", pdf_path)
  return(invisible(pdf_path))
}

# Utility Functions -----------------------------------------------------------

#' Get animal ID for a track
#' @param data move2 object
#' @param track_id track identifier
#' @param original_track_id_column track ID column name
#' @return animal ID
get_animal_id <- function(data, track_id, original_track_id_column) {
  animal_id <- mt_track_data(data) |>
    filter(!!sym(original_track_id_column) == track_id) |>
    pull(individual_local_identifier) |>
    first()
  
  return(animal_id)
}

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
#' @return updated move2 object with parturition indicators
rFunction <- function(data, threshold = NULL, window = 72, 
                      events_file = NULL, yaxis_limit = 1000) {
  

  # Setup
  original_track_id_column <- mt_track_id_column(data)
  app_artifacts_base_path <- Sys.getenv("APP_ARTIFACTS_DIR", "/tmp/")
  
  # Prepare data
  data_df <- prepare_movement_data(data)
  track_ids <- unique(mt_track_id(data))
  
  # Load known events if provided
  known_events <- load_known_events(events_file)
  
  # Initialize storage
  all_processed_data <- list()
  all_event_summaries <- list()
  all_plots <- list()
  
  # Process each track
  for (i in seq_along(track_ids)) {
    track_id <- track_ids[i]
    animal_id <- get_animal_id(data, track_id, original_track_id_column)
    
    message("Processing track ", track_id, " (animal ", animal_id, ")")
    
    track_data <- get_track_data(data_df, track_id, original_track_id_column)
    
    # Skip tracks with insufficient data
    if (!has_sufficient_data(track_data, window)) {
      message("Skipping track ", track_id, " - insufficient data")
      next
    }
    
    # Calculate movement metrics
    processed_data <- tryCatch({
      calculate_movement_metrics(track_data, window, track_id)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(processed_data)) next
    
    # Determine threshold
    working_threshold <- threshold %||% mean(processed_data$speed_rolling_mean, na.rm = TRUE)
    
    # Identify parturition events
    processed_data <- identify_parturition_events(processed_data, working_threshold, window)
    
    # Create event summary
    event_summary <- create_event_summary(
      processed_data, track_id, animal_id, working_threshold, 
      window, original_track_id_column
    )
    
    # Add known events if available
    if (!is.null(known_events)) {
      event_summary <- event_summary |>
        left_join(known_events, by = setNames("track_id", original_track_id_column))
    }
    
    # Store results
    all_processed_data[[i]] <- processed_data
    all_event_summaries[[i]] <- event_summary
    
    # Create plots
    if (nrow(event_summary) > 0) {
      all_plots[[i]] <- create_track_plots(
        processed_data, event_summary, track_id, working_threshold, yaxis_limit
      )
    }
  }
  
  # Combine results
  final_processed_data <- do.call(rbind, all_processed_data)
  final_event_summaries <- do.call(rbind, all_event_summaries)
  
  # Generate outputs
  if (!is.null(final_event_summaries) && nrow(final_event_summaries) > 0) {
    # Save CSV
    csv_path <- str_interp("${app_artifacts_base_path}parturition_output_${window}h.csv")
    
    final_event_summaries |>
      filter(!is.na(start_date)) |>
      mutate(
        start_date = format(start_date, "%Y-%m-%d %H:%M:%S"),
        end_date = format(end_date, "%Y-%m-%d %H:%M:%S")
      ) |>
      write_csv(csv_path)
    
    message("Results saved to: ", csv_path)
  }
  
  # Render plots
  if (length(all_plots) > 0) {
    render_plots_to_pdf(all_plots, app_artifacts_base_path, window)
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