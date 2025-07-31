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

### Function for plotting the individual speed with ggplot2
plot_speed <- function(dat, dat_outp, yul, track_id, threshold) {
  yr <- year(dat$timestamp[1])
  
  # Prepare data for ggplot
  plot_data <- data.frame(
    timestamp = dat$timestamp,
    speed = dat$speed,
    rollm = dat$rollm
  )
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = timestamp)) +
    geom_point(aes(y = speed), color = "grey30", size = 0.4, alpha = 0.7) +
    geom_line(aes(y = speed), color = "grey30", alpha = 0.5) +
    geom_line(aes(y = rollm), color = "brown4", size = 1.5) +
    geom_hline(
      yintercept = ifelse(is.null(threshold), mean(dat$speed, na.rm = TRUE), threshold),
      linetype = "dotted", size = 2, color = "coral"
    ) +
    labs(
      title = paste(track_id, yr, sep = "_"),
      x = "Time",
      y = expression(paste("Distance /", Delta, "t"))
    ) +
    ylim(0, yul) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
  
  # Add known calving events if provided
  if (!is.null(dat_outp$known_birthdate)) {
    for (i in 1:nrow(dat_outp)) {
      p <- p + geom_vline(
        xintercept = as.numeric(dat_outp$known_birthdate[i]),
        linetype = "solid", size = 2, color = alpha("grey50", 0.5)
      )
    }
  }
  
  # Add identified calving events
  for (i in 1:nrow(dat_outp)) {
    # Add shaded region
    p <- p + annotate("rect",
                      xmin = as.numeric(dat_outp$V5[i]),
                      xmax = as.numeric(dat_outp$V6[i]),
                      ymin = -Inf, ymax = Inf,
                      fill = "grey50", alpha = 0.3
    )
    
    # Add start and end lines
    p <- p + geom_vline(
      xintercept = as.numeric(dat_outp$V5[i]),
      linetype = "dashed", size = 1.5, color = "green4"
    )
    p <- p + geom_vline(
      xintercept = as.numeric(dat_outp$V6[i]),
      linetype = "dotdash", size = 1.5, color = "royalblue"
    )
  }
  
  return(p)
}

### Function for plotting the individual location with ggplot2
plot_loc <- function(dat, dat_outp, track_id) {
  yr <- year(dat$timestamp[1])
  
  # Prepare data
  plot_data <- data.frame(
    location_long = dat$location_long,
    location_lat = dat$location_lat
  )
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = location_long, y = location_lat)) +
    geom_point(size = 0.4, alpha = 0.7) +
    geom_path() +
    labs(
      title = paste(track_id, yr, sep = "_"),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
  
  # Add calving event locations
  if (nrow(dat_outp) > 0) {
    calving_points <- data.frame(
      long = dat_outp$V8,
      lat = dat_outp$V9
    )
    
    p <- p +
      geom_point(
        data = calving_points, aes(x = long, y = lat),
        shape = 4, size = 3, color = "green4"
      ) +
      geom_point(
        data = calving_points, aes(x = long, y = lat),
        shape = 19, size = 1.5, color = "royalblue"
      )
  }
  
  return(p)
}

### Function for plotting the net-squared displacement with ggplot2
plot_nsd <- function(dat, dat_outp, track_id) {
  yr <- year(dat$timestamp[1])
  
  # Prepare data
  plot_data <- data.frame(
    timestamp = dat$timestamp,
    nsd = dat$nsd,
    rollnsd = dat$rollnsd
  )
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = timestamp)) +
    geom_line(aes(y = nsd), color = "black") +
    geom_line(aes(y = rollnsd), color = "brown4", size = 1) +
    labs(
      title = paste(track_id, yr, sep = "_"),
      x = "Time",
      y = "Net squared displacement (km)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
  
  # Add known calving events if provided
  if (!is.null(dat_outp$known_birthdate)) {
    for (i in 1:nrow(dat_outp)) {
      p <- p + geom_vline(
        xintercept = as.numeric(dat_outp$known_birthdate[i]),
        linetype = "solid", size = 2, color = alpha("grey50", 0.5)
      )
    }
  }
  
  # Add identified calving events
  for (i in 1:nrow(dat_outp)) {
    # Add shaded region
    p <- p + annotate("rect",
                      xmin = as.numeric(dat_outp$V5[i]),
                      xmax = as.numeric(dat_outp$V6[i]),
                      ymin = -Inf, ymax = Inf,
                      fill = "grey50", alpha = 0.3
    )
    
    # Add start and end lines
    p <- p + geom_vline(
      xintercept = as.numeric(dat_outp$V5[i]),
      linetype = "dashed", size = 1.5, color = "green4"
    )
    p <- p + geom_vline(
      xintercept = as.numeric(dat_outp$V6[i]),
      linetype = "dotdash", size = 1.5, color = "royalblue"
    )
  }
  
  return(p)
}


create_plots_for_track <- function(track_data_temp, track_data_output, track_id) {
  # Create plots using ggplot2
  p1 <- plot_speed(track_data_temp, track_data_output,
                   yul = yaxis_limit,
                   track_id = track_id, threshold = working_threshold
  )
  
  p2 <- plot_loc(track_data_temp, track_data_output, track_id = track_id)
  p3 <- plot_nsd(track_data_temp, track_data_output, track_id = track_id)
  
  return(list(speed = p1, location = p2, nsd = p3))
}


render_plots_to_pdf <- function(plot_list) {
  # Create combined plots and save as PDF
  if (length(plot_list) > 0) {
    pdf_path <- paste0(
      app_artifacts_base_path,
      paste("parturition_velocity", window, "h.pdf", sep = "")
    )
    
    # Arrange plots in groups of 4 rows (12 plots per page)
    plots_per_page <- 4
    n_pages <- ceiling(length(plot_list) / plots_per_page)
    
    pdf(pdf_path, width = 8, height = 12)
    
    for (page in 1:n_pages) {
      start_idx <- (page - 1) * plots_per_page + 1
      end_idx <- min(page * plots_per_page, length(plot_list))
      
      page_plots <- list()
      for (idx in start_idx:end_idx) {
        if (idx <= length(plot_list) && !is.null(plot_list[[idx]])) {
          page_plots <- c(page_plots, plot_list[[idx]])
        }
      }
      
      if (length(page_plots) > 0) {
        grid.arrange(grobs = page_plots, ncol = 3)
      }
    }
    
    dev.off()
  }
}


get_animal_id <- function(move_data, track_id, original_track_id_column) {
  animal_id <- mt_track_data(move_data) |>
    filter(
      !!rlang::sym(original_track_id_column) == track_id
    ) |>
    dplyr::select(individual_local_identifier) |>
    first()
}


get_track_data <- function(all_track_data, track_id, original_track_id_column) {
  track_data <- all_track_data |>
    filter(
      !!rlang::sym(original_track_id_column) == track_id
    ) 
  
  return(track_data)
}


rFunction <- function(data, threshold = NULL, window = 72,
                      events_file = NULL, yaxis_limit = 1000) {
  
  browser()
  original_track_id_column <- mt_track_id_column(data)
  
  data_df <- data |>
    mutate(
      location_long = sf::st_coordinates(data)[, 1],
      location_lat = sf::st_coordinates(data)[, 2],
      distance = as.numeric(mt_distance(data))
    ) |>
    as.data.frame()
  
  track_ids <- unique(mt_track_id(data))
  
  all_move_data_updated <- list()
  all_data_output <- list()
  
  plot_list <- list() # Store plots for combining
  
  app_artifacts_base_path <- Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/")
  
  for (i in 1:length(track_ids)) {
    track_id <- track_ids[i]
    
    animal_id <- get_animal_id(move_data = data, track_id = track_id, 
                               original_track_id_column = original_track_id_column)
    
    logger.info(str_interp("Running parturition analysis for track ID ${track_id} of animal ${animal_id}"))
    
    track_data <- get_track_data(all_track_data, track_id,
                                 original_track_id_column)
    
    total_track_time <- as.numeric(
      as.POSIXct(max(track_data$timestamp)) - as.POSIXct(min(track_data$timestamp)),
      units = "hours"
    )
    
    if (nrow(track_data) > 10 & total_track_time > window) {
      track_data_temp <- tryCatch(
        track_data |>
          mutate(
            timediff = magic::shift(
              as.numeric(as.POSIXct(track_data$timestamp) - as.POSIXct(lag(track_data$timestamp)),
                         units = "hours"
              ), -1
            )
          ) |>
          filter(
            timediff != 0
          ) |>
          mutate(
            nsd_km = distVincentyEllipsoid(
              cbind(location_long, location_lat),
              cbind(first(location_long), first(location_lat))
            ) / 1000,
            speed = distance / as.numeric(timediff),
            speed_rolling_mean = rollmean(
              speed,
              window / median(as.numeric(timediff), na.rm = T),
              fill = NA
            ),
            nsd_km_rolling_mean = rollmean(
              nsd_km,
              window / median(as.numeric(timediff), na.rm = T),
              fill = NA
            )
          ),
        error = function(e) {
          if (grepl("rollmean", e$message)) {
            logger.error(stringr::str_interp(
              "App failed because the window of ${window} is less than median difference in time between locations for track ${track_id} and animal ${animal_id}. Please increase your window size."
            ))
          }
        }
      )
      
      if (is.null(track_data_temp)) {
        return(NULL)
      }
      
      # user-passed threshold or default to mean rollm
      working_threshold <- ifelse(!is.null(threshold), threshold, mean(track_data_temp$rollm, na.rm = T))
      
      # Input condition for the clustering
      track_data_temp <- track_data_temp |>
        mutate(
          cnd = ifelse(speed < working_threshold & !is.na(speed), 1, 0),
          run = sequence(rle(cnd)$lengths),
          run_positive = ifelse(cnd == 0, 0, as.numeric(run)),
          crun = abs(run_positive - lag(run_positive)),
          case = 0  # this is default, will be updated later
        )
      
      # Make sure last row has a value
      track_data_temp$crun[nrow(data_temp)] <- track_data_temp$run_positive[nrow(track_data_temp) - 1]
      
      cutoff <- floor(window / median(as.numeric(track_data_temp$timediff), na.rm = T))
      
      nrun <- ifelse(is.na(tabulate(track_data_temp$run_positive)[cutoff + 1]), 1,
                     tabulate(track_data_temp$run_positive)[cutoff + 1]
      )
      
      track_data_output <- data.frame()
      
      for (j in 1:nrun) {
        track_data_output[j, {{ original_track_id_column }}] <- track_id
        track_data_output[j, "individual_local_identifier"] <- animal_id
        nrun_ind <- which(track_data_temp$crun >= cutoff - 1)
        
        index_start <- ifelse(length(nrun_ind) == 0, NA, nrun_ind[j] - track_data_temp$run_positive[nrun_ind[j] - 1])
        index_end <- ifelse(length(nrun_ind) == 0, NA, nrun_ind[j])
        
        if (!is.na(index_start)) {
          track_data_temp$case[index_start:index_end] <- 1
        }
        
        track_data_output[j, "number_max_reloc"] <- ifelse(
          length(nrun_ind) == 0, NA, track_data_temp$run_positive[nrun_ind[j] - 1])
        
        track_data_output[j, "threshold_speed_meters_per_hour"] <- working_threshold
        
        track_data_output[j, "start_date"] <- as.POSIXct(
          ifelse(
            length(nrun_ind) == 0, 
            NA,
            track_data_temp$timestamp[index_start]),
          origin = "1970-01-01")
        
        track_data_output[j, "end_date"] <- as.POSIXct(
          ifelse(
            length(nrun_ind) == 0, 
            NA, 
            track_data_temp$timestamp[index_end]), 
          origin = "1970-01-01")
        
        track_data_output[j, "number_detected_events"] <- nrun
        
        if (!is.na(working_threshold)) {
          track_data_output[j, "location_long"] <- ifelse(
            length(nrun_ind) == 0, 
            NA, 
            mean(track_data_temp$location_long[index_start:index_end], na.rm = T))
          
          track_data_output[j, "location_lat"] <- ifelse(
            length(nrun_ind) == 0, 
            NA, 
            mean(track_data_temp$location_lat[index_start:index_end], na.rm = T))
          
        } else {
          track_data_output[j, c("location_long", "location_lat")] <- NA
        }
      }
      
      
      # Read the local known calving events file, if provided
      known_calvings_file <- getAuxiliaryFilePath("events_file")
      if (!is.null(known_calving_file)) {
        known_calvings <- read.csv(known_calving_file,
                                   header = T, colClasses = "character",
                                   na.strings = c("NA", "n/a", "NaN", "")
        ) |>
          mutate(
            known_birthdate = as.POSIXct(birthdate,
                                         tz = "UTC",
                                         format = "%Y-%m-%d"),
            
          ) |>
          dplyr::select("track_id", "known_birthdate")
        
        track_data_output <- track_data_output |>
          dplyr::left_join(
            known_calvings, by = join_by({{original_track_id_column}} == track_id)
          )
      }
      
      all_move_data_updated[[i]] <- track_data_temp
      all_data_output[[i]] <- track_data_output
      
      plot_list[[i]] <- create_plots_for_track(track_data_temp,
                                               track_data_output,
                                               track_id = track_id)
    }
  }
  
  
  
  render_plots_to_pdf(plot_list)
  
  # Process final output
  final_updated_move_data <- do.call(rbind, all_move_data_updated)
  
  final_artefact_data <- do.call(rbind, all_data_output)
  
  if (!is.null(final_artefact_data)) {
    final_artefact_data <- final_artefact_data |>
      drop_na(start_date) |>
      mutate(
        start_date = format(as.POSIXct(dat_final_output$start_date, tz = "UTC"),
                            format = "%Y-%m-%d %H:%M:%S"),
        end_date = format(as.POSIXct(dat_final_output$end_date, tz = "UTC"),
                          format = "%Y-%m-%d %H:%M:%S"
        )
      )
    
    csv_filename <- str_interp("${app_artifacts_base_path}parturition_output_${window}h.csv")
    write.csv(final_artefact_data, file = csv_filename)
  }
  
  # convert the data.frame output into move2 object
  if (!is.null(final_updated_move_data)) {
    track_attribute_data <- mt_track_data(data)
    final_move_data <- final_updated_move_data |>
      left_join(
        track_attribute_data,
        by = {{original_track_id_column}}
      )
    
    updated_move2_data <- mt_as_move2(dat_final,
                                      coords = c("location_long", "location_lat"),
                                      time_column = "timestamp", crs = 4326,
                                      track_id_column = original_track_id_column,
                                      track_attributes = names(track_attribute_data)
    )
    
    return(updated_move2_data)
  }
  
  return(data)
}