library(move2)
library(zoo)
library(tidyverse, quietly = TRUE)
library(sf)
library(units)
library(magic)
library(geosphere)
library(lubridate)


### Function for plotting the individual speed
plot_speed <- function(dat, dat_outp, yul, track_id, threshold) {
  yr <- year(dat$timestamp[1])

  plot(dat$timestamp, dat$speed,
    main = paste(track_id, yr, sep = "_"), cex = 0.4, ylim = c(0, yul),
    ylab = expression(paste("Distance /", Delta, "t")), xlab = "Time", col = "grey40"
  )

  lines(dat$timestamp, dat$speed, col = "grey30", main = paste(track_id, yr, sep = "_"))
  lines(dat$timestamp, dat$rollm, col = "brown4", lwd = 1.5, main = paste(track_id, yr, sep = "_"))
  abline(h = ifelse(is.null(threshold), mean(dat$speed, na.rm = T), threshold), lty = 3, lwd = 2, col = "coral")

  for (i in 1:nrow(dat_outp)) {
    abline(v = dat_outp$V5, lty = 2, lwd = 1.5, col = "green4")
    abline(v = dat_outp$V6, lty = 4, lwd = 1.5, col = "royalblue")
  }
}

###  Function for plotting the individual location
plot_loc <- function(dat, dat_outp, track_id) {
  yr <- year(dat$timestamp[1])

  plot(dat$location_long, dat$location_lat,
    main = paste(track_id, yr, sep = "_"),
    xlab = "Longitude", ylab = "Latitude", cex = 0.4
  )
  lines(dat$location_long, dat$location_lat,
    main = paste(track_id, yr, sep = "_"),
    xlab = "Longitude", ylab = "Latitude"
  )
  for (i in 1:nrow(dat_outp)) {
    points(dat_outp$V8, dat_outp$V9, pch = 4, cex = 3, col = "green4")
    points(dat_outp$V8, dat_outp$V9, pch = 19, cex = 1.5, col = "royalblue")
  }
}

### plot the net-squared displacement along with the identified parturition time
plot_nsd <- function(dat, dat_outp, track_id) {
  yr <- year(dat$timestamp[1])

  plot(dat$timestamp, dat$nsd,
    type = "l", main = paste(track_id, yr, sep = "_"),
    ylab = "Net squared displacement (km)", xlab = "Time"
  )
  lines(dat$timestamp, dat$rollnsd, col = "brown4", lwd = 1)

  for (i in 1:nrow(dat_outp)) {
    abline(v = dat_outp$V5, lty = 2, lwd = 1.5, col = "green4")
    abline(v = dat_outp$V6, lty = 4, lwd = 1.5, col = "royalblue")
  }
}


rFunction <- function(data, threshold = NULL, window = 72, yaxs_limit = 1000) {
  original_track_id_column <- mt_track_id_column(data)
  track_attribute_data <- mt_track_data(data)

  data_df <- data |>
    mutate(
      location_long = sf::st_coordinates(data)[, 1],
      location_lat = sf::st_coordinates(data)[, 2],
      trackID = mt_track_id(data),
      distance = as.numeric(mt_distance(data))
    ) |>
    as.data.frame()

  track_ids <- unique(data_df$trackID)

  dat_updt <- list()
  dat_fin_output <- list()

  #app_artifacts_base_path <- Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/")

  pdf(appArtifactPath(paste0("Parturition_vel", window, ".pdf")), width = 8, height = 12)

  par(mfrow = c(4, 3), mar = c(4, 4, 3, 1))


  for (i in 1:length(track_ids)) {
    track_id <- track_ids[i]

    animal_id <- track_attribute_data |>
      filter(
        !!rlang::sym(original_track_id_column) == track_id
      ) |>
      dplyr::select(individual_local_identifier) |>
      first()

    track_data <- data_df |>
      filter(trackID == track_id)

    tint <- as.numeric(
      as.POSIXct(max(track_data$timestamp)) - as.POSIXct(min(track_data$timestamp)),
      units = "hours"
    )

    if (nrow(track_data) > 10 & tint > window) { ## To filter individuals with very few relocations
      data_temp <- tryCatch(
        track_data |>
          mutate(
            # calculate the difference between consecutive timestamps
            # the shift is used to move the first NA in time difference to the last position so that
            # it matches with the distance column for actual speed calculation
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
            # Calculating the nsd using geosphere package to support the identified parturition
            nsd = distVincentyEllipsoid(
              cbind(location_long, location_lat),
              cbind(first(location_long), first(location_lat))
            ) / 1000,

            # moving average to be calculated over the window time
            speed = distance / as.numeric(timediff),
            rollm = rollmean(speed,
              window / median(as.numeric(timediff), na.rm = T),
              fill = NA
            ),
            rollnsd = rollmean(nsd,
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

      if (is.null(data_temp)) {
        return()
      }


      # user-passed threshold or default to mean rollm
      working_threshold <- if (!is.null(threshold)) threshold else mean(data_temp$rollm, na.rm = T)

      ### Input condition for the clustering
      data_temp$cnd <- ifelse((data_temp$speed) < working_threshold & !is.na(data_temp$speed), 1, 0)

      ### Count the sequence length and print the maximum length time
      data_temp$run <- sequence(rle(data_temp$cnd)$lengths)
      data_temp$run_positive <- as.numeric(ifelse(data_temp$cnd == 0, 0, data_temp$run))
      data_temp$crun <- abs(data_temp$run_positive - lag(data_temp$run_positive))
      data_temp$crun[nrow(data_temp)] <- data_temp$run_positive[nrow(data_temp) - 1]
      data_temp$case <- NA

      cutoff <- floor(window / median(as.numeric(data_temp$timediff), na.rm = T))

      nrun <- ifelse(is.na(tabulate(data_temp$run_positive)[cutoff + 1]), 1,
        tabulate(data_temp$run_positive)[cutoff + 1]
      )

      dat_output <- data.frame()

      for (j in 1:nrun) {
        dat_output[j, 1] <- track_id
        dat_output[j, 2] <- animal_id

        nrun_ind <- which(data_temp$crun >= cutoff - 1)

        ### Added the extra value as the rolling mean will show a earlier time compared to
        ### the actual parturition time
        index_start <- ifelse(length(nrun_ind) == 0, NA, nrun_ind[j] - data_temp$run_positive[nrun_ind[j] - 1])
        index_end <- ifelse(length(nrun_ind) == 0, NA, nrun_ind[j])

        ### Include a column for locations that satisfy the clustering scheme
        if (!is.na(index_start)) {
          data_temp$case[index_start:index_end] <- 1
        }

        dat_output[j, 3] <- ifelse(length(nrun_ind) == 0, NA, data_temp$run_positive[nrun_ind[j] - 1])
        dat_output[j, 4] <- working_threshold
        dat_output[j, 5] <- as.POSIXct(ifelse(length(nrun_ind) == 0, NA, data_temp$timestamp[index_start]), origin = "1970-01-01")
        dat_output[j, 6] <- as.POSIXct(ifelse(length(nrun_ind) == 0, NA, data_temp$timestamp[index_end]), origin = "1970-01-01")
        dat_output[j, 7] <- nrun

        if (!is.na(dat_output[j, 4])) {
          dat_output[j, 8] <- ifelse(length(nrun_ind) == 0, NA, mean(data_temp$location_long[index_start:index_end], na.rm = T)) ## Change the start
          dat_output[j, 9] <- ifelse(length(nrun_ind) == 0, NA, mean(data_temp$location_lat[index_start:index_end], na.rm = T))
        } else {
          dat_output[j, 8:9] <- NA
        }
      }

      dat_updt[[i]] <- data_temp ### append data for multiple individuals

      dat_fin_output[[i]] <- dat_output

      # plot the figures
      plot_speed(data_temp, dat_output,
        yul = yaxs_limit,
        track_id = track_id, threshold = working_threshold
      )
      plot_loc(data_temp, dat_output, track_id = track_id)
      plot_nsd(data_temp, dat_output, track_id = track_id)
    }
  }

  dev.off()

  dat_final <- do.call(rbind, dat_updt)
  dat_final$case[is.na(dat_final$case)] <- 0
  dat_final_output <- do.call(rbind, dat_fin_output)
  names(dat_final_output) <- c(
    "track_id", "individual_id", "number_of_max_reloc",
    "threshold_speed_meters_per_hour", "start_date", "end_date",
    "num_detected_events", "location_long", "location_lat"
  )

  # drop NA columns
  dat_final_output <- dat_final_output |>
    drop_na(start_date)

  # write app artefact
  write.csv(dat_final_output, appArtifactPath(paste0("Parturition_output", window, ".csv")))

  # convert the data.frame output into move2 object
  dat_final <- left_join(
    dat_final,
    track_attribute_data,
    join_by(trackID == !!original_track_id_column)
  ) |>
    dplyr::select(!one_of(!!original_track_id_column)) |>
    rename(
      !!original_track_id_column := trackID
    )


  data_move <- mt_as_move2(dat_final,
    coords = c("location_long", "location_lat"),
    time_column = "timestamp", crs = 4326,
    track_id_column = original_track_id_column,
    track_attributes = names(track_attribute_data)
  )

  return(data_move)
}
