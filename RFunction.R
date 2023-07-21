library(move)
library(zoo)
library(tidyverse)
library(readr)
library(sf)
library(rgeos)
library(units)
library(geosphere)

rFunction <-function(data, threshold=NULL, window=72, yaxs_limit=NULL){
  units_options(allow_mixed = TRUE)
  if(st_crs(crs(data))$IsGeographic){ ## using pkg units so units are kept for the future
    unt <- "m" ## latlong result is in m/s
  }else{
    unt <- st_crs(crs(data))$units ## get units from projection
  }
  if(is.null(unt)){logger.warn("It seems that the projection does not have defined units, please check the projection in the study summary, and use changeProjection if necesary")} ## THIS WARNING HAS TO BE REWRITTEN!!!!! ITS BASICALLY A PLACEHOLDER. I actually do not know if this can happen, but just in case...
  udunits_from_proj <-  list( ## function borrowed from R library "sf", and modified
    #   PROJ.4     UDUNITS
    `km` =    "km",
    `m` =      "m",
    `dm` =     "dm",
    `cm` =     "cm",
    `mm` =     "mm",
    `kmi` =    "nautical_mile",
    `in` =     "in",
    `ft` =     "ft",
    `yd` =     "yd",
    `mi` =     "mi",
    `fath` =   "fathom",
    `ch` =     "chain",
    `link` =   "link", # not (yet) existing; set in .onLoad()
    `us-in` =  "us_in",
    `us-ft` =  "US_survey_foot",
    `us-yd` =  "US_survey_yard",
    `us-ch` =  "chain",
    `us-mi` =  "US_survey_mile",
    `ind-yd` = "ind_yd",
    `ind-ft` = "ind_ft", 
    `ind-ch` = "ind_ch"
  )
  udunt <- udunits_from_proj[[unt]]
  unts <- as_units(udunt, check_is_valid = FALSE)
  data$distance <- set_units(unlist(lapply(distance(data), function(x) c(as.vector(x), NA))), unts, mode = "standard")
  class(data$distance) <-"numeric"
  data_df <-as.data.frame(data)
  names(data_df) <- gsub("[.]", "_", names(data_df))
  uid <-unique(data_df$trackId)
  
  ###  Function for plotting the individual speed
  plot_speed <-function(dat, dat_outp)
  {
    plot(dat$timestamp, dat$speed, main= uid[i], cex=0.5, #ylim=c(0,400),
         ylab= expression(paste("Distance /", Delta, "t")), xlab= "Time")
    lines(dat$timestamp, dat$speed)
    lines(dat$timestamp, dat$rollm, col ="brown", lwd=2)
    #legend('topright', legend = rp, bty = 'n')
    abline(h=mean(dat$speed, na.rm=T), lty=2, lwd=2, col= "red")
    for (i in 1:nrow(dat_outp))
    {abline(v= dat_outp$V5, lty=3, lwd=2, col= "blue")
      abline(v= dat_outp$V6, lty=3, lwd=2, col= "blue")
    }
  }
  
  ###  Function for plotting the individual location
  plot_loc <-function(dat, dat_outp)
  {
  plot(dat$location_long, dat$location_lat, main= uid[i], xlab= "Longitude", ylab= "Latitude")
  lines(dat$location_long, dat$location_lat, main= uid[i], xlab= "Longitude", ylab= "Latitude")
  for(i in 1: nrow(dat_outp))
  {points(dat_outp$V8,dat_outp$V9, pch=4, cex=4, col= "blue")
    points(dat_outp$V8,dat_outp$V9, pch=19, cex=2, col= "blue")
  }
    }
 
  ### plot the net-squared displacement along with the identified parturition time
  plot_nsd <-function(dat, dat_outp)
  {
  plot(dat$timestamp, dat$nsd, type="l",main= uid[i], 
       ylab= "Net squared displacement (km)", xlab= "Time")
    for(i in 1: nrow(dat_outp)){
      abline(v= dat_outp$V5, lty=3, lwd=2, col= "blue")
      abline(v= dat_outp$V6, lty=3, lwd=2, col= "blue")
      
    }
  }
  #dat_output <-as.data.frame(uid) ## Save the different individuals 
  #plot.new()
  dat_updt <-list()
  dat_fin_output <-list()
  
  pdf("Parturition_velney.pdf", width = 8, height = 12)
  # pdf(paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),
  #            paste("Parturition_vel",window,".pdf")), width = 8, height = 12)
  par(mfrow=c(4,3), mar=c(4,4,3,1))
  
  ## if no values are specified as threshold then use the mean as the threshold
  if(is.null(threshold)){
  for(i in 1:length(uid))
  {
    data_temp1 <-subset(data_df, data_df$trackId==uid[i])
    tint <-as.numeric(as.POSIXct(max(data_temp1$timestamp)) - as.POSIXct(min(data_temp1$timestamp)), units="hours")
    if(dim(data_temp1)[1]>10 & tint > window) ## To filter individuals with very few relocations
    { 
      ## calculates the difference between consecutive timestamp
      data_temp <-data_temp1 %>% 
        mutate(timediff = as.numeric(as.POSIXct(data_temp1$timestamp)- as.POSIXct(lag(data_temp1$timestamp)), units = "hours"))
      # the shift is used to move the first NA in time difference to the last position so that 
      # it matches with the distance column for actual speed calculation
      data_temp$timediff <-  magic::shift(data_temp$timediff, -1)
      data_temp<-subset(data_temp, timediff !=0)
      # Calculating the nsd using geosphere package to support the identified parturition 
      data_temp$nsd <- geosphere::distm(cbind(data_temp$location_long,data_temp$location_lat), 
                                        cbind(data_temp$location_long[1],data_temp$location_lat[1]), fun = geosphere::distHaversine)/1000
      data_temp <-data_temp %>% mutate(speed = distance/as.numeric(timediff)) %>%
        mutate(rollm =rollapply(speed, window/median(as.numeric(timediff), na.rm=T), mean, na.rm=T, fill=NA))
      ##moving average to be calculated over the window time
      
      ### Input condition for the clustering 
      ### used a mean criteria here can also be used a user specified cutoff 
      data_temp$cnd <- ifelse((data_temp$speed) < mean(data_temp$rollm, na.rm=T) & !is.na(data_temp$speed),1,0)
      
      ### Count the sequence length and print the maximum length time
      data_temp$run <-sequence(rle(data_temp$cnd)$lengths)
      data_temp$run_positive <- as.numeric(ifelse(data_temp$cnd == 0, 0, data_temp$run))
      cutoff<- floor(window/median(as.numeric(data_temp$timediff), na.rm=T))
      data_temp$crun <- abs(data_temp$run_positive - lag(data_temp$run_positive))
      data_temp$crun[nrow(data_temp)] <-data_temp$run_positive[nrow(data_temp)]
      
      dat_updt[[i]]<- data_temp ### append data for multiple individuals
      
      
      nrun<- ifelse(is.na(tabulate(data_temp$run_positive)[cutoff+1]),1,
             tabulate(data_temp$run_positive)[cutoff+1])
      dat_output <-data.frame()
      
      for(j in 1:nrun){
        dat_output[j,1] <- uid[i]
        #dat_output[i,2] <- unique(data_temp$local_identifier) #ERROR
        if (any(names(data_temp)=="local_identifier")) #need to account for the fact that not all data sets have the variable local_identifier or individual_local_identifier
        {
          dat_output[j,2] <- unique(data_temp$local_identifier)
        } else if (any(names(data_temp)=="individual_local_identifier"))
        {
          dat_output[j,2] <- unique(data_temp$individual_local_identifier)
        } else
        {
          logger.info("There is no standard variable for animal ID in your data set, therefore trackId is used.")
          dat_output[j,2] <- unique(data_temp$trackId)
        }
        nrun_ind <- which(data_temp$crun > cutoff)
        ### Added the extra value as the rolling mean will show a earlier time compard to 
        ### the actual parturition time
        index.start <- ifelse(length(nrun_ind)==0,NA,nrun_ind[j]-data_temp$run_positive[nrun_ind[j]-1])
        index.end   <- ifelse(length(nrun_ind)==0,NA,nrun_ind[j])
        
        dat_output[j,3] <- ifelse(length(nrun_ind)==0,NA,data_temp$run_positive[nrun_ind[j]-1])
        dat_output[j,4] <- mean(data_temp$rollm, na.rm=T)
        dat_output[j,5] <- ifelse(length(nrun_ind)==0,NA,data_temp$timestamp[index.start])
        dat_output[j,6] <- ifelse(length(nrun_ind)==0,NA,data_temp$timestamp[index.end])
        dat_output[j,7] <- nrun
        if(!is.na(dat_output[j,4])){
          dat_output[j,8] <- ifelse(length(nrun_ind)==0,NA,mean(data_temp$location_long[index.start:index.end], na.rm=T)) ##Change the start
          dat_output[j,9] <- ifelse(length(nrun_ind)==0,NA,mean(data_temp$location_lat[index.start:index.end], na.rm=T))
        } else
        {
          dat_output[j, 8:9]<-NA
        }
      }
      
      dat_fin_output[[i]] <- dat_output
      ### Plot the step length with identified parturition time
      # rp = vector('expression',2)
      # rp[1] = substitute(expression("Parturition time"))[2]
      # rp[2] = ifelse(as.character(data_temp$timestamp[index.start])=="NULL", 
      #                as.character(data_temp$timestamp[index.end]),
      #                as.character(data_temp$timestamp[index.start]))
       
   #plot the  figures    
      plot_speed(data_temp, dat_output)
      plot_loc(data_temp, dat_output)
      plot_nsd(data_temp, dat_output)
          
    }
  }
  }
  else
  {
    for(i in 1:length(uid))
    {
      data_temp1 <-subset(data_df, data_df$trackId==uid[i])
      tint <-as.numeric(as.POSIXct(max(data_temp1$timestamp)) - as.POSIXct(min(data_temp1$timestamp)), units="hours")
      if(dim(data_temp1)[1]>10 & tint > window) ## To filter individuals with very few relocations
      {
        ## calculates the difference between consecutive timestamp
        data_temp <-data_temp1 %>% 
          mutate(timediff = as.numeric(as.POSIXct(data_temp1$timestamp)- as.POSIXct(lag(data_temp1$timestamp)), units = "hours"))
        # the shift is used to move the first NA in time difference to the last position so that 
        # it matches with the distance column for actual speed calculation
        data_temp$timediff <-  magic::shift(data_temp$timediff, -1) 
        data_temp<-subset(data_temp, timediff !=0)
        # Calculating the nsd using geosphere package to support the identified parturition 
        data_temp$nsd <- geosphere::distm(cbind(data_temp$location_long,data_temp$location_lat), 
                                          cbind(data_temp$location_long[1],data_temp$location_lat[1]), fun = geosphere::distHaversine)/1000
        data_temp <-data_temp %>% mutate(speed = distance/as.numeric(timediff)) %>%
          mutate(rollm =rollapply(speed, window/median(as.numeric(timediff), na.rm=T), mean, na.rm=T, fill=NA))
        ##moving average to be calculated over the window time
        
        ### Input condition for the clustering 
        ### used a mean criteria here can also be used a user specified cutoff 
        data_temp$cnd <- ifelse((data_temp$speed) < threshold & !is.na(data_temp$speed),1,0)
        
        ### Count the sequence length and print the maximum length time
        data_temp$run <-sequence(rle(data_temp$cnd)$lengths)
        data_temp$run_positive <- as.numeric(ifelse(data_temp$cnd == 0, 0, data_temp$run))
        cutoff<- floor(window/median(as.numeric(data_temp$timediff), na.rm=T))
        
        dat_updt[[i]]<- data_temp ### append data for multiple individuals
        
        index.start <- which.max(data_temp$run_positive)-max(data_temp$run_positive)+1
        index.end   <- which.max(data_temp$run_positive)
          
        #dat_output[i,2] <- unique(data_temp$local_identifier) #ERROR
        if (any(names(data_temp)=="local_identifier")) #need to account for the fact that not all data sets have the variable local_identifier or individual_local_identifier
        {
          dat_output[i,2] <- unique(data_temp$local_identifier)
        } else if (any(names(data_temp)=="individual_local_identifier"))
        {
          dat_output[i,2] <- unique(data_temp$individual_local_identifier)
        } else
        {
          logger.info("There is no standard variable for animal ID in your data set, therefore trackId is used.")
          dat_output[i,2] <- unique(data_temp$trackId)
        }
        
        dat_output[i,3] <- max(data_temp$run_positive)
        dat_output[i,4] <- threshold
        dat_output[i,5] <- data_temp$timestamp[index.start]
        dat_output[i,6] <- data_temp$timestamp[index.end]
        dat_output[i,7] <- ifelse(is.na(tabulate(data_temp$run_positive)[cutoff+1]),1,
                                  tabulate(data_temp$run_positive)[cutoff+1])
        if(!is.na(dat_output[i,4])){
          dat_output[i,8] <- data_temp$location_long[index.start] ##Change the start
          dat_output[i,9] <- data_temp$location_lat[index.start]
        } else
        {
          dat_output[i, 8:9]<-NA
        }
        
        dat_fin_output[[i]] <- dat_output
        ### Plot the step length with identified parturition time
        # rp = vector('expression',2)
        # rp[1] = substitute(expression("Parturition time"))[2]
        # rp[2] = ifelse(as.character(data_temp$timestamp[index.start])=="NULL", 
        #                as.character(data_temp$timestamp[index.end]),
        #                as.character(data_temp$timestamp[index.start]))
        
        #plot the  figures    
        plot_speed(data_temp, dat_output)
        plot_loc(data_temp, dat_output)
        plot_nsd(data_temp, dat_output)
        
        # plot(data_temp$timestamp, data_temp$speed, main= uid[i], cex=0.5,
        #      ylab= expression(paste("Distance /", Delta, "t")), xlab= "Time")
        # lines(data_temp$timestamp, data_temp$speed)
        # lines(data_temp$timestamp, data_temp$rollm, col ="grey30", lwd=2)
        # legend('topright', legend = rp, bty = 'n')
        # abline(h=mean(data_temp$speed, na.rm=T), lty=2, lwd=2, col= "red")
        # abline(v= data_temp$timestamp[index.end], lty=3, lwd=2, col= "blue")
        # abline(v= data_temp$timestamp[index.start], lty=3, lwd=2, col= "blue")
        # 
        # ### plot the spatial location with identified parturirion location
        # plot(data_temp$location_long, data_temp$location_lat, main= uid[i], xlab= "Longitude", ylab= "Latitude")
        # lines(data_temp$location_long, data_temp$location_lat, main= uid[i], xlab= "Longitude", ylab= "Latitude")
        # points(mean(data_temp$location_long[index.start:index.end]),
        #        mean(data_temp$location_lat[index.start:index.end]), pch=4, cex=4, col= "blue")
        # points(mean(data_temp$location_long[index.start:index.end]),
        #        mean(data_temp$location_lat[index.start:index.end]), pch=19, cex=2, col= "blue")
        # 
        # ### plot the net-squared displacement along with the identified parturition time
        # plot(data_temp$timestamp, data_temp$nsd, type="l",main= uid[i], 
        #      ylab= "Net squared displacement (km)", xlab= "Time")
        # abline(v= data_temp$timestamp[index.end], lty=3, lwd=2, col= "blue")
        # abline(v= data_temp$timestamp[index.start], lty=3, lwd=2, col= "blue")
      }
    }
    
  }
  dev.off()
  names(dat_output) <-c("Track_id", "Individual_id", "Number_of_max_reloc","Threshold_speed(m/h)",
                        "Start_date", "End_date", "Numbers_of\ndetected_events","location_long", "location_lat")
  
  dat_final <-do.call(rbind,dat_updt)
  dat_final_output <- do.call(rbind, dat_fin_output)
  names(dat_final) <- make.names(names(dat_final),allow_=FALSE)
  # write.csv(dat_final_output, file= paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),
  #                                    paste("Parturition_output",window,".csv")))
  write.csv(dat_output,"Parturition_outputy.csv")
  
  ###Converting the data.frame output into move-stack object
  data_move <- move(x=dat_final$location.long, y=dat_final$location.lat, 
                time=as.POSIXct(dat_final$timestamp,format="%Y-%m-%d %H:%M:%S"), 
                data=dat_final, proj=CRS("+proj=longlat +ellps=WGS84"),
                animal=dat_final$trackId)
  
  return(data_move)
  
 
}
