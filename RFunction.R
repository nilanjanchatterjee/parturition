library(move2)
library(zoo)
library(tidyverse, quietly = TRUE)
library(sf)
library(units)
library(magic)
library(geosphere)
library(lubridate)

rFunction <-function(data, threshold=NULL, window=72, yaxs_limit=1000){
  
   data <- data |> mutate(location_long = sf::st_coordinates(data)[,1],
                         location_lat = sf::st_coordinates(data)[,2],
                         trackID = mt_track_id(data),
                         distance = mt_distance(data))

   #data$distance <- mt_distance(data, units="m") # like this units will always be in meters, please adjust to the units you see most fit
  class(data$distance) <-"numeric"
  data_df <-as.data.frame(data)
  names(data_df) <- gsub("[.]", "_", names(data_df))
  # uid <-unique(data_df$individual_local_identifier)
  uid <-unique(data$trackID) 
  
  ###  Function for plotting the individual speed
  plot_speed <-function(dat, dat_outp, yul=yaxs_limit)
  {
    yr <- year(dat$timestamp[1])
    plot(dat$timestamp, dat$speed, main= paste(uid[i], yr, sep = "_"), cex=0.4, ylim=c(0,yul),
         ylab= expression(paste("Distance /", Delta, "t")), xlab= "Time", col= "grey40")
    lines(dat$timestamp, dat$speed,col= "grey30", main= paste(uid[i], yr, sep = "_"))
    lines(dat$timestamp, dat$rollm, col ="brown4", lwd=1.5, main= paste(uid[i], yr, sep = "_"))
    #legend('topright', legend = rp, bty = 'n')
    abline(h=mean(dat$speed, na.rm=T), lty=3, lwd=2, col= "coral")
    for (i in 1:nrow(dat_outp))
    {abline(v= dat_outp$V5, lty=2, lwd=1.5, col= "green4")
      abline(v= dat_outp$V6, lty=4, lwd=1.5, col= "royalblue")
    }
  }
  
  ###  Function for plotting the individual location
  plot_loc <-function(dat, dat_outp)
  {
    yr <- year(dat$timestamp[1])
  plot(dat$location_long, dat$location_lat, main= paste(uid[i], yr, sep = "_"), 
       xlab= "Longitude", ylab= "Latitude", cex=0.4)
  lines(dat$location_long, dat$location_lat, main= paste(uid[i], yr, sep = "_"), 
        xlab= "Longitude", ylab= "Latitude")
  for(i in 1: nrow(dat_outp))
  {points(dat_outp$V8,dat_outp$V9, pch=4, cex=3, col= "green4")
    points(dat_outp$V8,dat_outp$V9, pch=19, cex=1.5, col= "royalblue")
  }
    }
 
  ### plot the net-squared displacement along with the identified parturition time
  plot_nsd <-function(dat, dat_outp)
  {
    yr <- year(dat$timestamp[1])
  plot(dat$timestamp, dat$nsd, type="l",main= paste(uid[i], yr, sep = "_"), 
       ylab= "Net squared displacement (km)", xlab= "Time")
  lines(dat$timestamp, dat$rollnsd, col ="brown4", lwd=1)
    for(i in 1: nrow(dat_outp)){
      abline(v= dat_outp$V5, lty=2, lwd=1.5, col= "green4")
      abline(v= dat_outp$V6, lty=4, lwd=1.5, col= "royalblue")
      
    }
  }
  #dat_output <-as.data.frame(uid) ## Save the different individuals 
  #plot.new()
  dat_updt <-list()
  dat_fin_output <-list()
  
  #pdf("Parturition_velney.pdf", width = 8, height = 12)
  pdf(paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),
              paste("Parturition_vel",window,".pdf")), width = 8, height = 12)
  par(mfrow=c(4,3), mar=c(4,4,3,1))
  
  ## if no values are specified as threshold then use the mean as the threshold
  if(is.null(threshold)){
  for(i in 1:length(uid))
  {
    data_temp1 <-subset(data_df, data_df$trackID ==uid[i])
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
      data_temp$nsd <- distVincentyEllipsoid(cbind(data_temp$location_long,data_temp$location_lat), 
                                             cbind(data_temp$location_long[1],data_temp$location_lat[1]))/1000
      
      data_temp <-data_temp %>% mutate(speed = distance/as.numeric(timediff)) %>%
        mutate(rollm =rollapply(speed, window/median(as.numeric(timediff), na.rm=T), mean, na.rm=T, fill=NA),
               rollnsd = rollapply(nsd, window/median(as.numeric(timediff), na.rm=T), mean, na.rm=T, fill=NA))
      ##moving average to be calculated over the window time
      
      ### Input condition for the clustering 
      ### used a mean criteria here can also be used a user specified cutoff 
      data_temp$cnd <- ifelse((data_temp$speed) < mean(data_temp$rollm, na.rm=T) & !is.na(data_temp$speed),1,0)
      
      ### Count the sequence length and print the maximum length time
      data_temp$run <-sequence(rle(data_temp$cnd)$lengths)
      data_temp$run_positive <- as.numeric(ifelse(data_temp$cnd == 0, 0, data_temp$run))
      cutoff<- floor(window/median(as.numeric(data_temp$timediff), na.rm=T))
      data_temp$crun <- abs(data_temp$run_positive - lag(data_temp$run_positive))
      data_temp$crun[nrow(data_temp)] <-data_temp$run_positive[nrow(data_temp)-1]
      
      
      
      nrun<- ifelse(is.na(tabulate(data_temp$run_positive)[cutoff+1]),1,
             tabulate(data_temp$run_positive)[cutoff+1])
      dat_output <-data.frame()
      
      for(j in 1:nrun){
        dat_output[j,1] <- uid[i]
        dat_output[i,2] <- unique(data_temp$trackID) #ERROR
        # if (any(names(data_temp)=="local_identifier")) #need to account for the fact that not all data sets have the variable local_identifier or individual_local_identifier
        # {
        #   dat_output[j,2] <- unique(data_temp$local_identifier)
        # } else if (any(names(data_temp)=="individual_local_identifier"))
        # {
        #   dat_output[j,2] <- unique(data_temp$individual_local_identifier)
        # } else
        # {
        #   logger.info("There is no standard variable for animal ID in your data set, therefore trackId is used.")
        #   dat_output[j,2] <- unique(data_temp$trackId)
        # }
        nrun_ind <- which(data_temp$crun >= cutoff-1)
        ### Added the extra value as the rolling mean will show a earlier time compard to 
        ### the actual parturition time
        index.start <- ifelse(length(nrun_ind)==0,NA,nrun_ind[j]-data_temp$run_positive[nrun_ind[j]-1])
        index.end   <- ifelse(length(nrun_ind)==0,NA,nrun_ind[j])
        
        ### Include a column for locations that satisfy the clustering scheme
        data_temp$case <- NA
        if(!is.na(index.start)){data_temp$case[index.start:index.end] <-  1}
        
        dat_output[j,3] <- ifelse(length(nrun_ind)==0,NA,data_temp$run_positive[nrun_ind[j]-1])
        dat_output[j,4] <- mean(data_temp$rollm, na.rm=T)
        dat_output[j,5] <- as.POSIXct(ifelse(length(nrun_ind)==0,NA,data_temp$timestamp[index.start]), origin = "1970-01-01")
        dat_output[j,6] <- as.POSIXct(ifelse(length(nrun_ind)==0,NA,data_temp$timestamp[index.end]), origin = "1970-01-01")
        dat_output[j,7] <- nrun
        if(!is.na(dat_output[j,4])){
          dat_output[j,8] <- ifelse(length(nrun_ind)==0,NA,mean(data_temp$location_long[index.start:index.end], na.rm=T)) ##Change the start
          dat_output[j,9] <- ifelse(length(nrun_ind)==0,NA,mean(data_temp$location_lat[index.start:index.end], na.rm=T))
        } else
        {
          dat_output[j, 8:9]<-NA
        }
      }
      
      dat_updt[[i]]<- data_temp ### append data for multiple individuals
      
      dat_fin_output[[i]] <- dat_output
      
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
      data_temp1 <-subset(data_df, data_df$trackID==uid[i])
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
        data_temp$nsd <- distVincentyEllipsoid(cbind(data_temp$location_long,data_temp$location_lat), 
                                          cbind(data_temp$location_long[1],data_temp$location_lat[1]))/1000
        
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
        data_temp$crun <- abs(data_temp$run_positive - lag(data_temp$run_positive))
        data_temp$crun[nrow(data_temp)] <-data_temp$run_positive[nrow(data_temp)-1]
        
        
        nrun<- ifelse(is.na(tabulate(data_temp$run_positive)[cutoff+1]),1,
                      tabulate(data_temp$run_positive)[cutoff+1])
        dat_output <-data.frame()
        
        for(j in 1:nrun){
          dat_output[j,1] <- uid[i]
          dat_output[j,2] <- unique(data_temp$trackID) #ERROR
          # if (any(names(data_temp)=="local_identifier")) #need to account for the fact that not all data sets have the variable local_identifier or individual_local_identifier
          # {
          #   dat_output[j,2] <- unique(data_temp$local_identifier)
          # } else if (any(names(data_temp)=="individual_local_identifier"))
          # {
          #   dat_output[j,2] <- unique(data_temp$individual_local_identifier)
          # } else
          # {
          #   logger.info("There is no standard variable for animal ID in your data set, therefore trackId is used.")
          #   dat_output[j,2] <- unique(data_temp$trackId)
          # }
          nrun_ind <- which(data_temp$crun >= cutoff)
          ### Added the extra value as the rolling mean will show a earlier time compard to 
          ### the actual parturition time
          index.start <- ifelse(length(nrun_ind)==0,NA,nrun_ind[j]-data_temp$run_positive[nrun_ind[j]-1])
          index.end   <- ifelse(length(nrun_ind)==0,NA,nrun_ind[j])
          
          ### Include a column for locations that satisfy the clustering scheme
          data_temp$case <- NA
          if(!is.na(index.start)){data_temp$case[index.start:index.end] <-  1}
          
          dat_output[j,3] <- ifelse(length(nrun_ind)==0,NA,data_temp$run_positive[nrun_ind[j]-1])
          dat_output[j,4] <- mean(data_temp$rollm, na.rm=T)
          dat_output[j,5] <- as.POSIXct(ifelse(length(nrun_ind)==0,NA,data_temp$timestamp[index.start]), origin = "1970-01-01")
          dat_output[j,6] <- as.POSIXct(ifelse(length(nrun_ind)==0,NA,data_temp$timestamp[index.end]), origin = "1970-01-01")
          dat_output[j,7] <- nrun
          if(!is.na(dat_output[j,4])){
            dat_output[j,8] <- ifelse(length(nrun_ind)==0,NA,mean(data_temp$location_long[index.start:index.end], na.rm=T)) ##Change the start
            dat_output[j,9] <- ifelse(length(nrun_ind)==0,NA,mean(data_temp$location_lat[index.start:index.end], na.rm=T))
          } else
          {
            dat_output[j, 8:9]<-NA
          }
        }
        
        dat_updt[[i]]<- data_temp ### append data for multiple individuals
        
        dat_fin_output[[i]] <- dat_output
        
        #plot the  figures    
        plot_speed(data_temp, dat_output)
        plot_loc(data_temp, dat_output)
        plot_nsd(data_temp, dat_output)
        
        
      }
    }
    
  }
  dev.off()
    
  dat_final <-do.call(rbind,dat_updt)
  dat_final$segid[is.na(dat_final$segid)]<-0
  dat_final_output <- do.call(rbind, dat_fin_output)
  names(dat_final_output) <-c("Track_id", "Individual_id", "Number_of_max_reloc","Threshold_speed(m/h)",
                              "Start_date", "End_date", "Numbers_of\ndetected_events","location_long", "location_lat")
  
  names(dat_final) <- make.names(names(dat_final),allow_=FALSE)
  
  ### Drop NA columns
  dat_final_output <-  dat_final_output |> drop_na(Start_date)
  
  write.csv(dat_final_output, file= paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),
                                       paste("Parturition_output",window,".csv")))
  #write.csv(dat_final_output,"Parturition_output2510.csv")
  
  ###Converting the data.frame output into move-stack object
  data_move <- mt_as_move2(dat_final, coords = c("location.long", "location.lat"),
                           time_column = "timestamp", crs = 4326, 
                           track_id_column = "trackID")
  
  return(data_move)
  
 
}
