library(zoo)
library(tidyverse)
library(readr)

rFunction <-function(data, threshold=NULL, window=72){
  
   
  data_df <-as.data.frame(data)
  uid <-unique(data_df$tag_local_identifier)
  
  dat_output <-as.data.frame(uid) ## Save the different individuals 
  plot.new()
  
  pdf("Partuition_vel.pdf", width = 8, height = 12)
  par(mfrow=c(4,3), mar=c(4,4,3,1))
  
  ## if no values are specified as threshold then use the mean as the threshold
  if(is.null(threshold)){
  for(i in 1:length(uid))
  {
    data_temp1 <-subset(data_df, data_df$tag_local_identifier==uid[i])
    if(dim(data_temp1)[1]>10) ## To filter individuals with very few relocations
    {
      ## calculates the difference between consecutive timestamp
      data_temp <-data_temp1 %>% mutate(timediff = timestamp- lag(timestamp))
      # the shift is used to move the first NA in time difference to the last position so that 
      # it matches with the distance column for actual speed calculation
      data_temp$timediff <-  magic::shift(data_temp$timediff, -1) 
      data_temp <-data_temp %>% mutate(speed = distance/as.numeric(timediff)) %>%
        mutate(rollm =rollapply(speed, 72/median(as.numeric(timediff), na.rm=T), mean, na.rm=T, fill=NA))
      ##moving average to be calculated over the window time
      
      ### Input condition for the clustering 
      ### used a mean criteria here can also be used a user specified cutoff 
      data_temp$cnd <- ifelse((data_temp$speed) < mean(data_temp$rollm, na.rm=T) & !is.na(data_temp$speed),1,0)
      
      ### Count the sequence length and print the maximum length time
      data_temp$run <-sequence(rle(data_temp$cnd)$lengths)
      data_temp$run_positive <- ifelse(data_temp$cnd == 0, 0, data_temp$run)
      
      dat_output[i,2] <- max(data_temp$run_positive)
      dat_output[i,3] <- data_temp$timestamp[which.max(data_temp$run_positive)-max(data_temp$run_positive)]
      dat_output[i,4] <- data_temp$timestamp[which.max(data_temp$run_positive)]
      if(!is.na(dat_output[i,3])){
        dat_output[i,5] <- data_temp$location_long[which.max(data_temp$run_positive)-max(data_temp$run_positive)]
        dat_output[i,6] <- data_temp$location_lat[which.max(data_temp$run_positive)-max(data_temp$run_positive)]
      } else
      {
        dat_output[i, 5:6]<-NA
      }
      
      plot(data_temp$timestamp, data_temp$distance, main= uid[i])
      lines(data_temp$timestamp, data_temp$distance)
      abline(h=mean(data_temp$speed, na.rm=T)*median(as.numeric(data_temp$timediff), na.rm=T), lty=2, lwd=2, col= "red")
      abline(v= data_temp$timestamp[which.max(data_temp$run_positive)], lty=3, lwd=2, col= "blue")
      abline(v= data_temp$timestamp[which.max(data_temp$run_positive)-max(data_temp$run_positive)], lty=3, lwd=2, col= "blue")
    }
  }
  }
  else
  {
    for(i in 1:length(uid))
    {
      data_temp1 <-subset(data_df, data_df$tag_local_identifier==uid[i])
      if(dim(data_temp1)[1]>10) ## To filter individuals with very few relocations
      {
        ## calculates the difference between consecutive timestamp
        data_temp <-data_temp1 %>% mutate(timediff = timestamp- lag(timestamp))
        # the shift is used to move the first NA in time difference to the last position so that 
        # it matches with the distance column for actual speed calculation
        data_temp$timediff <-  magic::shift(data_temp$timediff, -1) 
        data_temp <-data_temp %>% mutate(speed = distance/as.numeric(timediff)) %>%
          mutate(rollm =rollapply(speed, 72/median(as.numeric(timediff), na.rm=T), mean, na.rm=T, fill=NA))
        ##moving average to be calculated over the window time
        
        ### Input condition for the clustering 
        ### used a mean criteria here can also be used a user specified cutoff 
        data_temp$cnd <- ifelse((data_temp$speed) < threshold & !is.na(data_temp$speed),1,0)
        
        ### Count the sequence length and print the maximum length time
        data_temp$run <-sequence(rle(data_temp$cnd)$lengths)
        data_temp$run_positive <- ifelse(data_temp$cnd == 0, 0, data_temp$run)
        
        dat_output[i,2] <- max(data_temp$run_positive)
        dat_output[i,3] <- data_temp$timestamp[which.max(data_temp$run_positive)-max(data_temp$run_positive)]
        dat_output[i,4] <- data_temp$timestamp[which.max(data_temp$run_positive)]
        if(!is.na(dat_output[i,3])){
          dat_output[i,5] <- data_temp$location_long[which.max(data_temp$run_positive)-max(data_temp$run_positive)]
          dat_output[i,6] <- data_temp$location_lat[which.max(data_temp$run_positive)-max(data_temp$run_positive)]
        } else
        {
          dat_output[i, 5:6]<-NA
        }
        
        plot(data_temp$timestamp, data_temp$distance, main= uid[i])
        lines(data_temp$timestamp, data_temp$distance)
        abline(h=mean(data_temp$speed, na.rm=T)*median(as.numeric(data_temp$timediff), na.rm=T), lty=2, lwd=2, col= "red")
        abline(v= data_temp$timestamp[which.max(data_temp$run_positive)], lty=3, lwd=2, col= "blue")
        abline(v= data_temp$timestamp[which.max(data_temp$run_positive)-max(data_temp$run_positive)], lty=3, lwd=2, col= "blue")
      }
    }
    
  }
  dev.off()
  
  names(dat_output) <-c("Individual_id", "Number_of_max_reloc", "Start_date", "End_date", "location_long", "location_lat")
  write.csv(dat_output, "Partuition_date_output.csv")
}
