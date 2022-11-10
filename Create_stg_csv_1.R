
# Chapter 1: Extract sleep stage and epochs -------

library(data.table)
library(caret)
library(tidyverse)
library(signal)
library(lubridate)
library(readxl)
library(rvest)
library(stringr)

rm(list=ls())
sub_ls = c(5,6) #done TBI_1, TBI_2, TBI_3,
general <- "Z:/TBI Sleep/Pilot_data/PSG/"
for (sub in sub_ls){
  setwd(paste0(general,"TBI_",sub,"/labeled/"))
  dt <- read_html("Epoch Report.html")
  # remember to move Epoch Report.html out at the folder "labeled"
  dtnode <- html_nodes(dt,"table")
  #dtnode
  dt_all <- data.table()
  for (i in 1:(length(dtnode)-3)){
    dt_node_table <- html_table(dtnode, header = TRUE, fill = TRUE)[[(i+3)]]
    stg<-dt_node_table$Stg
    epoch<-dt_node_table$`#`
    #browser()
    dt_current <- data.frame(epoch,stg)
    dt_all <- rbind(dt_all,dt_current)
    # ls_stg<-append(ls_stg,stg)
    # ls_epc<-append(ls_epc,epoch)
    # ls_stg[[i]] <- stg
    # ls_epc[[i]] <- epoch
  }
  
  #browser()
  setwd(general)
  psg_time <- fread("PSG_start_stop.csv")
  start_time <- psg_time[Sub %in% paste0("TBI_",sub),`Lights out` ]
  end_time <- psg_time[Sub %in% paste0("TBI_",sub),`Lights on` ]
  start_time_pt <- as.POSIXct(start_time, format = "%m/%d/%YT%I:%M:%S %p", tz = "America/Chicago")
  end_time_pt <- as.POSIXct(end_time, format = "%m/%d/%YT%I:%M:%S %p", tz = "America/Chicago")
  dt_all_rm<-dt_all[!(stg %in% "L"),]
  dt_all_rm[,new_epoch := 1:nrow(dt_all_rm)]
  dt_all_rm[1,timestamp := as.POSIXct(start_time_pt)]
  dt_all_rm[, epoch_lag := shift(new_epoch,1,type="lag")]
  dt_all_rm[2:nrow( dt_all_rm),timestamp := (epoch_lag*30 + start_time_pt)]
  end_time_diff<-dt_all_rm[nrow( dt_all_rm),timestamp] - end_time_pt
  dt_all_rm$epoch_lag <- NULL
  
  setwd(paste0(general,"TBI_",sub,"/labeled/"))
  fwrite( dt_all_rm,"stg.csv")
}

