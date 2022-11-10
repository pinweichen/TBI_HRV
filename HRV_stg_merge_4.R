# Merge stage data and ANNE data
library(data.table)
library(caret)
library(tidyverse)
library(signal)
library(lubridate)
library(readxl)
library(fuzzyjoin)
rm(list=ls())
# Actigraph data guide =======================
# Actigraph data required "Feature Extraction" from ActiLife software. 
# Select x,y,z,vector mag.for data process of both time and frequnecy doman
# Time domain: mean,sd, median crossing, all coorelations, 25th and 75th percentile
# Frequency domain: Dominant Frequency and Dom freq magnitude


#path = "/Volumes/RTO/SIESTA"
general <- "Z:/TBI Sleep"
data_path <- paste0(general,"/Pilot_data")
ANNE_p <- paste0(data_path,"/ANNE")
feat_p <- paste0(data_path,"/Preprocessed/ANNE/HRV/")
level2_p <- paste0(data_path,"/Preprocessed/level_2/")
actigraph_p <- paste0(data_path,"/Preprocessed/Actigraph/")

setwd(feat_p)
missing <- fread("missing_summary.csv")
load_list <- missing$sub
setwd(feat_p)
dt_ls <- list()

load_list <- c("TBI_1","TBI_2", "TBI_3" , "TBI_5", "TBI_6")

for (i in 1:length(load_list)){
  dt_name_p <- load_list[i]
  setwd(paste0(feat_p,dt_name_p,"/"))
  dt_pre <- fread(paste0(dt_name_p,"_all.csv"))
  dt_pre[,sub := dt_name_p]
  dt_pre[,timestamp := as.POSIXct(timestamp,tz = "America/Chicago")]
  attr(dt_pre$timestamp,"tzone") <- "America/Chicago"
  # load sleep stage data
  PSG_p <- paste0(data_path,"/PSG/")
  stg_p <- paste0(PSG_p,"/",dt_name_p,"/labeled/")
  setwd(stg_p)
  stg_dt <- fread("stg.csv")
  stg_dt[,timestamp := as.POSIXct(timestamp,tz = "America/Chicago")]
  attr(dt_pre$timestamp,"tzone") <- "America/Chicago"
  # dt_pre <-merge_asof(dt_pre,stg_dt,by.x="timestamp",by.y = "timestamp", all = T)
  #browser()
  dt_pre_new<- as.data.table(stg_dt)[dt_pre, on = .(timestamp), roll = TRUE]
  #dt_pre_rm<-dt_pre_new[!(t_start %in% NA),]
  dt_pre_new[stg %in% NA, epoch := -1]
  dt_pre_new[stg %in% NA, stg := "W"]
  
  # Load Actigraph data
  acti_dt_p <- paste0(actigraph_p,dt_name_p,"/")
  setwd(acti_dt_p)
  acti_dt <- fread(paste0(dt_name_p,"-FE.csv"))
  colnames(acti_dt)<-paste(acti_dt[1,],acti_dt[2,],acti_dt[3,],sep = ".")
  acti_dt<-acti_dt[4:nrow(acti_dt),]
  acti_dt[,timestamp := as.POSIXct(Domain.Array.Feature,format = "%m/%d/%Y %I:%M:%S %p", tz = "America/Chicago")]
  attr(acti_dt$timestamp,"tzone") <- "America/Chicago"
  acti_dt$Domain.Array.Feature<-NULL
  acti_dt$NA.NA.NA <- NULL
  dt_pre_acti<- as.data.table(acti_dt)[dt_pre_new, on = .(timestamp), roll = TRUE]
  #dt_pre <- merge(dt_pre,stg_dt,by.x="timestamp",by.y = "timestamp", all = T)
  dt_ls[[i]] <- dt_pre_acti
} 

dt_pre_all <- do.call(rbind,dt_ls)
setwd(level2_p)
fwrite(dt_pre_all, "dt_all.csv")
