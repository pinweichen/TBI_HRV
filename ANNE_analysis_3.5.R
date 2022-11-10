#ANNE data analysis # 1
library(data.table)
library(caret)
library(tidyverse)
library(signal)
library(lubridate)
library(readxl)
rm(list=ls())

# Screen existed ANNE folders ---------------------------------------------

#path = "/Volumes/RTO/SIESTA"
path = "Z:/TBI Sleep/"
ANNE_p <- paste0(path,"/Pilot_data/ANNE")
setwd(ANNE_p)
ls_fd = list.files(pattern = "TBI*")
ls_subfd <- list()

for (it in 1:length(ls_fd)){
  p <- ls_fd[it]
  setwd(ANNE_p)
  lsf =  list.dirs(path = ".", full.names = F, recursive = F)
  #browser()
  if (length(lsf) >0){
    lsfa <- list()
    
    pa <- ls_fd[it]
    paf<-paste0(ANNE_p,"/",pa,"/transcribe")
    setwd(paf)
    ls_subfile <- list.files(path = ".", full.names = F, recursive = F)
    if (length(ls_subfile) > 0) {
      dt <-data.table(files = ls_subfile, 
                      subfolder = pa)
      lsfa <- append(lsfa, list(dt), after = length(lsfa))
    } else {
      dt <-data.table(files = "N/A", 
                      subfolder = pa)
      lsfa <- append(lsfa, list(dt), after = length(lsfa))
    }
      
    
    lsfadt <- do.call(rbind,lsfa)
    setDT(lsfadt)
    lsfadt<-lsfadt[,subject := p]
    ls_subfd <- append(ls_subfd, list(lsfadt), after = length(ls_subfd))
  }
}
sbdt <- do.call(rbind,ls_subfd)
sbdt <- na.omit(sbdt)

preprocessed_p <- sbdt[files %in% "processed_vital.csv", ]
# Load processed data list  ------------------
ANNE_hrv_p <- paste0(path,"/Pilot_data/Preprocessed/ANNE/HRV/")
setwd(ANNE_hrv_p)
#ls_fd_pre = list.files(pattern = "SIESTA*")
#ls_subfd_pre <- list()
lsf =   list.files(pattern = "TBI*")
#browser()
ls_subfd <- list()
lsfa <- data.table()
for (it in 1:length(lsf)){
  p <- lsf[it]
  paf<-paste0(ANNE_hrv_p,"/",p)
  setwd(paf)
  lsfd_ =  list.files(path = ".", full.names = F, recursive = F)
  #browser()
  if (length(lsfd_) >0){
    #browser()
    ls_subfile <- list.files(path = ".", full.names = F, recursive = F)
    if (length(ls_subfile) > 0) {
      dt <-data.table(files = ls_subfile, 
                      subfolder = p)
      #browser()
      lsfa <- rbind(lsfa,dt)
    } else {
      dt <-data.table(files = "N/A", 
                      subfolder = p)
      lsfa <- rbind(lsfa,dt)
    }
    
  }
}

lsfa<-lsfa[files %in% "HRV_results_allwindows.csv",]

setwd(ANNE_hrv_p)
fwrite(lsfa,"hrv_processed_list.csv")

# Grab vital sign data -------------------------------------

rm(list=ls())
path = "Z:/TBI Sleep/"
ANNE_p <- paste0(path,"/Pilot_data/ANNE")
ANNE_hrv_p <- paste0(path,"/Pilot_data/Preprocessed/ANNE/HRV/")
setwd(ANNE_hrv_p)
lsfadt <- fread("hrv_processed_list.csv")

ls_start_date <- {}
ls_start_time <- {}
ls_amount_dt <- {}

# Mode function 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

lssdt<-data.table()
ls_miss_vital<-list()
# finished 1:142
for (n in 1:nrow(lsfadt)){
  #n = 500
  subf <- lsfadt[[n, 1]]
  sub <- lsfadt[[n, 2]]
  file_p <- paste0(ANNE_p,"/",sub,"/transcribe")
  setwd(file_p)
  if (length(list.files(path = file_p, pattern = "processed_vital.csv")) >0) {
    vital <- fread("processed_vital.csv")
    vital$`Epoch(ms)` <- as.POSIXct((vital$'Epoch(ms)')/1000, origin = '1970-1-1', tz="America/Chicago")
    #  ecg_dt <- fread("processed_chest_ecg.csv")
    #browser()
    # Reorganized vital sign into 30seconds interval
    div = 60 # 30 seconds / 0.5 seconds (vital data frequency) = 60
    feature_create <- function(x) {
      x[,.(timestamp = min(`Epoch(ms)`),
           time_ind = min(as.numeric(`Time(ms)`))/1000,
           HR_vital = mean(`HR(bpm)`),
           sd_HR = sd(`HR(bpm)`),
           QRS_width = mean(`QRS Width(ms)`),
           QRS_amp = mean(`QRS Amplitude(mV)`),
           PR_vital = mean(`PR(bpm)`),
           spo2 = mean(`SpO2(%)`),
           sd_spo2 = sd(`SpO2(%)`),
           spo2_ratio = mean(`SpO2 Ratio`),
           pulse_amp = mean(`Pulse Amplitude`),
           pulse_base = mean(`Pulse Baseline`),
           respiratory_rate = mean(`RR(rpm)`),
           sd_respiratory = sd(`RR(rpm)`),
           chest_x = mean(`Accl X(g)`),
           chest_y = mean(`Accl Y(g)`),
           chest_z = mean(`Accl Z(g)`),
           chest_temp = mean(`Central Temp(°C)`),
           limb_temp = mean(`Peripheral Temp(°C)`),
           posture = getmode(`Posture`)
      ), 
      by = (seq(from = 1, to = nrow(x))-1) %/% div]}
    vital_feat <- feature_create(vital)
    
    # Load HRV data 
    hrv_dt_p <- paste0(ANNE_hrv_p,sub,"/")
    hrv_dt <- fread(paste0(hrv_dt_p,"/HRV_results_allwindows.csv"))
    if (nrow(hrv_dt) > nrow(vital_feat)) {
      hrv_dt<-hrv_dt[1:nrow(vital_feat),]
    } else if (nrow(vital_feat) > nrow(hrv_dt)) {
      vital_feat<-vital_feat[1:nrow(hrv_dt)]
    }
    # SpO2 filter
    vital_feat[spo2 < 80, spo2:=NA]
    vital_feat[HR_vital <25, HR_vital := NA]
    vital_feat[respiratory_rate <0, respiratory_rate := NA]
    full_dt<-cbind(hrv_dt,vital_feat)
    setwd(paste0(ANNE_hrv_p,"/",sub))
    ls_fefiles <- list.files(path = paste0(ANNE_hrv_p,"/",sub), pattern = paste0(sub,"_all.csv"))
    #browser()
    if (length(ls_fefiles) == 0) {
    fwrite(full_dt, paste0(sub,"_all.csv"))
    }

    
    # MIssing Summary
    missing_hrv <-sum(is.na(hrv_dt$NNmean))
    missing_vital_hr <- sum(is.na(vital_feat$HR_vital))
    missing_vital_spo2 <- sum(is.na(vital_feat$spo2))
    
    
    summary_dt<-data.table(sub = sub,
                           total_num_dt = nrow(vital_feat),
                           total_minutes = nrow(vital_feat)/2,
                           missing_hrv = missing_hrv,
                           missing_vital_hr = missing_vital_hr,
                           missing_vital_spo2 = missing_vital_spo2
    )
    lssdt <- rbind(lssdt, summary_dt)
    
    
    #   ggplot(vital_feat,aes(x=spo2)) + 
    #     geom_histogram()
    #   ggplot(vital_feat,aes(x=HR_vital)) + 
    #     geom_histogram()
    #   ggplot(vital_feat,aes(x=respiratory_rate)) + 
    #     geom_histogram()
    #   ggplot(vital_feat,aes(x=limb_temp)) + 
    #     geom_histogram()
    #   ggplot(vital_feat,aes(x=chest_temp)) + 
    #     geom_histogram()
    
    rm(vital, vital_feat, full_dt, hrv_dt)
    
    
    
    # 
    #   start_time <- as.POSIXct(vital[[1, 'Epoch(ms)']]/1000, origin = '1970-1-1', tz="America/Chicago")
    #   # Find date of the recording
    #   start_date <- as.Date(start_time, tz="America/Chicago")
    #   
    #   
    #   # Create a timestamp column
    # #  vital[,timestamp := as.POSIXct(`Epoch(ms)`/1000, origin = '1970-1-1', tz="America/Chicago")]
    # #  ecg_dt[,timestamp := start_time + `time(ms)`]
    #   ls_start_date[[n]] <- start_date
    #   ls_start_time[[n]] <- start_time
    #   rm(vital)
    
  } else {
    miss_vital <- data.table(sub=sub,
                             fold = subf)
    ls_miss_vital <- append(ls_miss_vital, list(miss_vital), after = length(ls_miss_vital))
  }
  
}

setwd(ANNE_hrv_p)
miss_vital_dt <- do.call(rbind,ls_miss_vital)
fwrite(miss_vital_dt, "missing_vital_list.csv")

lssdt[,missing_hrv_ratio := missing_hrv/total_num_dt]
#lssdt[,date := as.Date(date)]
#lssdt[,min_date := min(date), by = .(sub)]
#lssdt[,dif_from_min_date := date - min(date), by = .(sub)]
#lssdt[dif_from_min_date > 4,pre_post := "post" ]
#lssdt[!(pre_post %in% "post"), pre_post := "pre" ]
lssdt[,vital_ecg_missing_ratio := missing_vital_hr/total_num_dt]
lssdt[,vital_spo2_missing_ratio := missing_vital_spo2/total_num_dt]
lssdt[ , hrs_of_clean_dt := (total_num_dt - missing_hrv)/120]

lssdt$min_date<-NULL

fwrite(lssdt,"missing_summary.csv")

miss_more <- lssdt[missing_hrv_ratio > 0.5,] 
Better_half <- lssdt[missing_hrv_ratio < 0.5, ]


# ggplot(data = lssdt[date > as.Date("2021-07-01"),], aes(x=date, y = missing_hrv_ratio)) + 
#   geom_point()

# ggplot(data = lssdt, aes(x = missing_hrv_ratio, fill = pre_post)) + 
#   geom_histogram(bins =50) + facet_wrap(~sub)

ggplot(data = lssdt, aes(x = sub, y = missing_hrv_ratio)) +
   geom_point()

ggplot(data = lssdt, aes(x = sub, y = vital_ecg_missing_ratio)) + 
   geom_point()

ggplot(data = lssdt, aes(x = sub, y = vital_spo2_missing_ratio)) + 
  geom_point()

# 
# ggplot(data = miss_more, aes(x = date, y = missing_hrv_ratio)) +
#   geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# ggplot(data = Better_half, aes(x = date, y = missing_hrv_ratio)) +
#   geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# ggplot(data = Better_half, aes(x = date, y = missing_hrv_ratio, color = pre_post)) +
#   geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_wrap(~sub)
# 
# ggplot(data = miss_more, aes(x = date, y = missing_hrv_ratio, color = pre_post)) +
#   geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_wrap(~sub)
