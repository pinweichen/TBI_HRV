#ANNE data analysis # 1
library(data.table)
library(caret)
library(tidyverse)
library(signal)
library(lubridate)
library(readxl)
rm(list=ls())

 #path = "/Volumes/RTO/SIESTA"
general <- "Z:/TBI Sleep"
data_path <- paste0(general,"/Pilot_data")
ANNE_p <- paste0(data_path,"/ANNE")
feat_p <- paste0(data_path,"/Preprocessed/ANNE/HRV/")
level2_p <- paste0(data_path,"/Preprocessed/level_2/")

setwd(feat_p)
missing <- fread("missing_summary.csv")


#sub_missing<-base_cnt[missing_hrv_ratio > 0.5,.(sub,date)]

load_list <- missing$sub
setwd(feat_p)
dt_ls <- list()
for (i in 1:length(load_list)){
  dt_name_p <- load_list[i]
  setwd(paste0(feat_p,dt_name_p,"/"))
  dt_pre <- fread(paste0(dt_name_p,"_all.csv"))
  dt_pre[,sub := dt_name_p]
  dt_pre[,timestamp := as.POSIXct(timestamp,tz = "America/Chicago")]
  attr(dt_pre$timestamp,"tzone") <- "America/Chicago"
  dt_ls[[i]] <- dt_pre
} 
dt_pre_all <- do.call(rbind,dt_ls)
rm(dt_ls)

# add floor column


dt_pre_all[spo2<85,spo2 := NA]
# 
# # load demographics
# demo <- fread(paste0(Prepro_p,"/demo_clean_baseline.csv"))
# demo[stroke_type %in% "", stroke_type := "unknown"]
# unique(demo$sub_or)
# unique(dt_pre_all$sub)

dt_pre_all[,timestamp := as.POSIXct(timestamp,tz = "America/Chicago")]
attr(dt_pre_all$timestamp,"tzone") <- "America/Chicago"

#dt_pre_all_demo<-left_join(dt_pre_all,demo,by = c("sub" = "sub_or"))

library(hms)
library(lubridate)
dt_pre_all[,time_only := as_hms(timestamp)]
dt_pre_all[,hour_only := factor(hour(time_only))]
dt_pre_all[,date_only := as.Date(timestamp,tz = '')]
dt_pre_all_rm <- na.omit(dt_pre_all)

#dt_pre_all_rm[,.(sub,hour_only,date_only,timestamp)]
dt_pre_all_rm <- dt_pre_all_rm[!(SDNN >90 & SDNN < 20),]

dt_pre_summary<-dt_pre_all_rm[,.(avg_NNmean = mean(NNmean),
                              sd_NNmean = sd(NNmean),
                              avg_NNmedian = mean(NNmedian),
                              sd_NNmedian = sd(NNmedian),
                              avg_NNmode = mean(NNmode),
                              sd_NNmode = sd(NNmode),
                              avg_NNvariance = mean(NNvariance),
                              sd_NNvariance = sd(NNvariance),
                              avg_SDNN = mean(SDNN),
                              sd_SDNN = sd(SDNN),
                              avg_RMSSD = mean(RMSSD),
                              sd_RMSSD = sd(RMSSD),
                              avg_pnn50 = mean(pnn50),
                              sd_pnn50 = sd(pnn50),
                              avg_pnn20 = mean(pnn20),
                              sd_pnn20 = sd(pnn20),
                              avg_atc = mean(atc),
                              sd_atc = sd(atc),
                              avg_vlf = mean(vlf),
                              sd_vlf = sd(vlf),
                              avg_lfhf = mean(lfhf),
                              sd_lfhf = sd(lfhf),
                              avg_SD1SD2 = mean(SD1SD2),
                              sd_SD1SD2 = sd(SD1SD2),
                              avg_respiratory_rate = mean(respiratory_rate),
                              avg_spo2  = mean(spo2)
                              ),by = c("hour_only","date_only","sub")]

dt_sum_rm <- na.omit(dt_pre_summary)


# ggplot(dt_pre_all, aes(y = NNmean)) + 
#   geom_boxplot()
# ggplot(dt_sum_rm, aes(y = avg_NNmean)) + 
#   geom_boxplot()
# ggplot(dt_sum_rm, aes(y = avg_pnn50)) + 
#   geom_boxplot()
# 
# ggplot(dt_sum_rm, aes(y = avg_lfhf)) + 
#   geom_boxplot()
# ggplot(dt_sum_rm, aes(y = avg_SD1SD2)) + 
#   geom_boxplot()

sd(dt_sum_rm$avg_NNmean)

dt_sum_by_sub <- dt_sum_rm[,.(avg_NNmean = mean(avg_NNmean),
                                   sd_NNmean = sd(avg_NNmean),
                                   avg_NNmedian = mean(avg_NNmedian),
                                   sd_NNmedian = sd(avg_NNmedian),
                                   avg_NNmode = mean(avg_NNmode),
                                   sd_NNmode = sd(avg_NNmode),
                                   avg_NNvariance = mean(avg_NNvariance),
                                   sd_NNvariance = sd(avg_NNvariance),
                                   avg_SDNN = mean(avg_SDNN),
                                   sd_SDNN = sd(avg_SDNN),
                                   avg_RMSSD = mean(avg_RMSSD),
                                   sd_RMSSD = sd(avg_RMSSD),
                                   avg_pnn50 = mean(avg_pnn50),
                                   sd_pnn50 = sd(avg_pnn50),
                                   avg_pnn20 = mean(avg_pnn20),
                                   sd_pnn20 = sd(avg_pnn20),
                                   avg_atc = mean(avg_atc),
                                   sd_atc = sd(avg_atc),
                                   avg_vlf = mean(avg_vlf),
                                   sd_vlf = sd(avg_vlf),
                                   avg_lfhf = mean(avg_lfhf),
                                   sd_lfhf = sd(avg_lfhf),
                                   avg_SD1SD2 = mean(avg_SD1SD2),
                                   sd_SD1SD2 = sd(avg_SD1SD2), 
                                   avg_respiratory_rate = mean(avg_respiratory_rate),
                                   avg_spo2  = mean(avg_spo2)
                                  ),by = c("hour_only","sub")]

#dt_demo_sum<-left_join(dt_sum_rm,demo,by = c("sub" = "sub_or"), all = T)
#unique(dt_demo_sum$sub)

# dt_sum_by_hour <- dt_sum_rm[,.(count = .N,
#                        avg_NNmean = mean(avg_NNmean),
#                       sd_NNmean = sd(avg_NNmean)/sqrt(.N),
#                       avg_NNmedian = mean(avg_NNmedian),
#                       sd_NNmedian = sd(avg_NNmedian)/sqrt(.N),
#                       avg_NNmode = mean(avg_NNmode),
#                       sd_NNmode = sd(avg_NNmode)/sqrt(.N),
#                       avg_NNvariance = mean(avg_NNvariance),
#                       sd_NNvariance = sd(avg_NNvariance)/sqrt(.N),
#                       avg_SDNN = mean(avg_SDNN),
#                       sd_SDNN = sd(avg_SDNN)/sqrt(.N),
#                       avg_RMSSD = mean(avg_RMSSD),
#                       sd_RMSSD = sd(avg_RMSSD)/sqrt(.N),
#                       avg_pnn50 = mean(avg_pnn50),
#                       sd_pnn50 = sd(avg_pnn50)/sqrt(.N),
#                       avg_pnn20 = mean(avg_pnn20),
#                       sd_pnn20 = sd(avg_pnn20)/sqrt(.N),
#                       avg_atc = mean(avg_atc),
#                       sd_atc = sd(avg_atc)/sqrt(.N),
#                       avg_vlf = mean(avg_vlf),
#                       sd_vlf = sd(avg_vlf)/sqrt(.N),
#                       avg_lfhf = mean(avg_lfhf),
#                       sd_lfhf = sd(avg_lfhf)/sqrt(.N),
#                       avg_SD1SD2 = mean(avg_SD1SD2),
#                       sd_SD1SD2 = sd(avg_SD1SD2)/sqrt(.N),
#                       avg_respiratory_rate = mean(avg_respiratory_rate),
#                       sd_respiratory_rate = sd(avg_respiratory_rate)/sqrt(.N),
#                       avg_spo2  = mean(avg_spo2),
#                       sd_spo2 = sd(avg_spo2)/sqrt(.N)
#                       ),by = c("hour_only")]

setwd(level2_p)
# fwrite(dt_demo_sum,"hrv_sum_demo.csv")

dt_sum_by_sub$hour_only<-factor(dt_sum_by_sub$hour_only, levels=c("18","19","20","21","22","23","0", "1","2", "3", "4", "5","6","7","8"))
#levels(dt_sum_by_stroke$hour_only)

ggplot(dt_sum_by_sub,aes(x = hour_only, y = avg_lfhf,color = sub, group = sub)) + 
  geom_point(size = 1) + geom_line()
  #geom_ribbon(aes( y = avg_lfhf,ymin = avg_lfhf -sd_lfhf,ymax = avg_lfhf+sd_lfhf,fill = sub),alpha = .2)+
  xlab("Hour") + ylab("LF/HF ratio ") + theme(legend.title = element_blank())

ggsave("lfhf_by_hour.png", plot = last_plot(),path = paste0(level2_p,"/Graph/"),
        device = "png")

ggplot(dt_sum_by_sub,aes(x = hour_only, y = avg_SDNN,color = sub, group = sub)) + 
  geom_point(size = 1) + geom_line()+
  #geom_ribbon(aes( y = avg_vlf,ymin = avg_vlf -sd_vlf,ymax = avg_vlf+sd_vlf, fill = stroke_type),alpha = .2)+
  xlab("Hour") + ylab("SDNN") + theme(legend.title = element_blank())

ggsave("SDNN_by_hour.png", plot = last_plot(),path = paste0(level2_p,"/Graph/"),
       device = "png")
# 
# ggplot(dt_sum_by_stroke,aes(x = hour_only, y = avg_SDNN, color = stroke_type, group = stroke_type)) + 
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_SDNN,ymin = avg_SDNN -sd_SDNN,ymax = avg_SDNN+sd_SDNN, fill = stroke_type),alpha = .2)+
#   xlab("Hour") + ylab("SDNN") + theme(legend.title = element_blank())
# 
# ggsave("sdnn_by_hour_stroke_type.png", plot = last_plot(),path = paste0(level_2_p,"/Graph/"),
#        device = "png")
# 
# ggplot(dt_sum_by_stroke,aes(x = hour_only, y = avg_RMSSD, color = stroke_type, group = stroke_type)) + 
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_RMSSD,ymin = avg_RMSSD -sd_RMSSD,ymax = avg_RMSSD+sd_RMSSD, fill = stroke_type),alpha = .2)+
#   xlab("Hour") + ylab("RMSSD") + theme(legend.title = element_blank())
# 
# ggsave("RMSSD_by_hour_stroke_type.png", plot = last_plot(),path = paste0(level_2_p,"/Graph/"),
#        device = "png")
# 
# 
# ggplot(dt_sum_by_stroke,aes(x = hour_only, y = avg_NNmean, color = stroke_type, group = stroke_type)) + 
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_NNmean,ymin = avg_NNmean -sd_NNmean,ymax = avg_NNmean+sd_NNmean, fill = stroke_type),alpha = .2)+
#   xlab("Hour") + ylab("NNmean") + theme(legend.title = element_blank())
# 
# ggsave("NNmean_by_hour_stroke_type.png", plot = last_plot(),path = paste0(level_2_p,"/Graph/"),
#        device = "png")
# 
# ggplot(dt_sum_by_stroke,aes(x = hour_only, y = avg_spo2, color = stroke_type, group = stroke_type)) + 
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_spo2,ymin = avg_spo2 -sd_spo2,ymax = avg_spo2+sd_spo2, fill = stroke_type),alpha = .2)+
#   xlab("Hour") + ylab("spo2") + theme(legend.title = element_blank())
# 
# ggsave("spo2_by_hour_stroke_type.png", plot = last_plot(),path = paste0(level_2_p,"/Graph/"),
#        device = "png")
# 
# dt_sum_by_stroke[stroke_type %in% "both",]
# 
# 
# ggplot(dt_sum_by_stroke,aes(x = hour_only, y = avg_respiratory_rate, color = stroke_type, group = stroke_type)) + 
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_respiratory_rate,ymin = avg_respiratory_rate -sd_respiratory_rate,ymax = avg_respiratory_rate+sd_respiratory_rate, fill = stroke_type),alpha = .2)+
#   xlab("Hour") + ylab("respiratory_rate") + theme(legend.title = element_blank())
# 
# ggsave("respiratory_rate_by_hour_stroke_type.png", plot = last_plot(),path = paste0(level_2_p,"/Graph/"),
#        device = "png")
# 
# 
# 
# ggplot(dt_sum_by_stroke,aes(x = hour_only, y = avg_pnn50, color = stroke_type, group = stroke_type)) + 
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_pnn50,ymin = avg_pnn50 -sd_pnn50,ymax = avg_pnn50+sd_pnn50, fill = stroke_type),alpha = .2)+
#   xlab("Hour") + ylab("pnn50") + theme(legend.title = element_blank())
# 
# ggsave("pnn50_by_hour_stroke_type.png", plot = last_plot(),path = paste0(level_2_p,"/Graph/"),
#        device = "png")
# 
# 
# # Sum by age
# # ggplot(dt_demo_sum, aes(x = stroke_type, y = age)) +
# #   geom_point()
# 
# dt_demo_sum$age <- as.numeric(dt_demo_sum$age)
# dt_demo_sum[age <= 40, age_gp := "below_40"]
# dt_demo_sum[age > 40 & age <= 65, age_gp := "40-60"]
# dt_demo_sum[age > 65, age_gp := "above_65"]
# dt_demo_sum[age_gp %in% NA,age]
# 
# dt_sum_by_age <- dt_demo_sum[,.(count = .N,
#                                    avg_NNmean = mean(avg_NNmean),
#                                    sd_NNmean = sd(avg_NNmean)/sqrt(.N),
#                                    avg_NNmedian = mean(avg_NNmedian),
#                                    sd_NNmedian = sd(avg_NNmedian)/sqrt(.N),
#                                    avg_NNmode = mean(avg_NNmode),
#                                    sd_NNmode = sd(avg_NNmode)/sqrt(.N),
#                                    avg_NNvariance = mean(avg_NNvariance),
#                                    sd_NNvariance = sd(avg_NNvariance)/sqrt(.N),
#                                    avg_SDNN = mean(avg_SDNN),
#                                    sd_SDNN = sd(avg_SDNN)/sqrt(.N),
#                                    avg_RMSSD = mean(avg_RMSSD),
#                                    sd_RMSSD = sd(avg_RMSSD)/sqrt(.N),
#                                    avg_pnn50 = mean(avg_pnn50),
#                                    sd_pnn50 = sd(avg_pnn50)/sqrt(.N),
#                                    avg_pnn20 = mean(avg_pnn20),
#                                    sd_pnn20 = sd(avg_pnn20)/sqrt(.N),
#                                    avg_atc = mean(avg_atc),
#                                    sd_atc = sd(avg_atc)/sqrt(.N),
#                                    avg_vlf = mean(avg_vlf),
#                                    sd_vlf = sd(avg_vlf)/sqrt(.N),
#                                    avg_lfhf = mean(avg_lfhf),
#                                    sd_lfhf = sd(avg_lfhf)/sqrt(.N),
#                                    avg_SD1SD2 = mean(avg_SD1SD2),
#                                    sd_SD1SD2 = sd(avg_SD1SD2)/sqrt(.N),
#                                    avg_respiratory_rate = mean(avg_respiratory_rate),
#                                    sd_respiratory_rate = sd(avg_respiratory_rate)/sqrt(.N),
#                                    avg_spo2  = mean(avg_spo2),
#                                    sd_spo2 = sd(avg_spo2)/sqrt(.N)
# ),by = c("hour_only","age_gp")]
# #dt_demo_sum_rm <- na.omit(dt_demo_sum)
# #dt_sum_by_age[age_gp]
# 
# dt_sum_by_age<-dt_sum_by_age[!(age_gp %in% NA),]
# 
# ggplot(dt_sum_by_age,aes(x = hour_only, y = avg_lfhf, color = age_gp, group = age_gp)) + 
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_lfhf,ymin = avg_lfhf -sd_lfhf,ymax = avg_lfhf+sd_lfhf, fill = age_gp),alpha = .2)+
#   xlab("Hour") + ylab("LF/HF ratio ") + theme(legend.title = element_blank())
# 
# ggsave("lfhf_by_hour_age_type.png", plot = last_plot(),path = paste0(level_2_p,"/Graph/"),
#        device = "png")
# 
# ggplot(dt_sum_by_age,aes(x = hour_only, y = avg_vlf, color = age_gp, group = age_gp)) + 
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_vlf,ymin = avg_vlf -sd_vlf,ymax = avg_vlf+sd_vlf, fill = age_gp),alpha = .2)+
#   xlab("Hour") + ylab("VLF") + theme(legend.title = element_blank())
# 
# ggsave("vlf_by_hour_age_type.png", plot = last_plot(),path = paste0(level_2_p,"/Graph/"),
#        device = "png")
# 
# ggplot(dt_sum_by_age,aes(x = hour_only, y = avg_SDNN, color = age_gp, group = age_gp)) + 
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_SDNN,ymin = avg_SDNN -sd_SDNN,ymax = avg_SDNN+sd_SDNN, fill = age_gp),alpha = .2)+
#   xlab("Hour") + ylab("SDNN") + theme(legend.title = element_blank())
# 
# ggsave("sdnn_by_hour_age_type.png", plot = last_plot(),path = paste0(level_2_p,"/Graph/"),
#        device = "png")
# 
# ggplot(dt_sum_by_age,aes(x = hour_only, y = avg_RMSSD, color = age_gp, group = age_gp)) + 
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_RMSSD,ymin = avg_RMSSD -sd_RMSSD,ymax = avg_RMSSD+sd_RMSSD, fill = age_gp),alpha = .2)+
#   xlab("Hour") + ylab("RMSSD") + theme(legend.title = element_blank())
# 
# ggsave("RMSSD_by_hour_age_type.png", plot = last_plot(),path = paste0(level_2_p,"/Graph/"),
#        device = "png")
# 
# 
# ggplot(dt_sum_by_age,aes(x = hour_only, y = avg_NNmean, color = age_gp, group = age_gp)) + 
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_NNmean,ymin = avg_NNmean -sd_NNmean,ymax = avg_NNmean+sd_NNmean, fill = age_gp),alpha = .2)+
#   xlab("Hour") + ylab("NNmean") + theme(legend.title = element_blank())
# 
# ggsave("NNmean_by_hour_age_type.png", plot = last_plot(),path = paste0(level_2_p,"/Graph/"),
#        device = "png")
# 
# ggplot(dt_sum_by_age,aes(x = hour_only, y = avg_spo2, color = age_gp, group = age_gp)) + 
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_spo2,ymin = avg_spo2 -sd_spo2,ymax = avg_spo2+sd_spo2, fill = age_gp),alpha = .2)+
#   xlab("Hour") + ylab("spo2") + theme(legend.title = element_blank())
# 
# ggsave("spo2_by_hour_age_type.png", plot = last_plot(),path = paste0(level_2_p,"/Graph/"),
#        device = "png")
# 
# 
# 
# 
# ggplot(dt_sum_by_age,aes(x = hour_only, y = avg_respiratory_rate, color = age_gp, group = age_gp)) + 
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_respiratory_rate,ymin = avg_respiratory_rate -sd_respiratory_rate,ymax = avg_respiratory_rate+sd_respiratory_rate, fill = age_gp),alpha = .2)+
#   xlab("Hour") + ylab("respiratory_rate") + theme(legend.title = element_blank())
# 
# ggsave("respiratory_rate_by_hour_age_type.png", plot = last_plot(),path = paste0(level_2_p,"/Graph/"),
#        device = "png")
# 
# 
# # pre and post  ----
# 
# pre_max_hrs<-max_hrs[pre_post %in% "pre",]
# post_max_hrs <- max_hrs[pre_post %in% "post", ]
# 
# pre_post_max <- max_hrs[!(sub %in% setdiff(pre_max_hrs$sub,post_max_hrs$sub)),]
# pre_post_max <- pre_post_max[!(sub %in% setdiff(pre_post_max$sub,pre_max_hrs$sub)),]
# pre_post_max[, dt_name := paste0(sub,"d",as.character(date))]
# load_list <- paste0(pre_post_max$dt_name,".csv")
# prepro_sub_ls <- pre_post_max$sub
# prepro_sub_ls <- word(prepro_sub_ls,2, sep = "A_")
# prepost_num_ls <- pre_post_max$pre_post
# 
# setwd(feat_p)
# dt_ls <- list()
# for (i in 1:length(load_list)){
#   dt_name_p <- load_list[i]
#   dt_pre <- fread(dt_name_p)
#   dt_pre[,sub := prepro_sub_ls[i]]
#   dt_pre[,pre_post := prepost_num_ls[[i]]]
#   dt_pre[,timestamp := as.POSIXct(timestamp,tz = "America/Chicago")]
#   attr(dt_pre$timestamp,"tzone") <- "America/Chicago"
#   dt_ls[[i]] <- dt_pre
# } 
# dt_prepro_all <- do.call(rbind,dt_ls)
# rm(dt_ls)
# 
# 
# 
# # add floor column
# dt_prepro_all[,floor := word(sub,1,sep = "_")]
# 
# dt_prepro_all[spo2<85,spo2 := NA]
# 
# # load demographics
# demo <- fread(paste0(Prepro_p,"/demo_clean_baseline.csv"))
# demo[stroke_type %in% "", stroke_type := "unknown"]
# unique(demo$sub_or)
# unique(dt_prepro_all$sub)
# 
# dt_prepro_all[,timestamp := as.POSIXct(timestamp,tz = "America/Chicago")]
# attr(dt_prepro_all$timestamp,"tzone") <- "America/Chicago"
# 
# #dt_prepro_all_demo<-left_join(dt_prepro_all,demo,by = c("sub" = "sub_or"))
# 
# library(hms)
# library(lubridate)
# dt_prepro_all[,time_only := as_hms(timestamp)]
# dt_prepro_all[,hour_only := factor(hour(time_only))]
# dt_prepro_all[,date_only := as.Date(timestamp,tz = '')]
# dt_prepro_all<-dt_prepro_all[!(lf > 8000),]
# dt_prepro_all<-dt_prepro_all[!(hf > 8000),]
# dt_prepro_all_rm <- na.omit(dt_prepro_all)
# colnames(dt_prepro_all)
# 
# #dt_prepro_all[sub %in% "23_005",vlf]
# #dt_prepro_all_rm[,.(sub,hour_only,date_only,timestamp)]
# 
# dt_prepro_summary<-dt_prepro_all_rm[,.(avg_NNmean = mean(NNmean),
#                                  sd_NNmean = sd(NNmean),
#                                  avg_NNmedian = mean(NNmedian),
#                                  sd_NNmedian = sd(NNmedian),
#                                  avg_NNmode = mean(NNmode),
#                                  sd_NNmode = sd(NNmode),
#                                  avg_NNvariance = mean(NNvariance),
#                                  sd_NNvariance = sd(NNvariance),
#                                  avg_SDNN = mean(SDNN),
#                                  sd_SDNN = sd(SDNN),
#                                  avg_RMSSD = mean(RMSSD),
#                                  sd_RMSSD = sd(RMSSD),
#                                  avg_pnn50 = mean(pnn50),
#                                  sd_pnn50 = sd(pnn50),
#                                  avg_pnn20 = mean(pnn20),
#                                  sd_pnn20 = sd(pnn20),
#                                  avg_atc = mean(atc),
#                                  sd_atc = sd(atc),
#                                  avg_vlf = mean(vlf),
#                                  sd_vlf = sd(vlf),
#                                  avg_hf = mean(hf),
#                                  sd_hf = sd(hf),
#                                  avg_lf = mean(lf),
#                                  avg_lf = sd(lf),
#                                  avg_lfhf = mean(lfhf),
#                                  sd_lfhf = sd(lfhf),
#                                  avg_SD1SD2 = mean(SD1SD2),
#                                  sd_SD1SD2 = sd(SD1SD2),
#                                  avg_respiratory_rate = mean(respiratory_rate),
#                                  avg_spo2  = mean(spo2)
# ),by = c("hour_only","date_only","sub","pre_post")]
# 
# 
# 
# 
# # ggplot(dt_pre_all, aes(y = NNmean)) + 
# #   geom_boxplot()
# # ggplot(dt_sum_rm, aes(y = avg_NNmean)) + 
# #   geom_boxplot()
# # ggplot(dt_sum_rm, aes(y = avg_pnn50)) + 
# #   geom_boxplot()
# # 
# # ggplot(dt_sum_rm, aes(y = avg_lfhf)) + 
# #   geom_boxplot()
# # ggplot(dt_sum_rm, aes(y = avg_SD1SD2)) + 
# #   geom_boxplot()
# 
# # sd(dt_sum_rm$avg_NNmean)
# 
# dt_sum_by_stroke <- dt_sum_rm[,.(avg_NNmean = mean(avg_NNmean),
#                                  sd_NNmean = sd(avg_NNmean),
#                                  avg_NNmedian = mean(avg_NNmedian),
#                                  sd_NNmedian = sd(avg_NNmedian),
#                                  avg_NNmode = mean(avg_NNmode),
#                                  sd_NNmode = sd(avg_NNmode),
#                                  avg_NNvariance = mean(avg_NNvariance),
#                                  sd_NNvariance = sd(avg_NNvariance),
#                                  avg_SDNN = mean(avg_SDNN),
#                                  sd_SDNN = sd(avg_SDNN),
#                                  avg_RMSSD = mean(avg_RMSSD),
#                                  sd_RMSSD = sd(avg_RMSSD),
#                                  avg_pnn50 = mean(avg_pnn50),
#                                  sd_pnn50 = sd(avg_pnn50),
#                                  avg_pnn20 = mean(avg_pnn20),
#                                  sd_pnn20 = sd(avg_pnn20),
#                                  avg_atc = mean(avg_atc),
#                                  sd_atc = sd(avg_atc),
#                                  avg_vlf = mean(avg_vlf),
#                                  sd_vlf = sd(avg_vlf),
#                                  avg_hf = mean(hf),
#                                  sd_hf = sd(hf),
#                                  avg_lf = mean(lf),
#                                  sd_lf = sd(lf),
#                                  avg_lfhf = mean(avg_lfhf),
#                                  sd_lfhf = sd(avg_lfhf),
#                                  avg_SD1SD2 = mean(avg_SD1SD2),
#                                  sd_SD1SD2 = sd(avg_SD1SD2), 
#                                  avg_respiratory_rate = mean(avg_respiratory_rate),
#                                  avg_spo2  = mean(avg_spo2)
# ),by = c("hour_only","sub", "pre_post")]
# demo_sum_rm<-demo[sub_or %in% dt_sum_rm$sub,]
# dt_demo_sum<-left_join(dt_sum_rm,demo_sum_rm,by = c("sub" = "sub_or"), all = T)
# 
# # Only the midnight ------------
# 
# dt_demo_midnight <- dt_demo_sum[hour_only %in% "2" , ]
# dt_sum_by_stroke_midnight <- dt_demo_midnight[,.(count = .N,
#                                    avg_NNmean = mean(avg_NNmean),
#                                    sd_NNmean = sd(avg_NNmean)/sqrt(.N),
#                                    avg_NNmedian = mean(avg_NNmedian),
#                                    sd_NNmedian = sd(avg_NNmedian)/sqrt(.N),
#                                    avg_NNmode = mean(avg_NNmode),
#                                    sd_NNmode = sd(avg_NNmode)/sqrt(.N),
#                                    avg_NNvariance = mean(avg_NNvariance),
#                                    sd_NNvariance = sd(avg_NNvariance)/sqrt(.N),
#                                    avg_SDNN = mean(avg_SDNN),
#                                    sd_SDNN = sd(avg_SDNN)/sqrt(.N),
#                                    avg_RMSSD = mean(avg_RMSSD),
#                                    sd_RMSSD = sd(avg_RMSSD)/sqrt(.N),
#                                    avg_pnn50 = mean(avg_pnn50),
#                                    sd_pnn50 = sd(avg_pnn50)/sqrt(.N),
#                                    avg_pnn20 = mean(avg_pnn20),
#                                    sd_pnn20 = sd(avg_pnn20)/sqrt(.N),
#                                    avg_atc = mean(avg_atc),
#                                    sd_atc = sd(avg_atc)/sqrt(.N),
#                                    avg_vlf = mean(avg_vlf),
#                                    sd_vlf = sd(avg_vlf)/sqrt(.N),
#                                    avg_lfhf = mean(avg_lfhf),
#                                    sd_lfhf = sd(avg_lfhf)/sqrt(.N),
#                                    avg_SD1SD2 = mean(avg_SD1SD2),
#                                    sd_SD1SD2 = sd(avg_SD1SD2)/sqrt(.N),
#                                    avg_respiratory_rate = mean(avg_respiratory_rate),
#                                    sd_respiratory_rate = sd(avg_respiratory_rate)/sqrt(.N),
#                                    avg_spo2  = mean(avg_spo2),
#                                    sd_spo2 = sd(avg_spo2)/sqrt(.N),
#                                    stroke_type = unique(stroke_type),
#                                    lifestyle = unique(lifestyle),
#                                    gender = unique(gender),
#                                    BMI = unique(BMI)
#                                    
# ),by = c("sub", "pre_post")]
# ggplot(data = dt_sum_by_stroke_midnight, aes( x = pre_post, y =avg_NNmean, group = sub, color = sub)) +
#   geom_line() + facet_grid(cols=vars(stroke_type)) +theme(legend.position="none")
# ggsave("overnight_avg_NNmean_stroke_type_midnight.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# 
# ggplot(data = dt_sum_by_stroke_midnight, aes( x = pre_post, y =avg_SDNN, group = sub, color = sub)) +
#   geom_line() + facet_grid(cols=vars(stroke_type)) +theme(legend.position="none")
# ggsave("overnight_avg_SDNN_stroke_type.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# ggplot(data = dt_sum_by_stroke_midnight, aes( x = pre_post, y =avg_NNmean, group = sub, color = sub)) +
#   geom_line() + facet_grid(cols=vars(lifestyle)) +theme(legend.position="none")
# ggsave("overnight_avg_NNmean_lifestyle_midnight.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# 
# ggplot(data = dt_sum_by_stroke_midnight, aes( x = pre_post, y =avg_SDNN, group = sub, color = sub)) +
#   geom_line() + facet_grid(cols=vars(lifestyle)) +theme(legend.position="none")
# ggsave("overnight_avg_SDNN_lifestyle_midnight.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# dt_sum_by_stroke_midnight[BMI >= 30, BMI_gp := "Obesity"]
# dt_sum_by_stroke_midnight[BMI >= 25 & BMI <30, BMI_gp := "Overweight"]
# dt_sum_by_stroke_midnight[BMI >= 18.5 & BMI <25, BMI_gp := "Normal weight"]
# dt_sum_by_stroke_midnight[BMI < 18.5, BMI_gp := "Underweight"]
# 
# dt_sum_by_stroke_midnight$BMI_gp <- factor(dt_sum_by_stroke_midnight$BMI_gp, levels = c("Underweight","Normal weight",
#                                                                       "Overweight","Obesity"))
# dt_sum_by_stroke_midnight<-dt_sum_by_stroke_midnight[!(BMI_gp %in% NA),]
# ggplot(data = dt_sum_by_stroke_midnight, aes( x = pre_post, y =avg_NNmean, group = sub, color = sub)) +
#   geom_line() + facet_grid(cols=vars(BMI_gp)) +theme(legend.position="none")
# ggsave("overnight_avg_NNmean_bmi_midnight_2.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# dt_sum_by_stroke_midnight$pre_post <- factor(dt_sum_by_stroke_midnight$pre_post, levels = c("pre","post"))
# 
# ggplot(data = dt_sum_by_stroke_midnight, aes( x = pre_post, y =avg_SDNN, group = sub, color = sub)) +
#   geom_line() + facet_grid(cols=vars(BMI_gp)) +theme(legend.position="none")
# ggsave("overnight_avg_SDNN_bmi_midnight.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# # ============
# pre_post_p <- paste0(level_2_p,"/pre_post")
# dir.create(pre_post_p)
# setwd(pre_post_p)
# dir.create(paste0(pre_post_p,"/Graph"))
# dt_sum_by_stroke <- dt_demo_sum[,.(count = .N,
#                                    avg_NNmean = mean(avg_NNmean),
#                                    sd_NNmean = sd(avg_NNmean)/sqrt(.N),
#                                    avg_NNmedian = mean(avg_NNmedian),
#                                    sd_NNmedian = sd(avg_NNmedian)/sqrt(.N),
#                                    avg_NNmode = mean(avg_NNmode),
#                                    sd_NNmode = sd(avg_NNmode)/sqrt(.N),
#                                    avg_NNvariance = mean(avg_NNvariance),
#                                    sd_NNvariance = sd(avg_NNvariance)/sqrt(.N),
#                                    avg_SDNN = mean(avg_SDNN),
#                                    sd_SDNN = sd(avg_SDNN)/sqrt(.N),
#                                    avg_RMSSD = mean(avg_RMSSD),
#                                    sd_RMSSD = sd(avg_RMSSD)/sqrt(.N),
#                                    avg_pnn50 = mean(avg_pnn50),
#                                    sd_pnn50 = sd(avg_pnn50)/sqrt(.N),
#                                    avg_pnn20 = mean(avg_pnn20),
#                                    sd_pnn20 = sd(avg_pnn20)/sqrt(.N),
#                                    avg_atc = mean(avg_atc),
#                                    sd_atc = sd(avg_atc)/sqrt(.N),
#                                    avg_vlf = mean(avg_vlf),
#                                    sd_vlf = sd(avg_vlf)/sqrt(.N),
#                                    avg_lfhf = mean(avg_lfhf),
#                                    sd_lfhf = sd(avg_lfhf)/sqrt(.N),
#                                    avg_SD1SD2 = mean(avg_SD1SD2),
#                                    sd_SD1SD2 = sd(avg_SD1SD2)/sqrt(.N),
#                                    avg_respiratory_rate = mean(avg_respiratory_rate),
#                                    sd_respiratory_rate = sd(avg_respiratory_rate)/sqrt(.N),
#                                    avg_spo2  = mean(avg_spo2),
#                                    sd_spo2 = sd(avg_spo2)/sqrt(.N)
# ),by = c("hour_only","stroke_type", "pre_post")]
# dt_demo_sum_rm <- na.omit(dt_demo_sum)
# 
# fwrite(dt_demo_sum,"hrv_sum_demo.csv")
# 
# dt_sum_by_stroke$hour_only<-factor(dt_sum_by_stroke$hour_only, levels=c("18","19","20","21","22","23","0", "1","2", "3", "4", "5","6","7","8"))
# #levels(dt_sum_by_stroke$hour_only)
# 
# dt_sum_by_stroke[stroke_type %in% "Hemorrhagic,Ischemic", stroke_type := "both"]
# dt_sum_by_stroke <- dt_sum_by_stroke[!(stroke_type %in% "unknown"),]
# 
# dt_sum_by_stroke$pre_post <- factor(dt_sum_by_stroke$pre_post, levels = c("pre","post"))
# 
# levels(dt_sum_by_stroke$pre_post)
# 
# ggplot(dt_sum_by_stroke,aes(x = hour_only, y = avg_lfhf, color = stroke_type, group = stroke_type)) + 
#   facet_grid(cols = vars(pre_post)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_lfhf,ymin = avg_lfhf -sd_lfhf,ymax = avg_lfhf+sd_lfhf, fill = stroke_type),alpha = .2)+
#   xlab("Hour") + ylab("LF/HF ratio ") + theme(legend.title = element_blank())
# 
# ggsave("lfhf_by_hour_stroke_type.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# ggplot(dt_sum_by_stroke,aes(x = hour_only, y = avg_vlf, color = stroke_type, group = stroke_type)) + 
#   facet_grid(cols = vars(pre_post)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_vlf,ymin = avg_vlf -sd_vlf,ymax = avg_vlf+sd_vlf, fill = stroke_type),alpha = .2)+
#   xlab("Hour") + ylab("VLF") + theme(legend.title = element_blank())
# 
# ggsave("vlf_by_hour_stroke_type.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# ggplot(dt_sum_by_stroke,aes(x = hour_only, y = avg_SDNN, color = stroke_type, group = stroke_type)) + 
#   facet_grid(cols = vars(pre_post)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_SDNN,ymin = avg_SDNN -sd_SDNN,ymax = avg_SDNN+sd_SDNN, fill = stroke_type),alpha = .2)+
#   xlab("Hour") + ylab("SDNN") + theme(legend.title = element_blank())
# 
# ggsave("sdnn_by_hour_stroke_type.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# ggplot(dt_sum_by_stroke,aes(x = hour_only, y = avg_RMSSD, color = stroke_type, group = stroke_type)) + 
#   facet_grid(cols = vars(pre_post)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_RMSSD,ymin = avg_RMSSD -sd_RMSSD,ymax = avg_RMSSD+sd_RMSSD, fill = stroke_type),alpha = .2)+
#   xlab("Hour") + ylab("RMSSD") + theme(legend.title = element_blank())
# 
# ggsave("RMSSD_by_hour_stroke_type.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# 
# ggplot(dt_sum_by_stroke,aes(x = hour_only, y = avg_NNmean, color = stroke_type, group = stroke_type)) + 
#   facet_grid(cols = vars(pre_post)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_NNmean,ymin = avg_NNmean -sd_NNmean,ymax = avg_NNmean+sd_NNmean, fill = stroke_type),alpha = .2)+
#   xlab("Hour") + ylab("NNmean") + theme(legend.title = element_blank())
# 
# ggsave("NNmean_by_hour_stroke_type.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# ggplot(dt_sum_by_stroke,aes(x = hour_only, y = avg_spo2, color = stroke_type, group = stroke_type)) + 
#   facet_grid(cols = vars(pre_post)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_spo2,ymin = avg_spo2 -sd_spo2,ymax = avg_spo2+sd_spo2, fill = stroke_type),alpha = .2)+
#   xlab("Hour") + ylab("spo2") + theme(legend.title = element_blank())
# 
# ggsave("spo2_by_hour_stroke_type.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# #dt_sum_by_stroke[stroke_type %in% "both",]
# 
# 
# ggplot(dt_sum_by_stroke,aes(x = hour_only, y = avg_respiratory_rate, color = stroke_type, group = stroke_type)) + 
#   facet_grid(cols = vars(pre_post)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_respiratory_rate,ymin = avg_respiratory_rate -sd_respiratory_rate,ymax = avg_respiratory_rate+sd_respiratory_rate, fill = stroke_type),alpha = .2)+
#   xlab("Hour") + ylab("respiratory_rate") + theme(legend.title = element_blank())
# 
# ggsave("respiratory_rate_by_hour_stroke_type.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# 
# 
# ggplot(dt_sum_by_stroke,aes(x = hour_only, y = avg_pnn50, color = stroke_type, group = stroke_type)) + 
#   facet_grid(cols = vars(pre_post)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes( y = avg_pnn50,ymin = avg_pnn50 -sd_pnn50,ymax = avg_pnn50+sd_pnn50, fill = stroke_type),alpha = .2)+
#   xlab("Hour") + ylab("pnn50") + theme(legend.title = element_blank())
# 
# ggsave("pnn50_by_hour_stroke_type.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# # Compare overnight heart rate changes
# 
# dt_prepro_overnight<-dt_prepro_all_rm[,.(avg_NNmean = mean(NNmean),
#                                        sd_NNmean = sd(NNmean),
#                                        avg_NNmedian = mean(NNmedian),
#                                        sd_NNmedian = sd(NNmedian),
#                                        avg_NNmode = mean(NNmode),
#                                        sd_NNmode = sd(NNmode),
#                                        avg_NNvariance = mean(NNvariance),
#                                        sd_NNvariance = sd(NNvariance),
#                                        avg_SDNN = mean(SDNN),
#                                        sd_SDNN = sd(SDNN),
#                                        avg_RMSSD = mean(RMSSD),
#                                        sd_RMSSD = sd(RMSSD),
#                                        avg_pnn50 = mean(pnn50),
#                                        sd_pnn50 = sd(pnn50),
#                                        avg_pnn20 = mean(pnn20),
#                                        sd_pnn20 = sd(pnn20),
#                                        avg_atc = mean(atc),
#                                        sd_atc = sd(atc),
#                                        avg_vlf = mean(vlf),
#                                        sd_vlf = sd(vlf),
#                                        avg_hf = mean(hf),
#                                        sd_hf = sd(hf),
#                                        avg_lf = mean(lf),
#                                        sd_lf = sd(lf),
#                                        avg_lfhf = mean(lfhf),
#                                        sd_lfhf = sd(lfhf),
#                                        avg_SD1SD2 = mean(SD1SD2),
#                                        sd_SD1SD2 = sd(SD1SD2),
#                                        avg_respiratory_rate = mean(respiratory_rate),
#                                        sd_respiratory_rate = sd(respiratory_rate),
#                                        avg_spo2  = mean(spo2),
#                                        sd_spo2 = sd(spo2)
#                                         ),by = c("sub","pre_post")]
# 
# 
# 
# dt_prepro_overnight$pre_post <- factor(dt_prepro_overnight$pre_post, levels = c("pre","post"))
# demo_pre_post<-demo[sub_or %in% dt_prepro_overnight$sub,]
# dt_prepro_overnight<-left_join(dt_prepro_overnight,demo_pre_post,by = c("sub" = "sub_or"), all = T)
# 
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =avg_NNmean)) +
#   geom_boxplot()
# ggsave("overnight_NNmean.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =avg_SDNN)) +
#   geom_boxplot()
# ggsave("overnight_SDNN.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =avg_SD1SD2)) +
#   geom_boxplot()
# ggsave("overnight_SD12.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =avg_respiratory_rate)) +
#   geom_boxplot()
# ggsave("overnight_respiratory_rate.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =avg_spo2)) +
#   geom_boxplot()
# ggsave("overnight_spo2.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# # standard deviation
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =sd_NNmean)) +
#   geom_boxplot()
# ggsave("overnight_sd_NNmean.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =sd_SDNN)) +
#   geom_boxplot()
# ggsave("overnight_sd_SDNN.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =sd_SD1SD2)) +
#   geom_boxplot()
# ggsave("overnight_sd_SD12.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =sd_respiratory_rate)) +
#   geom_boxplot()
# ggsave("overnight_sd_respiratory_rate.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =sd_spo2)) +
#   geom_boxplot()
# ggsave("overnight_sd_spo2.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# dt_prepro_overnight$stroke_type <- as.factor(dt_prepro_overnight$stroke_type)
# dt_prepro_overnight$lifestyle <- as.factor(dt_prepro_overnight$lifestyle)
# 
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =avg_NNmean, group = sub, color = sub)) +
#   geom_line() + facet_grid(cols=vars(stroke_type)) +theme(legend.position="none")
# ggsave("overnight_avg_NNmean_stroke.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# 
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =avg_SDNN, group = sub, color = sub)) +
#   geom_line() + facet_grid(cols=vars(stroke_type)) +theme(legend.position="none")
# ggsave("overnight_avg_SDNN_stroke.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =avg_lfhf, group = sub, color = sub)) +
#   geom_line() + facet_grid(cols=vars(stroke_type)) +theme(legend.position="none")
# ggsave("overnight_avg_lfhf_stroke.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =avg_spo2, group = sub, color = sub)) +
#   geom_line() + facet_grid(cols=vars(stroke_type)) +theme(legend.position="none")
# ggsave("overnight_avg_lfhf_stroke_type.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =avg_hf, group = sub, color = sub)) +
#   geom_line() + facet_grid(cols=vars(stroke_type)) +theme(legend.position="none")
# ggsave("overnight_avg_hf_stroke.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# 
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =avg_lf, group = sub, color = sub)) +
#   geom_line() + facet_grid(cols=vars(stroke_type)) +theme(legend.position="none")
# ggsave("overnight_avg_lf_stroke.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# 
# 
# 
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =avg_NNmean, group = sub, color = sub)) +
#   geom_line() + facet_grid(cols=vars(lifestyle)) +theme(legend.position="none")
# ggsave("overnight_avg_NNmean_lifestyle.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# 
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =avg_SDNN, group = sub, color = sub)) +
#   geom_line() + facet_grid(cols=vars(lifestyle)) +theme(legend.position="none")
# ggsave("overnight_avg_SDNN_lifestyle.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
# 
# ggplot(data = dt_prepro_overnight, aes( x = pre_post, y =avg_lfhf, group = sub, color = sub)) +
#   geom_line() + facet_grid(cols=vars(lifestyle)) +theme(legend.position="none")
# ggsave("overnight_avg_lfhf_lifestyle.png", plot = last_plot(),path = paste0(pre_post_p,"/Graph/"),
#        device = "png")
