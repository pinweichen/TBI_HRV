library(data.table)
library(caret)
library(tidyverse)
library(signal)
library(lubridate)
library(readxl)
library(viridis)
library(hrbrthemes)
rm(list=ls())
# personalized <- F

# shhs preprocessed\level_3_ML

general <- "Z:/TBI Sleep/Pilot_data/Preprocessed"
#general <- "/Volumes/RTO/SIESTA/ANNE Validation/Sleep Study Stroke Validation/Data"
# if (personalized == T) {
#   result_p <- paste0(general,"/result_adam_original_within_sub")
#   #subj <- list("001", "003", "004", "005", "007", "008", "009", "010")
#   subj <- list("001", "003", "004", "005", "007", "008",  "010") # LDA only
# } else {
#   result_p <- paste0(general,"/result_adam_original")
#   subj <- list("001", "003", "004", "005", "006", "007", "008", "009", "010")
# }

result_p <- paste0(general,"/level_3_ML/Personal_model")
setwd(result_p)

#model_ls <- list("Balanced Bagging Classifier", "Balanced Random Forest", "Gradient Boosting", "XGBoost")
md <- "Balanced Random Forest"
stg <- "Max"
sub <- 2
subf_p <- paste0(result_p,"/",md,"/",stg)
dt <- fread(paste0(subf_p,"/pred_actual_TBI_",sub,"_skf.csv"))

# Graph Hypnogram
dt$Actual <- (dt$Actual + 1)
dt$Actual <- as.factor(dt$Actual)

dt$Predicted <- (dt$Predicted + 1)
dt$Predicted <- as.factor(dt$Predicted)

setDT(dt)
dt[Predicted %in% 1, Predicted := "Wake"]
dt[Predicted %in% 2, Predicted := "Light"]
dt[Predicted %in% 3, Predicted := "Deep"]
dt[Predicted %in% 4, Predicted := "REM"]

dt[Actual %in% 1, Actual := "Wake"]
dt[Actual %in% 2, Actual := "Light"]
dt[Actual %in% 3, Actual := "Deep"]
dt[Actual %in% 4, Actual := "REM"]

dt$Predicted <- factor(dt$Predicted, levels = c("Deep", "Light",  "REM", "Wake" ))
dt$Actual <- factor(dt$Actual, levels = c("Deep", "Light",  "REM", "Wake" ))
#levels(dt$Predicted) <- c("Wake",  "REM", "Light", "Deep")
#levels(dt$Actual) <- c("Wake",  "REM", "Light", "Deep")
a<- 1:nrow(dt)
b <- a/2
dt[, time := b,]
dt[, time := time/60]

ggplot(data=dt, aes(x=time, y=Actual, group = 1)) +
  geom_line( color = "black", size = 6) + geom_line(data=dt, aes(x=time, y=Predicted, group = 1),
                                                    color = "Dark Green", size = 2) +
  scale_x_continuous(name="Time (hrs)", limits=c(0, 8), breaks=seq(0,8,1)) + ylab("Sleep Stages") +
  theme_classic(
    base_size = 50,
    base_family = "",
    base_line_size = 5)
#ggsave(paste0(result_p,"/","Hypnogram_",unique(dt$sub),"RF.eps"), last_plot(), 
#       device = "eps")

## Calculate Each stage percentage

actual_dt <- dt[,.(count = .N,
                   percent = .N/nrow(dt),
                   ML_type = "Actual"), by = Actual]
colnames(actual_dt) <- c("Sleep_stage","count",   "percent", "ML_type")
predict_dt <- dt[,.(count = .N,
                    percent = .N/nrow(dt),
                    ML_type = "Predicted"), by = Predicted]
colnames(predict_dt) <- c("Sleep_stage","count",   "percent", "ML_type")
summary_dt <- rbind(actual_dt,predict_dt)

#setwd(result_p)
#fwrite(summary_dt, "TBI_2_percentage.csv")


ggplot(summary_dt, aes(x="", y=percent, fill=Sleep_stage)) +
  
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_label(aes(label = paste0(round(percent*100,1),"%")),
             colour = c("black",  "white", "white","white", "black", "white","white", "white"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  scale_fill_viridis_d()  + 
  xlab("") + ylab("") + labs(fill = "Sleep Stage")  + 
  facet_grid(.~ ML_type) +
  theme_void() + 
  theme(strip.text.x = element_text(size=12, face="bold"))

ggsave("TBI_2_percentage_pie.eps", last_plot(), device = "eps")




## Sleep Metrics ---
## Actual
#Find Start and stop time of sleep
dt[, Actual_s := shift(Actual, n=1)]
dt[!(Actual %in% "Wake"),sleep_wake_act := "sleep"]
dt[(Actual %in% "Wake"),sleep_wake_act := "wake"]
first_act<-dt[match(unique(dt$Actual), dt$Actual),]
start_time <- dt[min(which(dt$sleep_wake_act %in% "sleep")),time]
stop_time <- dt[max(which(dt$sleep_wake_act %in% "sleep")),time]

sleep_metrics_act <- data.table(
  tst = length(which(dt$sleep_wake_act %in% "sleep"))*0.5,
  WASO = (nrow(dt)-nrow(dt[time >= start_time & sleep_wake_act %in% "sleep",]))*0.5,
  latency = start_time*60,
  efficiency = length(which(dt$sleep_wake_act %in% "sleep"))/nrow(dt)
  )
# Predicted

dt[, Predicted_s := shift(Predicted, n=1)]
dt[!(Predicted %in% "Wake"),sleep_wake_pred := "sleep"]
dt[(Predicted %in% "Wake"),sleep_wake_pred := "wake"]
first_pred<-dt[match(unique(dt$Predicted), dt$Predicted),]
start_time <- dt[min(which(dt$sleep_wake_pred %in% "sleep")),time]
stop_time <- dt[max(which(dt$sleep_wake_pred %in% "sleep")),time]

sleep_metrics_pred <- data.table(
  tst = length(which(dt$sleep_wake_pred %in% "sleep"))*0.5,
  WASO = (nrow(dt)-nrow(dt[time >= start_time & sleep_wake_pred %in% "sleep",]))*0.5,
  latency = start_time*60,
  efficiency = length(which(dt$sleep_wake_pred %in% "sleep"))/nrow(dt)
)

sleep_metrics_act$ML_type <- "Actual"
sleep_metrics_pred$ML_type <- "Predicted"

sleep_metrics <- rbind(sleep_metrics_act,sleep_metrics_pred)
fwrite(sleep_metrics,"TBI_2_sleep_metrics.csv")


