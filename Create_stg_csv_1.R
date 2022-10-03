
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
sub = 1
general <- "Z:/TBI Sleep/Pilot_data/PSG/"
setwd(paste0(general,"Pilot_",sub,"/labeled/xq3ksytvnbv1t2q.reports/"))
dt <- read_html("Epoch Report.html")

dtnode <- html_nodes(dt,"table")
dtnode

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

setwd(paste0(general,"Pilot_",sub,"/labeled/"))
fwrite(dt_all,"stg.csv")
