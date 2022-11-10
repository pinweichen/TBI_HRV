
# Chapter 1: Demographics -------

library(data.table)
library(caret)
library(tidyverse)
library(signal)
library(lubridate)
library(readxl)

rm(list=ls())
# Setup paths

#general <- "/Volumes/RTO/TBI Sleep"
general <- "Z:/TBI Sleep"
data_path <- paste0(general,"/Pilot_data")

# Load ANNE folders --------------------------------
ANNE_p <- paste0(data_path,"/ANNE")
setwd(ANNE_p)
ls_fd = list.files(pattern = "TBI*")
ls_subfd <- list()

for (it in 1:length(ls_fd)){
  p <- ls_fd[it]
  anne_p <- paste0(ANNE_p,"/",p,"/")
  setwd(anne_p)
  lsf =  list.dirs(path = ".", full.names = F, recursive = F)
  #browser()
  if (length(lsf) >0){
    lsfa <- list()
    for (ut in 1:length(lsf)){
      pa <- lsf[ut]
      paf<-paste0(anne_p,"/",pa)
      ls_subfile <- list.files(path = paf, full.names = F, recursive = F)
      if (length(ls_subfile) > 0) {
        dt <-data.table(files = ls_subfile, 
                        subfolder = pa)
        lsfa <- append(lsfa, list(dt), after = length(lsfa))
      } else {
        dt <-data.table(files = "N/A", 
                        subfolder = pa)
        lsfa <- append(lsfa, list(dt), after = length(lsfa))
      }
    }
    lsfadt <- do.call(rbind,lsfa)
    setDT(lsfadt)
    lsfadt<-lsfadt[,subject := p]
    ls_subfd <- append(ls_subfd, list(lsfadt), after = length(ls_subfd))
  }
}
sbdt <- do.call(rbind,ls_subfd)
sbdt <- na.omit(sbdt)

transcr<-sbdt[grep("tra", sbdt$subfolder),]
transcr<-transcr[!(subfolder %in% "transcribe"),]
setwd(paste0(data_path))
fwrite(transcr,"folder_incorrect_list.csv")

rm(lsfa,lsfadt,ls_subfd,dt, transcr)

# Count data existed with
# Load ANNE folders --------------------------------

preprocessed_p <- sbdt[files %in% "processed_chest_ecg.csv", ]


#diff_sub <- setdiff(preprocessed_p$subject,demo$ID)


# Count and organized preprocessed HRV data folder ===============
ANNE_hrv_p <- paste0(data_path,"/Preprocessed/ANNE/HRV")
setwd(ANNE_hrv_p)
# ls_fd_pre = list.files(pattern = "SIESTA*")
# ls_subfd_pre <- list()
lsf =  list.files(pattern = "TBI*")
#browser()
lsfa <- list()
if (length(lsf) > 0) {
    
  for (ut in 1:length(lsf)){
    #browser()
    pa <- lsf[ut]
    paf<-paste0(ANNE_hrv_p,"/",pa)
    #setwd(paf)
    ls_subfile <- list.files(path = paf, full.names = F, recursive = F)
    if (length(ls_subfile) > 0) {
      dt <-data.table(files = ls_subfile, 
                      subfolder = pa)
      lsfa <- append(lsfa, list(dt), after = length(lsfa))
    } else {
      dt <-data.table(files = "N/A", 
                      subfolder = pa)
      lsfa <- append(lsfa, list(dt), after = length(lsfa))
    }
    
  }
  lsfadt <- do.call(rbind,lsfa)
  setDT(lsfadt)
} else {
  lsfadt <- data.table()
}

setwd(ANNE_hrv_p)
fwrite(lsfadt,"hrv_processed_list.csv")

preprocessed_p[,index := 1:nrow(preprocessed_p)]
ls_ind <- {}
ls_total_ind <- {}
for (i in unique(lsfadt$subfolder)) {
  #browser()
  ls_ind <- preprocessed_p[subject %in% i,index]
  ls_total_ind <- append(ls_total_ind,ls_ind,after = length(ls_total_ind))
  
}
preprocessed_px <- preprocessed_p[!(index %in% ls_total_ind),]


#preprocessed_sublist <- preprocessed_px[!(subject %in% diff_sub),.(subject,subfolder)]
setwd(data_path)
fwrite(preprocessed_px,"preprocessed_sublist.csv")
