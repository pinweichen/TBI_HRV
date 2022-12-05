library(data.table)
library(caret)
library(tidyverse)
library(signal)
library(lubridate)
library(readxl)
library(viridis)
library(hrbrthemes)
library(irr)
rm(list=ls())
personalized <- T

general <- "Z:/TBI Sleep/Pilot_data/Preprocessed/level_3_ML/Personal_model/"
#general <- "/Volumes/RTO/TBI Sleep/Pilot_data/Preprocessed/level_3_ML"
if (personalized == T) {
  # result_p <- paste0(general,"/result_adam_original_within_sub")
  #subj <- list("001", "003", "004", "005", "007", "008", "009", "010")
  # subj <- list("001", "003", "004", "005", "007", "008",  "010") # LDA only
} else {
  result_p <- paste0(general,"/result_adam_original")
  subj <- list("001", "003", "004", "005", "006", "007", "008", "009", "010")
}

#result_p <- paste0(general,"/result_adam_original_record_wise")

#result_p <- paste0(general,"/result_adam_original_acti")
result_p <- paste0(general)
setwd(result_p)
subj <- list("TBI_1","TBI_2","TBI_3", "TBI_5", "TBI_6")
#model_ls <- list("Balanced Bagging Classifier", "Balanced Random Forest", "Gradient Boosting", "XGBoost")
#model_ls <- list("Balanced Bagging Classifier", "Balanced Random Forest")
#model_ls <- list("Balanced Bagging Classifier", "Balanced Random Forest", "Gradient Boosting", "XGBoost","LDA", "KNN", "SVC")
model_ls <- list("Balanced Random Forest")

stg_ls <- list("Min", "Med", "Max")
# Actigraph has only 9 subject's data
#subj <- list("001", "002", "003", "004", "005", "007", "008", "009", "010")
#subj <- seq(1,10,1)
# # Full set without actigraph read this list below

# personal model balance bag and balance rf


# md<-model_ls[[1]]
# stg<-stg_ls[[1]]
cnt_n <-1

stg_sum_ls <- list()
stg_ovall_ls <-list()
stg_cnt_ls <- list()

cm_skf_ls <- list()
conmat_skf_ls <- list()
sub_sum_ls <- list()
sub_conmat_ls <- list()


if (personalized == T) {
  for (p in 1:length(stg_ls)){
    #browser()
    stg<- stg_ls[[p]]
    conmat_md_ls <- list()
    cm_st_ls <- list()
    cnt_ls <- list()
    #kappa_ls <- list()
    for (q in 1:length(model_ls)){
      md <- model_ls[[q]]
      subf_p <- paste0(result_p,"/",md,"/",stg)
      sub_ls <- list()
      #browser()
      for (n in 1:length(subj)) {
        sub <- subj[[n]]
        dt <- fread(paste0(subf_p,"/pred_actual_",sub,"_skf.csv"))
        #dt <- fread(paste0(subf_p,"/pred_actual_",sub,".csv"))
        #dt <- fread(paste0(subf_p,"/pred_actual_",sub,".csv"))
        #skf_dt <- fread(paste0(subf_p,"/skf_index_",stg,"_",sub,".csv"))
        dt$V1 <- NULL
        dt$sub <- sub
        # browser()
        sub_ls[[n]] <- dt
        
        rm(dt)
      }
      
      full_dt <- do.call(rbind,sub_ls)
      full_dt$Actual<-as.factor((full_dt$Actual)+1)
      full_dt$Predicted<-as.factor((full_dt$Predicted)+1)
      
      full_cm<-confusionMatrix(data = full_dt$Predicted, reference = full_dt$Actual, dnn = c("Prediction", "Reference"))
      #browser()
      full_cm_st <-data.frame(full_cm$overall)
      setDT(full_cm_st)
      full_cm_st[,md := md]
      full_cm_st[,stg := stg]
      full_cm_st[,stat := names(full_cm$overall)]
      full_kappa <- full_cm_st[stat %in% "Kappa",]
      write.table(full_kappa,"kappa.csv", append = T)
      #browser()
      a_cnt<-summary(full_dt$Actual)
      a_cnt_dt<-data.frame(matrix(unlist(a_cnt), nrow=length(a_cnt), byrow=TRUE))
      colnames(a_cnt_dt) <- "counts"
      setDT(a_cnt_dt)
      a_cnt_dt[,md := md]
      a_cnt_dt[,class := seq(1,nrow(a_cnt_dt))]
      cnt_ls[[q]]<-a_cnt_dt
      sub_uni_ls <- unique(full_dt$sub)
      skf_uni_ls <- unique(full_dt$skf_index)

      for (sucnt in 1:length(unique(full_dt$sub))){
        for (skf_cnt in 1:length(unique(full_dt$skf_index))){
          sub_target <- sub_uni_ls[sucnt]
          skf_target <- skf_uni_ls[skf_cnt]
          subdt <- full_dt[sub %in% sub_target & skf_index %in% skf_target,]
          cm<-confusionMatrix(data = subdt$Predicted, reference = subdt$Actual, dnn = c("Prediction", "Reference"))
          # extract the confusion matrix values as data.frame
          cm_d <- as.data.frame(cm$table)
          # confusion matrix statistics as data.frame
          cm_st <-data.frame(cm$overall)
          setDT(cm_st)
          cm_st[,md := md]
          cm_st[,stg := stg]
          cm_st[,stat := names(cm$overall)]
          # round the values
          cm_st$cm.overall <- round(cm_st$cm.overall,2)
          # round the F1
          cm_bc <- data.frame(cm$byClass)
          
          #browser()
          if (ncol(cm_bc) == 1) {
            colnames(cm_bc) <- c("2 Stages")
            cm_bc <- data.frame(t(cm_bc))
            levels(cm_d$Prediction) <- c("Wake","Sleep")
            levels(cm_d$Reference) <- c("Wake","Sleep")
            
          } else if (ncol(cm_bc) == 3){
            colnames(cm_bc) <- c("Wake", "NREM", "REM")
            cm_bc <- data.frame(t(cm_bc))
            levels(cm_d$Prediction) <- c("Wake", "NREM", "REM")
            levels(cm_d$Reference) <- c("Wake", "NREM", "REM")
          } else if (ncol(cm_bc) == 4){
            colnames(cm_bc) <- c("Wake", "Light", "Deep", "REM")
            levels(cm_d$Prediction) <- c("Wake", "Light", "Deep", "REM")
            levels(cm_d$Reference) <- c("Wake", "Light", "Deep", "REM")
          } else {
            if (nrow(cm_bc) == 1) {
              rownames(cm_bc) <- c("2 Stages")
              stg_nm<-c("2 Stages")
              levels(cm_d$Prediction) <- c("Wake","Sleep")
              levels(cm_d$Reference) <- c("Wake","Sleep")
            } else if (nrow(cm_bc) == 3){
              stg_nm<-c("Wake", "NREM", "REM")
              rownames(cm_bc) <- stg_nm
              levels(cm_d$Prediction) <- stg_nm
              levels(cm_d$Reference) <- stg_nm
            } else if (nrow(cm_bc) == 4){
              stg_nm<-c("Wake", "Light", "Deep", "REM")
              rownames(cm_bc) <- stg_nm
              levels(cm_d$Prediction) <- stg_nm
              levels(cm_d$Reference) <- stg_nm
              
            }
          }
          
          
          
          #browser()
          setDT(cm_bc)
          cm_bc <- cm_bc[,c(2,5,6,7,11),with=F]
          cm_bc <- round(cm_bc,2)
          #browser()
          # here we also have the rounded percentage values
          cm_p <- as.data.frame(prop.table(full_cm$table,margin = 2))
          
          cm_d$Perc <- round(cm_p$Freq*100,2)
          
          library(ggplot2)     # to plot
          library(gridExtra)   # to put more
          library(grid)        # plot together
          #browser()
          
          if (ncol(cm_bc) == 1 | nrow(cm_bc) == 1) {
            # For sleep as the true class
            tp = cm_d$Freq[4]
            fp = cm_d$Freq[2]
            fn = cm_d$Freq[3]
            tn = cm_d$Freq[1]
            prec = tp/(tp+fp)
            rec = tp/(tp+fn)
            spec = tn/(tn+fp)
            cm_add <- data.frame((tn/(tn+fp)),
                                 (tp/(tp+fp)),
                                 (tp/(tp+fn)),
                                 ((2*prec*rec)/(prec+rec)),
                                 ((rec+spec)/2)
            )
            colnames(cm_add) <- colnames(cm_bc)
            cm_bc <- rbind(cm_bc,cm_add)
          }
          
          #browser()
          setDT(cm_bc)
          cm_bc[,md := md]
          setwd(result_p)
          rownames(cm_bc)
          cm_bc[,class := rownames(cm_bc)]
          cm_bc[,skf_index := skf_cnt]
          cm_bc[,sub := sub_target]
          cm_st[,skf_index := skf_cnt]
          cm_st[,sub := sub_target]
          
          # # all together
          # g<-grid.arrange(cm_d_p, cm_bc_p, nrow = 1, ncol = 2,
          #                 top=textGrob(paste0(stg," ",md," Confusion Matrix"),gp=gpar(fontsize=25,font=1)))
          # 
          cm_skf_ls [[skf_cnt]] <- cm_st
          conmat_skf_ls[[skf_cnt]] <- cm_bc
        }
        cm_skf <- do.call(rbind,cm_skf_ls)
        setDT(cm_skf)
        sub_sum_ls[[sucnt]] <- cm_skf
        
        conmat_skf <- do.call(rbind,conmat_skf_ls)
        setDT(conmat_skf)
        sub_conmat_ls[[sucnt]] <- conmat_skf
        
      }
      cm_sub <- do.call(rbind,sub_sum_ls)
      setDT(cm_sub)
      
      
      sub_conmat <- do.call(rbind,sub_conmat_ls)
      setDT(sub_conmat)
      #browser()
      
      # ============================================= 
      # Prepare Confusion Matrix Plot 
      # =============================================
      full_cm_d <- data.frame(full_cm$table)
      full_cm_bc <- data.frame(full_cm$byClass)
      full_cm_p <- as.data.frame(prop.table(full_cm$table,margin = 2))
      
      full_cm_d$Perc <- round(full_cm_p$Freq,2)
      
      library(ggplot2)     # to plot
      library(gridExtra)   # to put more
      library(grid)        # plot together
      #browser()
      if (ncol(full_cm_bc) == 1) {
        colnames(full_cm_bc) <- c("2 Stages")
        full_cm_bc <- data.frame(t(full_cm_bc))
        levels(full_cm_d$Prediction) <- c("Wake","Sleep")
        levels(full_cm_d$Reference) <- c("Wake","Sleep")
        
      } else if (ncol(full_cm_bc) == 3){
        colnames(full_cm_bc) <- c("Wake", "NREM", "REM")
        full_cm_bc <- data.frame(t(full_cm_bc))
        levels(full_cm_d$Prediction) <- c("Wake", "NREM", "REM")
        levels(full_cm_d$Reference) <- c("Wake", "NREM", "REM")
      } else if (ncol(full_cm_bc) == 4){
        colnames(full_cm_bc) <- c("Wake", "Light", "Deep", "REM")
        levels(full_cm_d$Prediction) <- c("Wake", "Light", "Deep", "REM")
        levels(full_cm_d$Reference) <- c("Wake", "Light", "Deep", "REM")
      } else {
        if (nrow(full_cm_bc) == 1) {
          rownames(full_cm_bc) <- c("2 Stages")
          stg_nm<-c("2 Stages")
          levels(full_cm_d$Prediction) <- c("Wake","Sleep")
          levels(full_cm_d$Reference) <- c("Wake","Sleep")
        } else if (nrow(full_cm_bc) == 3){
          stg_nm<-c("Wake", "NREM", "REM")
          rownames(full_cm_bc) <- stg_nm
          levels(full_cm_d$Prediction) <- stg_nm
          levels(full_cm_d$Reference) <- stg_nm
        } else if (nrow(full_cm_bc) == 4){
          stg_nm<-c("Wake", "Light", "Deep", "REM")
          rownames(full_cm_bc) <- stg_nm
          levels(full_cm_d$Prediction) <- stg_nm
          levels(full_cm_d$Reference) <- stg_nm
          
        }
      }
      # # plotting the matrix

      full_cm_d_p <-  ggplot(data = full_cm_d, aes( x =  Reference, y = Prediction , fill = Perc))+
        geom_tile() +
        #geom_text(aes(label = paste(Freq)), color = 'black', size = 5) +
        geom_text(aes(label = paste(Perc)), color = 'white', size = 5) +
        theme_light() + #guides(fill = "none") +
        #scale_fill_viridis(discrete=FALSE) +
        scale_fill_distiller(palette="Greens", direction=1,limits = c(0,1)) +
        ylim(rev(levels(full_cm_d$Prediction))) + xlab("Actual Stage") + ylab("Predicted Stage")
      #scale_fill_gradient(low="white", high="Black",limits = c(0,100))
      
      
      
      
      #browser()
      # plotting the stats
      # cm_st_p <-  tableGrob(cm_st)
      # cm_bc_p <-  tableGrob(cm_bc)
      fwrite(cm_sub,paste0(subf_p,"/",stg,"_",md,"_cm_subs.csv") ,row.names = T)
      fwrite(sub_conmat,paste0(subf_p,"/",stg,"_",md,"_conmat.csv") ,row.names = T)
      cm_sub[is.na(cm_sub)] <- 0
      cm_sub_sum<- cm_sub[,.(cm.overall = mean(cm.overall),
                std = sd(cm.overall)), by = . (stat,sub,md,stg)]
      sub_conmat[is.na(sub_conmat)] <- 0
      sub_conmat_sum <- sub_conmat[,.(
                Specificity = mean(Specificity),
                std_Specificity = sd(Specificity),
                Precision = mean(Precision),
                std_Precision = sd(Precision),
                Recall = mean(Recall),
                std_Recall = sd(Recall),
                F1 = mean(F1),
                std_F1 = sd(F1),
                Balanced.Accuracy = mean(Balanced.Accuracy),
                std_Bal_Accuracy = sd(Balanced.Accuracy)
                ), by = . (class,sub,md)]
      fwrite(cm_sub_sum,paste0(subf_p,"/",stg,"_",md,"_cm_sub_statistics.csv") ,row.names = T)
      fwrite(sub_conmat_sum,paste0(subf_p,"/",stg,"_",md,"_sub_conmat_statistics.csv") ,row.names = T)
      ggsave(paste0(subf_p,"/",stg,"_",md,"_Con_mat_new.png"), full_cm_d_p, width = 20, height = 10, units = "cm")
      conmat_md_ls[[q]] <- cm_sub_sum
      cm_st_ls[[q]] <- sub_conmat_sum
    }
    conmat_sum <- do.call(rbind,conmat_md_ls)
    setDT(conmat_sum)
    conmat_sum[,stg := stg]
    stg_sum_ls[[p]] <- conmat_sum
    
    cm_overall <- do.call(rbind, cm_st_ls)
    setDT(cm_overall)
    stg_ovall_ls[[p]] <- cm_overall
    #browser()
    cnt_dt <- do.call(rbind,cnt_ls)
    setDT(cnt_dt)
    cnt_dt[,stg := stg]
    stg_cnt_ls[[p]] <- cnt_dt
    
    # kappa_full_ls <- reduce(rbind,kappa_ls)
    # setDT(kappa_full_ls)
    # kappa_full_ls[,stg:=stg]
    # kappa_full_stg_ls[[p]] <- kappa_full_ls
  }
  
  names(stg_ovall_ls) <- stg_ls
  names(stg_sum_ls) <- stg_ls
  names(stg_cnt_ls) <- stg_ls
  
  # kapp_full_reulst <- reduce(rbind,kappa_full_stg_ls)
  # fwrite(kappa_full_result,"kappa_full.csv")
  
  min<-stg_sum_ls$Min
  med<-stg_sum_ls$Med
  max<-stg_sum_ls$Max
  
  min_ov<-stg_ovall_ls$Min
  med_ov<-stg_ovall_ls$Med
  max_ov<-stg_ovall_ls$Max
  
  min_cnt<-stg_cnt_ls$Min
  med_cnt<-stg_cnt_ls$Med
  max_cnt<-stg_cnt_ls$Max
  
  setDT(min_cnt)
  setDT(med_cnt)
  setDT(max_cnt)
  min_cnt_r <- min_cnt[,.(ratio = (sum(`counts`)/`counts`),
                          class = class
  ), by = "md"]
  med_cnt_r <- med_cnt[,.(ratio = (sum(`counts`)/`counts`),
                          class = class), by = "md"]
  
  max_cnt_r <- max_cnt[,.(ratio = (sum(`counts`)/`counts`),
                          class = class), by = "md"]
  
  
  fwrite(min,"2_stages.csv")
  fwrite(med,"3_stages.csv")
  fwrite(max,"4_stages.csv")
  fwrite(min_ov,"ov_2_stages.csv")
  fwrite(med_ov,"ov_3_stages.csv")
  fwrite(max_ov,"ov_4_stages.csv")
  
  
  
  
  
  
  
  setDT(min_ov)
  setDT(med_ov)
  setDT(max_ov)
  length(sub_ls)
  
  
  min_sum <- min_ov[,.(F1 = mean(F1), F1_sd = sd(F1)/sqrt(length(sub_ls)), B_accu = mean(`Balanced.Accuracy`), B_accu_sd = sd(`Balanced.Accuracy`)/sqrt(length(sub_ls))),by = .(md,class)]
  med_sum <- med_ov[,.(F1 = mean(F1), F1_sd = sd(F1)/sqrt(length(sub_ls)), B_accu = mean(`Balanced.Accuracy`), B_accu_sd = sd(`Balanced.Accuracy`)/sqrt(length(sub_ls))),by = .(md,class)]
  max_sum <- max_ov[,.(F1 = mean(F1), F1_sd = sd(F1)/sqrt(length(sub_ls)), B_accu = mean(`Balanced.Accuracy`), B_accu_sd = sd(`Balanced.Accuracy`)/sqrt(length(sub_ls))),by = .(md,class)]
  colnames(min_sum) <- c("model","class", "F1", "F1_SE", "Balanced Accuracy", "BA_SE")
  colnames(med_sum) <- c("model","class", "F1", "F1_SE", "Balanced Accuracy", "BA_SE")
  colnames(max_sum) <- c("model","class", "F1", "F1_SE", "Balanced Accuracy", "BA_SE")
  min_sum[,Stage := "2 Stages"]
  med_sum[,Stage := "3 Stages"]
  max_sum[,Stage := "4 Stages"]
  f1_sum <- rbind(min_sum,med_sum,max_sum)
  f1_sum$model <- as.factor(f1_sum$model)
  f1_sum$F1 <- as.numeric(f1_sum$F1)
  f1_sum$Stage <- as.factor(f1_sum$Stage)
  
  ggplot(data=f1_sum, aes(x=class, y=F1, fill = Stage)) +
    geom_bar(stat="identity") + 
    coord_flip() + 
    facet_grid(rows = vars(Stage),cols = vars(model), scales = "fixed") +
    scale_fill_brewer(palette = "RdYlBu") + 
    geom_errorbar(aes(ymin=F1-F1_SE, ymax=F1+F1_SE), width=.2,
                  position=position_dodge(.9)) +
    geom_text(aes(label=F1, y = F1 - 0.2))
  ggsave(paste0(result_p,"/","F1_sum_Balanced_RF_bag.png"), last_plot(), width = 25, height = 25, units = "cm")

  f1_sum$`Balanced Accuracy`<- round(f1_sum$`Balanced Accuracy`, digits = 3)
  ggplot(data=f1_sum, aes(x=class, y=`Balanced Accuracy`, fill = Stage)) +
    geom_bar(stat="identity") +
    coord_flip() +
    facet_grid(rows = vars(Stage),cols = vars(model), scales = "fixed") +
    scale_fill_brewer(palette = "RdYlBu") + 
    geom_errorbar(aes(ymin=`Balanced Accuracy`-BA_SE, ymax=`Balanced Accuracy`+BA_SE), width=.2,
                  position=position_dodge(.9)) 
  ggsave(paste0(result_p,"/","Balance_accu_sum_balance_rf_bag.png"), last_plot(), width = 25, height = 25, units = "cm")
  setwd(result_p)
  #levels(f1_sum$Stage) <- c("4 stage", "3 stages", "2 stages")
  f1_sum_w<-reshape(f1_sum, idvar = c("class","model"),timevar = "Stage", direction = "wide")
  setDT(f1_sum_w)
  fwrite(f1_sum_w,"summary_statistics.csv")
  
  
  
  
  f1_macro <-f1_sum[,.(
    F1 = mean(F1),
    `Balanced Accuracy` = mean(`Balanced Accuracy`)
  ),
  by = .(model,Stage)]
  
  fwrite(f1_macro,"macro_statistics.csv")
} else {

      for (p in 1:length(stg_ls)){
        #browser()
        stg<- stg_ls[[p]]
        conmat_md_ls <- list()
        cm_st_ls <- list()
        cnt_ls <- list()
        #kappa_ls <- list()
        for (q in 1:length(model_ls)){
          md <- model_ls[[q]]
          subf_p <- paste0(result_p,"/",md,"/",stg)
          sub_ls <- list()
          #browser()
          for (n in 1:length(subj)) {
            sub <- subj[[n]]
            dt <- fread(paste0(subf_p,"/pred_actual_",sub,".csv"))
            #dt <- fread(paste0(subf_p,"/pred_actual_",sub,".csv"))
            #dt <- fread(paste0(subf_p,"/pred_actual_",sub,".csv"))
            #skf_dt <- fread(paste0(subf_p,"/skf_index_",stg,"_",sub,".csv"))
            dt$V1 <- NULL
            dt$sub <- sub
            # browser()
          
            sub_ls[[n]] <- dt
            rm(dt)
          }
          
          full_dt <- do.call(rbind,sub_ls)
          full_dt$Actual<-as.factor((full_dt$Actual)+1)
          full_dt$Predicted<-as.factor((full_dt$Predicted)+1)
          
          full_cm<-confusionMatrix(data = full_dt$Predicted, reference = full_dt$Actual, dnn = c("Prediction", "Reference"))
          full_cm_st <-data.frame(full_cm$overall)
          setDT(full_cm_st)
          full_cm_st[,md := md]
          full_cm_st[,stg := stg]
          full_cm_st[,stat := names(full_cm$overall)]
          full_kappa <- full_cm_st[stat %in% "Kappa",]
          
          write.table(full_kappa,"kappa.csv", append = T, row.names = F)
          #browser()
          a_cnt<-summary(full_dt$Actual)
          a_cnt_dt<-data.frame(matrix(unlist(a_cnt), nrow=length(a_cnt), byrow=TRUE))
          colnames(a_cnt_dt) <- "counts"
          setDT(a_cnt_dt)
          a_cnt_dt[,md := md]
          a_cnt_dt[,class := seq(1,nrow(a_cnt_dt))]
          cnt_ls[[q]]<-a_cnt_dt
          sub_uni_ls <- unique(full_dt$sub)
          skf_uni_ls <- unique(full_dt$skf_index)
          
          # round the values
          
          for (sucnt in 1:length(unique(full_dt$sub))){
              sub_target <- sub_uni_ls[sucnt]
              subdt <- full_dt[sub %in% sub_target,]
              cm<-confusionMatrix(data = subdt$Predicted, reference = subdt$Actual, dnn = c("Prediction", "Reference"))
              # extract the confusion matrix values as data.frame
              cm_d <- as.data.frame(cm$table)
              # confusion matrix statistics as data.frame
              cm_st <-data.frame(cm$overall)
              setDT(cm_st)
              cm_st[,md := md]
              cm_st[,stg := stg]
              cm_st[,stat := names(cm$overall)]
              # round the values
              cm_st$cm.overall <- round(cm_st$cm.overall,2)
              # round the F1
              cm_bc <- data.frame(cm$byClass)
              
              #browser()
              if (ncol(cm_bc) == 1) {
                colnames(cm_bc) <- c("2 Stages")
                cm_bc <- data.frame(t(cm_bc))
                levels(cm_d$Prediction) <- c("Wake","Sleep")
                levels(cm_d$Reference) <- c("Wake","Sleep")
                
              } else if (ncol(cm_bc) == 3){
                colnames(cm_bc) <- c("Wake", "NREM", "REM")
                cm_bc <- data.frame(t(cm_bc))
                levels(cm_d$Prediction) <- c("Wake", "NREM", "REM")
                levels(cm_d$Reference) <- c("Wake", "NREM", "REM")
              } else if (ncol(cm_bc) == 4){
                colnames(cm_bc) <- c("Wake", "Light", "Deep", "REM")
                levels(cm_d$Prediction) <- c("Wake", "Light", "Deep", "REM")
                levels(cm_d$Reference) <- c("Wake", "Light", "Deep", "REM")
              } else {
                if (nrow(cm_bc) == 1) {
                  rownames(cm_bc) <- c("2 Stages")
                  stg_nm<-c("2 Stages")
                  levels(cm_d$Prediction) <- c("Wake","Sleep")
                  levels(cm_d$Reference) <- c("Wake","Sleep")
                } else if (nrow(cm_bc) == 3){
                  stg_nm<-c("Wake", "NREM", "REM")
                  rownames(cm_bc) <- stg_nm
                  levels(cm_d$Prediction) <- stg_nm
                  levels(cm_d$Reference) <- stg_nm
                } else if (nrow(cm_bc) == 4){
                  stg_nm<-c("Wake", "Light", "Deep", "REM")
                  rownames(cm_bc) <- stg_nm
                  levels(cm_d$Prediction) <- stg_nm
                  levels(cm_d$Reference) <- stg_nm
                  
                }
              }
              
              
              
              #browser()
              setDT(cm_bc)
              cm_bc <- cm_bc[,c(2,5,6,7,11),with=F]
              cm_bc <- round(cm_bc,2)
              #browser()
              # here we also have the rounded percentage values
              cm_p <- as.data.frame(prop.table(cm$table,margin = 2))
              
              cm_d$Perc <- round(cm_p$Freq*100,2)
              
              library(ggplot2)     # to plot
              library(gridExtra)   # to put more
              library(grid)        # plot together
              #browser()
              
              if (ncol(cm_bc) == 1 | nrow(cm_bc) == 1) {
                # For sleep as the true class
                tp = cm_d$Freq[4]
                fp = cm_d$Freq[2]
                fn = cm_d$Freq[3]
                tn = cm_d$Freq[1]
                prec = tp/(tp+fp)
                rec = tp/(tp+fn)
                spec = tn/(tn+fp)
                cm_add <- data.frame((tn/(tn+fp)),
                                     (tp/(tp+fp)),
                                     (tp/(tp+fn)),
                                     ((2*prec*rec)/(prec+rec)),
                                     ((rec+spec)/2)
                )
                colnames(cm_add) <- colnames(cm_bc)
                cm_bc <- rbind(cm_bc,cm_add)
              }
              
              #browser()
              setDT(cm_bc)
              cm_bc[,md := md]
              setwd(result_p)
              rownames(cm_bc)
              cm_bc[,class := rownames(cm_bc)]
              cm_bc[,sub := sub_target]
              cm_st[,sub := sub_target]
              
              # all together
              # g<-grid.arrange(cm_d_p, cm_bc_p, nrow = 1, ncol = 2, 
              #                 top=textGrob(paste0(stg," ",md," Confusion Matrix"),gp=gpar(fontsize=25,font=1)))
              

            sub_sum_ls[[sucnt]] <- cm_st
            sub_conmat_ls[[sucnt]] <- cm_bc
            
          }
          cm_sub <- do.call(rbind,sub_sum_ls)
          setDT(cm_sub)
          
          
          sub_conmat <- do.call(rbind,sub_conmat_ls)
          setDT(sub_conmat)
          # browser()
          
          # ============================================= 
          # Prepare Confusion Matrix Plot 
          # =============================================
          # 
          # 
          # 
          full_cm_d<-data.frame(full_cm$table)
          full_cm_bc<-data.frame(full_cm$byClass)
          #browser()
          if (ncol(full_cm_bc) == 1) {
            colnames(full_cm_bc) <- c("2 Stages")
            full_cm_bc <- data.frame(t(full_cm_bc))
            levels(full_cm_d$Prediction) <- c("Wake","Sleep")
            levels(full_cm_d$Reference) <- c("Wake","Sleep")
            
          } else if (ncol(full_cm_bc) == 3){
            colnames(full_cm_bc) <- c("Wake", "NREM", "REM")
            full_cm_bc <- data.frame(t(full_cm_bc))
            levels(full_cm_d$Prediction) <- c("Wake", "NREM", "REM")
            levels(full_cm_d$Reference) <- c("Wake", "NREM", "REM")
          } else if (ncol(full_cm_bc) == 4){
            colnames(full_cm_bc) <- c("Wake", "Light", "Deep", "REM")
            levels(full_cm_d$Prediction) <- c("Wake", "Light", "Deep", "REM")
            levels(full_cm_d$Reference) <- c("Wake", "Light", "Deep", "REM")
          } else {
            if (nrow(full_cm_bc) == 1) {
              rownames(full_cm_bc) <- c("2 Stages")
              stg_nm<-c("2 Stages")
              levels(full_cm_d$Prediction) <- c("Wake","Sleep")
              levels(full_cm_d$Reference) <- c("Wake","Sleep")
            } else if (nrow(full_cm_bc) == 3){
              stg_nm<-c("Wake", "NREM", "REM")
              rownames(full_cm_bc) <- stg_nm
              levels(full_cm_d$Prediction) <- stg_nm
              levels(full_cm_d$Reference) <- stg_nm
            } else if (nrow(full_cm_bc) == 4){
              stg_nm<-c("Wake", "Light", "Deep", "REM")
              rownames(full_cm_bc) <- stg_nm
              levels(full_cm_d$Prediction) <- stg_nm
              levels(full_cm_d$Reference) <- stg_nm
              
            }
          }
          
          
          
          #browser()
          setDT(full_cm_bc)
          full_cm_bc <- full_cm_bc[,c(2,5,6,7,11),with=F]
          full_cm_bc <- round(full_cm_bc,2)
          #browser()
          # here we also have the rounded percentage values
          full_cm_p <- as.data.frame(prop.table(full_cm$table,margin = 2))
          
          full_cm_d$Perc <- round(full_cm_p$Freq,2)
          
          
          
          # # plotting the matrix
          full_cm_d_p <-  ggplot(data = full_cm_d, aes( x =  Reference, y = Prediction , fill = Perc))+
            geom_tile() +
            #geom_text(aes(label = paste(Freq)), color = 'black', size = 5) +
            geom_text(aes(label = paste(Perc)), color = 'white', size = 5) +
            theme_light() + #guides(fill = "none") +
            #scale_fill_viridis(discrete=FALSE) +
            scale_fill_distiller(palette="Greens", direction=1,limits = c(0,1)) +
            ylim(rev(levels(full_cm_d$Prediction))) + xlab("Actual Stage") + ylab("Predicted Stage")
          #scale_fill_gradient(low="white", high="Black",limits = c(0,100))
          
          
          
          
          #browser()
          # plotting the stats
          # cm_st_p <-  tableGrob(cm_st)
          # cm_bc_p <-  tableGrob(cm_bc)
          fwrite(cm_sub,paste0(subf_p,"/",stg,"_",md,"_cm_subs.csv") ,row.names = T)
          fwrite(sub_conmat,paste0(subf_p,"/",stg,"_",md,"_conmat.csv") ,row.names = T)
          cm_sub[is.na(cm_sub)] <- 0
          cm_sub_sum<- cm_sub[,.(cm.overall = mean(cm.overall),
                                 std = sd(cm.overall)), by = . (stat,md,stg)]
          sub_conmat[is.na(sub_conmat)] <- 0
          sub_conmat_sum <- sub_conmat[,.(
            Specificity = mean(Specificity),
            std_Specificity = sd(Specificity),
            Precision = mean(Precision),
            std_Precision = sd(Precision),
            Recall = mean(Recall),
            std_Recall = sd(Recall),
            F1 = mean(F1),
            std_F1 = sd(F1),
            Balanced.Accuracy = mean(Balanced.Accuracy),
            std_Bal_Accuracy = sd(Balanced.Accuracy)
          ), by = . (class,md)]
          fwrite(cm_sub_sum,paste0(subf_p,"/",stg,"_",md,"_cm_statistics.csv") ,row.names = T)
          fwrite(sub_conmat_sum,paste0(subf_p,"/",stg,"_",md,"_conmat_statistics.csv") ,row.names = T)
          ggsave(paste0(subf_p,"/",stg,"_",md,"_Con_mat_new.png"), cm_d_p, width = 20, height = 10, units = "cm")
          conmat_md_ls[[q]] <- cm_sub_sum
          cm_st_ls[[q]] <- sub_conmat_sum
        }
        conmat_sum <- do.call(rbind,conmat_md_ls)
        setDT(conmat_sum)
        conmat_sum[,stg := stg]
        stg_sum_ls[[p]] <- conmat_sum
    cm_overall <- do.call(rbind, cm_st_ls)
    setDT(cm_overall)
    stg_ovall_ls[[p]] <- cm_overall
    #browser()
    cnt_dt <- do.call(rbind,cnt_ls)
    setDT(cnt_dt)
    cnt_dt[,stg := stg]
    stg_cnt_ls[[p]] <- cnt_dt
    
    # kappa_full_ls <- reduce(rbind,kappa_ls)
    # setDT(kappa_full_ls)
    # kappa_full_ls[,stg:=stg]
    # kappa_full_stg_ls[[p]] <- kappa_full_ls
  }
  
  # kapp_full_reulst <- reduce(rbind,kappa_full_stg_ls)
  # fwrite(kappa_full_result,"kappa_full.csv")
  
  
  names(stg_ovall_ls) <- stg_ls
  names(stg_sum_ls) <- stg_ls
  names(stg_cnt_ls) <- stg_ls
  
  
  
  min<-stg_sum_ls$Min
  med<-stg_sum_ls$Med
  max<-stg_sum_ls$Max
  
  min_ov<-stg_ovall_ls$Min
  med_ov<-stg_ovall_ls$Med
  max_ov<-stg_ovall_ls$Max
  
  min_cnt<-stg_cnt_ls$Min
  med_cnt<-stg_cnt_ls$Med
  max_cnt<-stg_cnt_ls$Max
  
  setDT(min_cnt)
  setDT(med_cnt)
  setDT(max_cnt)
  min_cnt_r <- min_cnt[,.(ratio = (sum(`counts`)/`counts`),
                          class = class
  ), by = "md"]
  med_cnt_r <- med_cnt[,.(ratio = (sum(`counts`)/`counts`),
                          class = class), by = "md"]
  
  max_cnt_r <- max_cnt[,.(ratio = (sum(`counts`)/`counts`),
                          class = class), by = "md"]
  
  
  fwrite(min,"2_stages.csv")
  fwrite(med,"3_stages.csv")
  fwrite(max,"4_stages.csv")
  fwrite(min_ov,"ov_2_stages.csv")
  fwrite(med_ov,"ov_3_stages.csv")
  fwrite(max_ov,"ov_4_stages.csv")


setDT(min_ov)
setDT(med_ov)
setDT(max_ov)
length(sub_ls)

min_sum <- min_ov[,.(F1, F1_sd = std_F1/sqrt(length(sub_ls)), B_accu = mean(`Balanced.Accuracy`), B_accu_sd = std_Bal_Accuracy/sqrt(length(sub_ls))),by = .(md,class)]
med_sum <- med_ov[,.(F1, F1_sd = std_F1/sqrt(length(sub_ls)), B_accu = mean(`Balanced.Accuracy`), B_accu_sd = std_Bal_Accuracy/sqrt(length(sub_ls))),by = .(md,class)]
max_sum <- max_ov[,.(F1, F1_sd = std_F1/sqrt(length(sub_ls)), B_accu = mean(`Balanced.Accuracy`), B_accu_sd = std_Bal_Accuracy/sqrt(length(sub_ls))),by = .(md,class)]
colnames(min_sum) <- c("model","class", "F1", "F1_SE", "Balanced Accuracy", "BA_SE")
colnames(med_sum) <- c("model","class", "F1", "F1_SE", "Balanced Accuracy", "BA_SE")
colnames(max_sum) <- c("model","class", "F1", "F1_SE", "Balanced Accuracy", "BA_SE")
min_sum[,Stage := "2 Stages"]
med_sum[,Stage := "3 Stages"]
max_sum[,Stage := "4 Stages"]
f1_sum <- rbind(min_sum,med_sum,max_sum)
f1_sum$model <- as.factor(f1_sum$model)
f1_sum$F1 <- as.numeric(f1_sum$F1)
f1_sum$Stage <- as.factor(f1_sum$Stage)
f1_sum$F1 <- round(f1_sum$F1,3)
ggplot(data=f1_sum, aes(x=class, y=F1, fill = Stage)) +
  geom_bar(stat="identity") + 
  coord_flip() + 
  facet_grid(rows = vars(Stage), cols = vars(model), scales = "fixed") +
  scale_fill_brewer(palette = "RdYlBu") + 
  geom_errorbar(aes(ymin=F1-F1_SE, ymax=F1+F1_SE), width=.2,
                position=position_dodge(.9)) +
  geom_text(aes(label=F1, y = F1 - 0.2))
ggsave(paste0(result_p,"/","F1_sum_Balanced_RF_bag.png"), last_plot(), width = 25, height = 25, units = "cm")

ggplot(data=f1_sum, aes(x=class, y=`Balanced Accuracy`, fill = Stage)) +
  geom_bar(stat="identity") +
  coord_flip() +
  facet_grid(rows = vars(Stage), cols = vars(model), scales = "fixed") +
  scale_fill_brewer(palette = "RdYlBu") + 
  geom_errorbar(aes(ymin=`Balanced Accuracy`-BA_SE, ymax=`Balanced Accuracy`+BA_SE), width=.2,
                position=position_dodge(.9)) +
  geom_text(aes(label=`Balanced Accuracy`, y = `Balanced Accuracy` - 0.2))
ggsave(paste0(result_p,"/","Balance_accu_sum_balance_rf_bag.png"), last_plot(), width = 25, height = 25, units = "cm")
setwd(result_p)
#levels(f1_sum$Stage) <- c("4 stage", "3 stages", "2 stages")
f1_sum_w<-reshape(f1_sum, idvar = c("class","model"),timevar = "Stage", direction = "wide")
setDT(f1_sum_w)
fwrite(f1_sum_w,"summary_statistics.csv")

f1_macro <-f1_sum[,.(
          F1 = mean(F1),
          `Balanced Accuracy` = mean(`Balanced Accuracy`)
          ),
       by = .(model,Stage)]

fwrite(f1_macro,"macro_statistics.csv")
}

# Graph Hypnogram
sub <- full_dt[sub %in% "TBI_6",]
setDT(sub)
sub[Predicted %in% 1, Predicted := "Wake"]
sub[Predicted %in% 2, Predicted := "Light"]
sub[Predicted %in% 3, Predicted := "Deep"]
sub[Predicted %in% 4, Predicted := "REM"]

sub[Actual %in% 1, Actual := "Wake"]
sub[Actual %in% 2, Actual := "Light"]
sub[Actual %in% 3, Actual := "Deep"]
sub[Actual %in% 4, Actual := "REM"]

sub$Predicted <- factor(sub$Predicted, levels = c("Deep", "Light",  "REM", "Wake" ))
sub$Actual <- factor(sub$Actual, levels = c("Deep", "Light",  "REM", "Wake" ))
#levels(sub$Predicted) <- c("Wake",  "REM", "Light", "Deep")
#levels(sub$Actual) <- c("Wake",  "REM", "Light", "Deep")
a<- 1:nrow(sub)
b <- a/2
sub[, time := b,]
sub[, time := time/60]

ggplot(data=sub, aes(x=time, y=Actual, group = 1)) +
  geom_line( color = "black", size = 6) + geom_line(data=sub, aes(x=time, y=Predicted, group = 1),
                                                    color = "Dark Green", size = 2) +
  scale_x_continuous(name="Time (hrs)", limits=c(0, 8), breaks=seq(0,8,1)) + ylab("Sleep Stages") +
  theme_classic(
    base_size = 50,
    base_family = "",
    base_line_size = 5)
ggsave(paste0(result_p,"/","Hypnogram_",unique(sub$sub),"RF.eps"), last_plot(), 
       device = "eps")


# 
#   
#   
# # ============== Deep learning  =================
# 
# library(data.table)
# library(caret)
# library(tidyverse)
# library(signal)
# library(lubridate)
# library(readxl)
# rm(list=ls())
# 
# general <- "Z:/SIESTA/ANNE Validation/Sleep Study Stroke Validation/Data"
# result_p <- paste0(general,"/Preprocessed/result_cnnrnn/stage_3")
# setwd(result_p)
# 
# dt<-fread("original_sub_6_dt.csv")
# dt$predictions <- as.factor(dt$predictions)
# levels(dt$predictions) <- c("0","1","2")
# dt$truth <- as.factor(dt$truth)
# cm<-confusionMatrix(data = dt$predictions, reference = dt$truth, dnn = c("Prediction", "Reference"))
# 
# 
# 
# # ============================================= 
# # Prepare Confusion Matrix Plot 
# # =============================================
# 
# 
# # extract the confusion matrix values as data.frame
# cm_d <- as.data.frame(cm$table)
# # confusion matrix statistics as data.frame
# cm_st <-data.frame(cm$overall)
# # round the values
# cm_st$cm.overall <- round(cm_st$cm.overall,2)
# # round the F1
# cm_bc <- data.frame(cm$byClass)
# cm_bc <- cm_bc[,c(1,2,5,6,7,11)]
# cm_bc <- t(cm_bc)
# colnames(cm_bc) <- c("Wake", "NREM", "REM")
# cm_bc <- round(cm_bc,2)
# 
# # here we also have the rounded percentage values
# cm_p <- as.data.frame(prop.table(cm$table))
# cm_d$Perc <- round(cm_p$Freq*100,2)
# 
# library(ggplot2)     # to plot
# library(gridExtra)   # to put more
# library(grid)        # plot together
# # browser()
# # plotting the matrix
# cm_d_p <-  ggplot(data = cm_d, aes(x = Prediction , y =  ordered(Reference, leveles = rev(levels)), fill = Perc))+
#   geom_tile() +
#   geom_text(aes(label = paste(Freq)), color = 'white', size = 5) +
#   #geom_text(aes(label = paste(Perc,"%")), color = 'white', size = 5) +
#   theme_light() + guides(fill = "none") +
#    scale_fill_continuous(low="light green", high="dark green")
# 
# # plotting the stats
# cm_st_p <-  tableGrob(cm_st)
# cm_bc_p <-  tableGrob(cm_bc)
# # all together
# g<-grid.arrange(cm_d_p, cm_bc_p, nrow = 1, ncol = 2, 
#              top=textGrob("Confusion Matrix and Statistics",gp=gpar(fontsize=25,font=1)))
# 
# ggsave(paste0(result_p,"/Con_mat.png"), g, width = 20, height = 10, units = "cm")
