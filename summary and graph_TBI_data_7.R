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

result_p <- paste0(general,"/level_3_ML/Population_model")

#result_p <- paste0(general,"/result_adam_original_acti")
setwd(result_p)

#model_ls <- list("Balanced Bagging Classifier", "Balanced Random Forest", "Gradient Boosting", "XGBoost")
model_ls <- list("Balanced Random Forest")
#model_ls <- list("Balanced Bagging Classifier")
#model_ls <- list("Balanced Bagging Classifier", "Balanced Random Forest", "Gradient Boosting", "XGBoost","LDA", "KNN", "SVC")
#
stg_ls <- list("Min", "Med", "Max")
# Actigraph has only 9 subject's data
skf_ls <- 1:5
sub_ls <- c(3,4,5)

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

# if (personalized == T) {
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
    #browser()
    stats_p <- paste0(subf_p,"/stats/")
    dir.create(stats_p)
    setwd(stats_p)
    sub_sum_ls <- list()
    sub_conmat_ls <- list()
    sub_dt_ls <- list()
      for (n in 1:length(sub_ls)) {
        skf_dt_ls <- list()
        skf_ls <- 1:length(sub_ls)
        for (skf_n in 1:length(skf_ls)){
        sub <- sub_ls[[n]]
        skf_ind <- skf_ls[[skf_n]]
        dt <- fread(paste0(subf_p,"/pred_actual_",md,"_",stg,"-",sub,"_",skf_ind,"_skf.csv"))
        #dt <- fread(paste0(subf_p,"/pred_actual_SSV_",sub,".csv"))
        #dt <- fread(paste0(subf_p,"/pred_actual_",sub,".csv"))
        #skf_dt <- fread(paste0(subf_p,"/skf_index_",stg,"_SSV_",sub,".csv"))
        dt$V1 <- NULL
        dt$sub <- sub
        dt$skf_index <-skf_ind
        skf_dt_ls[[skf_n]] <- dt
        # browser()
        rm(dt)
        }
        skf_dt <- do.call(rbind,skf_dt_ls)
        skf_dt$Actual<-as.factor((skf_dt$Actual)+1)
        skf_dt$Predicted<-as.factor((skf_dt$Predicted)+1)
        skf_cm<-confusionMatrix(data = skf_dt$Predicted, reference = skf_dt$Actual, dnn = c("Prediction", "Reference"))
        #browser()
        skf_cm_st <-data.frame(skf_cm$overall)
        setDT(skf_cm_st)
        skf_cm_st[,md := md]
        skf_cm_st[,stg := stg]
        skf_cm_st[,sub := sub]
        skf_cm_st[,stat := names(skf_cm$overall)]
        skf_kappa <- skf_cm_st[stat %in% "Kappa",]
        
        write.table(skf_kappa,paste0("kappa",sub,".csv"), append = T)
        
        #browser()
        a_cnt<-summary(skf_dt$Actual)
        a_cnt_dt<-data.frame(matrix(unlist(a_cnt), nrow=length(a_cnt), byrow=TRUE))
        colnames(a_cnt_dt) <- "counts"
        setDT(a_cnt_dt)
        a_cnt_dt[,md := md]
        a_cnt_dt[,sub := sub]
        a_cnt_dt[,class := seq(1,nrow(a_cnt_dt))]
        cnt_ls[[q]]<-a_cnt_dt
        sub_uni_ls <- unique(skf_dt$sub)
        #sam_size_ls <- unique(skf_dt$sample_size)
        skf_uni_ls <- unique(skf_dt$skf_index)
        
  
  
        sub_dt_ls[[n]] <- skf_dt
        rm(skf_dt)
        # ============================================= 
        # Prepare Confusion Matrix Plot 
        # =============================================
        skf_cm_d <- data.frame(skf_cm$table)
        skf_cm_bc <- data.frame(skf_cm$byClass)
        skf_cm_p <- as.data.frame(prop.table(skf_cm$table,margin = 2))
        setDT(skf_cm_d)
        #browser()
        skf_cm_d$Perc <- round(skf_cm_p$Freq*100,2)

        library(ggplot2)     # to plot
        library(gridExtra)   # to put more
        library(grid)        # plot together
        #browser()

       
        
            if (ncol(skf_cm_bc) == 1 | nrow(skf_cm_bc) == 1) {
              skf_cm_bc_t <- t(skf_cm_bc)
              skf_cm_bc<-as.data.frame(skf_cm_bc_t)
              # For sleep as the true class
              tp = skf_cm_d$Freq[4]
              fp = skf_cm_d$Freq[2]
              fn = skf_cm_d$Freq[3]
              tn = skf_cm_d$Freq[1]
              prec = tp/(tp+fp)
              rec = tp/(tp+fn)
              spec = tn/(tn+fp)
              skf_cm_add <- data.frame((tn/(tn+fp)),
                                       (tp/(tp+fp)),
                                       (tp/(tp+fn)),
                                       ((2*prec*rec)/(prec+rec)),
                                       ((rec+spec)/2)
              )
              setDT(skf_cm_bc)
              skf_cm_bc <- skf_cm_bc[,.(Specificity,Precision,Recall,F1,`Balanced Accuracy`),with=T]
              colnames(skf_cm_add) <- colnames(skf_cm_bc)
              skf_cm_bc <- rbind(skf_cm_bc,skf_cm_add)
              skf_cm_bc <- round(skf_cm_bc,2)
            }
        # # plotting the matrix
        skf_cm_d_p <-  ggplot(data = skf_cm_d, aes( x =  Reference, y = Prediction , fill = Perc))+
          geom_tile() +
          geom_text(aes(label = paste(Freq)), color = 'black', size = 5) +
          #geom_text(aes(label = paste(Perc,"%")), color = 'white', size = 5) +
          theme_light() + #guides(fill = "none") +
          #scale_fill_viridis(discrete=FALSE) +
          scale_fill_distiller(palette="Greens", direction=1,limits = c(0,100)) +
          ylim(rev(levels(skf_cm_d$Prediction))) + xlab("Truth") + ylab("Predicted")
        #scale_fill_gradient(low="white", high="Black",limits = c(0,100))
        ggsave(paste0(stg,"_",md,"_",sub,"_Con_mat_new.png"), skf_cm_d_p, width = 20, height = 10, units = "cm")
        
        #browser()
        # plotting the stats
        # cm_st_p <-  tableGrob(cm_st)
        # cm_bc_p <-  tableGrob(cm_bc)
        if (ncol(skf_cm_bc) == 2) {
          colnames(skf_cm_bc) <- c("2 Stages")
          skf_cm_bc <- data.frame(t(skf_cm_bc))
          levels(skf_cm_d$Prediction) <- c("Wake","Sleep")
          levels(skf_cm_d$Reference) <- c("Wake","Sleep")
          
        } else if (ncol(skf_cm_bc) == 3){
          colnames(skf_cm_bc) <- c("Wake", "NREM", "REM")
          skf_cm_bc <- data.frame(t(skf_cm_bc))
          levels(skf_cm_d$Prediction) <- c("Wake", "NREM", "REM")
          levels(skf_cm_d$Reference) <- c("Wake", "NREM", "REM")
        } else if (ncol(skf_cm_bc) == 4){
          colnames(skf_cm_bc) <- c("Wake", "Light", "Deep", "REM")
          levels(skf_cm_d$Prediction) <- c("Wake", "Light", "Deep", "REM")
          levels(skf_cm_d$Reference) <- c("Wake", "Light", "Deep", "REM")
        } else {
          if (nrow(skf_cm_bc) == 2) {
            skf_cm_bc$class <- c("Wake","Sleep")
            stg_nm<-c("Wake","Sleep")
            levels(skf_cm_d$Prediction) <- c("Wake","Sleep")
            levels(skf_cm_d$Reference) <- c("Wake","Sleep")

          } else if (nrow(skf_cm_bc) == 3){
            setDT(skf_cm_bc)
            skf_cm_bc <- skf_cm_bc[,.(Specificity,Precision,Recall,F1,`Balanced.Accuracy`)]
            colnames(skf_cm_bc) <- c("Specificity","Precision","Recall","F1","Balanced Accuracy")
            skf_cm_bc <- round(skf_cm_bc,2)
            stg_nm<-c("Wake", "NREM", "REM")
            skf_cm_bc$class <- stg_nm
            
            levels(skf_cm_d$Prediction) <- stg_nm
            levels(skf_cm_d$Reference) <- stg_nm

            skf_cm_bc$class <- stg_nm

          } else if (nrow(skf_cm_bc) == 4){
            setDT(skf_cm_bc)
            skf_cm_bc <- skf_cm_bc[,.(Specificity,Precision,Recall,F1,`Balanced.Accuracy`)]
            colnames(skf_cm_bc) <- c("Specificity","Precision","Recall","F1","Balanced Accuracy")
            skf_cm_bc <- round(skf_cm_bc,2)
            stg_nm<-c("Wake", "Light", "Deep", "REM")
            
            skf_cm_bc$class <- stg_nm
            levels(skf_cm_d$Prediction) <- stg_nm
            levels(skf_cm_d$Reference) <- stg_nm

          }
        }
        #skf_cm_bc$class <-rownames(skf_cm_bc)
        setDT(skf_cm_d)
        setDT(skf_cm_bc)
        skf_cm_d[,md := md]
        skf_cm_d[,sub := sub]
        #browser()
        skf_support <- skf_cm_d[,.(freq = sum(Freq)), by = c("Reference")]
        total_freq <- sum(skf_support$freq)
        skf_support[,support := freq/total_freq]
        skf_cm_bc[,md := md]
        skf_cm_bc[,sub:=sub]
        skf_cm_bc_a<-merge(skf_cm_bc, skf_support, by.x = "class", by.y = "Reference")
        #skf_cm_bc[,]
        #fwrite(skf_cm_d,paste0(stg,"_",md,"_",sub,"_cm_subs.csv") ,row.names = T)
        #fwrite(skf_cm_bc,paste0(stg,"_",md,"_",sub,"_conmat.csv") ,row.names = T)
      
        sub_sum_ls[[n]] <- skf_cm_d
        sub_conmat_ls[[n]] <- skf_cm_bc_a
      }
      sub_all <- do.call(rbind,sub_dt_ls)
      sub_cm_all <- do.call(rbind,sub_sum_ls)
      sub_conmat_all <- do.call(rbind,sub_conmat_ls)
      
      fwrite(sub_cm_all,paste0(stg,"_",md,"_cm_subs.csv") ,row.names = T)
      fwrite(sub_conmat_all,paste0(stg,"_",md,"_conmat.csv") ,row.names = T)
      fwrite(sub_all,paste0(stg,"_",md,"_dt.csv"))
      #browser()
      conmat_class_all <- sub_conmat_all[,.(
        Specificity = mean(Specificity),
        std_Specificity = sd(Specificity),
        Precision = mean(Precision),
        std_Precision = sd(Precision),
        Recall = mean(Recall),
        std_Recall = sd(Recall),
        F1 = mean(F1),
        std_F1 = sd(F1),
        weighted_F1 = mean(F1*support),
        Balanced.Accuracy = mean(`Balanced Accuracy`),
        std_Bal_Accuracy = sd(`Balanced Accuracy`)
      ), by = . (class,sub,md)]
      #fwrite(conmat_class_all, paste0(stg,"_",md,"_conmat_all.csv"))

      # ggsave(paste0(stg,"_",md,"_F1_samplesize.png"), F1_p, width = 20, height = 10, units = "cm")
      conmat_all <- sub_conmat_all[,.(
        Specificity = mean(Specificity),
        std_Specificity = sd(Specificity),
        Precision = mean(Precision),
        std_Precision = sd(Precision),
        Recall = mean(Recall),
        std_Recall = sd(Recall),
        F1 = mean(F1),
        std_F1 = sd(F1),
        weighted_F1 = mean(F1*support),
        Balanced.Accuracy = mean(`Balanced Accuracy`),
        std_Bal_Accuracy = sd(`Balanced Accuracy`)
      ), by = . (sub)]
      fwrite(conmat_all, paste0(stg,"_",md,"_conmat_all.csv"))
      F1_pa <- ggplot()+ 
        geom_point(data = conmat_class_all, aes(x = sub, y = F1, color = class)) +
        geom_line(data = conmat_class_all, aes(x = sub, y = F1, color = class)) +
        geom_point(data = conmat_all, aes(x = sub, y = weighted_F1)) +
        geom_line(data = conmat_all, aes(x = sub, y = weighted_F1))
      ggsave(paste0(stg,"_",md,"_F1_samplesize.png"), F1_pa, width = 20, height = 10, units = "cm")
  }
}
