## FreeSurfer ROI analysis
library(stringr)
install.packages("Hmisc")
library(Hmisc)
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
setwd("~/Documents/foshan/stats_analysis")

################################################################################################################
#################################### Part1 r & p value results in excel ########################################
################################################################################################################

Behavioral_assessment <- readxl::read_excel("data220715/Assessment_for_analysis.xlsx")
##View(Behavioral_assessment)

## function 'Cortest' is for rh.thickness, lh.thickness, rh.surface & lh.surface
## function 'Cortest2' is for rh.volume, lh.volume
Cortest <- function(NeuralData,Behavioral_assessment){
  
  ########### extract ROI ###########
  # which(grepl("temp",colnames(NeuralData)))
  # colnames(NeuralData)[c(1,which(grepl("temp",colnames(NeuralData))))]
  # NeuralData <- as.data.frame(NeuralData[,c(1,which(grepl("temp",colnames(NeuralData))))])
  # ##View(NeuralData)
  
  #Merge NeuralData and behavioral assessments by subject names
  NeuralData_assessments <- merge(NeuralData,Behavioral_assessment,
                                  by.x = colnames(NeuralData[1]),
                                  by.y = colnames(Behavioral_assessment[1]))
  colnames(NeuralData_assessments)[79] <- "Group"
  ###View(NeuralData_assessments)
  
  #table 1 all subjects
  NeuralData_assessments <- na.omit(NeuralData_assessments[,c(1:79,83:88)])
  corNeuralData_assessments <- Hmisc::rcorr(as.matrix(NeuralData_assessments[,-c(1,79)]))
  corNeuralData_assessments_r <- 
    as.data.frame(round(corNeuralData_assessments$r[c(1:77),c(78:83)],digits = 2))
  corNeuralData_assessments_p <- 
    as.data.frame(round(corNeuralData_assessments$P[c(1:77),c(78:83)],digits = 3))
  ###View(corNeuralData_assessments_r)
  ###View(corNeuralData_assessments_p)
  colnames(corNeuralData_assessments_r)
  
  NeuralData_all_assessments <- cbind.data.frame(
    Gesell_Total.r = corNeuralData_assessments_r[,1],Gesell_Total.p = corNeuralData_assessments_p[,1],
    Gesell_Adap.r = corNeuralData_assessments_r[,2],Gesell_Adap.p = corNeuralData_assessments_p[,2],
    Gesell_MtrGross.r = corNeuralData_assessments_r[,3],Gesell_MtrGross.p = corNeuralData_assessments_p[,3],
    Gesell_MtrFine.r = corNeuralData_assessments_r[,4],Gesell_MtrFine.p = corNeuralData_assessments_p[,4],
    Gesell_Lang.r = corNeuralData_assessments_r[,5],Gesell_Lang.p = corNeuralData_assessments_p[,5],
    Gesell_Social.r = corNeuralData_assessments_r[,6],Gesell_Social.p = corNeuralData_assessments_p[,6]
  )
  ###View(NeuralData_all_assessments)
  rownames(NeuralData_all_assessments) <- colnames(NeuralData[-1])
  ###View(NeuralData_all_assessments)
  
  #table2:ASD   vs.   table3:nonASD group
  ASD <- dplyr::filter(NeuralData_assessments,NeuralData_assessments$Group == "ASD")
  corNeuralData_assessments_ASD <- Hmisc::rcorr(as.matrix(ASD[,-c(1,79)]))
  corNeuralData_assessments_ASD_r <- round(corNeuralData_assessments_ASD$r[c(1:77),c(78:83)],digits = 2)
  corNeuralData_assessments_ASD_p <- round(corNeuralData_assessments_ASD$P[c(1:77),c(78:83)],digits = 3)
  # ##View(corNeuralData_assessments_ASD_r)
  # ##View(corNeuralData_assessments_ASD_p)
  NeuralData_ASD <- cbind.data.frame(
    Gesell_Total.r = corNeuralData_assessments_ASD_r[,1],Gesell_Total.p = corNeuralData_assessments_ASD_p[,1],
    Gesell_Adap.r = corNeuralData_assessments_ASD_r[,2],Gesell_Adap.p = corNeuralData_assessments_ASD_p[,2],
    Gesell_MtrGross.r = corNeuralData_assessments_ASD_r[,3],Gesell_MtrGross.p = corNeuralData_assessments_ASD_p[,3],
    Gesell_MtrFine.r = corNeuralData_assessments_ASD_r[,4],Gesell_MtrFine.p = corNeuralData_assessments_ASD_p[,4],
    Gesell_Lang.r = corNeuralData_assessments_ASD_r[,5],Gesell_Lang.p = corNeuralData_assessments_ASD_p[,5],
    Gesell_Social.r = corNeuralData_assessments_ASD_r[,6],Gesell_Social.p = corNeuralData_assessments_ASD_p[,6]
  )
  rownames(NeuralData_ASD) <- colnames(NeuralData[-1])
  ###View(NeuralData_ASD)
  
  
  nonASD <- dplyr::filter(NeuralData_assessments,NeuralData_assessments$Group == "nonASD")
  corNeuralData_assessments_nonASD <- Hmisc::rcorr(as.matrix(nonASD[,-c(1,79)]))
  corNeuralData_assessments_nonASD_r <- round(corNeuralData_assessments_nonASD$r[c(1:77),c(78:83)],digits = 2)
  corNeuralData_assessments_nonASD_p <- round(corNeuralData_assessments_nonASD$P[c(1:77),c(78:83)],digits = 3)
  # ##View(corNeuralData_assessments_nonASD_r)
  # ##View(corNeuralData_assessments_nonASD_p)
  NeuralData_nonASD <- cbind.data.frame(
    Gesell_Total.r = corNeuralData_assessments_nonASD_r[,1],Gesell_Total.p = corNeuralData_assessments_nonASD_p[,1],
    Gesell_Adap.r = corNeuralData_assessments_nonASD_r[,2],Gesell_Adap.p = corNeuralData_assessments_nonASD_p[,2],
    Gesell_MtrGross.r = corNeuralData_assessments_nonASD_r[,3],Gesell_MtrGross.p = corNeuralData_assessments_nonASD_p[,3],
    Gesell_MtrFine.r = corNeuralData_assessments_nonASD_r[,4],Gesell_MtrFine.p = corNeuralData_assessments_nonASD_p[,4],
    Gesell_Lang.r = corNeuralData_assessments_nonASD_r[,5],Gesell_Lang.p = corNeuralData_assessments_nonASD_p[,5],
    Gesell_Social.r = corNeuralData_assessments_nonASD_r[,6],Gesell_Social.p = corNeuralData_assessments_nonASD_p[,6]
  )
  rownames(NeuralData_nonASD) <- colnames(NeuralData[-1])
  ###View(NeuralData_nonASD)
  
  
  #write 3 dataframes results into one table
  Subjects1 <- c("All subjects",replicate(11,'NA'))
  #Subjects1 <- as.matrix(c('All subjects',replicate(11,'NA')))
  Subjects2 <- c("ASD",replicate(11,'NA'))
  Subjects3 <- c("nonASD",replicate(11,'NA'))
  
  Subjects1 <- data.frame(matrix(c('All subjects',replicate(11,'NA')),1,12))
  Subjects2 <- data.frame(matrix(c('ASD Group',replicate(11,'NA')),1,12))
  Subjects3 <- data.frame(matrix(c('nonASD Group',replicate(11,'NA')),1,12))
  colnames(Subjects1) <- colnames(NeuralData_all_assessments)
  colnames(Subjects2) <- colnames(NeuralData_all_assessments)
  colnames(Subjects3) <- colnames(NeuralData_all_assessments)
  
  NeuralData_assessments_cor <- as.data.frame(rbind(Subjects1,NeuralData_all_assessments,
                                                    Subjects2,NeuralData_ASD,
                                                    Subjects3,NeuralData_nonASD))
  
  table <- as.matrix(rownames(NeuralData_assessments_cor),48,1)
  NeuralData_assessments_cor <- cbind(table,NeuralData_assessments_cor)
  #View(NeuralData_assessments_cor)
}

################################## Cor test 1 Gesell & rh.aparc.a2009s.thickness##########################
NeuralData <- read.table("data220715/rh.a2009s.thickness.txt",header = T)
###View(NeuralData)
kk <- Cortest(NeuralData,Behavioral_assessment)
kk
writexl::write_xlsx(kk,"results220715/cor_Gesell_rh.a2009s.thickness_all1.xlsx")


################################## Cor test 2 Gesell & lh.aparc.a2009s.thickness##########################
NeuralData <- read.table("data220715/lh.a2009s.thickness.txt",header = T)
###View(NeuralData)
kk <- Cortest(NeuralData,Behavioral_assessment)
kk
writexl::write_xlsx(kk,"results220715/cor_Gesell_lh.a2009s.thickness_all.xlsx")

################################## Cor test 3 Gesell & rh.aparc.a2009s.surface##########################
NeuralData <- read.table("data220715/rh.a2009s.surface.txt",header = T)
##View(NeuralData)
kk <- Cortest(NeuralData,Behavioral_assessment)
kk
writexl::write_xlsx(kk,"results220715/cor_Gesell_rh.a2009s.surface_all.xlsx")

################################## Cor test 4 Gesell & lh.aparc.a2009s.surface##########################
NeuralData <- read.table("data220715/lh.a2009s.surface.txt",header = T)
##View(NeuralData)
kk <- Cortest(NeuralData,Behavioral_assessment)
kk
writexl::write_xlsx(kk,"results220715/cor_Gesell_lh.a2009s.surface_all.xlsx")

################################## Cor test 5 Gesell & rh.aparc.a2009s.volume##########################
NeuralData <- read.table("data220715/rh.a2009s.volume.txt",header = T)
##View(NeuralData)
Cortest2 <- function(NeuralData,Behavioral_assessment){
  
  # which(grepl("temp",colnames(NeuralData)))
  # colnames(NeuralData)[c(1,which(grepl("temp",colnames(NeuralData))))]
  # NeuralData <- as.data.frame(NeuralData[,c(1,which(grepl("temp",colnames(NeuralData))))])
  # #View(NeuralData)
  #Merge NeuralData and behavioral assessments by subject names
  NeuralData_assessments <- merge(NeuralData,Behavioral_assessment,
                                  by.x = colnames(NeuralData[1]),
                                  by.y = colnames(Behavioral_assessment[1]))
  colnames(NeuralData_assessments)[78] <- "Group"
  ##View(NeuralData_assessments)
  
  #table 1 all subjects
  NeuralData_assessments <- na.omit(NeuralData_assessments[,c(1:78,82:87)])
  corNeuralData_assessments <- Hmisc::rcorr(as.matrix(NeuralData_assessments[,-c(1,78)]))
  corNeuralData_assessments_r <- 
    as.data.frame(round(corNeuralData_assessments$r[c(1:76),c(77:82)],digits = 2))
  corNeuralData_assessments_p <- 
    as.data.frame(round(corNeuralData_assessments$P[c(1:76),c(77:82)],digits = 3))
  ##View(corNeuralData_assessments_r)
  ##View(corNeuralData_assessments_p)
  colnames(corNeuralData_assessments_r)
  
  NeuralData_all_assessments <- cbind.data.frame(
    Gesell_Total.r = corNeuralData_assessments_r[,1],Gesell_Total.p = corNeuralData_assessments_p[,1],
    Gesell_Adap.r = corNeuralData_assessments_r[,2],Gesell_Adap.p = corNeuralData_assessments_p[,2],
    Gesell_MtrGross.r = corNeuralData_assessments_r[,3],Gesell_MtrGross.p = corNeuralData_assessments_p[,3],
    Gesell_MtrFine.r = corNeuralData_assessments_r[,4],Gesell_MtrFine.p = corNeuralData_assessments_p[,4],
    Gesell_Lang.r = corNeuralData_assessments_r[,5],Gesell_Lang.p = corNeuralData_assessments_p[,5],
    Gesell_Social.r = corNeuralData_assessments_r[,6],Gesell_Social.p = corNeuralData_assessments_p[,6]
  )
  ##View(NeuralData_all_assessments)
  rownames(NeuralData_all_assessments) <- colnames(NeuralData[-1])
  ##View(NeuralData_all_assessments)
  
  #table2:ASD   vs.   table3:nonASD group
  ASD <- dplyr::filter(NeuralData_assessments,NeuralData_assessments$Group == "ASD")
  corNeuralData_assessments_ASD <- Hmisc::rcorr(as.matrix(ASD[,-c(1,78)]))
  corNeuralData_assessments_ASD_r <- round(corNeuralData_assessments_ASD$r[c(1:76),c(77:82)],digits = 2)
  corNeuralData_assessments_ASD_p <- round(corNeuralData_assessments_ASD$P[c(1:76),c(77:82)],digits = 3)
  # #View(corNeuralData_assessments_ASD_r)
  # #View(corNeuralData_assessments_ASD_p)
  NeuralData_ASD <- cbind.data.frame(
    Gesell_Total.r = corNeuralData_assessments_ASD_r[,1],Gesell_Total.p = corNeuralData_assessments_ASD_p[,1],
    Gesell_Adap.r = corNeuralData_assessments_ASD_r[,2],Gesell_Adap.p = corNeuralData_assessments_ASD_p[,2],
    Gesell_MtrGross.r = corNeuralData_assessments_ASD_r[,3],Gesell_MtrGross.p = corNeuralData_assessments_ASD_p[,3],
    Gesell_MtrFine.r = corNeuralData_assessments_ASD_r[,4],Gesell_MtrFine.p = corNeuralData_assessments_ASD_p[,4],
    Gesell_Lang.r = corNeuralData_assessments_ASD_r[,5],Gesell_Lang.p = corNeuralData_assessments_ASD_p[,5],
    Gesell_Social.r = corNeuralData_assessments_ASD_r[,6],Gesell_Social.p = corNeuralData_assessments_ASD_p[,6]
  )
  rownames(NeuralData_ASD) <- colnames(NeuralData[-1])
  ##View(NeuralData_ASD)
  
  
  nonASD <- dplyr::filter(NeuralData_assessments,NeuralData_assessments$Group == "nonASD")
  corNeuralData_assessments_nonASD <- Hmisc::rcorr(as.matrix(nonASD[,-c(1,78)]))
  corNeuralData_assessments_nonASD_r <- round(corNeuralData_assessments_nonASD$r[c(1:76),c(77:82)],digits = 2)
  corNeuralData_assessments_nonASD_p <- round(corNeuralData_assessments_nonASD$P[c(1:76),c(77:82)],digits = 3)
  # #View(corNeuralData_assessments_nonASD_r)
  # #View(corNeuralData_assessments_nonASD_p)
  NeuralData_nonASD <- cbind.data.frame(
    Gesell_Total.r = corNeuralData_assessments_nonASD_r[,1],Gesell_Total.p = corNeuralData_assessments_nonASD_p[,1],
    Gesell_Adap.r = corNeuralData_assessments_nonASD_r[,2],Gesell_Adap.p = corNeuralData_assessments_nonASD_p[,2],
    Gesell_MtrGross.r = corNeuralData_assessments_nonASD_r[,3],Gesell_MtrGross.p = corNeuralData_assessments_nonASD_p[,3],
    Gesell_MtrFine.r = corNeuralData_assessments_nonASD_r[,4],Gesell_MtrFine.p = corNeuralData_assessments_nonASD_p[,4],
    Gesell_Lang.r = corNeuralData_assessments_nonASD_r[,5],Gesell_Lang.p = corNeuralData_assessments_nonASD_p[,5],
    Gesell_Social.r = corNeuralData_assessments_nonASD_r[,6],Gesell_Social.p = corNeuralData_assessments_nonASD_p[,6]
  )
  rownames(NeuralData_nonASD) <- colnames(NeuralData[-1])
  ##View(NeuralData_nonASD)
  
  
  #write 3 dataframes results into one table
  Subjects1 <- c("All subjects",replicate(11,'NA'))
  #Subjects1 <- as.matrix(c('All subjects',replicate(11,'NA')))
  Subjects2 <- c("ASD",replicate(11,'NA'))
  Subjects3 <- c("nonASD",replicate(11,'NA'))
  
  Subjects1 <- data.frame(matrix(c('All subjects',replicate(11,'NA')),1,12))
  Subjects2 <- data.frame(matrix(c('ASD Group',replicate(11,'NA')),1,12))
  Subjects3 <- data.frame(matrix(c('nonASD Group',replicate(11,'NA')),1,12))
  colnames(Subjects1) <- colnames(NeuralData_all_assessments)
  colnames(Subjects2) <- colnames(NeuralData_all_assessments)
  colnames(Subjects3) <- colnames(NeuralData_all_assessments)
  
  NeuralData_assessments_cor <- as.data.frame(rbind(Subjects1,NeuralData_all_assessments,
                                                    Subjects2,NeuralData_ASD,
                                                    Subjects3,NeuralData_nonASD))
  table <- as.matrix(rownames(NeuralData_assessments_cor),48,1)
  NeuralData_assessments_cor <- cbind(table,NeuralData_assessments_cor)
}
kk2 <- Cortest2(NeuralData,Behavioral_assessment)
kk2
writexl::write_xlsx(kk2,"results220715/cor_Gesell_rh.a2009s.volume_all.xlsx")

################################## Cor test 6 Gesell & lh.aparc.a2009s.volume##########################
NeuralData <- read.table("data220715/lh.a2009s.volume.txt",header = T)
kk2 <- Cortest2(NeuralData,Behavioral_assessment)
kk2
writexl::write_xlsx(kk2,"results220715/cor_Gesell_lh.a2009s.volume_all.xlsx")




################################################################################################################
#################################### Part2 scatterplot correlation#############################################
################################################################################################################

library(ggpubr) #stat_cor
############################################################################
############################################################################
#### 1.Gesell & rh.aparc.a2009s.volume ####
#### Gesell Total ####
#### 1.1.Gesell_Total_rh_G_temp_sup.Plan_tempo_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=rh_G_temp_sup.Plan_tempo_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 800, ymax = 2200
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=rh_G_temp_sup.Plan_tempo_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=rh_G_temp_sup.Plan_tempo_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 2550,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/1.1.Gesell_Total_rh_G_temp_sup.Plan_tempo_volume.png')
#### 1.2.Gesell_Total_rh_S_oc.temp_med_and_Lingual_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=rh_S_oc.temp_med_and_Lingual_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 800, ymax = 2200
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=rh_S_oc.temp_med_and_Lingual_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=rh_S_oc.temp_med_and_Lingual_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3620,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/1.2.Gesell_Total_rh_S_oc.temp_med_and_Lingual_volume.png')

#### Gesell_Total_rh_G_oc.temp_med.Lingual_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=rh_G_oc.temp_med.Lingual_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 4000, ymax = 10000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=rh_G_oc.temp_med.Lingual_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=rh_G_oc.temp_med.Lingual_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 9250,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Total_rh_G_oc.temp_med.Lingual_volume.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Total_rh_G_temporal_middle_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=rh_G_temporal_middle_volume,
                                       xmin = 30, xmax = 90,
                                       ymin = 6000, ymax = 15000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=rh_G_temporal_middle_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=rh_G_temporal_middle_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 13800,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Total_rh_G_temporal_middle_volume.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Total_rh_S_temporal_inf_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=rh_S_temporal_inf_volume,
                                       xmin = 30, xmax = 95,
                                       ymin = 0, ymax = 4000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=rh_S_temporal_inf_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=rh_S_temporal_inf_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3500,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Total_rh_S_temporal_inf_volume.png',
       width = 2737,height = 1795,units = "px")

#### Gesell Adap ####
#### 1.3.Gesell_Adap_rh_G_temp_sup.Plan_tempo_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Adap,y=rh_G_temp_sup.Plan_tempo_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 800, ymax = 2200
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=rh_G_temp_sup.Plan_tempo_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=rh_G_temp_sup.Plan_tempo_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 2630,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/1.3.Gesell_Adap_rh_G_temp_sup.Plan_tempo_volume.png',
       width = 2737,height = 1795,units = "px")

#### 1.4.Gesell_Adap_rh_G_temporal_inf_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Adap,y=rh_G_temporal_inf_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 3000,ymax = 11000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=rh_G_temporal_inf_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           
           p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=rh_G_temporal_inf_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 10200,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/1.4.Gesell_Adap_rh_G_temporal_inf_volume.png',
       width = 2737,height = 1795,units = "px")

#### 1.5.Gesell_Adap_rh_S_oc.temp_med_and_Lingual_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Adap,y=rh_S_oc.temp_med_and_Lingual_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 1000, ymax = 4500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=rh_S_oc.temp_med_and_Lingual_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=rh_S_oc.temp_med_and_Lingual_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 4000,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/1.5.Gesell_Adap_rh_S_oc.temp_med_and_Lingual_volume.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Adap_rh_G_front_middle_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Adap,y=rh_G_front_middle_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 8000, ymax = 16000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=rh_G_front_middle_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=rh_G_front_middle_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 15200,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Adap_rh_G_front_middle_volume.png',
       width = 2737,height = 1795,units = "px")

#### Gesell MtrGross & Mtfine ####
#### 1.6.Gesell_MtrGross_rh_S_oc.temp_lat_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_MtrGross,y=rh_S_oc.temp_lat_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 800, ymax = 2500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrGross,
               y=rh_S_oc.temp_lat_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrGross,
               y=rh_S_oc.temp_lat_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 2220,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/1.6.Gesell_MtrGross_rh_S_oc.temp_lat_volume.png')

#### 1.7.Gesell_MtrFine_rh_G_oc.temp_med.Lingual_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_MtrFine,y=rh_G_oc.temp_med.Lingual_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 4000, ymax = 10000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrFine,
               y=rh_G_oc.temp_med.Lingual_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrFine,
               y=rh_G_oc.temp_med.Lingual_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 9000,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/1.7.Gesell_MtrFine_rh_G_oc.temp_med.Lingual_volume.png')

#### 1.8.Gesell_MtrFine_rh_G_temp_sup.Plan_tempo_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_MtrFine,y=rh_G_temp_sup.Plan_tempo_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 1000, ymax = 3000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrFine,
               y=rh_G_temp_sup.Plan_tempo_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrFine,
               y=rh_G_temp_sup.Plan_tempo_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 2680,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/1.8.Gesell_MtrFine_rh_G_temp_sup.Plan_tempo_volume.png')

#### 1.9.Gesell_MtrFine_rh_G_temporal_inf_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_MtrFine,y=rh_G_temporal_inf_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 3000, ymax = 12000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrFine,
               y=rh_G_temporal_inf_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrFine,
               y=rh_G_temporal_inf_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 10500,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/1.9.Gesell_MtrFine_rh_G_temporal_inf_volume.png')

#### 1.10.Gesell_MtrFine_rh_S_oc.temp_med_and_Lingual_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_MtrFine,y=rh_S_oc.temp_med_and_Lingual_volume,
                                       xmin = 40, xmax = 90,
                                       ymax = 5000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrFine,
               y=rh_S_oc.temp_med_and_Lingual_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrFine,
               y=rh_S_oc.temp_med_and_Lingual_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 4400,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/1.10.Gesell_MtrFine_rh_S_oc.temp_med_and_Lingual_volume.png')





#### Gesell Language ####
#### Gesell_Lang_rh_G_oc.temp_med.Parahip_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=rh_G_oc.temp_med.Parahip_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 800, ymax = 4000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_G_oc.temp_med.Parahip_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_G_oc.temp_med.Parahip_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 3550,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_rh_G_oc.temp_med.Parahip_volume.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Lang_rh_G_temp_sup.Plan_tempo_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=rh_G_temp_sup.Plan_tempo_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 800, ymax = 2200
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_G_temp_sup.Plan_tempo_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_G_temp_sup.Plan_tempo_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 2640,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_rh_G_temp_sup.Plan_tempo_volume.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Lang_rh_G_temporal_inf_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=rh_G_temporal_inf_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 2000, ymax = 12000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_G_temporal_inf_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_G_temporal_inf_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 10800,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_rh_G_temporal_inf_volume.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Lang_rh_G_temporal_middle_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=rh_G_temporal_middle_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 5000, ymax = 15000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_G_temporal_middle_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_G_temporal_middle_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 13800,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_rh_G_temporal_middle_volume.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Lang_rh_S_oc.temp_med_and_Lingual_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=rh_S_oc.temp_med_and_Lingual_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 1000, ymax = 5000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_S_oc.temp_med_and_Lingual_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_S_oc.temp_med_and_Lingual_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 4520,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_rh_S_oc.temp_med_and_Lingual_volume.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Lang_rh_S_temporal_inf_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=rh_S_temporal_inf_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 0, ymax = 3200
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_S_temporal_inf_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_S_temporal_inf_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 2820,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_rh_S_temporal_inf_volume.png',
       width = 2737,height = 1795,units = "px")


#### Gesell Social ####
#### Gesell_Social_rh_G_temp_sup.Plan_tempo_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=rh_G_temp_sup.Plan_tempo_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 800, ymax = 2200
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=rh_G_temp_sup.Plan_tempo_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=rh_G_temp_sup.Plan_tempo_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 2600,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_rh_G_temp_sup.Plan_tempo_volume.png',
       width = 2737,height = 1795,units = "px")


#### Gesell_Social_rh_S_oc.temp_med_and_Lingual_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=rh_S_oc.temp_med_and_Lingual_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 1000, ymax = 4500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=rh_S_oc.temp_med_and_Lingual_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=rh_S_oc.temp_med_and_Lingual_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 4080,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_rh_S_oc.temp_med_and_Lingual_volume.png',
       width = 2737,height = 1795,units = "px")




#### Gesell_Social_rh_S_temporal_inf_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=rh_S_temporal_inf_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 0, ymax = 3500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=rh_S_temporal_inf_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=rh_S_temporal_inf_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 3100,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_rh_S_temporal_inf_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Social_rh_G_front_middle_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=rh_G_front_middle_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 8000, ymax = 16000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Social,
               y=rh_G_front_middle_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Social,
               y=rh_G_front_middle_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 15200,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_rh_G_front_middle_volume.png',
       width = 2737,height = 1795,units = "px")

############################################################################
#### 2.Gesell & rh.aparc.a2009s.thickness ####
#### Gesell Total ####
#### Gesell_Total_rh_G_temp_sup.Lateral_thickness ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=rh_G_temp_sup.Lateral_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 3.0, ymax = 4
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=rh_G_temp_sup.Lateral_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=rh_G_temp_sup.Lateral_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.85,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Total_rh_G_temp_sup.Lateral_thickness.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Total_rh_Pole_temporal_thickness ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=rh_Pole_temporal_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 3.0, ymax = 4.5
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=rh_Pole_temporal_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=rh_Pole_temporal_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 4.3,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Total_rh_Pole_temporal_thickness.png',
       width = 2737,height = 1795,units = "px")

#### Gesell Adap ####
#### 2.2.Gesell_Adap_rh_G_temp_sup.Plan_polar_thickness ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Adap,y=rh_G_temp_sup.Plan_polar_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 2.8, ymax = 4.0
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=rh_G_temp_sup.Plan_polar_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=rh_G_temp_sup.Plan_polar_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.8,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/2.2.Gesell_Adap_rh_G_temp_sup.Plan_polar_thickness.png')

#### Gesell_Adap_rh_Pole_temporal_thickness ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Adap,y=rh_Pole_temporal_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 3.0, ymax = 4.5
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=rh_Pole_temporal_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=rh_Pole_temporal_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 4.3,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Adap_rh_Pole_temporal_thickness.png',
       width = 2737,height = 1795,units = "px")

#### Gesell MtrGross & Mtfine ####
#### 2.3.Gesell_MtrGross_rh_S_oc.temp_med_and_Lingual_thickness ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_MtrGross,y=rh_S_oc.temp_med_and_Lingual_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 2.4, ymax = 3.2
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrGross,
               y=rh_S_oc.temp_med_and_Lingual_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrGross,
               y=rh_S_oc.temp_med_and_Lingual_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.07,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/2.3.Gesell_MtrGross_rh_S_oc.temp_med_and_Lingual_thickness.png')








#### Gesell Language ####
#### Gesell_Lang_rh_Pole_temporal_thickness ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=rh_Pole_temporal_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 3.0, ymax = 4.5
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Lang,
               y=rh_Pole_temporal_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Lang,
               y=rh_Pole_temporal_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 4.3,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_rh_Pole_temporal_thickness.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Lang_rh_S_temporal_transverse_thickness ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=rh_S_temporal_transverse_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 2, ymax = 4.5
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Lang,
               y=rh_S_temporal_transverse_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 65,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Lang,
               y=rh_S_temporal_transverse_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 65,
           label.y = 4.2,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_rh_S_temporal_transverse_thickness.png',
       width = 2737,height = 1795,units = "px")

#### Gesell Social ####
############################################################################
#### 3.Gesell & lh.aparc.a2009s.volume ####
#### Gesell Total ####
#### 3.1.Gesell_Total_lh_G_temp_sup.G_T_transv_volume ####
#pdf("plots/cor_Gesell__Total_lh_G_temp_sup.G_T_transv_volume.pdf")
q <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=lh_G_temp_sup.G_T_transv_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 800, ymax = 2200
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=lh_G_temp_sup.G_T_transv_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=lh_G_temp_sup.G_T_transv_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 1980,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
q

ggsave('plots220715/scatterplot correlation/3.1.Gesell_Total_lh_G_temp_sup.G_T_transv_volume.png')

#### Gesell_Total_lh_G_oc.temp_lat.fusifor_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=lh_G_oc.temp_lat.fusifor_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 3000, ymax = 7000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=lh_G_oc.temp_lat.fusifor_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=lh_G_oc.temp_lat.fusifor_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 6500,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Total_lh_G_oc.temp_lat.fusifor_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Total_lh_G_oc.temp_med.Lingual_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=lh_G_oc.temp_med.Lingual_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 4000, ymax = 10000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=lh_G_oc.temp_med.Lingual_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=lh_G_oc.temp_med.Lingual_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 9200,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Total_lh_G_oc.temp_med.Lingual_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Total_lh_G_temp_sup.G_T_transv_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=lh_G_temp_sup.G_T_transv_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 500, ymax = 2500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=lh_G_temp_sup.G_T_transv_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=lh_G_temp_sup.G_T_transv_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 2250,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Total_lh_G_temp_sup.G_T_transv_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Total_lh_G_temporal_inf_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=lh_G_temporal_inf_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 2500, ymax = 12500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=lh_G_temporal_inf_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=lh_G_temporal_inf_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 11000,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Total_lh_G_temporal_inf_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Total_lh_Pole_temporal_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=lh_Pole_temporal_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 1000, ymax = 8000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=lh_Pole_temporal_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=lh_Pole_temporal_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 7400,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Total_lh_Pole_temporal_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Total_lh_S_oc.temp_med_and_Lingual_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=lh_S_oc.temp_med_and_Lingual_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 1000, ymax = 5000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=lh_S_oc.temp_med_and_Lingual_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=lh_S_oc.temp_med_and_Lingual_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 4500,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Total_lh_S_oc.temp_med_and_Lingual_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell Adap ####
#### 3.2.Gesell_Adap_lh_G_temp_sup.G_T_transv_volume ####
q <- ggplot(NeuralData_assessments,aes(x=Gesell_Adap,y=lh_G_temp_sup.G_T_transv_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 800, ymax = 2200
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=lh_G_temp_sup.G_T_transv_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=lh_G_temp_sup.G_T_transv_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.y = 1985, label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),
  )
q
#dev.off()
ggsave('plots220715/scatterplot correlation/3.2.Gesell_Adap_lh_G_temp_sup.G_T_transv_volume.png')

#### Gesell_Adap_lh_S_oc.temp_med_and_Lingual_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Adap,y=lh_S_oc.temp_med_and_Lingual_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 1000, ymax = 5000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Adap,
                       y=lh_S_oc.temp_med_and_Lingual_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Adap,
                       y=lh_S_oc.temp_med_and_Lingual_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 4500,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Adap_lh_S_oc.temp_med_and_Lingual_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Adap_lh_S_oc.temp_med_and_Lingual_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Adap,y=lh_S_oc.temp_med_and_Lingual_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 1000, ymax = 5000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Adap,
                       y=lh_S_oc.temp_med_and_Lingual_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Adap,
                       y=lh_S_oc.temp_med_and_Lingual_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 4500,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Adap_lh_S_oc.temp_med_and_Lingual_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Adap_lh_G_front_middle_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Adap,y=lh_G_front_middle_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 8000, ymax = 16000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=lh_G_front_middle_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=lh_G_front_middle_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 15900,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Adap_lh_G_front_middle_volume.png',
       width = 2737,height = 1795,units = "px")

#### Gesell MtrGross & Mtfine ####
#### 3.3.Gesell_MtrGross_lh_G_temp_sup.G_T_transv_volume ####
q <- ggplot(NeuralData_assessments,aes(x=Gesell_MtrGross,y=lh_G_temp_sup.G_T_transv_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 800, ymax = 2200
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrGross,
               y=lh_G_temp_sup.G_T_transv_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrGross,
               y=lh_G_temp_sup.G_T_transv_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.y = 1985, label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),
  )
q
ggsave('plots220715/scatterplot correlation/3.3.Gesell_MtrGross_lh_G_temp_sup.G_T_transv_volume.png')

#### 3.4.Gesell_MtrFine_lh_G_temporal_inf_volume ####
q <- ggplot(NeuralData_assessments,aes(x=Gesell_MtrFine,y=lh_G_temporal_inf_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 4000, ymax = 12500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrFine,
               y=lh_G_temporal_inf_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrFine,
               y=lh_G_temporal_inf_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.y = 11000, label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),
  )
q
ggsave('plots220715/scatterplot correlation/3.4.Gesell_MtrFine_lh_G_temporal_inf_volume.png')



#### Gesell Language ####
#### Gesell_Lang_lh_G_oc.temp_lat.fusifor_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=lh_G_oc.temp_lat.fusifor_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 3000, ymax = 7000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_G_oc.temp_lat.fusifor_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_G_oc.temp_lat.fusifor_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 6500,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_lh_G_oc.temp_lat.fusifor_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Lang_lh_G_temp_sup.G_T_transv_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=lh_G_temp_sup.G_T_transv_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 500, ymax = 2500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_G_temp_sup.G_T_transv_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_G_temp_sup.G_T_transv_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 2250,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_lh_G_temp_sup.G_T_transv_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Lang_lh_G_temp_sup.Plan_tempo_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=lh_G_temp_sup.Plan_tempo_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 1000, ymax = 4000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_G_temp_sup.Plan_tempo_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_G_temp_sup.Plan_tempo_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 3600,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_lh_G_temp_sup.Plan_tempo_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Lang_lh_G_temporal_inf_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=lh_G_temporal_inf_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 2500, ymax = 12500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_G_temporal_inf_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_G_temporal_inf_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 11200,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_lh_G_temporal_inf_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Lang_lh_G_temporal_middle_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=lh_G_temporal_middle_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 5000, ymax = 15000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_G_temporal_middle_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_G_temporal_middle_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 13500,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_lh_G_temporal_middle_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Lang_lh_Pole_temporal_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=lh_Pole_temporal_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 2000, ymax = 8000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_Pole_temporal_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_Pole_temporal_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 7500,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_lh_Pole_temporal_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Lang_lh_S_oc.temp_med_and_Lingual_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=lh_S_oc.temp_med_and_Lingual_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 1000, ymax = 5000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_S_oc.temp_med_and_Lingual_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_S_oc.temp_med_and_Lingual_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 4500,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_lh_S_oc.temp_med_and_Lingual_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell Social ####
#### Gesell_Social_lh_G_temp_sup.Plan_tempo_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=lh_G_temp_sup.Plan_tempo_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 1000, ymax = 4000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=lh_G_temp_sup.Plan_tempo_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=lh_G_temp_sup.Plan_tempo_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 3600,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_lh_G_temp_sup.Plan_tempo_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Social_lh_G_temporal_middle_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=lh_G_temporal_middle_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 5000, ymax = 15000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=lh_G_temporal_middle_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=lh_G_temporal_middle_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 13500,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_lh_G_temporal_middle_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Social_lh_Pole_temporal_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=lh_Pole_temporal_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 2000, ymax = 9000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=lh_Pole_temporal_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=lh_Pole_temporal_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 8000,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_lh_Pole_temporal_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Social_lh_S_oc.temp_med_and_Lingual_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=lh_S_oc.temp_med_and_Lingual_volume,
                                       xmin = 20, xmax = 100,
                                       ymin = 1000, ymax = 5000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=lh_S_oc.temp_med_and_Lingual_volume,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=lh_S_oc.temp_med_and_Lingual_volume,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 4500,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_lh_S_oc.temp_med_and_Lingual_volume.png',
       width = 2737,height = 1795,units = "px")





#### Gesell_Social_lh_G_front_middle_volume ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=lh_G_front_middle_volume,
                                       xmin = 40, xmax = 90,
                                       ymin = 8000, ymax = 16000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Social,
               y=lh_G_front_middle_volume,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Social,
               y=lh_G_front_middle_volume,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 15900,
           p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_lh_G_front_middle_volume.png',
       width = 2737,height = 1795,units = "px")

############################################################################
#### 4.Gesell & lh.aparc.a2009s.thickness ####
#### Gesell Total ####
####4.1.Gesell_Total_lh_G_temp_sup.G_T_transv_thickness####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=lh_G_temp_sup.G_T_transv_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 2.4, ymax = 3.8
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=lh_G_temp_sup.G_T_transv_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=lh_G_temp_sup.G_T_transv_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.6,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/4.1.Gesell_Total_lh_G_temp_sup.G_T_transv_thickness.png',
       width = 2737,height = 1795,units = "px")

####4.2.Gesell_Total_lh_G_temp_sup.Plan_polar_thickness####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=lh_G_temp_sup.Plan_polar_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 3.2, ymax = 4.0
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=lh_G_temp_sup.Plan_polar_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Total,
               y=lh_G_temp_sup.Plan_polar_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.7,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/4.2.Gesell_Total_lh_G_temp_sup.Plan_polar_thickness.png')

#### Gesell Adap ####
####4.3.Gesell_Adap_lh_G_temp_sup.Plan_polar_thickness####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Adap,y=lh_G_temp_sup.Plan_polar_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 3.2, ymax = 4.0
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=lh_G_temp_sup.Plan_polar_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=lh_G_temp_sup.Plan_polar_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.7,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/4.3.Gesell_Adap_lh_G_temp_sup.Plan_polar_thickness.png')

####4.4.Gesell_Adap_lh_G_temporal_inf_thickness####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Adap,y=lh_G_temporal_inf_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 3.0, ymax = 3.8
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=lh_G_temporal_inf_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=lh_G_temporal_inf_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.67,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/4.4.Gesell_Adap_lh_G_temporal_inf_thickness.png')

####Gesell_Adap_lh_S_oc.temp_med_and_Lingual_thickness####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Adap,y=lh_S_oc.temp_med_and_Lingual_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 2.25, ymax = 3.25
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=lh_S_oc.temp_med_and_Lingual_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=lh_S_oc.temp_med_and_Lingual_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.1,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Adap_lh_S_oc.temp_med_and_Lingual_thickness.png',
       width = 2737,height = 1795,units = "px")

####Gesell_Adap_lh_S_temporal_sup_thickness####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Adap,y=lh_S_temporal_sup_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 2.5, ymax = 3.2
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=lh_S_temporal_sup_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Adap,
               y=lh_S_temporal_sup_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.1,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Adap_lh_S_temporal_sup_thickness.png',
       width = 2737,height = 1795,units = "px")


#### Gesell MtrGross & Mtfine ####
####4.5.Gesell_MtrGross_lh_G_oc.temp_lat.fusifor_thickness####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_MtrGross,y=lh_G_oc.temp_lat.fusifor_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 2.8, ymax = 3.9
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrGross,
               y=lh_G_oc.temp_lat.fusifor_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrGross,
               y=lh_G_oc.temp_lat.fusifor_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.7,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/4.5.Gesell_MtrGross_lh_G_oc.temp_lat.fusifor_thickness.png')

####4.6.Gesell_MtrGross_lh_G_oc.temp_med.Lingual_thickness####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_MtrGross,y=lh_G_oc.temp_med.Lingual_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 2.0, ymax = 3.5
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrGross,
               y=lh_G_oc.temp_med.Lingual_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrGross,
               y=lh_G_oc.temp_med.Lingual_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.24,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/4.6.Gesell_MtrGross_lh_G_oc.temp_med.Lingual_thickness.png')

####4.7.Gesell_MtrGross_lh_G_temp_sup.G_T_transv_thickness####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_MtrGross,y=lh_G_temp_sup.G_T_transv_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 2.4, ymax = 3.8
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrGross,
               y=lh_G_temp_sup.G_T_transv_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_MtrGross,
               y=lh_G_temp_sup.G_T_transv_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.58,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/4.7.Gesell_MtrGross_lh_G_temp_sup.G_T_transv_thickness.png')

#### Gesell Language ####
####4.8.Gesell_Lang_lh_G_temp_sup.G_T_transv_thickness####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=lh_G_temp_sup.G_T_transv_thickness,
                                       xmin = 20, xmax = 100,
                                       ymin = 2.4, ymax = 3.6
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Lang,
               y=lh_G_temp_sup.G_T_transv_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Lang,
               y=lh_G_temp_sup.G_T_transv_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.45,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/4.8.Gesell_Lang_lh_G_temp_sup.G_T_transv_thickness.png',
       width = 2737,height = 1795,units = "px")

####Gesell_Lang_lh_G_temp_sup.Plan_polar_thickness####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=lh_G_temp_sup.Plan_polar_thickness,
                                       xmin = 20, xmax = 100,
                                       ymin = 2.5, ymax = 4.0
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Lang,
               y=lh_G_temp_sup.Plan_polar_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Lang,
               y=lh_G_temp_sup.Plan_polar_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.8,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_lh_G_temp_sup.Plan_polar_thickness.png',
       width = 2737,height = 1795,units = "px")

#### Gesell Social ####
####4.9.Gesell_Social_lh_G_temp_sup.G_T_transv_thickness####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=lh_G_temp_sup.G_T_transv_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 2.4, ymax = 3.8
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Social,
               y=lh_G_temp_sup.G_T_transv_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Social,
               y=lh_G_temp_sup.G_T_transv_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.58,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/4.9.Gesell_Social_lh_G_temp_sup.G_T_transv_thickness.png')

####4.10.Gesell_Social_lh_G_temp_sup.Plan_tempo_thickness####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=lh_G_temp_sup.Plan_tempo_thickness,
                                       xmin = 40, xmax = 90,
                                       ymin = 2.4, ymax = 3.8
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Social,
               y=lh_G_temp_sup.Plan_tempo_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Social,
               y=lh_G_temp_sup.Plan_tempo_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.58,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/4.10.Gesell_Social_lh_G_temp_sup.Plan_tempo_thickness.png')







####Gesell_Social_lh_G_temp_sup.Lateral_thickness####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=lh_G_temp_sup.Lateral_thickness,
                                       xmin = 20, xmax = 100,
                                       ymin = 3.0, ymax = 4.0
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Social,
               y=lh_G_temp_sup.Lateral_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Social,
               y=lh_G_temp_sup.Lateral_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.86,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_lh_G_temp_sup.Lateral_thickness.png',
       width = 2737,height = 1795,units = "px")

####Gesell_Social_lh_G_temp_sup.Plan_polar_thickness####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=lh_G_temp_sup.Plan_polar_thickness,
                                       xmin = 20, xmax = 100,
                                       ymin = 2.5, ymax = 4.0
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  stat_cor(method = 'pearson',
           aes(x=Gesell_Social,
               y=lh_G_temp_sup.Plan_polar_thickness,color=Group,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           size = 5,
           label.x = 70,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # red and green r,p value
  stat_cor(method = 'pearson',
           aes(x=Gesell_Social,
               y=lh_G_temp_sup.Plan_polar_thickness,
               label = paste(gsub("R", "r", ..r.label..),..p.label..,
                             sep = "~`,`~")),
           color = "gray",
           size = 5,
           label.x = 70,
           label.y = 3.8,
           p.accuracy = 0.001, r.accuracy = 0.001)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_lh_G_temp_sup.Plan_polar_thickness.png',
       width = 2737,height = 1795,units = "px")

############################################################################
#### 5.Gesell & rh.aparc.a2009s.surface ####
#### Gesell Total ####
#### Gesell_Total_rh_G_temp_sup.Lateral_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=rh_G_temp_sup.Lateral_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 500, ymax = 2000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=rh_G_temp_sup.Lateral_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=rh_G_temp_sup.Lateral_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 1800,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Total_rh_G_temp_sup.Lateral_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Total_rh_G_temp_sup.Plan_tempo_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=rh_G_temp_sup.Plan_tempo_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 200, ymax = 1000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=rh_G_temp_sup.Plan_tempo_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=rh_G_temp_sup.Plan_tempo_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 900,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Total_rh_G_temp_sup.Plan_tempo_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Total_rh_S_oc.temp_med_and_Lingual_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=rh_S_oc.temp_med_and_Lingual_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 500, ymax = 2000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=rh_S_oc.temp_med_and_Lingual_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=rh_S_oc.temp_med_and_Lingual_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 1800,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Total_rh_S_oc.temp_med_and_Lingual_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Total_rh_S_temporal_inf_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=rh_S_temporal_inf_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 0, ymax = 1500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=rh_S_temporal_inf_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=rh_S_temporal_inf_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 1300,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Total_rh_S_temporal_inf_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell Adap ####
#### Gesell_Adap_rh_G_temp_sup.Plan_tempo_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Adap,y=rh_G_temp_sup.Plan_tempo_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 200, ymax = 1000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Adap,
                       y=rh_G_temp_sup.Plan_tempo_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Adap,
                       y=rh_G_temp_sup.Plan_tempo_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 900,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Adap_rh_G_temp_sup.Plan_tempo_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell Language ####
#### Gesell_Lang_rh_G_oc.temp_med.Parahip_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=rh_G_oc.temp_med.Parahip_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 250, ymax = 000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_G_oc.temp_med.Parahip_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_G_oc.temp_med.Parahip_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 850,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_rh_G_oc.temp_med.Parahip_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Lang_rh_G_temporal_inf_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=rh_G_temporal_inf_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 500, ymax = 2500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_G_temporal_inf_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_G_temporal_inf_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 2250,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_rh_G_temporal_inf_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Lang_rh_G_temporal_middle_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=rh_G_temporal_middle_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 1000, ymax = 3000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_G_temporal_middle_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_G_temporal_inf_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 2750,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_rh_G_temporal_middle_area.png',
       width = 2737,height = 1795,units = "px")



#### Gesell_Lang_rh_Pole_temporal_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=rh_Pole_temporal_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 0, ymax = 1500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_Pole_temporal_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_Pole_temporal_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 1300,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_rh_Pole_temporal_area.png',
       width = 2737,height = 1795,units = "px")



#### Gesell_Lang_rh_S_temporal_inf_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=rh_S_temporal_inf_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 0, ymax = 1500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_S_temporal_inf_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=rh_S_temporal_inf_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 1300,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_rh_S_temporal_inf_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell Social ####
#### Gesell_Social_rh_G_temp_sup.Plan_tempo_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=rh_G_temp_sup.Plan_tempo_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 200, ymax = 1000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=rh_G_temp_sup.Plan_tempo_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=rh_G_temp_sup.Plan_tempo_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 900,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_rh_G_temp_sup.Plan_tempo_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Social_rh_Pole_temporal_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=rh_Pole_temporal_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 0, ymax = 1500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=rh_Pole_temporal_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=rh_Pole_temporal_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 1300,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_rh_Pole_temporal_area.png',
       width = 2737,height = 1795,units = "px")



#### Gesell_Social_rh_S_temporal_inf_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=rh_S_temporal_inf_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 0, ymax = 1500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=rh_S_temporal_inf_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=rh_S_temporal_inf_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 1300,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_rh_S_temporal_inf_area.png',
       width = 2737,height = 1795,units = "px")

############################################################################
#### 6.Gesell & lh.aparc.a2009s.surface ####
#### Gesell Total ####
#### Gesell_Total_lh_Pole_temporal_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=lh_Pole_temporal_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 250, ymax = 1500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=lh_Pole_temporal_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=lh_Pole_temporal_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 1450,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Total_lh_Pole_temporal_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Total_lh_S_oc.temp_med_and_Lingual_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Total,y=lh_S_oc.temp_med_and_Lingual_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 500, ymax = 2000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=lh_S_oc.temp_med_and_Lingual_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Total,
                       y=lh_S_oc.temp_med_and_Lingual_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 1800,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Total_lh_S_oc.temp_med_and_Lingual_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell Adap ####
#### Gesell_Adap_lh_S_oc.temp_med_and_Lingual_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Adap,y=lh_S_oc.temp_med_and_Lingual_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 500, ymax = 2000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Adap,
                       y=lh_S_oc.temp_med_and_Lingual_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Adap,
                       y=lh_S_oc.temp_med_and_Lingual_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 1800,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Adap_lh_S_oc.temp_med_and_Lingual_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell Language ####
#### Gesell_Lang_lh_G_temporal_middle_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=lh_G_temporal_middle_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 500, ymax = 2500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_G_temporal_middle_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_G_temporal_middle_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 2200,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_lh_G_temporal_middle_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Lang_lh_Pole_temporal_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=lh_Pole_temporal_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 250, ymax = 1800
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_Pole_temporal_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_Pole_temporal_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 1600,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_lh_Pole_temporal_area.png',
       width = 2737,height = 1795,units = "px")
#### Gesell_Lang_lh_S_oc.temp_med_and_Lingual_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=lh_S_oc.temp_med_and_Lingual_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 500, ymax = 2000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_S_oc.temp_med_and_Lingual_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_S_oc.temp_med_and_Lingual_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 1800,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_lh_S_oc.temp_med_and_Lingual_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Lang_lh_S_temporal_inf_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Lang,y=lh_S_temporal_inf_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 0, ymax = 1500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_S_temporal_inf_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Lang,
                       y=lh_S_temporal_inf_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 1300,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Lang_lh_S_temporal_inf_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell Social ####
#### Gesell_Social_lh_G_temp_sup.Plan_tempo_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=lh_G_temp_sup.Plan_tempo_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 250, ymax = 1500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=lh_G_temp_sup.Plan_tempo_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=lh_G_temp_sup.Plan_tempo_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 1350,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_lh_G_temp_sup.Plan_tempo_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Social_lh_Pole_temporal_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=lh_Pole_temporal_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 250, ymax = 1800
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=lh_Pole_temporal_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=lh_Pole_temporal_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 1600,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_lh_Pole_temporal_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Social_lh_S_oc.temp_med_and_Lingual_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=lh_S_oc.temp_med_and_Lingual_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 500, ymax = 2000
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=lh_S_oc.temp_med_and_Lingual_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=lh_S_oc.temp_med_and_Lingual_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 1800,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_lh_S_oc.temp_med_and_Lingual_area.png',
       width = 2737,height = 1795,units = "px")

#### Gesell_Social_lh_S_temporal_transverse_area ####
p <- ggplot(NeuralData_assessments,aes(x=Gesell_Social,y=lh_S_temporal_transverse_area,
                                       xmin = 20, xmax = 100,
                                       ymin = 100, ymax = 500
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(data =NeuralData_assessments, method = lm, se = F, fullrange = TRUE, color = 'gray')+
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=lh_S_temporal_transverse_area,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=lh_S_temporal_transverse_area,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 450,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('plots220715/scatterplot correlation/Gesell_Social_lh_S_temporal_transverse_area.png',
       width = 2737,height = 1795,units = "px")
