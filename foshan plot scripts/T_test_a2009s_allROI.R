install.packages("tidyverse")
install.packages('writexl')
library(tidyverse)
setwd("~/Documents/foshan/stats_analysis")

Behavioral_assessment <- readxl::read_excel("data220715/Assessment_for_analysis.xlsx")
#View(Behavioral_assessment)
## ttest is for thickness & surface
## ttest2 is for volume
ttest <- function(NeuralData,Behavioral_assessment){
  
  ### extract "temp*" colnames ###
  # which(grepl("temp",colnames(NeuralData)))
  # colnames(NeuralData)[c(1,which(grepl("temp",colnames(NeuralData))))]
  # NeuralData <- as.data.frame(NeuralData[,c(1,which(grepl("temp",colnames(NeuralData))))])
  #View(NeuralData)
  
  #Merge NeuralData and behavioral assessments by subject names, in order to get ASD/nonASD for each subjects
  NeuralData_assessments <- merge(NeuralData,Behavioral_assessment,
                                  by.x = colnames(NeuralData[1]),
                                  by.y = colnames(Behavioral_assessment[1]))
  colnames(NeuralData_assessments)[79] <- "Group"
  
  #View(NeuralData_assessments)
  
  #filter ASD and nonASD subjects, and assign 2 different dataframes 
  NeuralData <- NeuralData_assessments[,c(1:79)]
  #View(NeuralData)
  NeuralData_ASD <- NeuralData %>%
    filter(NeuralData[79] == "ASD")
  #View(NeuralData_ASD)
  NeuralData_nonASD <- NeuralData %>%
    filter(NeuralData[79] == "nonASD")
  #View(NeuralData_nonASD)
  #create dataframe
  NeuralData_ttest <- as.data.frame(matrix(0,77,2))
  colnames(NeuralData_ttest) <- c("t value","p value")
  rownames(NeuralData_ttest) <- colnames(NeuralData)[2:78]
  #View(NeuralData_ttest)
  
  # for loop to input t value and p value in to one table
  for (x in 2:78){
    NeuralData_ttest[x-1,1] <- t.test(NeuralData_ASD[x],NeuralData_nonASD[x])$statistic
    NeuralData_ttest[x-1,2] <- t.test(NeuralData_ASD[x],NeuralData_nonASD[x])$p.value
    NeuralData_ttest[,1] <- round(NeuralData_ttest[,1], digits = 2)
    NeuralData_ttest[,2] <- round(NeuralData_ttest[,2], digits = 3)
  }
  table <- as.matrix(rownames(NeuralData_ttest),77,1)
  NeuralData_ttest <- cbind(table,NeuralData_ttest)
  #View(NeuralData_ttest)
}
ttest2 <- function(NeuralData,Behavioral_assessment){
  
  # which(grepl("temp",colnames(NeuralData)))
  # colnames(NeuralData)[c(1,which(grepl("temp",colnames(NeuralData))))]
  # NeuralData <- as.data.frame(NeuralData[,c(1,which(grepl("temp",colnames(NeuralData))))])
  #View(NeuralData)
  
  #Merge NeuralData and behavioral assessments by subject names, in order to get ASD/nonASD for each subjects
  NeuralData_assessments <- merge(NeuralData,Behavioral_assessment,
                                  by.x = colnames(NeuralData[1]),
                                  by.y = colnames(Behavioral_assessment[1]))
  colnames(NeuralData_assessments)[78] <- "Group"
  
  #View(NeuralData_assessments)
  
  #filter ASD and nonASD subjects, and assign 2 different dataframes 
  NeuralData <- NeuralData_assessments[,c(1:78)]
  #View(NeuralData)
  NeuralData_ASD <- NeuralData %>%
    filter(NeuralData[78] == "ASD")
  #View(NeuralData_ASD)
  NeuralData_nonASD <- NeuralData %>%
    filter(NeuralData[78] == "nonASD")
  #View(NeuralData_nonASD)
  #create dataframe
  NeuralData_ttest <- as.data.frame(matrix(0,76,2))
  colnames(NeuralData_ttest) <- c("t value","p value")
  rownames(NeuralData_ttest) <- colnames(NeuralData)[2:77]
  #View(NeuralData_ttest)
  
  # for loop to input t value and p value in to one table
  for (x in 2:77){
    NeuralData_ttest[x-1,1] <- t.test(NeuralData_ASD[x],NeuralData_nonASD[x])$statistic
    NeuralData_ttest[x-1,2] <- t.test(NeuralData_ASD[x],NeuralData_nonASD[x])$p.value
    NeuralData_ttest[,1] <- round(NeuralData_ttest[,1], digits = 2)
    NeuralData_ttest[,2] <- round(NeuralData_ttest[,2], digits = 3)
  }
  
  table <- as.matrix(rownames(NeuralData_ttest),76,1)
  NeuralData_ttest <- cbind(table,NeuralData_ttest)
  #View(NeuralData_ttest)
}

################################ t test 1 for rh.a2009s.thickness######################################
NeuralData <- read.table("data220715/rh.a2009s.thickness.txt",header = T)
#View(NeuralData)
pp <- ttest(NeuralData,Behavioral_assessment)
pp
# rh.a2009s.thickness <- as.matrix(rownames(NeuralData_ttest),77,1)
# NeuralData_ttest <- cbind(rh.a2009s.thickness,NeuralData_ttest)
# View(NeuralData_ttest)
writexl::write_xlsx(pp,"results220715/rh.a2009s.thickness_ttest_all.xlsx")

################################ t test 4 for lh.a2009s.thickness######################################
NeuralData <- read.table("data220715/lh.a2009s.thickness.txt",header = T)
#View(NeuralData)
pp <- ttest(NeuralData,Behavioral_assessment)
pp
writexl::write_xlsx(pp,"results220715/lh.a2009s.thickness_ttest_all.xlsx")
################################ t test 2 for rh.a2009s.volume ######################################
NeuralData <- read.table("data220715/rh.a2009s.volume.txt",header = T)
#View(NeuralData)
pp2 <- ttest2(NeuralData,Behavioral_assessment)
pp2
writexl::write_xlsx(pp2,"results220715/rh.a2009s.volume_ttest_all.xlsx")
################################ t test 5 for lh.a2009s.volume######################################
NeuralData <- read.table("data220715/lh.a2009s.volume.txt",header = T)
#View(NeuralData)
pp2 <- ttest2(NeuralData,Behavioral_assessment)
pp2
writexl::write_xlsx(pp2,"results220715/lh.a2009s.volume_ttest_all.xlsx")
################################ t test 3 for rh.a2009s.surface######################################
NeuralData <- read.table("data220715/rh.a2009s.surface.txt",header = T)
#View(NeuralData)
pp <- ttest(NeuralData,Behavioral_assessment)
pp
writexl::write_xlsx(pp,"results220715/rh.a2009s.surface_ttest_all.xlsx")

################################ t test 6 for lh.a2009s.surface######################################
NeuralData <- read.table("data220715/lh.a2009s.surface.txt",header = T)
#View(NeuralData)
pp <- ttest(NeuralData,Behavioral_assessment)
pp
writexl::write_xlsx(pp,"results220715/lh.a2009s.surface_ttest_all.xlsx")



