setwd("~/Documents/foshan/stats_analysis")
##Descriptive Statistics of all subjects with Gesell scores
Behavioral_assessment <- readxl::read_excel("data220715/Assessment_for_analysis.xlsx")
NeuralData <- read.table("data220715/rh.a2009s.thickness.txt",header = T)

  which(grepl("temp",colnames(NeuralData)))
  colnames(NeuralData)[c(1,which(grepl("temp",colnames(NeuralData))))]
  NeuralData <- as.data.frame(NeuralData[,c(1,which(grepl("temp",colnames(NeuralData))))])
  #View(NeuralData)
  #Merge NeuralData and behavioral assessments by subject names
  NeuralData_assessments <- merge(NeuralData,Behavioral_assessment,
                                  by.x = colnames(NeuralData[1]),
                                  by.y = colnames(Behavioral_assessment[1]))
  colnames(NeuralData_assessments)[17] <- "Group"
  View(NeuralData_assessments)
  
  #table 1 all subjects
  NeuralData_assessments <- NeuralData_assessments[,c(1,17:19,21:26)]
  NeuralData_assessments <- na.omit(NeuralData_assessments)
  View(NeuralData_assessments)
  
  ASD <- dplyr::filter(NeuralData_assessments, Group == 'ASD')
  nonASD <- dplyr::filter(NeuralData_assessments, Group == 'nonASD')
  View(ASD)
  View(nonASD)
  
  # ASD <- NeuralData_assessments %>%
  #   filter(NeuralData_assessments[2] == "ASD")
  # View(ASD)
  # nonASD <- NeuralData_assessments %>%
  #   filter(NeuralData_assessments[2] == "nonASD")
  # View(nonASD)
  
  #create dataframe
  DescriptiveStats <- as.data.frame(matrix(0,8,5))
  colnames(DescriptiveStats) <- c('All subjects with Gesell','ASD',
                                  'nonASD','t-value/odds ration','p-value')
  rownames(DescriptiveStats) <- c('Age(months)','Gender(Females/Males)',
                                  colnames(NeuralData_assessments)[which(grepl("Gesell",colnames(NeuralData_assessments)))])
                                  
  View(DescriptiveStats)
  
  
####Age$all subjects####
  x <- NeuralData_assessments
  sd <- sd(x$age)
  mean <- mean(x$age)
  sd <- round(sd,digits = 2)
  mean <- round(mean,digits = 2)
  DescriptiveStats[1,1] <- stringr::str_c(mean,"±",sd)
  
  #Age$ASD vs nonASD
  age.mean <- 
    aggregate(x$age,by=list(Group=x$Group),mean)
  age.sd <- 
    aggregate(x$age,by=list(Group=x$Group),sd)
  ASD.age.mean <- round(age.mean$x[1],digits = 2)
  ASD.age.sd <- round(age.sd$x[1],digits = 2)
  
  nonASD.age.mean <- round(age.mean$x[2],digits = 2)
  nonASD.age.sd <- round(age.sd$x[2],digits = 2)
  
  DescriptiveStats[1,2] <- stringr::str_c(ASD.age.mean,"±",ASD.age.sd)
  DescriptiveStats[1,3] <- stringr::str_c(nonASD.age.mean,"±",nonASD.age.sd)
  
  DescriptiveStats[1,4] <- t.test(ASD[4],nonASD[4])$statistic
  DescriptiveStats[1,5] <- t.test(ASD[4],nonASD[4])$p.value
  DescriptiveStats[1,4] <- round(DescriptiveStats[1,4],digits = 2)
  DescriptiveStats[1,5] <- round(DescriptiveStats[1,5],digits = 3)
  

#### Gesell_Total$all subjects ####
  x <- NeuralData_assessments
  sd <- sd(x$Gesell_Total)
  mean <- mean(x$Gesell_Total)
  sd <- round(sd,digits = 2)
  mean <- round(mean,digits = 2)
  DescriptiveStats[3,1] <- stringr::str_c(mean,"±",sd)
  
  #Gesell_Total$ASD vs nonASD
  Gesell_Total.mean <- 
    aggregate(x$Gesell_Total,by=list(Group=x$Group),mean)
  Gesell_Total.sd <- 
    aggregate(x$Gesell_Total,by=list(Group=x$Group),sd)
  ASD.Gesell_Total.mean <- round(Gesell_Total.mean$x[1],digits = 2)
  ASD.Gesell_Total.sd <- round(Gesell_Total.sd$x[1],digits = 2)
  
  nonASD.Gesell_Total.mean <- round(Gesell_Total.mean$x[2],digits = 2)
  nonASD.Gesell_Total.sd <- round(Gesell_Total.sd$x[2],digits = 2)
  
  DescriptiveStats[3,2] <- stringr::str_c(ASD.Gesell_Total.mean,"±",ASD.Gesell_Total.sd)
  DescriptiveStats[3,3] <- stringr::str_c(nonASD.Gesell_Total.mean,"±",nonASD.Gesell_Total.sd)

  DescriptiveStats[3,4] <- t.test(ASD[5],nonASD[5])$statistic
  DescriptiveStats[3,5] <- t.test(ASD[5],nonASD[5])$p.value
  DescriptiveStats[3,4] <- round(DescriptiveStats[3,4],digits = 2)
  DescriptiveStats[3,5] <- round(DescriptiveStats[3,5],digits = 3)
  
#### Gesell_Adap$all subjects ####
  x <- NeuralData_assessments
  sd <- sd(x$Gesell_Adap)
  mean <- mean(x$Gesell_Adap)
  sd <- round(sd,digits = 2)
  mean <- round(mean,digits = 2)
  DescriptiveStats[4,1] <- stringr::str_c(mean,"±",sd)
  
  #Gesell_Adap$ASD vs nonASD
  Gesell_Adap.mean <- 
    aggregate(x$Gesell_Adap,by=list(Group=x$Group),mean)
  Gesell_Adap.sd <- 
    aggregate(x$Gesell_Adap,by=list(Group=x$Group),sd)
  ASD.Gesell_Adap.mean <- round(Gesell_Adap.mean$x[1],digits = 2)
  ASD.Gesell_Adap.sd <- round(Gesell_Adap.sd$x[1],digits = 2)
  
  nonASD.Gesell_Adap.mean <- round(Gesell_Adap.mean$x[2],digits = 2)
  nonASD.Gesell_Adap.sd <- round(Gesell_Adap.sd$x[2],digits = 2)
  
  DescriptiveStats[4,2] <- stringr::str_c(ASD.Gesell_Adap.mean,"±",ASD.Gesell_Adap.sd)
  DescriptiveStats[4,3] <- stringr::str_c(nonASD.Gesell_Adap.mean,"±",nonASD.Gesell_Adap.sd)
  
  DescriptiveStats[4,4] <- t.test(ASD[6],nonASD[6])$statistic
  DescriptiveStats[4,5] <- t.test(ASD[6],nonASD[6])$p.value
  DescriptiveStats[4,4] <- round(DescriptiveStats[4,4],digits = 2)
  DescriptiveStats[4,5] <- round(DescriptiveStats[4,5],digits = 3)
#### Gesell_MtrGross$all subjects ####
  x <- NeuralData_assessments
  sd <- sd(x$Gesell_MtrGross)
  mean <- mean(x$Gesell_MtrGross)
  sd <- round(sd,digits = 2)
  mean <- round(mean,digits = 2)
  DescriptiveStats[5,1] <- stringr::str_c(mean,"±",sd)
  
  #Gesell_MtrGross$ASD vs nonASD
  Gesell_MtrGross.mean <- 
    aggregate(x$Gesell_MtrGross,by=list(Group=x$Group),mean)
  Gesell_MtrGross.sd <- 
    aggregate(x$Gesell_MtrGross,by=list(Group=x$Group),sd)
  ASD.Gesell_MtrGross.mean <- round(Gesell_MtrGross.mean$x[1],digits = 2)
  ASD.Gesell_MtrGross.sd <- round(Gesell_MtrGross.sd$x[1],digits = 2)
  
  nonASD.Gesell_MtrGross.mean <- round(Gesell_MtrGross.mean$x[2],digits = 2)
  nonASD.Gesell_MtrGross.sd <- round(Gesell_MtrGross.sd$x[2],digits = 2)
  
  DescriptiveStats[5,2] <- stringr::str_c(ASD.Gesell_MtrGross.mean,"±",ASD.Gesell_MtrGross.sd)
  DescriptiveStats[5,3] <- stringr::str_c(nonASD.Gesell_MtrGross.mean,"±",nonASD.Gesell_MtrGross.sd)
  
  DescriptiveStats[5,4] <- t.test(ASD[7],nonASD[7])$statistic
  DescriptiveStats[5,5] <- t.test(ASD[7],nonASD[7])$p.value
  DescriptiveStats[5,4] <- round(DescriptiveStats[5,4],digits = 2)
  DescriptiveStats[5,5] <- round(DescriptiveStats[5,5],digits = 3)
#### Gesell_MtrFine$all subjects ####
  x <- NeuralData_assessments
  sd <- sd(x$Gesell_MtrFine)
  mean <- mean(x$Gesell_MtrFine)
  sd <- round(sd,digits = 2)
  mean <- round(mean,digits = 2)
  DescriptiveStats[6,1] <- stringr::str_c(mean,"±",sd)
  
  #Gesell_MtrFine$ASD vs nonASD
  Gesell_MtrFine.mean <- 
    aggregate(x$Gesell_MtrFine,by=list(Group=x$Group),mean)
  Gesell_MtrFine.sd <- 
    aggregate(x$Gesell_MtrFine,by=list(Group=x$Group),sd)
  ASD.Gesell_MtrFine.mean <- round(Gesell_MtrFine.mean$x[1],digits = 2)
  ASD.Gesell_MtrFine.sd <- round(Gesell_MtrFine.sd$x[1],digits = 2)
  
  nonASD.Gesell_MtrFine.mean <- round(Gesell_MtrFine.mean$x[2],digits = 2)
  nonASD.Gesell_MtrFine.sd <- round(Gesell_MtrFine.sd$x[2],digits = 2)
  
  DescriptiveStats[6,2] <- stringr::str_c(ASD.Gesell_MtrFine.mean,"±",ASD.Gesell_MtrFine.sd)
  DescriptiveStats[6,3] <- stringr::str_c(nonASD.Gesell_MtrFine.mean,"±",nonASD.Gesell_MtrFine.sd)

  DescriptiveStats[6,4] <- t.test(ASD[8],nonASD[8])$statistic
  DescriptiveStats[6,5] <- t.test(ASD[8],nonASD[8])$p.value
  DescriptiveStats[6,4] <- round(DescriptiveStats[6,4],digits = 2)
  DescriptiveStats[6,5] <- round(DescriptiveStats[6,5],digits = 3)
#### Gesell_Lang$all subjects ####
  x <- NeuralData_assessments
  sd <- sd(x$Gesell_Lang)
  mean <- mean(x$Gesell_Lang)
  sd <- round(sd,digits = 2)
  mean <- round(mean,digits = 2)
  DescriptiveStats[7,1] <- stringr::str_c(mean,"±",sd)
  
  #Gesell_Lang$ASD vs nonASD
  Gesell_Lang.mean <- 
    aggregate(x$Gesell_Lang,by=list(Group=x$Group),mean)
  Gesell_Lang.sd <- 
    aggregate(x$Gesell_Lang,by=list(Group=x$Group),sd)
  ASD.Gesell_Lang.mean <- round(Gesell_Lang.mean$x[1],digits = 2)
  ASD.Gesell_Lang.sd <- round(Gesell_Lang.sd$x[1],digits = 2)
  
  nonASD.Gesell_Lang.mean <- round(Gesell_Lang.mean$x[2],digits = 2)
  nonASD.Gesell_Lang.sd <- round(Gesell_Lang.sd$x[2],digits = 2)
  
  DescriptiveStats[7,2] <- stringr::str_c(ASD.Gesell_Lang.mean,"±",ASD.Gesell_Lang.sd)
  DescriptiveStats[7,3] <- stringr::str_c(nonASD.Gesell_Lang.mean,"±",nonASD.Gesell_Lang.sd)
  
  
  DescriptiveStats[7,4] <- t.test(ASD[9],nonASD[9])$statistic
  DescriptiveStats[7,5] <- t.test(ASD[9],nonASD[9])$p.value
  DescriptiveStats[7,4] <- round(DescriptiveStats[7,4],digits = 2)
  DescriptiveStats[7,5] <- round(DescriptiveStats[7,5],digits = 3)
#### Gesell_Social$all subjects ####
  x <- NeuralData_assessments
  sd <- sd(x$Gesell_Social)
  mean <- mean(x$Gesell_Social)
  sd <- round(sd,digits = 2)
  mean <- round(mean,digits = 2)
  DescriptiveStats[8,1] <- stringr::str_c(mean,"±",sd)
  
  #Gesell_Social$ASD vs nonASD
  Gesell_Social.mean <- 
    aggregate(x$Gesell_Social,by=list(Group=x$Group),mean)
  Gesell_Social.sd <- 
    aggregate(x$Gesell_Social,by=list(Group=x$Group),sd)
  ASD.Gesell_Social.mean <- round(Gesell_Social.mean$x[1],digits = 2)
  ASD.Gesell_Social.sd <- round(Gesell_Social.sd$x[1],digits = 2)
  
  nonASD.Gesell_Social.mean <- round(Gesell_Social.mean$x[2],digits = 2)
  nonASD.Gesell_Social.sd <- round(Gesell_Social.sd$x[2],digits = 2)
  
  DescriptiveStats[8,2] <- stringr::str_c(ASD.Gesell_Social.mean,"±",ASD.Gesell_Social.sd)
  DescriptiveStats[8,3] <- stringr::str_c(nonASD.Gesell_Social.mean,"±",nonASD.Gesell_Social.sd)

  
  DescriptiveStats[8,4] <- t.test(ASD[10],nonASD[10])$statistic
  DescriptiveStats[8,5] <- t.test(ASD[10],nonASD[10])$p.value
  DescriptiveStats[8,4] <- round(DescriptiveStats[8,4],digits = 2)
  DescriptiveStats[8,5] <- round(DescriptiveStats[8,5],digits = 3)
  
  
#### Fisher's exact test of gender ####
#Gender$All 
  a <-  as.data.frame(table(NeuralData_assessments$Gender))
  a
  DescriptiveStats[2,1] <- paste0(a[1,2],'/',a[2,2])
#Gender$ASD
  b <-  as.data.frame(table(ASD$Gender))
  b
  DescriptiveStats[2,2] <- paste0(b[1,2],'/',b[2,2])
#Gender$nonASD
  c <-  as.data.frame(table(nonASD$Gender))
  c
  DescriptiveStats[2,3] <- paste0(c[1,2],'/',c[2,2])
# #Gender t-value:ASD & nonASD
#   DescriptiveStats[2,4] <- t.test(ASD$Gender,nonASD$Gender)$statistic
#   DescriptiveStats[2,5] <- t.test(ASD$Gender,nonASD$Gender)$p.value
#   DescriptiveStats[2,4] <- round(DescriptiveStats[2,4],digits = 2)
#   DescriptiveStats[2,5] <- round(DescriptiveStats[2,5],digits = 3)

  #ASD vs nonASD
  oo <- xtabs(~NeuralData_assessments$Group
              +NeuralData_assessments$Gender,
              data = NeuralData_assessments)
  #chisq.test(oo) 
  #pvalue <- chisq.test(oo)$p.value
  #round(pvalue,digits = 3)
  #The chi-squared test applies an approximation assuming the sample is large, 
  #while the Fisher's exact test runs an exact procedure especially for small-sized samples
  
  fisher.test(oo)
  odd_ratio <- round(fisher.test(oo)$estimate,digits = 3)
  pvalue <- round(fisher.test(oo)$p.value,digits = 3)
  odd_ratio
  pvalue
  DescriptiveStats[2,4] <- odd_ratio
  DescriptiveStats[2,5] <- pvalue
  
  rowname <- as.data.frame(rownames(DescriptiveStats))
  DescriptiveStats <- cbind.data.frame(rowname,DescriptiveStats)
  writexl::write_xlsx(DescriptiveStats,"results220715/DescriptiveStats.xlsx")
  