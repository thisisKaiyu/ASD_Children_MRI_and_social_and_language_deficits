setwd("~/Documents/foshan")
library(readxl)
library(dplyr)
library(ggpubr)

G_WMV <- readxl::read_excel('stats_analysis/group2.xlsx',sheet = 'GMV&WMV')
View(G_WMV)
colnames(G_WMV)

######################################################################################################
######################################### Part 1: Gray Matter Volume #################################
######################################################################################################

### correlation analysis ####
cor <- cor(G_WMV[,-c(1,2)])
cor


ASD <- dplyr::filter(G_WMV, Group == 'ASD')
cor_ASD <- cor(ASD[,-c(1,2)])
cor.test(ASD$Gesell_Social,ASD$GMV)
cor_ASD

nonASD <- dplyr::filter(G_WMV, Group == 'nonASD')
cor_nonASD <- cor(nonASD[,-c(1,2)])
cor.test(nonASD$Gesell_Social,nonASD$GMV)
cor_nonASD


## scatterplot: change ggplot(y=***) & ggpubr::stat_cor(aes(y=***))####
p <- ggplot(G_WMV,aes(x=Gesell_Social,y=GMV_nonASDSocial_mask,
                                       xmin = 40, xmax = 90
                                       #ymin = 800, ymax = 2200
)) +
  geom_point(aes(color=Group), size = 4) +
  #scale_color_brewer(palette="RdPu") +
  #geom_smooth(method = lm, se = F, fullrange = T, color = 'gray')+ #show gray regression line
  geom_smooth(method = lm, se = F, fullrange = T, aes(color = Group))+ #show red & green regression line
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=GMV_nonASDSocial_mask,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 68,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ylab('GMV')+
  # ggpubr::stat_cor(method = 'pearson',
  #                  aes(x=Gesell_Social,
  #                      y=GMV,
  #                      label = paste(gsub("R", "r", ..r.label..),..p.label..,
  #                                    sep = "~`,`~")),
  #                  color = "gray",
  #                  size = 5,
  #                  label.x = 70,
  #                  label.y = 0.58,
  #                  p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=25,face="bold"),
        axis.title.y = element_text(size=25,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
  
p
ggsave('stats_analysis/plots220715/GMV_Gesell_nonASDSocialmask.png',
       width = 7,height = 6)

### violinplot of all subjects (with and without Gesell): comparison of GMV between ASD and nonASD group ####
# 
G_WMV_all <- readxl::read_excel('stats_analysis/group2.xlsx',sheet = 'GMV&WMV(all)')
View(G_WMV_all)
a <- ggplot(G_WMV_all,aes(Group,GMV))+ 
  theme(axis.title = element_blank()) + #delete x,y,title 
  geom_violin(trim = F,width = 1.0) +
  geom_boxplot(width=0.1,fill = c('#77933B','#498399'))+ # add box
  geom_jitter(size = 4,width = 0.2)+ # higher width, more scattered dots
  theme_classic() +# set y/x color as black, set backgroud as white
  theme(legend.position  = c(1.5,0.5),
        axis.text=element_text(size=18,face = "bold",color = 'black'),
        axis.title.x = element_text(size=20,face="bold"),
        axis.title.y = element_text(size=20,face="bold"),
        axis.line.x = element_line(size = 1,color = 'black'),
        axis.line.y = element_line(size = 1,color = 'black'))+
  #ylim(-0.2,1.5) +
  # ggpubr::stat_compare_means(method = 't.test',
  #                            aes(label = paste0('p = ', ..p.format.., ..p.signif..)),
  #                            size = 6,
  #                            #label.y = 1.2,
  #                            label.x = 1.5)+
  ylab('GMV')# change y label
#scale_fill_brewer(palette = c('#77933B','#498399'))+ #fill the violin
a
t.test(G_WMV_all$GMV_sing1[G_WMV_all$Group =='ASD'],G_WMV_all$GMV_sing1[G_WMV_all$Group =='nonASD'])
t.test(G_WMV_all$GMV_sing2[G_WMV_all$Group =='ASD'],G_WMV_all$GMV_sing2[G_WMV_all$Group =='nonASD'])
t.test(G_WMV_all$GMV[G_WMV_all$Group =='ASD'],G_WMV_all$GMV[G_WMV_all$Group =='nonASD'])

ggsave('stats_analysis/plots220715/violinplot_ASD&nonASDsing2mask.png')




### Regression analysis ####
reg <- lm(GMV ~ Gesell_Social * Group + Age + Gender + TIV,
              G_WMV)
reg

reg_ASD <- lm(GMV ~ Gesell_Social + Age + Gender + TIV,
          data=G_WMV[G_WMV$Group == "ASD",])
anova(reg_ASD)

reg_nonASD <- lm(GMV ~ Gesell_Social + Age + Gender + TIV,
              data=G_WMV[G_WMV$Group == "nonASD",])


anova(reg_nonASD)
scatter.smooth(x=G_WMV$Gesell_Social, y=G_WMV$GMV, main="Gesell_Social ~ GMV", 
               xlab = "Gesell_Social", ylab="GMV")

# reg_ASD <- lm(GMV ~ Gesell_Social,data=ASD)
# reg_ASD
# 
# reg_nonASD <- lm(GMV ~ Gesell_Social,data=nonASD)
# reg_nonASD

summary(reg)
anova(reg)
# summary(reg_ASD)
# summary(reg_nonASD)
## for Linear regression with multiple predictors
# fit2 <- lm(y ~ x + z, data=dat)   
# Using the columns x, y, and z from the data frame

### Partial correlation (偏相关)####
#install.packages('ppcor')
library(ppcor)
library(MASS)
library(ggplot2)
##1) all subj
pcor.test(G_WMV$Gesell_Social,G_WMV$GMV,G_WMV[,c(8:10)],method="spearman") #6-8:Age; Gender; TIV
##plot##
Y_resid<-resid(lm(GMV ~ Age + Gender + TIV,G_WMV))
X_resid<-resid(lm(Gesell_Social ~ Age + Gender + TIV,G_WMV))


m<-ggplot(G_WMV, aes(x=X_resid, y=Y_resid)) +
  geom_point() +
  labs(x="Gesell_Social | Age + Gender + TIV", y = "GMV | Age + Gender + TIV")+
  scale_size_manual(values=c(15))+
  theme_classic()
m +  geom_smooth(method=lm)
ggsave('stats_analysis/plot220715/allsubj_partial_correlation_GMV_Gesell_Social.png')

##2) ASD
pcor.test(ASD$Gesell_Social,ASD$GMV,ASD[,c(8:9)],method="pearson")
##plot##
Y_resid<-resid(lm(GMV ~ Age + Gender,ASD))
X_resid<-resid(lm(Gesell_Social ~ Age + Gender,ASD))


m<-ggplot(ASD, aes(x=X_resid, y=Y_resid)) +
  geom_point() +
  labs(x="Gesell_Social | Age + Gender", y = "GMV | Age + Gender")+
  scale_size_manual(values=c(15))+
  theme_classic()
m +  geom_smooth(method=lm)
ggsave('stats_analysis/plot220715/ASD_partial_correlation_GMV_Gesell_Social.png')

##3) nonASD
pcor.test(nonASD$Gesell_Social,nonASD$GMV,nonASD[,c(8:9)],method="pearson")
##plot##
Y_resid<-resid(lm(GMV ~ Age + Gender,nonASD))
X_resid<-resid(lm(Gesell_Social ~ Age + Gender,nonASD))

library(ggplot2)
m<-ggplot(nonASD, aes(x=X_resid, y=Y_resid)) +
  geom_point() +
  labs(x="Gesell_Social | Age + Gender", y = "GMV | Age + Gender")+
  scale_size_manual(values=c(15))+
  theme_classic()
m +  geom_smooth(method=lm)
ggsave('stats_analysis/plot220715/nonASD_partial_correlation_GMV_Gesell_Social.png')

######################################################################################################
######################################### Part 2: White Matter Volume #################################
######################################################################################################
### correlation analysis ####
cor <- cor(G_WMV[,c(2,3,4,6,7,8)])
cor

ASD <- dplyr::filter(G_WMV, Group == 'ASD')
cor_ASD <- cor(ASD[,c(2,3,4,6,7,8)])
cor.test(ASD$Gesell_Social,ASD$WMV)
cor_ASD

nonASD <- dplyr::filter(G_WMV, Group == 'nonASD')
cor_nonASD <- cor(nonASD[,c(2,3,4,6,7,8)])
cor.test(nonASD$Gesell_Social,nonASD$WMV)
cor_nonASD


## scatterplot 
library(ggpubr)
p <- ggplot(G_WMV,aes(x=Gesell_Social,y=WMV,
                    xmin = 40, xmax = 90
                    #ymin = 800, ymax = 2200
)) +
  geom_point(aes(color=Group), size = 3.5) +
  #scale_color_brewer(palette="RdPu") +
  geom_smooth(method = lm, se = F, fullrange = T, color = 'gray')+ #show gray regression line
  geom_smooth(method = lm, se = F, fullrange = T, aes(color = Group))+ #show red & green regression line
  theme_classic()+
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=WMV,color=Group,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   size = 5,
                   label.x = 70,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # red and green r,p value
  ggpubr::stat_cor(method = 'pearson',
                   aes(x=Gesell_Social,
                       y=WMV,
                       label = paste(gsub("R", "r", ..r.label..),..p.label..,
                                     sep = "~`,`~")),
                   color = "gray",
                   size = 5,
                   label.x = 70,
                   label.y = 0.155,
                   p.accuracy = 0.001, r.accuracy = 0.01)+   # gray r,p value
  theme(axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)
  )
p
ggsave('stats_analysis/plot220715/WMV_Gesell_Social.png')


### Regression analysis ####
reg <- lm(WMV ~ Gesell_Social + Age + Gender + TIV,
          G_WMV)
reg

reg_ASD <- lm(WMV ~ Gesell_Social + Age + Gender + TIV,
              data=G_WMV[G_WMV$Group == "ASD",])
reg_ASD

reg_nonASD <- lm(WMV ~ Gesell_Social + Age + Gender + TIV,
                 data=G_WMV[G_WMV$Group == "nonASD",])
reg_nonASD

# reg_ASD <- lm(GMV ~ Gesell_Social,data=ASD)
# reg_ASD
# 
# reg_nonASD <- lm(GMV ~ Gesell_Social,data=nonASD)
# reg_nonASD

summary(reg)
anova(reg)
# summary(reg_ASD)
# summary(reg_nonASD)
## for Linear regression with multiple predictors
# fit2 <- lm(y ~ x + z, data=dat)   
# Using the columns x, y, and z from the data frame

### Partial correlation (偏相关)####
#install.packages('ppcor')
library(ppcor)
library(MASS)
##1) all subj
pcor.test(G_WMV$Gesell_Social,G_WMV$WMV,G_WMV[,c(6:8)],method="spearman") #6-8:Age; Gender; TIV
##plot##
Y_resid<-resid(lm(WMV ~ Age + Gender + TIV,G_WMV))
X_resid<-resid(lm(Gesell_Social ~ Age + Gender + TIV,G_WMV))

m<-ggplot(G_WMV, aes(x=X_resid, y=Y_resid)) +
  geom_point() +
  labs(x="Gesell_Social | Age + Gender + TIV", y = "WMV | Age + Gender + TIV")+
  scale_size_manual(values=c(15))+
  theme_classic()
m +  geom_smooth(method=lm)
ggsave('stats_analysis/plot220715/allsubj_partial_correlation_WMV_Gesell_Social.png')

##2) ASD
pcor.test(ASD$Gesell_Social,ASD$WMV,ASD[,c(6:8)],method="spearman")
##plot##
Y_resid<-resid(lm(WMV ~ Age + Gender + TIV,ASD))
X_resid<-resid(lm(Gesell_Social ~ Age + Gender + TIV,ASD))

m<-ggplot(ASD, aes(x=X_resid, y=Y_resid)) +
  geom_point() +
  labs(x="Gesell_Social | Age + Gender + TIV", y = "WMV | Age + Gender + TIV")+
  scale_size_manual(values=c(15))+
  theme_classic()
m +  geom_smooth(method=lm)
ggsave('stats_analysis/plot220715/ASD_partial_correlation_WMV_Gesell_Social.png')

##3) nonASD
pcor.test(nonASD$Gesell_Social,nonASD$GMV,nonASD[,c(6:8)],method="pearson")
##plot##
Y_resid<-resid(lm(WMV ~ Age + Gender + TIV,nonASD))
X_resid<-resid(lm(Gesell_Social ~ Age + Gender + TIV,nonASD))

library(ggplot2)
m<-ggplot(nonASD, aes(x=X_resid, y=Y_resid)) +
  geom_point() +
  labs(x="Gesell_Social | Age + Gender + TIV", y = "WMV | Age + Gender + TIV")+
  scale_size_manual(values=c(15))+
  theme_classic()
m +  geom_smooth(method=lm)
ggsave('stats_analysis/plot220715/nonASD_partial_correlation_WMV_Gesell_Social.png')
