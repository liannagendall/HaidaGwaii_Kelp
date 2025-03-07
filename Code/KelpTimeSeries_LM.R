

library(Rcpp)
library(tidyverse)
library(ggplot2)
library(lme4)
library(AICcmodavg)
library(lmerTest)
library(MuMIn)
library(blme)
library("lattice")
library(ggplot2)
library(ggpubr)
library(raster)
library(dplyr)

setwd("C:/Users/genda/Dropbox/RProjects/HaidaGwaii_Kelp")

data <- read.csv( "./Data/Model_Data_Revised2023.csv", skip=0 )

data <- filter(data, Cluster == 0) #remove total values from clusters

data <- data[, c(1:48)]

data<- na.omit(data) #remove NAs

data$kmarea <- data$Area/1000000


clus <- read.csv( "./Data/Model_Data_Revised2023.csv", skip=0 )

clus  <- clus[, c(1:48)]

clus1 <- filter(clus, Cluster == 1)

clus1 <- clus1$Norm_Area

clus1<- na.omit(clus1)


clus<- na.omit(clus) #remove NAs

clus <- filter(clus, Cluster >1) #remove total values from clusters

clus3 <- filter(clus, Cluster == 2) #fixing the cluster numbers so they coincide with subregions (the cluster column is from the clustering analysis)
clus5 <- filter(clus, Cluster == 3)
clus2 <- filter(clus, Cluster == 4)
clus4 <- filter(clus, Cluster == 5)


#Average norm kelp area, SD and VAR####

mean(data$Norm_Area)
sd(data$Norm_Area)
var(data$Norm_Area)
cv(data$Norm_Area)

mean(clus1)
sd(clus1)
var(clus1)
cv(clus1)

mean(clus2$Norm_Area)
sd(clus2$Norm_Area)
var(clus2$Norm_Area)
cv(clus2$Norm_Area)

mean(clus3$Norm_Area)
sd(clus3$Norm_Area)
var(clus3$Norm_Area)
cv(clus3$Norm_Area)

mean(clus4$Norm_Area)
sd(clus4$Norm_Area)
var(clus4$Norm_Area)
cv(clus4$Norm_Area)

mean(clus5$Norm_Area)
sd(clus5$Norm_Area)
var(clus5$Norm_Area)
cv(clus5$Norm_Area)



#Correlation of kelp clusters####
clusterarea <- read.csv( "./Data/Clusters_NormArea.csv", skip=0 )

clusterarea<- na.omit(clusterarea)

ggscatter(clusterarea, x = "Clus1", y = "Clus2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Cluster 1", ylab = "Cluster 2")
ggscatter(clusterarea, x = "Clus1", y = "Clus3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Cluster 1", ylab = "Cluster 3")
ggscatter(clusterarea, x = "Clus1", y = "Clus4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Cluster 1", ylab = "Cluster 4")
ggscatter(clusterarea, x = "Clus1", y = "Clus5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Cluster 1", ylab = "Cluster 5")


ggscatter(clusterarea, x = "Clus2", y = "Clus3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Cluster 2", ylab = "Cluster 3")
ggscatter(clusterarea, x = "Clus2", y = "Clus4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Cluster 2", ylab = "Cluster 4")
ggscatter(clusterarea, x = "Clus2", y = "Clus5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Cluster 2", ylab = "Cluster 5")

ggscatter(clusterarea, x = "Clus3", y = "Clus4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Cluster 3", ylab = "Cluster 4")
ggscatter(clusterarea, x = "Clus3", y = "Clus5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Cluster 3", ylab = "Cluster 5")

ggscatter(clusterarea, x = "Clus4", y = "Clus5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Cluster 4", ylab = "Cluster 5")

ggscatter(clusterarea, x = "Clus4", y = "Clus3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Cluster 4", ylab = "Cluster 3")

#Correlation metrics & trend through time###########
##PDO####
mixed.lm <- lm(PDO_AJ ~ Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

ggscatter(data, x = "PDO_AJ", y = "ENSO_AJ", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PDO", ylab = "ENSO")


ggscatter(data, x = "PDO_AJ", y = "NPGO_AJ", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PDO", ylab = "NPGO")

ggscatter(data, x = "PDO_AJ", y = "Bonilla_SSTAnomaly", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PDO", ylab = "SST Anomaly")

ggscatter(data, x = "PDO_AJ", y = "bonlight_MHW_days", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PDO", ylab = "MHW days")

ggscatter(data, x = "PDO_AJ", y = "bonlight_MHW_cum_int", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PDO", ylab = "MHW CI")

##ENSO####
mixed.lm <- lm(ENSO_AJ ~ Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

ggscatter(data, x = "ENSO_AJ", y = "NPGO_AJ", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "ENSO", ylab = "NPGO")

ggscatter(data, x = "ENSO_AJ", y = "Bonilla_SSTAnomaly", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "ENSO", ylab = "SST Anomaly")

ggscatter(data, x = "ENSO_AJ", y = "bonlight_MHW_days", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "ENSO", ylab = "MHW days")

ggscatter(data, x = "ENSO_AJ", y = "bonlight_MHW_cum_int", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "ENSO", ylab = "MHW CI")

mixed.lm <- lm(Bonilla_SSTAnomaly ~ Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##NPGO####

mixed.lm <- lm(NPGO_AJ ~ Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


ggscatter(data, x = "NPGO_AJ", y = "Bonilla_SSTAnomaly", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab ="NPGO", ylab = "SST Anomaly")

ggscatter(data, x = "NPGO_AJ", y = "bonlight_MHW_days", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "NPGO", ylab = "MHW days")

ggscatter(data, x = "NPGO_AJ", y = "bonlight_MHW_cum_int", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "NPGO", ylab = "MHW CI")

##SST####
ggscatter(data, x = "Bonilla_SSTAnomaly", y = "bonlight_MHW_days", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "SST Anomaly", ylab = "MHW days")

ggscatter(data, x = "Bonilla_SSTAnomaly", y = "bonlight_MHW_cum_int", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "SST Anomaly", ylab = "MHW CI")
##MHW days####
ggscatter(data, x = "bonlight_MHW_days", y = "bonlight_MHW_cum_int", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MHW days", ylab = "MHW CI")

#ALL LMS#### 

###Tide####

mixed.lm <- lm(Norm_Area ~ Tide, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(data, aes(x = Tide, y = Norm_Area)) +
    geom_point() +
    labs(x = "Tide (m)", y = "Area (%)")+
    geom_smooth(method = "lm"))



##Resolution####

mixed.lm <- lm(Norm_Area ~ Res_Number, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(data, aes(x = Res_Number, y = Norm_Area)) +
    geom_point() +
    geom_smooth(method = "lm"))


#Total Area####
##YEAR####
mixed.lm <- lm(Norm_Area ~ Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(data, aes(x = Year, y = Norm_Area)) +
    geom_point() +
    geom_smooth(method = "lm"))
 
##SST Year 1####
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##SST Year 2####
mixed.lm <- lm(Norm_Area ~ SST1 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##SST Year 2####
mixed.lm <- lm(Norm_Area ~ SST2 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)
 
##SST GA Year 1####
mixed.lm <- lm(Norm_Area ~ SST_GA , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##PDO Year 1####
mixed.lm <- lm(Norm_Area ~ PDO_AJ , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##PDO Year 2####
mixed.lm <- lm(Norm_Area ~ PDO1 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


##PDO Year 2####
mixed.lm <- lm(Norm_Area ~ PDO2 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


##ENSO Year 1####
mixed.lm <- lm(Norm_Area ~ ENSO_AJ , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##ENSO Year 2####
mixed.lm <- lm(Norm_Area ~ ENSO1 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##ENSO Year 2####
mixed.lm <- lm(Norm_Area ~ ENSO2 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

 
##NPGO Year 1####
mixed.lm <- lm(Norm_Area ~ NPGO_AJ , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##NPGO Year 2####
mixed.lm <- lm(Norm_Area ~ NPGO1 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##NPGO Year 3####
mixed.lm <- lm(Norm_Area ~ NPGO2 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Days Year 1####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Days Year 2####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days1 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Days Year 3####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days2 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Cum Int 1####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Cum Int 2####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int1 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Cum Int 3####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int2 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


##multivariate model####

mixed.lm <- lm(Norm_Area ~ ENSO_AJ + PDO_AJ, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly + ENSO_AJ, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly + bonlight_MHW_days1, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly + bonlight_MHW_cum_int1, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days2 + ENSO_AJ, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int2 + ENSO_AJ, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int + ENSO_AJ + Bonilla_SSTAnomaly, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


mixed.lm <- lm(Norm_Area ~ ENSO_AJ + SST1, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ ENSO_AJ + Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ PDO_AJ + SST1, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


mixed.lm <- lm(Norm_Area ~ PDO_AJ + bonlight_MHW_days, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days + SST1, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ PDO_AJ + ENSO_AJ + SST1, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~  PDO_AJ + ENSO_AJ + Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~  SST1 + ENSO_AJ + Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~  PDO_AJ + ENSO_AJ + Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~  PDO_AJ + ENSO_AJ + SST1 + Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)




#CLUSTER 2####
 
##YEAR####
mixed.lm <- lm(Norm_Area ~ Year, data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)
 
##SST Year 1####
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##SST Year 2####
mixed.lm <- lm(Norm_Area ~ SST1 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##SST Year 3####
mixed.lm <- lm(Norm_Area ~ SST2 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)
 
##SST GA Year 1####
mixed.lm <- lm(Norm_Area ~ SST_GA , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)
 
##PDO Year 1####
mixed.lm <- lm(Norm_Area ~ PDO_AJ , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##PDO Year 2####
mixed.lm <- lm(Norm_Area ~ PDO1 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##PDO Year 2####
mixed.lm <- lm(Norm_Area ~ PDO2 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)
 
##ENSO Year 1####
mixed.lm <- lm(Norm_Area ~ ENSO_AJ , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##ENSO Year 2####
mixed.lm <- lm(Norm_Area ~ ENSO1 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##ENSO Year 2####
mixed.lm <- lm(Norm_Area ~ ENSO2 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)
 
##NPGO Year 1####
mixed.lm <- lm(Norm_Area ~ NPGO_AJ , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##NPGO Year 2####
mixed.lm <- lm(Norm_Area ~ NPGO1 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##NPGO Year 2####
mixed.lm <- lm(Norm_Area ~ NPGO2 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Days Year 1####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Days Year 2####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days1 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Days Year 3####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days2 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Cum Int 1####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Cum Int 2####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int1 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Cum Int 3####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int2 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#Multivariate models####

mixed.lm <- lm(Norm_Area ~ ENSO2 + Bonilla_SSTAnomaly, data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ ENSO2 + SST1, data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ ENSO2 + SST2, data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


#CLUSTER 3####
##YEAR####
mixed.lm <- lm(Norm_Area ~ Year, data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


 
##SST Year 1####
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


##SST Year 2####
mixed.lm <- lm(Norm_Area ~ SST1 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##SST Year 3####
mixed.lm <- lm(Norm_Area ~ SST2 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


 
##SST GA Year 1####
mixed.lm <- lm(Norm_Area ~ SST_GA , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


 
##PDO Year 1####
mixed.lm <- lm(Norm_Area ~ PDO_AJ , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##PDO Year 2####
mixed.lm <- lm(Norm_Area ~ PDO1 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##PDO Year 3####
mixed.lm <- lm(Norm_Area ~ PDO2 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)



 
##ENSO Year 1####
mixed.lm <- lm(Norm_Area ~ ENSO_AJ , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##ENSO Year 2####
mixed.lm <- lm(Norm_Area ~ ENSO1 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##ENSO Year 2####
mixed.lm <- lm(Norm_Area ~ ENSO2 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)



 
##NPGO Year 1####
mixed.lm <- lm(Norm_Area ~ NPGO_AJ , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##NPGO Year 2####
mixed.lm <- lm(Norm_Area ~ NPGO1 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##NPGO Year 2####
mixed.lm <- lm(Norm_Area ~ NPGO2 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


##MHW Days Year 1####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Days Year 2####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days1 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Days Year 3####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days2 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Cum Int 1####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Cum Int 2####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int1 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Cum Int 3####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int2 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##Multivariate models####

mixed.lm <- lm(Norm_Area ~ PDO_AJ + Bonilla_SSTAnomaly, data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


mixed.lm <- lm(Norm_Area ~ ENSO_AJ + Bonilla_SSTAnomaly, data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


mixed.lm <- lm(Norm_Area ~ ENSO1 + Bonilla_SSTAnomaly, data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


mixed.lm <- lm(Norm_Area ~ PDO_AJ + ENSO_AJ, data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ PDO_AJ + ENSO1, data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


mixed.lm <- lm(Norm_Area ~ PDO1 + ENSO1, data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)



mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int + ENSO_AJ, data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int + ENSO1, data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ PDO_AJ + bonlight_MHW_cum_int , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)



mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days + ENSO_AJ, data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days1 + ENSO1, data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ PDO_AJ + bonlight_MHW_days2 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)



#CLUSTER 4####
##YEAR####
mixed.lm <- lm(Norm_Area ~ Year, data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##SST Year 1####
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


##SST Year 2####
mixed.lm <- lm(Norm_Area ~ SST1 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##SST Year 2####
mixed.lm <- lm(Norm_Area ~ SST2 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##SST GA Year 1####
mixed.lm <- lm(Norm_Area ~ SST_GA , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##PDO Year 1####
mixed.lm <- lm(Norm_Area ~ PDO_AJ , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##PDO Year 2####
mixed.lm <- lm(Norm_Area ~ PDO1 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##PDO Year 2####
mixed.lm <- lm(Norm_Area ~ PDO2 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)
 
##ENSO Year 1####
mixed.lm <- lm(Norm_Area ~ ENSO_AJ , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##ENSO Year 2####
mixed.lm <- lm(Norm_Area ~ ENSO1 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##ENSO Year 2####
mixed.lm <- lm(Norm_Area ~ ENSO2 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)
 
##NPGO Year 1####
mixed.lm <- lm(Norm_Area ~ NPGO_AJ , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##NPGO Year 2####
mixed.lm <- lm(Norm_Area ~ NPGO1 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##NPGO Year 2####
mixed.lm <- lm(Norm_Area ~ NPGO2 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


##MHW Days Year 1####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Days Year 2####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days1 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Days Year 3####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days2 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Cum Int 1####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Cum Int 2####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int1 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Cum Int 3####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int2 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


##Multivariate models####


mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int + ENSO_AJ, data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int + ENSO1, data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days + ENSO_AJ, data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)



#CLUSTER 5####
##YEAR####
mixed.lm <- lm(Norm_Area ~ Year, data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##SST Year 1####
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##SST Year 2####
mixed.lm <- lm(Norm_Area ~ SST1 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##SST Year 3####
mixed.lm <- lm(Norm_Area ~ SST2 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##SST GA Year 1####
mixed.lm <- lm(Norm_Area ~ SST_GA , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##PDO Year 1####
mixed.lm <- lm(Norm_Area ~ PDO_AJ , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##PDO Year 2####
mixed.lm <- lm(Norm_Area ~ PDO1 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##PDO Year 2####
mixed.lm <- lm(Norm_Area ~ PDO2 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##ENSO Year 1####
mixed.lm <- lm(Norm_Area ~ ENSO_AJ , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##ENSO Year 2####
mixed.lm <- lm(Norm_Area ~ ENSO1 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##ENSO Year 2####
mixed.lm <- lm(Norm_Area ~ ENSO2 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


##NPGO Year 1####
mixed.lm <- lm(Norm_Area ~ NPGO_AJ , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##NPGO Year 2####
mixed.lm <- lm(Norm_Area ~ NPGO1 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##NPGO Year 2####
mixed.lm <- lm(Norm_Area ~ NPGO2 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


##MHW Days Year 1####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Days Year 2####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days1 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Days Year 3####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_days2 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Cum Int 1####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Cum Int 2####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int1 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##MHW Cum Int 3####
mixed.lm <- lm(Norm_Area ~ bonlight_MHW_cum_int2 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

##Multivariate models####
mixed.lm <- lm(Norm_Area ~ PDO_AJ + ENSO_AJ, data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ PDO1 + ENSO_AJ, data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ PDO2 + ENSO_AJ, data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ PDO_AJ + ENSO1, data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ PDO_AJ + ENSO2, data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)
#Climate indices N SST trends####
mixed.lm <- lm(PDO_AJ ~ Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(data, aes(x = Year, y = PDO_AJ)) +
        geom_point() +
        geom_smooth(method = "lm"))

mixed.lm <- lm(ENSO_AJ ~ Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(data, aes(x = Year, y = ENSO_AJ)) +
        geom_point() +
        geom_smooth(method = "lm"))

mixed.lm <- lm(NPGO_AJ ~ Year , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(data, aes(x = Year, y = NPGO_AJ)) +
        geom_point() +
        geom_smooth(method = "lm"))

mixed.lm <- lm(Bonilla_SSTAnomaly ~ Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(data, aes(x = Year, y = Bonilla_SSTAnomaly)) +
        geom_point() +
        geom_smooth(method = "lm"))

mixed.lm <- lm(bonlight_MHW_days ~ Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(data, aes(x = Year, y = bonlight_MHW_days)) +
        geom_point() +
        geom_smooth(method = "lm"))

mixed.lm <- lm(bonlight_MHW_cum_int ~ Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(data, aes(x = Year, y = bonlight_cum_int)) +
        geom_point() +
        geom_smooth(method = "lm"))


#Decadal trends lm####
data73_82 <- data[data$Year >= 1973 & data$Year <= 1982, ]
data83_92 <- data[data$Year >= 1983 & data$Year <= 1992, ]
data93_02 <- data[data$Year >= 1993 & data$Year <= 2002, ]
data03_12 <- data[data$Year >= 2003 & data$Year <= 2012, ]
data13_21 <- data[data$Year >= 2013 & data$Year <= 2022, ]

mixed.lm <- lm(Norm_Area ~ Year, data = data73_82) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ Year , data = data83_92) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ Year , data = data93_02) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ Year , data = data03_12) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ Year , data = data13_21) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


#Local Environmental Variables & Persistence####

pers <- read.csv( "./Data/Local_Persistence.csv")
pers <- na.omit(pers)
pers$LSAT_SST <- as.numeric(pers$LSAT_SST)
pers$Fetch <- as.numeric(pers$Fetch)

pers <- pers[pers$Persistence != 0, ]                         

#Depth                
mixed.lm <- lm(Persistence ~ Depth, data = pers) 
summary(mixed.lm)
(P_Area <- ggplot(pers, aes(x = Depth, y = Persistence)) +
    geom_point() +
    geom_smooth(method = "lm"))
AICc(mixed.lm)

#SST landsat                 
mixed.lm <- lm(Persistence ~ LSAT_SST, data = pers) 
summary(mixed.lm)
(P_Area <- ggplot(pers, aes(x = LSAT_SST, y = Persistence)) +
        geom_point() +
        geom_smooth(method = "lm"))
AICc(mixed.lm)

#Fetch
mixed.lm <- lm( Persistence ~ Fetch, data = pers) 
summary(mixed.lm)
(P_Area <- ggplot(pers, aes(x = Fetch, y = Persistence)) +
        geom_point() +
        geom_smooth(method = "lm"))
AICc(mixed.lm)

#Wind
mixed.lm <- lm(Persistence ~ Wind, data = pers) 
summary(mixed.lm)
(P_Area <- ggplot(pers, aes(x = Wind, y = Persistence)) +
        geom_point() +
        geom_smooth(method = "lm"))
AICc(mixed.lm)

#Tidal Current
mixed.lm <- lm(Persistence ~ Tidal_Current, data = pers) 
summary(mixed.lm)
(P_Area <- ggplot(pers, aes(x = Tidal_Current, y = Persistence)) +
        geom_point() +
        geom_smooth(method = "lm"))
AICc(mixed.lm)

#Multi
mixed.lm <- lm(Persistence  ~ Wind + Tidal_Current, data = pers) 
summary(mixed.lm)
AICc(mixed.lm)

mixed.lm <- lm(Persistence  ~ LSAT_SST + Tidal_Current, data = pers) 
summary(mixed.lm)
AICc(mixed.lm)

mixed.lm <- lm(Persistence  ~ Wind + LSAT_SST + Tidal_Current + Depth, data = pers) 
summary(mixed.lm)
AICc(mixed.lm)

mixed.lm <- lm(Persistence  ~ Wind + LSAT_SST + Depth, data = pers) 
summary(mixed.lm)
AICc(mixed.lm)


##Summarize local conditions by subregion######

# Summarize mean and standard deviation by Cluster
summary_by_cluster <- pers %>%
  group_by(Cluster) %>%
  summarize(
    Mean_LSAT_SST = mean(LSAT_SST, na.rm = TRUE),
    SD_LSAT_SST = sd(LSAT_SST, na.rm = TRUE),
    Mean_Wind = mean(Wind, na.rm = TRUE),
    SD_Wind = sd(Wind, na.rm = TRUE),
    Mean_Fetch = mean(Fetch, na.rm = TRUE),
    SD_Fetch = sd(Fetch, na.rm = TRUE),
    Mean_Tidal_Current = mean(Tidal_Current, na.rm = TRUE),
    SD_Tidal_Current = sd(Tidal_Current, na.rm = TRUE)
  )

# View the summary table
print(summary_by_cluster)
