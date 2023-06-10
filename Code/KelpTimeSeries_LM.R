

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



data <- read.csv( "./Data/Model_Data_Revised2023.csv", skip=0 )

data <- filter(data, Cluster == 0) #remove total values from clusters

data<- na.omit(data) #remove NAs

data$kmarea <- data$Area/1000000


clus <- read.csv( "./Data/Model_Data_Revised2023.csv", skip=0 )

clus1 <- filter(clus, Cluster == 1)

clus1 <- clus1$Norm_Area

clus1<- na.omit(clus1)


clus<- na.omit(clus) #remove NAs

clus <- filter(clus, Cluster >1) #remove total values from clusters


lowd <- filter(data, Year < 2004)
highd <- filter(data, Year > 2004)

lowc <- filter(clus, Year < 2004)
highc <- filter(clus, Year > 2004)

clus3 <- filter(clus, Cluster == 2)
clus5 <- filter(clus, Cluster == 3)
clus2 <- filter(clus, Cluster == 4)
clus4 <- filter(clus, Cluster == 5)


lowc3 <- filter(clus3, Year < 2004)
highc3 <- filter(clus3, Year > 2004)
lowc5 <- filter(clus5, Year < 2004)
highc5 <- filter(clus5, Year > 2004)
lowc2 <- filter(clus2, Year < 2004)
highc2 <- filter(clus2, Year > 2004)
lowc4 <- filter(clus4, Year < 2004)
highc4 <- filter(clus4, Year > 2004)

###################
#Average norm kelp area, SD and VAR
#######################

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

#lowhigh

mean(lowd$Norm_Area)
sd(lowd$Norm_Area)
var(lowd$Norm_Area)
cv(lowd$Norm_Area)

mean(highd$Norm_Area)
sd(highd$Norm_Area)
var(highd$Norm_Area)
cv(highd$Norm_Area)

mean(lowc2$Norm_Area)
sd(lowc2$Norm_Area)
var(lowc2$Norm_Area)
cv(lowc2$Norm_Area)

mean(highc2$Norm_Area)
sd(highc2$Norm_Area)
var(highc2$Norm_Area)
cv(highc2$Norm_Area)

mean(lowc3$Norm_Area)
sd(lowc3$Norm_Area)
var(lowc3$Norm_Area)
cv(lowc3$Norm_Area)

mean(highc3$Norm_Area)
sd(highc3$Norm_Area)
var(highc3$Norm_Area)
cv(highc3$Norm_Area)

mean(lowc4$Norm_Area)
sd(lowc4$Norm_Area)
var(lowc4$Norm_Area)
cv(lowc4$Norm_Area)

mean(highc4$Norm_Area)
sd(highc4$Norm_Area)
var(highc4$Norm_Area)
cv(highc4$Norm_Area)

mean(lowc5$Norm_Area)
sd(lowc5$Norm_Area)
var(lowc5$Norm_Area)
cv(lowc5$Norm_Area)

mean(highc5$Norm_Area)
sd(highc5$Norm_Area)
var(highc5$Norm_Area)
cv(highc5$Norm_Area)

#correlationofkelpclusters
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
#Correlation metrics & trend through time

#PDO
ggscatter(data, x = "PDO_AJ", y = "ENSO_AJ", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PDO", ylab = "ENSO")

mixed.lm <- lm(PDO_AJ ~ Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

ggscatter(data, x = "PDO_AJ", y = "NPGO_AJ", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PDO", ylab = "NPGO")

ggscatter(data, x = "PDO_AJ", y = "Bonilla_SSTAnomaly", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PDO", ylab = "SST Anomaly")

#ENSO
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

mixed.lm <- lm(Bonilla_SSTAnomaly ~ Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#NPGO

mixed.lm <- lm(NPGO_AJ ~ Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

ggscatter(data, x = "NPGO_AJ", y = "Bonilla_SSTAnomaly", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab ="NPGO", ylab = "SST Anomaly")

#ALL LMS 

##########Tide

mixed.lm <- lm(Norm_Area ~ Tide, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(data, aes(x = Tide, y = Norm_Area)) +
    geom_point() +
    geom_smooth(method = "lm"))

#low res
mixed.lm <- lm(Norm_Area ~ Year, data = lowd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(lowd, aes(x = Tide, y = Norm_Area)) +
    geom_point() +
    geom_smooth(method = "lm"))

#high res
mixed.lm <- lm(Norm_Area ~ Year, data = highd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(highd, aes(x = Tide, y = Norm_Area)) +
    geom_point() +
    geom_smooth(method = "lm"))

##########Resolution

mixed.lm <- lm(Norm_Area ~ Res_Number, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(data, aes(x = Res_Number, y = Norm_Area)) +
    geom_point() +
    geom_smooth(method = "lm"))

#low res
mixed.lm <- lm(Norm_Area ~ Res_Number, data = lowd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(lowd, aes(x = Res_Number, y = Norm_Area)) +
    geom_point() +
    geom_smooth(method = "lm"))

#high res
mixed.lm <- lm(Norm_Area ~ Res_Number, data = highd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(highd, aes(x = Res_Number, y = Norm_Area)) +
    geom_point() +
    geom_smooth(method = "lm"))

###########Total Area
#################################################################
#YEAR
mixed.lm <- lm(Norm_Area ~ Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(data, aes(x = Year, y = Norm_Area)) +
    geom_point() +
    geom_smooth(method = "lm"))

#low res
mixed.lm <- lm(Norm_Area ~ Year, data = lowd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ Year, data = highd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)



#################################################################
#SST Year 1
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly, data = lowd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly, data = highd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#SST Year 2
mixed.lm <- lm(Norm_Area ~ SST1 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ SST1, data = lowd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ SST1, data = highd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#SST Year 2
mixed.lm <- lm(Norm_Area ~ SST2 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ SST2, data = lowd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ SST2, data = highd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#################################################################
#SST GA Year 1
mixed.lm <- lm(Norm_Area ~ SST_GA , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ SST_GA, data = lowd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ SST_GA, data = highd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#################################################################
#PDO Year 1
mixed.lm <- lm(Norm_Area ~ PDO_AJ , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ PDO_AJ, data = lowd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ PDO_AJ, data = highd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#PDO Year 2
mixed.lm <- lm(Norm_Area ~ PDO1 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ PDO1, data = lowd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ PDO1, data = highd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#PDO Year 2
mixed.lm <- lm(Norm_Area ~ PDO2 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ PDO2, data = lowd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ PDO2, data = highd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


#################################################################
#ENSO Year 1
mixed.lm <- lm(Norm_Area ~ ENSO_AJ , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ ENSO_AJ, data = lowd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ ENSO_AJ, data = highd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#ENSO Year 2
mixed.lm <- lm(Norm_Area ~ ENSO1 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ ENSO1, data = lowd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ ENSO1, data = highd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#ENSO Year 2
mixed.lm <- lm(Norm_Area ~ ENSO2 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ ENSO2, data = lowd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ ENSO2, data = highd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


#################################################################
#NPGO Year 1
mixed.lm <- lm(Norm_Area ~ NPGO_AJ , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ NPGO_AJ, data = lowd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ NPGO_AJ, data = highd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#NPGO Year 2
mixed.lm <- lm(Norm_Area ~ NPGO1 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ NPGO1, data = lowd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ NPGO1, data = highd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#NPGO Year 2
mixed.lm <- lm(Norm_Area ~ NPGO2 , data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ NPGO2, data = lowd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ NPGO2, data = highd) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

################################################
#multivariate model

mixed.lm <- lm(Norm_Area ~ ENSO_AJ + PDO_AJ, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly + ENSO_AJ, data = data) 
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

mixed.lm <- lm(Norm_Area ~ PDO_AJ + Year, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

mixed.lm <- lm(Norm_Area ~ Year + SST1, data = data) 
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











#CLUSTER 2
#################################################################
#YEAR
mixed.lm <- lm(Norm_Area ~ Year, data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ Year, data = lowc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ Year, data = highc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#################################################################
#SST Year 1
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly, data = lowc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly, data = highc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#SST Year 2
mixed.lm <- lm(Norm_Area ~ SST1 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ SST1, data = lowc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ SST1, data = highc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#SST Year 3
mixed.lm <- lm(Norm_Area ~ SST2 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ SST2, data = lowc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ SST2, data = highc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#################################################################
#SST GA Year 1
mixed.lm <- lm(Norm_Area ~ SST_GA , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ SST_GA, data = lowc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ SST_GA, data = highc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#################################################################
#PDO Year 1
mixed.lm <- lm(Norm_Area ~ PDO_AJ , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ PDO_AJ, data = lowc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ PDO_AJ, data = highc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#PDO Year 2
mixed.lm <- lm(Norm_Area ~ PDO1 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ PDO1, data = lowc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ PDO1, data = highc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#PDO Year 2
mixed.lm <- lm(Norm_Area ~ PDO2 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ PDO2, data = lowc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ PDO2, data = highc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


#################################################################
#ENSO Year 1
mixed.lm <- lm(Norm_Area ~ ENSO_AJ , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ ENSO_AJ, data = lowc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)
#high res
mixed.lm <- lm(Norm_Area ~ ENSO_AJ, data = highc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#ENSO Year 2
mixed.lm <- lm(Norm_Area ~ ENSO1 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ ENSO1, data = lowc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ ENSO1, data = highc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#ENSO Year 2
mixed.lm <- lm(Norm_Area ~ ENSO2 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ ENSO2, data = lowc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ ENSO2, data = highc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


#################################################################
#NPGO Year 1
mixed.lm <- lm(Norm_Area ~ NPGO_AJ , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ NPGO_AJ, data = lowc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ NPGO_AJ, data = highc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#NPGO Year 2
mixed.lm <- lm(Norm_Area ~ NPGO1 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ NPGO1, data = lowc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ NPGO1, data = highc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#NPGO Year 2
mixed.lm <- lm(Norm_Area ~ NPGO2 , data = clus2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ NPGO2, data = lowc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ NPGO2, data = highc2) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

######################Multivariate models

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


#CLUSTER 3
###########################################################################
#YEAR
mixed.lm <- lm(Norm_Area ~ Year, data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ Year, data = lowc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ Year, data = highc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#################################################################
#SST Year 1
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly, data = lowc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly, data = highc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#SST Year 2
mixed.lm <- lm(Norm_Area ~ SST1 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ SST1, data = lowc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ SST1, data = highc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#SST Year 3
mixed.lm <- lm(Norm_Area ~ SST2 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ SST2, data = lowc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ SST2, data = highc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#################################################################
#SST GA Year 1
mixed.lm <- lm(Norm_Area ~ SST_GA , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ SST_GA, data = lowc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ SST_GA, data = highc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#################################################################
#PDO Year 1
mixed.lm <- lm(Norm_Area ~ PDO_AJ , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ PDO_AJ, data = lowc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ PDO_AJ, data = highc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#PDO Year 2
mixed.lm <- lm(Norm_Area ~ PDO1 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ PDO1, data = lowc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ PDO1, data = highc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#PDO Year 2
mixed.lm <- lm(Norm_Area ~ PDO2 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ PDO2, data = lowc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ PDO2, data = highc3)
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


#################################################################
#ENSO Year 1
mixed.lm <- lm(Norm_Area ~ ENSO_AJ , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ ENSO_AJ, data = lowc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ ENSO_AJ, data = highc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#ENSO Year 2
mixed.lm <- lm(Norm_Area ~ ENSO1 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ ENSO1, data = lowc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ ENSO1, data = highc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#ENSO Year 2
mixed.lm <- lm(Norm_Area ~ ENSO2 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ ENSO2, data = lowc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ ENSO2, data = highc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


#################################################################
#NPGO Year 1
mixed.lm <- lm(Norm_Area ~ NPGO_AJ , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ NPGO_AJ, data = lowc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ NPGO_AJ, data = highc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#NPGO Year 2
mixed.lm <- lm(Norm_Area ~ NPGO1 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ NPGO1, data = lowc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ NPGO1, data = highc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#NPGO Year 2
mixed.lm <- lm(Norm_Area ~ NPGO2 , data = clus3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ NPGO2, data = lowc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ NPGO2, data = highc3) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

######################Multivariate models

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


#CLUSTER 4
###########################################################################
#YEAR
mixed.lm <- lm(Norm_Area ~ Year, data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ Year, data = lowc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ Year, data = highc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#################################################################
#SST Year 1
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly, data = lowc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly, data = highc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#SST Year 2
mixed.lm <- lm(Norm_Area ~ SST1 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ SST1, data = lowc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ SST1, data = highc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#SST Year 2
mixed.lm <- lm(Norm_Area ~ SST2 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ SST2, data = lowc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ SST2, data = highc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#################################################################
#SST GA Year 1
mixed.lm <- lm(Norm_Area ~ SST_GA , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ SST_GA, data = lowc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ SST_GA, data = highc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#################################################################
#PDO Year 1
mixed.lm <- lm(Norm_Area ~ PDO_AJ , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ PDO_AJ, data = lowc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ PDO_AJ, data = highc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#PDO Year 2
mixed.lm <- lm(Norm_Area ~ PDO1 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ PDO1, data = lowc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ PDO1, data = highc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#PDO Year 2
mixed.lm <- lm(Norm_Area ~ PDO2 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ PDO2, data = lowc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ PDO2, data = highc4)
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


#################################################################
#ENSO Year 1
mixed.lm <- lm(Norm_Area ~ ENSO_AJ , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ ENSO_AJ, data = lowc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ ENSO_AJ, data = highc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#ENSO Year 2
mixed.lm <- lm(Norm_Area ~ ENSO1 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ ENSO1, data = lowc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ ENSO1, data = highc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#ENSO Year 2
mixed.lm <- lm(Norm_Area ~ ENSO2 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ ENSO2, data = lowc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ ENSO2, data = highc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


#################################################################
#NPGO Year 1
mixed.lm <- lm(Norm_Area ~ NPGO_AJ , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ NPGO_AJ, data = lowc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ NPGO_AJ, data = highc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#NPGO Year 2
mixed.lm <- lm(Norm_Area ~ NPGO1 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ NPGO1, data = lowc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ NPGO1, data = highc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#NPGO Year 2
mixed.lm <- lm(Norm_Area ~ NPGO2 , data = clus4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ NPGO2, data = lowc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ NPGO2, data = highc4) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)









#CLUSTER 5
###########################################################################
#YEAR
mixed.lm <- lm(Norm_Area ~ Year, data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ Year, data = lowc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ Year, data = highc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#################################################################
#SST Year 1
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly, data = lowc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ Bonilla_SSTAnomaly, data = highc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#SST Year 2
mixed.lm <- lm(Norm_Area ~ SST1 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ SST1, data = lowc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ SST1, data = highc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#SST Year 2
mixed.lm <- lm(Norm_Area ~ SST2 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ SST2, data = lowc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ SST2, data = highc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#################################################################
#SST GA Year 1
mixed.lm <- lm(Norm_Area ~ SST_GA , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ SST_GA, data = lowc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ SST_GA, data = highc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#################################################################
#PDO Year 1
mixed.lm <- lm(Norm_Area ~ PDO_AJ , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ PDO_AJ, data = lowc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ PDO_AJ, data = highc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#PDO Year 2
mixed.lm <- lm(Norm_Area ~ PDO1 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ PDO1, data = lowc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ PDO1, data = highc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#PDO Year 2
mixed.lm <- lm(Norm_Area ~ PDO2 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ PDO2, data = lowc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ PDO2, data = highc5)
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


#################################################################
#ENSO Year 1
mixed.lm <- lm(Norm_Area ~ ENSO_AJ , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ ENSO_AJ, data = lowc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ ENSO_AJ, data = highc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#ENSO Year 2
mixed.lm <- lm(Norm_Area ~ ENSO1 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ ENSO1, data = lowc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ ENSO1, data = highc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#ENSO Year 2
mixed.lm <- lm(Norm_Area ~ ENSO2 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ ENSO2, data = lowc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ ENSO2, data = highc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)


#################################################################
#NPGO Year 1
mixed.lm <- lm(Norm_Area ~ NPGO_AJ , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ NPGO_AJ, data = lowc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ NPGO_AJ, data = highc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#NPGO Year 2
mixed.lm <- lm(Norm_Area ~ NPGO1 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ NPGO1, data = lowc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ NPGO1, data = highc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#NPGO Year 2
mixed.lm <- lm(Norm_Area ~ NPGO2 , data = clus5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#low res
mixed.lm <- lm(Norm_Area ~ NPGO2, data = lowc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#high res
mixed.lm <- lm(Norm_Area ~ NPGO2, data = highc5) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

#####################################
#Climate indices N SST trends

mixed.lm <- lm(Year ~ PDO_AJ, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(data, aes(x = Year, y = PDO_AJ)) +
        geom_point() +
        geom_smooth(method = "lm"))

mixed.lm <- lm(Year ~ ENSO_AJ, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(data, aes(x = Year, y = ENSO_AJ)) +
        geom_point() +
        geom_smooth(method = "lm"))

mixed.lm <- lm(Year ~ NPGO_AJ, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(data, aes(x = Year, y = NPGO_AJ)) +
        geom_point() +
        geom_smooth(method = "lm"))

mixed.lm <- lm(Year ~ Bonilla_SSTAnomaly, data = data) 
summary(mixed.lm)
AICc(mixed.lm)
r.squaredGLMM(mixed.lm)

(P_Area <- ggplot(data, aes(x = Year, y = Bonilla_SSTAnomaly)) +
        geom_point() +
        geom_smooth(method = "lm"))
