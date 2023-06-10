
library(ggplot2)
library(lubridate)
library(scales)
library(reshape2)
library(pals)
library(wesanderson)
library(tidyverse)



#Ocean Oscillations TS #######

osc <-read.csv("./Data/Climate_Indices_Monthly.csv", header = TRUE, sep = ",")


#### z-scores for indices, classification in colorbars  ##########
newosc <- osc[ which(osc$year>=1968
                     & osc$year <= 2021), ]
#newosc$SOI2 <- (newosc$SOI)* -1
newosc <- newosc %>%
  mutate(date = make_date(year, month, day))

#newosc <- newosc %>% 
  #mutate(zscore.SOI = (SOI2 - mean(SOI2))/sd(SOI2))
newosc <- newosc %>% 
  mutate(zscore.PDO = (PDO - mean(PDO))/sd(PDO))
newosc <- newosc %>% 
  mutate(zscore.NPGO = (NPGO - mean(NPGO))/sd(NPGO))
newosc <- newosc %>% 
  mutate(zscore.ENSO = (ENSO - mean(ENSO))/sd(ENSO))

#Selection of 
myvars <- c("year", "zscore.PDO", "zscore.ENSO", "zscore.NPGO")
#myvars <- c("year", "zscore.SOI", "zscore.PDO")## Only SOI and PDO
#myvars <- c("year",  "zscore.NPGO")#only NPGO (inverse color scale)

zscores  <- newosc[myvars] #use only the indices you need 


zscores$zscore.NPGO <- zscores$zscore.NPGO*-1

zscores1 <- zscores[8:643,]

zscores1$Group <- rep(1:53, each = 12)

PDOmean <- zscores1 %>%
  group_by(Group) %>%
  summarise_at(vars(zscore.PDO), list(PDOmean = mean))

PDOmean$year <- c(1969:2021)

ENSOmean <- zscores1 %>%
  group_by(Group) %>%
  summarise_at(vars(zscore.ENSO), list(ENSOmean = mean))

ENSOmean$year <- c(1969:2021)

NPGOmean <- zscores1 %>%
  group_by(Group) %>%
  summarise_at(vars(zscore.NPGO), list(NPGOmean = mean))

NPGOmean$year <- c(1969:2021)


blended <- cbind(ENSOmean, PDOmean, NPGOmean)

blended2 <- subset(blended, select = c(year,PDOmean,ENSOmean,NPGOmean))
zscores.m <- melt(blended2,id.vars = "year")


plot <- ggplot(zscores.m, aes(x = year, y = value, fill=variable)) +
  geom_bar(aes(year, value, fill = as.factor(variable)), 
           position = "dodge", stat = "summary", fun.y = "mean")+
  theme_minimal()+
  scale_x_continuous(breaks = round(seq(min(zscores.m$year), max(zscores.m$year), by = 1),1))+
  labs(y = "z-scores", x = "year")
plot


#plot +  jpeg("Zscore_osc.jpeg", units="in", width=9, height=3, res=300)
#dev.off()

findInterval(zscores.m$value, c(-2, 0, 2))


### Bars indicating year ##

myvars2 <- c("year", "PDO", "ENSO", "NPGO")#choose index
#myvars2 <- c("year", "ENSO")
osc.2  <- newosc[myvars2]
osc.2$year <- as.numeric(osc.2$year)
osc.2.m <- melt(osc.2,id.vars = "year")


plot <- ggplot(osc.2.m, aes(x = year, y = value, fill=variable)) +
  geom_col(stat='identity')+
  theme_minimal()+
  scale_x_continuous(breaks = round(seq(min(zscores.m$year), max(zscores.m$year), by = 1),1))+
  labs(y = "z-scores", x = "year")
plot



ggplot(osc.2.m) + 
  geom_bar(aes(year, value, fill = as.factor(variable)), 
           position = "dodge", stat = "summary", fun.y = "mean")+
  theme_minimal()+
  scale_x_continuous(breaks = round(seq(min(zscores.m$year), max(zscores.m$year), by = 1),1))+
  labs(y = "index", x = "year")



#plot +  jpeg("Zscore_osc.jpeg", units="in", width=9, height=3, res=300)
#dev.off()

findInterval(zscores.m$value, c(-2, 0, 2))


pal <- pal.bands(coolwarm)

m <- ggplot(zscores.m, aes(year, variable, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradientn(colours=coolwarm (100), guide = "colourbar") +
  scale_x_continuous(breaks = round(seq(min(zscores.m$year), max(zscores.m$year), by = 1),1))+
  labs(y = "index", x = "year")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_fixed()
m

