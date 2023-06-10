
library(tidyverse)
library(lubridate)
library(pracma)
library(DataCombine)
library(imputeTS)
library(fpp)
library(zoo)

#################################################
############Bonilla Island SST####################
###############################################
#set directory file to where the lighthouse data is by either using the 'Session' drop down menu or using the setwd(dir) function see https://stat.ethz.ch/R-manual/R-devel/library/base/html/getwd.html for more info
setwd("C:/Users/genda/Dropbox/Kelp Data & Questions/Data/SST Lighthouses/DATA_-_Active_Sites2022/DATA_-_Active_Sites/Bonilla_Point")

#Import the data as downloaded directly from the BC lighthouses website, skipping the first few rows which have some random meta-data; the exact number of lines to skip may vary depending on dataset 
temp.bon <- read_csv( "Bonilla_Island_-_Daily_Sea_Surface_Temperature_and_Salinity_1960-2021.csv",
                      skip=0 )
#Look at data (now a tiblle)
temp.bon 

#Replace 999.9 values which indicate an error with "NA"
temp.bon$`Temperature ( C )` <-gsub("999.9","NA",temp.bon$`Temperature ( C )`)

#Optional: remove data from before a given year, say 1985
temp.bon2<-temp.bon[temp.bon$Year>"1965",]

#Make temperature numeric and rename the columns
colnames(temp.bon2)<-c("Year","Month","Day","Salinity" , "Temperature")

temp.bon2$Temperature<-as.numeric(temp.bon2$Temperature)


###Calculate monthly average temperature
temp.bon.sum<-temp.bon2[temp.bon2$Temperature!="NA",] %>%
  group_by(Year, Month) %>%
  summarize(Temperature = mean(Temperature))

temp.bon.byMonth <- temp.bon.sum %>%
  group_by(Month) %>%
  summarize(avgTemperature = mean(Temperature))

temp.bon.byMonth<-temp.bon.byMonth[1:12,]
temp.bon.byMonth

##Calculate Anomalies
temp.bon.comb<-left_join(temp.bon.sum, temp.bon.byMonth, by="Month")
temp.bon.comb$Anomaly <- temp.bon.comb$Temperature - temp.bon.comb$avgTemperature
temp.bon.comb<-temp.bon.comb[complete.cases(temp.bon.comb$Anomaly),]
temp.bon.RockyTime<-temp.bon.comb[temp.bon.comb$Year>1966,]

#Add in months that have no data 
temp.bon.RockyTime<-InsertRow(temp.bon.RockyTime, NewRow= list(2016,5,NA,	9.55, NA), RowNum=592)
temp.bon.RockyTime<-InsertRow(temp.bon.RockyTime, NewRow= list(2012,7,NA,	12.36, NA), RowNum=547)

#Interpolate those months
temp.bon.RockyTime$Anomaly<-na.ma(temp.bon.RockyTime$Anomaly,k=12, weighting = "exponential")


#Assign colour to temperature anomaly direction
for (i in 1:length(temp.bon.RockyTime$Anomaly)) {
  if (temp.bon.RockyTime$Anomaly[i] > 0) {
    temp.bon.RockyTime$An.col[i]<-"#B51D2C"
  } else {
    temp.bon.RockyTime$An.col[i]<-"#296BA8"
  }
}

temp.bon.RockyTime$Date<-as.yearmon(paste(temp.bon.RockyTime$Year, temp.bon.RockyTime$Month), "%Y %m")
temp.bon.RockyTime$Date<-as.Date(temp.bon.RockyTime$Date)

##Plot SST through time
par(mar=c(4,4,2,1))
par(mfrow=c(1,1))
barplot(temp.bon.RockyTime$Anomaly, col=temp.bon.RockyTime$An.col, ylim=c(-3,3), cex.axis=1.75, space=0, border=TRUE, las=1, width=1, xlim=c(0,12*55))
title(ylab=expression(paste("Temperature (", degree, "C)")), line=2, cex.lab=2)
axis(1, at=seq(0,12*55,12), labels = seq(1967,2022,1), cex.axis=0.6, las = 2)
title(xlab ="Year", line=3, cex.lab=2)
lines(movavg(temp.bon.RockyTime$Anomaly, 4, type="s"),lwd=2,lty=1)
box()


########################################
#Average SST anomolies by year

temp.bon.RockyTime <- temp.bon.RockyTime[c(8:655),]


temp.bon.RockyTime$Group <- rep(1:54, each = 12)


temp.bon.byGroup <- temp.bon.RockyTime %>%
  group_by(Group) %>%
  summarize(avgAnomaly = mean(Anomaly))

temp.bon.byGroup $Year_end <- c(1968:2021)

for (i in 1:length(temp.bon.byGroup$avgAnomaly)) {
  if (temp.bon.byGroup$avgAnomaly[i] > 0) {
    temp.bon.byGroup$col[i]<-"#B51D2C"
  } else {
    temp.bon.byGroup$col[i]<-"#296BA8"
  }
}


##Plot SST anomalies by year through time
par(mar=c(4,4,1,1))
par(mfrow=c(1,1))
barplot(temp.bon.byGroup$avgAnomaly, col=temp.bon.byGroup$col, ylim=c(-1.5,1.5), cex.axis=1.5, space=0.5, border=TRUE, las=1, width=.66, xlim=c(0,54))
title(ylab=expression(paste("SST Anomalies")), line=4, cex.lab=2)
axis(1, at=seq(0,54,1), labels = seq(1968,2022,1),  cex.axis=0.6, las = 2)
title(xlab ="Year", line=3, cex.lab=2)
#lines(movavg(temp.bon.byGroup$avgAnomaly, 4, type="s"),lwd=2,lty=1)


write.csv(temp.bon.byGroup, 'SST_YearlyJAAnomaly.csv')


########################
#SST anomalies  growing season (march to august)
##########################

temp.bon.RockyTime <- read_csv( "SST_YearlyAnomaly_G.csv",
                                skip=0 )

temp.bon.RockyTime$Group <- rep(1:55, each = 6)


temp.bon.byGroup <- temp.bon.RockyTime %>%
  group_by(Group) %>%
  summarize(avgAnomaly = mean(Anomaly))

temp.bon.byGroup $Year_end <- c(1967:2021)

for (i in 1:length(temp.bon.byGroup$avgAnomaly)) {
  if (temp.bon.byGroup$avgAnomaly[i] > 0) {
    temp.bon.byGroup$col[i]<-"#B51D2C"
  } else {
    temp.bon.byGroup$col[i]<-"#296BA8"
  }
}


##Plot SST anomalies by year through time
par(mar=c(4,4,1,1))
par(mfrow=c(1,1))
barplot(temp.bon.byGroup$avgAnomaly, col=temp.bon.byGroup$col, ylim=c(-1.5,1.5), cex.axis=1.5, space=0.5, border=TRUE, las=1, width=.66, xlim=c(0,55))
title(ylab=expression(paste("SST Anomalies")), line=4, cex.lab=2)
axis(1, at=seq(0,55,1), labels = seq(1967,2022,1),  cex.axis=0.6, las = 2)
title(xlab ="Year", line=3, cex.lab=2)
#lines(movavg(temp.bon.byGroup$avgAnomaly, 4, type="s"),lwd=2,lty=1)


write.csv(temp.bon.byGroup, 'SST_YearlyAnomalyGrowing.csv')



###########################################
#aboslute temp

temp.bon.RockyTime <-  na.omit(temp.bon.RockyTime)


temp.bon.byG <- temp.bon.RockyTime %>%
  group_by(Group) %>%
  summarize(Temperature = mean(Temperature))

temp.bon.byG $Year_end <- c(1967:2021)


write.csv(temp.bon.byG, 'SST_YearlySSTabsoluteGrowing.csv')


par(mar=c(4,4,1,1))
par(mfrow=c(1,1))
barplot(temp.bon.byG$Temperature, col=temp.bon.byGroup$col, ylim=c(0,12), cex.axis=1.5, space=0.5, border=TRUE, las=1, width=.66, xlim=c(0,55))
title(ylab=expression(paste("SST Anomalies")), line=4, cex.lab=2)
axis(1, at=seq(0,55,1), labels = seq(1967,2022,1),  cex.axis=0.6, las = 2)
title(xlab ="Year", line=3, cex.lab=2)

