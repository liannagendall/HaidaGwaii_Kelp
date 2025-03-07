
library(tidyverse)
library(lubridate)
library(pracma)
library(DataCombine)
library(imputeTS)
library(fpp)
library(zoo)
library(dplyr)
library(ggplot2)
library(heatwaveR)
library(corrplot)
library(patchwork)


############Bonilla Island SST####################

#Import the data as downloaded directly from the BC lighthouses website, skipping the first few rows which have some random meta-data; the exact number of lines to skip may vary depending on dataset 
temp.bon <- read_csv( "./Data/Bonilla_Island_-_Daily_Sea_Surface_Temperature_and_Salinity_1960-2021.csv",
                      skip=0 )
#Look at data (now a tiblle)
temp.bon 

#Replace 999.9 values which indicate an error with "NA"
temp.bon$`Temperature ( C )` <-gsub("999.9","NA",temp.bon$`Temperature ( C )`)

#Optional: remove data from before a given year
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
temp.bon.RockyTime$Anomaly<-na_ma(temp.bon.RockyTime$Anomaly,k=12, weighting = "exponential")


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


######Average SST anomolies by year#######

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


#write.csv(temp.bon.byGroup, 'SST_YearlyJAAnomaly.csv')





##############Absolute temperature###############


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



#######SST anomalies growing season (march to august)######


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


#write.csv(temp.bon.byGroup, 'SST_YearlyAnomalyGrowing.csv')



#########MHW Analysis with lighthouse SST###############
#Import the data as downloaded directly from the BC lighthouses website, skipping the first few rows which have some random meta-data; the exact number of lines to skip may vary depending on dataset 
temp.bon3 <- read_csv( "./Data/Bonilla_Island_-_Daily_Sea_Surface_Temperature_and_Salinity_1960-2024.csv",
                      skip=1 )
#Look at data (now a tiblle)
temp.bon3

#Replace 999.9 values which indicate an error with "NA"
temp.bon3$`Temperature ( C )` <-gsub("999.9","NA",temp.bon3$`TEMPERATURE ( C )`)

#Filter dates to study period
temp.bon3 <- temp.bon3 %>%
  filter(`DATE (YYYY-MM-DD)` >= as.Date("1966-01-01") & `DATE (YYYY-MM-DD)` <= as.Date("2021-12-31"))

#Make temperature numeric and rename the columns
# Transform and rename the dataset
temp.bon3 <- temp.bon3 %>%
  select(`DATE (YYYY-MM-DD)`, `TEMPERATURE ( C )`) %>% # Reorder columns
  rename(
    t = `DATE (YYYY-MM-DD)`,
    temp = `TEMPERATURE ( C )`
  )

#Replace 999.9 values which indicate an error with "NA"
temp.bon3$temp <-gsub("999.9","NA",temp.bon3$temp)


#Interpolate those months
temp.bon3$temp <- as.numeric(temp.bon3$temp)
temp.bon3$temp <-na_ma(temp.bon3$temp, k=12, weighting = "exponential")
head(temp.bon3)

#remove the data with too large of a temporal gap to interpolate
temp.bon3 <- temp.bon3 %>%
  mutate(temp = ifelse(format(t, "%Y-%m") %in% c("2016-05", "2012-07"), NA, temp))

# Detect the events in a time series
ts <- ts2clm(temp.bon3, climatologyPeriod = c("1966-01-01", "2021-12-31"))
mhw <- detect_event(ts)
lolli_plot(mhw)

mhw <- detect_event(ts, categories = TRUE)
bubble_plot <- ggplot(data = mhw, aes(x = date_peak, y = intensity_max)) +
  geom_point(aes(size = intensity_cumulative, fill = intensity_max_abs), shape = 21, alpha = 0.8) +
  labs(x = NULL, y = "Maximum Intensity [°C] ", size = "Cumulative Intensity [°C x days]", fill = "Max Temperature [°C]") +
  scale_x_date(
    date_breaks = "2 years", 
    date_labels = "%Y", 
  ) + 
  scale_fill_continuous(low = "white", high = "red",
                        guide = guide_colorbar(title.position = "top", direction = "horizontal")) +
  scale_size_continuous(range = c(1, 10), 
                        guide = guide_legend(title.position = "top", direction = "horizontal")) +
  theme_minimal() +
  theme(
    legend.position = c(0.4, 0.8),
    legend.box.background = element_rect(colour = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

bubble_plot 



# Summarize by year
annualMHW_bonlight <- mhw %>%
  mutate(
    year_start = if_else(month(date_peak) >= 8, year(date_peak), year(date_peak) - 1),
    year_end = year_start + 1
  ) %>%
  group_by(year_start, year_end) %>%
  summarize(
    MHW_days = sum(duration, na.rm = TRUE),
    MHW_cum_int = sum(intensity_cumulative, na.rm = TRUE),
    MHW_max_int = max(intensity_max, na.rm = TRUE),
    MHW_max_int_abs = max(intensity_max_abs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    year_start = 1966:2021, 
    fill = list(MHW_days = 0, MHW_cum_int = 0, MHW_max_int = 0, MHW_max_int_abs = 0)
  ) %>%
  mutate(
    year_end = if_else(is.na(year_end), year_start + 1, year_end)
  )

# Bar chart
ggplot(data = annualMHW_bonlight, aes(x = year_end, y = MHW_days)) +
  geom_bar(stat = "identity", fill = "darkred", color = "black") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Sum of MHW Days"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(annualMHW_bonlight$year_end), max(annualMHW_bonlight$year_end), by = 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = annualMHW_bonlight, aes(x = year_end, y = MHW_cum_int)) +
  geom_bar(stat = "identity", fill = "darkred", color = "black") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Sum of Cumulative intensity"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(annualMHW_bonlight$year_end), max(annualMHW_bonlight$year_end), by = 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




ggplot(data = annualMHW_bonlight, aes(x = year_end, y =  MHW_days)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#CB081B", color = "black") +
  labs(
    x = "Year",
    y = "MHW Days"
  ) +
  theme_classic() +
  scale_x_continuous(
    breaks = seq(1969, 2021, by = 2), 
    limits = c(1969, 2021)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))












ggplot(data = annualMHW_bonlight, aes(x = year_end, y = MHW_max_int)) +
  geom_bar(stat = "identity", fill = "darkred", color = "black") +
  labs(
    x = "Year",
    y = "Maximum (peak) intensity [deg. C]."
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(annualMHW_bonlight$year_end), max(annualMHW_bonlight$year_end), by = 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = annualMHW_bonlight, aes(x = year_end, y = MHW_max_int_abs)) +
  geom_bar(stat = "identity", fill = "darkred", color = "black") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Maximum (peak) temperature [deg. C]."
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(annualMHW_bonlight$year_end), max(annualMHW_bonlight$year_end), by = 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###################OISST data#######################

OISSTcum <- readRDS("./Data/OISST_cumshewa.rds")
OISSTbon <- readRDS("./Data/OISST_bonilla.rds")

OISSTcum <- OISSTcum %>%
  select(t, temp) 
OISSTbon <- OISSTbon %>%
  select(t, temp) 

OISSTcum <- OISSTcum %>%
  filter(t <= as.Date("2021-12-31"))
OISSTbon <- OISSTbon %>%
  filter(t <= as.Date("2021-12-31"))


#cumshewa
# Detect the events in a time series
tscum <- ts2clm(OISSTcum, climatologyPeriod = c("1982-01-01", "2021-12-31"))
mhwcum <- detect_event(tscum)
lolli_plot(mhwcum)

mhwcum <- detect_event(tscum, categories = TRUE)
bubble_plot <- ggplot(data = mhwcum, aes(x = date_peak, y = intensity_max)) +
  geom_point(aes(size = intensity_cumulative, fill = intensity_max_abs), shape = 21, alpha = 0.8) +
  labs(x = NULL, y = "Maximum Intensity [°C] ", size = "Cumulative Intensity [°C x days]", fill = "Max Temperature [°C]") +
  scale_x_date(
    date_breaks = "2 years", 
    date_labels = "%Y", 
  ) + 
  scale_fill_continuous(low = "white", high = "red",
                        guide = guide_colorbar(title.position = "top", direction = "horizontal")) +
  scale_size_continuous(range = c(1, 10), 
                        guide = guide_legend(title.position = "top", direction = "horizontal")) +
  theme_minimal() +
  theme(
    legend.position = c(0.4, 0.8),
    legend.box.background = element_rect(colour = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

bubble_plot 



# Summarize by year
annualMHW_OISSTcum <- mhwcum %>%
  mutate(
    year_start = if_else(month(date_peak) >= 8, year(date_peak), year(date_peak) - 1),
    year_end = year_start + 1
  ) %>%
  group_by(year_start, year_end) %>%
  summarize(
    MHW_days = sum(duration, na.rm = TRUE),
    MHW_cum_int = sum(intensity_cumulative, na.rm = TRUE),
    MHW_max_int = max(intensity_max, na.rm = TRUE),
    MHW_max_int_abs = max(intensity_max_abs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    year_start = 1982:2021, 
    fill = list(MHW_days = 0, MHW_cum_int = 0, MHW_max_int = 0, MHW_max_int_abs = 0)
  ) %>%
  mutate(
    year_end = if_else(is.na(year_end), year_start + 1, year_end)
  )
# Bar chart
ggplot(data = annualMHW_OISSTcum, aes(x = year_end, y = MHW_days)) +
  geom_bar(stat = "identity", fill = "darkred", color = "black") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Sum of MHW Days"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(annualMHW_OISSTcum$year_end), max(annualMHW_OISSTcum$year_end), by = 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = annualMHW_OISSTcum, aes(x = year_end, y = MHW_days)) +
  geom_bar(stat = "identity", fill = "darkred", color = "black") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Sum of MHW Days"
  ) +
  scale_x_continuous(
    breaks = seq(1967, 2021, by = 2), 
    limits = c(1966, 2021)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(data = annualMHW_OISSTcum, aes(x = year_end, y = MHW_cum_int)) +
  geom_bar(stat = "identity", fill = "darkred", color = "black") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Sum of Cumulative intensity"
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(1967, 2021, by = 2), 
    limits = c(1966, 2021)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = annualMHW_OISSTcum, aes(x = year_end, y = MHW_max_int)) +
  geom_bar(stat = "identity", fill = "darkred", color = "black") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Maximum (peak) intensity [deg. C]."
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(annualMHW_OISSTcum$year_end), max(annualMHW_OISSTcum$year_end), by = 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = annualMHW_OISSTcum, aes(x = year_end, y = MHW_max_int_abs)) +
  geom_bar(stat = "identity", fill = "darkred", color = "black") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Maximum (peak) temperature [deg. C]."
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(annualMHW_OISSTcum$year_end), max(annualMHW_OISSTcum$year_end), by = 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#bonilla
# Detect the events in a time series
tsbon <- ts2clm(OISSTbon, climatologyPeriod = c("1982-01-01", "2021-12-31"))
mhwbon <- detect_event(tsbon)
lolli_plot(mhwbon)

mhwbon <- detect_event(tsbon, categories = TRUE)
bubble_plot <- ggplot(data = mhwbon, aes(x = date_peak, y = intensity_max)) +
  geom_point(aes(size = intensity_cumulative, fill = intensity_max_abs), shape = 21, alpha = 0.8) +
  labs(x = NULL, y = "Maximum Intensity [°C] ", size = "Cumulative Intensity [°C x days]", fill = "Max Temperature [°C]") +
  scale_x_date(
    date_breaks = "2 years", 
    date_labels = "%Y", 
  ) + 
  scale_fill_continuous(low = "white", high = "red",
                        guide = guide_colorbar(title.position = "top", direction = "horizontal")) +
  scale_size_continuous(range = c(1, 10), 
                        guide = guide_legend(title.position = "top", direction = "horizontal")) +
  theme_minimal() +
  theme(
    legend.position = c(0.4, 0.8),
    legend.box.background = element_rect(colour = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

bubble_plot 


#summarize yearly data between July of sampling year and August of the previous year. 
annualMHW_OISSTbon <- mhwbon %>%
  mutate(
    year_start = if_else(month(date_peak) >= 8, year(date_peak), year(date_peak) - 1),
    year_end = year_start + 1
  ) %>%
  group_by(year_start, year_end) %>%
  summarize(
    MHW_days = sum(duration, na.rm = TRUE),
    MHW_cum_int = sum(intensity_cumulative, na.rm = TRUE),
    MHW_max_int = max(intensity_max, na.rm = TRUE),
    MHW_max_int_abs = max(intensity_max_abs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    year_start = 1982:2021, 
    fill = list(MHW_days = 0, MHW_cum_int = 0, MHW_max_int = 0, MHW_max_int_abs = 0)
  ) %>%
  mutate(
    year_end = if_else(is.na(year_end), year_start + 1, year_end)
  )

# Bar chart
ggplot(data = annualMHW_OISSTbon, aes(x = year_end, y = MHW_days)) +
  geom_bar(stat = "identity", fill = "darkred", color = "black") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "MHW Days"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(annualMHW_OISSTbon$year_end), max(annualMHW_OISSTbon$year_end), by = 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = annualMHW_OISSTbon, aes(x = year_end, y = MHW_cum_int)) +
  geom_bar(stat = "identity", fill = "darkred", color = "black") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Sum of cumulative intensity [deg. C x days]"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(annualMHW_OISSTbon$year_end), max(annualMHW_OISSTbon$year_end), by = 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = annualMHW_OISSTbon, aes(x = year_end, y = MHW_max_int)) +
  geom_bar(stat = "identity", fill = "darkred", color = "black") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Maximum (peak) intensity [deg. C]."
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(annualMHW_OISSTbon$year_end), max(annualMHW_OISSTbon$year_end), by = 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = annualMHW_OISSTbon, aes(x = year_end, y = MHW_max_int_abs)) +
  geom_bar(stat = "identity", fill = "darkred", color = "black") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Maximum (peak) temperature [deg. C]."
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(annualMHW_OISSTbon$year_end), max(annualMHW_OISSTbon$year_end), by = 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#######Comparison between SST from bonilla lighthouse, OISST bonilla, and OISST cumshewa#########

# Add prefixes to column names (excluding 'year')
annualMHW_bonlight_p <- annualMHW_bonlight %>% rename_with(~ paste0("l_", .), -year_end, -year_start)

annualMHW_OISSTbon_p <- annualMHW_OISSTbon %>% rename_with(~ paste0("b_", .), -year_end, -year_start)
# annualMHW_OISSTbon_p <- annualMHW_OISSTbon_p %>%
#   filter(year >= 1982)
annualMHW_OISSTcum_p <- annualMHW_OISSTcum %>% rename_with(~ paste0("c_", .), -year_end, -year_start)
# annualMHW_OISSTcum_p <- annualMHW_OISSTcum_p %>%
#   filter(year >= 1982)

# Combine the datasets by year
combined <- annualMHW_bonlight_p %>%
  full_join(annualMHW_OISSTbon_p, by = "year_end") %>%
  full_join(annualMHW_OISSTcum_p, by = "year_end")

head(combined)

# Remove 'year' column
data <- combined %>% select(-year_end)

# Extract suffixes
suffixes <- gsub("^[a-z]_", "", colnames(data))

# Find matching columns by suffix
matching_columns <- split(colnames(data), suffixes)

# Compute correlations for each matching group
correlations <- lapply(matching_columns, function(cols) {
  # Subset the data to matching columns
  subset_data <- data %>% select(all_of(cols))
  
  # Compute correlation matrix
  cor_matrix <- cor(subset_data, use = "pairwise.complete.obs")
  return(cor_matrix)
})

# Combine results into a single output (e.g., a list or summary)
print(correlations)




# Compute correlations with significance testing for each matching group
correlations <- lapply(matching_columns, function(cols) {
  # Subset the data to matching columns
  subset_data <- data %>% select(all_of(cols))
  
  # Get pairwise combinations of columns
  col_pairs <- combn(cols, 2, simplify = FALSE)
  
  # Compute correlations and significance
  cor_results <- lapply(col_pairs, function(pair) {
    test <- cor.test(subset_data[[pair[1]]], subset_data[[pair[2]]], use = "pairwise.complete.obs")
    list(
      variables = paste(pair, collapse = " - "),
      correlation = test$estimate,
      p.value = test$p.value,
      df = test$parameter # Degrees of freedom
    )
  })
  
  return(cor_results)
})

# Combine results into a more readable format (e.g., data frame)
correlation_summary <- do.call(rbind, lapply(correlations, function(group) {
  do.call(rbind, lapply(group, as.data.frame))
}))

# Print results
print(correlation_summary)






# Create a function to generate scatter plots with a 1:1 line and correlation text
scatter_plot <- function(x, y, x_label, y_label) {
  # Calculate correlation
  cor_value <- cor(data[[x]], data[[y]], use = "pairwise.complete.obs")
  
  # Generate scatter plot
  ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(color = "blue", alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    annotate("text", x = max(data[[x]], na.rm = TRUE) * 0.7,
             y = max(data[[y]], na.rm = TRUE) * 1.05,
             label = paste0("r = ", round(cor_value, 2)),
             hjust = 0, size = 4) +
    labs(x = x_label, y = y_label) +
    theme_minimal()
}

# Variables for MHW_cum_int
vars_cum_int <- c("l_MHW_cum_int", "b_MHW_cum_int", "c_MHW_cum_int")

# Generate scatter plots for MHW_cum_int
plots_cum_int <- list()
for (i in seq_along(vars_cum_int)) {
  for (j in seq_along(vars_cum_int)) {
    if (i < j) {
      plots_cum_int[[length(plots_cum_int) + 1]] <- scatter_plot(
        vars_cum_int[i], vars_cum_int[j], vars_cum_int[i], vars_cum_int[j]
      )
    }
  }
}

# Variables for MHW_days
vars_days <- c("l_MHW_days", "b_MHW_days", "c_MHW_days")

# Generate scatter plots for MHW_days
plots_days <- list()
for (i in seq_along(vars_days)) {
  for (j in seq_along(vars_days)) {
    if (i < j) {
      plots_days[[length(plots_days) + 1]] <- scatter_plot(
        vars_days[i], vars_days[j], vars_days[i], vars_days[j]
      )
    }
  }
}

# Combine the plots using patchwork
plot_combined <- (wrap_plots(plots_cum_int) / wrap_plots(plots_days)) +
  plot_annotation(title = "Scatter Plots with 1:1 Line and Correlation",
                  subtitle = "Top: MHW_cum_int, Bottom: MHW_days")

# Display the plot
print(plot_combined)

write.csv(combined, "./Data/MHW_data.csv")
