
# Import data
seg <-read.csv("./Data/Cluster_Env_Segments.csv", header = TRUE, sep = ",")

#Change mean fetch from m to km
seg$fetch_km <- seg$MEAN_1*0.001


# Plot the chart
boxplot(LSAT_SST ~ Env_Clus, data = seg, xlab = "Subregion",
        ylab = "Temperature (°C)",
        col = c("#BF360C", "#FFD54F", "#1a5276", "#5499c7", "#E64A19" ))

boxplot(fetch_km ~ Env_Clus, data = seg, xlab = "Subregion",
        ylab = "Fetch (km)",
        col = c("#226541", "#244A37", "#46946D", "#8BC8BE", "#CCE5E9" ))

