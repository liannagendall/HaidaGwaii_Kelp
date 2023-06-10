
library(dplyr)

##########################################################
#PLotting indices

ci<-read.csv("./Data/Climate_Indices_Yearly.csv", header = TRUE, sep = ",")


#Assign colour to temperature anomaly direction
for (i in 1:length(ci$ENSO_G)) {
  if (ci$ENSO_G[i] > 0) {
    ci$ENG.col[i]<-"#B51D2C"
  } else {
    ci$ENG.col[i]<-"#296BA8"
  }
}

ci <- ci[c(8:57),]


##Plot ENSO through time
par(mar=c(4,4,2,1))
par(mfrow=c(1,1))
barplot(ci$ENSO_G, col=ci$ENG.col, ylim=c(-2,2), cex.axis=1.75, space=0.5, border=TRUE, las=1, width=.66, xlim=c(0,50))
title(ylab=expression(paste("Oceanic El Nino Index")), line=2, cex.lab=2)
axis(1, at=seq(0,49,1), labels = seq(1972,2021,1), srt=40, cex.axis=1.75)
title(xlab ="Year", line=3, cex.lab=2)
lines(movavg(ci$ENSO_G, 4, type="s"),lwd=2,lty=1)

##Plot PDO through time

for (i in 1:length(ci$ENSO_G)) {
  if (ci$PDO_G[i] > 0) {
    ci$PDO.col[i]<-"#B51D2C"
  } else {
    ci$PDO.col[i]<-"#296BA8"
  }
}

par(mar=c(4,4,2,1))
par(mfrow=c(1,1))
barplot(ci$PDO_G, col=ci$PDO.col, ylim=c(-3,3), cex.axis=1.75, space=0.5, border=TRUE, las=1, width=.66, xlim=c(0,50))
title(ylab=expression(paste("Pacific Decadal Oscillation Index")), line=2, cex.lab=2)
axis(1, at=seq(0,49,1), labels = seq(1972,2021,1), srt=40, cex.axis=1.75)
title(xlab ="Year", line=3, cex.lab=2)
lines(movavg(ci$PDO_G, 4, type="s"),lwd=2,lty=1)

##Plot NPGO through time
for (i in 1:length(ci$ENSO_G)) {
  if (ci$NPGO_G[i] > 0) {
    ci$NPGO.col[i]<-"#B51D2C"
  } else {
    ci$NPGO.col[i]<-"#296BA8"
  }
}

par(mar=c(4,4,2,1))
par(mfrow=c(1,1))
barplot(ci$NPGO_G, col=ci$NPGO.col, ylim=c(-3,3), cex.axis=1.75, space=0.5, border=TRUE, las=1, width=.66, xlim=c(0,50))
title(ylab=expression(paste("North Pacific Gyre Oscillation Index")), line=2, cex.lab=2)
axis(1, at=seq(0,49,1), labels = seq(1972,2021,1), srt=40, cex.axis=1.75)
title(xlab ="Year", line=3, cex.lab=2)
lines(movavg(ci$NPGO_G, 4, type="s"),lwd=2,lty=1)


