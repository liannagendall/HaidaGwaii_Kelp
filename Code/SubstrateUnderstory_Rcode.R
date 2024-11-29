

setwd("C:/Users/genda/Dropbox/Kelp Data & Questions/Data/TimeSeriesAnalysis/InSitu")

library(RColorBrewer)
library(reshape2)
library(ggplot2)
library(hrbrthemes)
library(viridis)

insitu <- read.csv(file = "Insitu_data.csv")

#########################
#SUBSTRATE

group <- table(insitu$Substrate, insitu$Group)

prop <- prop.table(group, 2)

barplot(prop, ylab = "Percent of samples", col = brewer.pal(n = 6, name = "YlGnBu"), legend = rownames(prop))


propdf <- as.data.frame.matrix(prop)

propdf$Substrate <- rownames(propdf)

propl <- melt(propdf, id.vars= c("Substrate"), value.name = "proportion")

names(propl)[2] <- paste("Group")

propl$proportion <- propl$proportion*100

ggplot() + 
  geom_bar(aes(y= proportion,
                       x= Group,
                       fill = Substrate),
                    data = propl,
                   stat = "identity") +
  labs(y= "Percent of sample sites (%)") +
  theme_minimal() +
  scale_fill_brewer(palette="YlOrBr", direction=-1) +
  theme_minimal(base_size = 20)
              

#########################
#UNDERSTORY

group <- table(insitu$Understory, insitu$Group)

prop <- prop.table(group, 2)

barplot(prop, ylab = "Percent of samples", col = brewer.pal(n = 6, name = "YlGnBu"), legend = rownames(prop))


propdf <- as.data.frame.matrix(prop)

propdf$Understory <- rownames(propdf)

propl <- melt(propdf, id.vars= c("Understory"), value.name = "proportion")

names(propl)[2] <- paste("Group")

propl$proportion <- propl$proportion*100

ggplot() + 
  geom_bar(aes(y= proportion,
               x= Group,
               fill = Understory),
           data = propl,
           stat = "identity") +
  labs(y= "Percent of sample sites (%)") +
  scale_fill_brewer(palette="GnBu", direction=-1) +
  theme_minimal(base_size = 20)
  
