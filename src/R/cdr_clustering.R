library(maps)
library(geosphere)
library(readr)
library(dplyr)
library(magrittr)
library(lubridate)
library(rgdal)
library(raster)
library(rgeos)
require(ggplot2)
library(e1071)
library(cwhmisc)
library(utils)
library(rpart)
library(randomForest)

setwd("/Users/myeong/git/DSSG/DSSG2016-SensingTheCensus/")
census = readOGR("data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") 
#%>% spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))

trueCentroids = gCentroid(census,byid=TRUE, id = as.vector(census@data$ACE))
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
trueCentroids <- spTransform(trueCentroids, CRS(llprj))


# clusters = read_delim("data/CDR/generated/time-cluster.csv", delim = ",",col_names = TRUE ) 
clusters = read_delim("src/notebooks/intermediate2.csv", delim = ",",col_names = TRUE ) 


clusters$region <- as.factor(clusters$region)
with_cluster <- census
with_cluster@data <-  census@data %>% left_join(clusters, by = c("ACE"="region")) 


# Drawing the map
par(mar=c(1,1,1,1)+0.1)

pal <- colorRampPalette(c("red", "#FFF5F3"))
pall <- rainbow(4, s = 1, v = 1, start = 1, end = 1, alpha = 1)
pal <- c("blue", "green", "red")
colors <- pal(4)

colindex <- round( (with_cluster@data$callout) )
plot(with_cluster, col=pal[colindex], bg="white",lwd=0.2)
text(trueCentroids@coords[,"x"],trueCentroids@coords[,"y"],row.names(trueCentroids@coords),cex=1,adj=0,col="#000000")

## ANOVA with Advantage Offerings from OSM
offerings = read_delim("data/OSM/offering_advantage.csv", delim = ",",col_names = TRUE )
offerings$ACE = as.factor(offerings$ACE)
offerings <-  offerings %>% left_join(clusters, by = c("ACE"="region"))

offerings$callin <- as.factor(offerings$callin)
offerings$callout <- as.factor(offerings$callin)
offerings$smsin <- as.factor(offerings$callin)
offerings$smsout <- as.factor(offerings$callin)
offerings$internet <- as.factor(offerings$callin)

#bars
ggplot(offerings, aes(x = callin, y = bar)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Call-In Clusters") +
  ylab("Bars")

callin_anova <-  lm(bar ~ callin, data = offerings)
summary(callin_anova)

#Cinema
ggplot(offerings, aes(x = callin, y = cinema)) + geom_boxplot(fill = "grey80", colour = "blue") + scale_x_discrete() + xlab("Call-In Clusters") + ylab("Cinema")
callin_anova <-  lm(cinema ~ callin, data = offerings)
summary(callin_anova)

#Fast Food
ggplot(offerings, aes(x = callin, y = fast_food)) + geom_boxplot(fill = "grey80", colour = "blue") + scale_x_discrete() + xlab("Call-In Clusters") + ylab("Fast Food")
callin_anova <-  lm(fast_food ~ callin, data = offerings)
summary(callin_anova)


## Fitting Street Network Features based on distances to clusters
distances = read_delim("data/CDR/generated/adjusted_internet_dist.csv", delim = ",",col_names = TRUE ) 
street_centrality = read_delim("data/CDR/hash/centrality_ace.csv", delim = ",",col_names = TRUE ) 
colnames(distances) <- c("region", "c1", "c2", "c3")
distances$region <- as.factor(distances$region)
street_centrality$ACE <- as.factor(street_centrality$ACE)
distances <-  distances %>% left_join(street_centrality, by = c("region"="ACE"))

internet_dist <-  lm(log(closeness) ~ c1+c2+c3, data = distances)
summary(internet_dist)

internet_dist <-  lm(log(betweenness) ~ c1+c2+c3, data = distances)
summary(internet_dist)

offerings <-  offerings %>% left_join(distances, by = c("ACE" = "region"))

internet_dist <-  lm( bar~ c1+c2+c3, data = offerings)
summary(internet_dist)

internet_dist <-  lm( bank~ c1+c2+c3, data = offerings)
summary(internet_dist)
