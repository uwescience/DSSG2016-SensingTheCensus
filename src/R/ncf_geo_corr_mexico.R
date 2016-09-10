library(readr)
library(maptools)
library(caret)
library(dplyr)
library(magrittr)
library(rgdal)
library(ncf)
library(maps)
library(geosphere)
library(rgeos)

setwd("/Users/myeong/git/DSSG/DSSG2016-SensingTheCensus/")

census = readOGR("data/census/mexico_city/mexico_city_census.shp", layer="mexico_city_census")
# census = readOGR("data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") 
proj4string(census) = CRS("+proj=utm +zone=32 +datum=WGS84")

trueCentroids = gCentroid(census,byid=TRUE, id = as.vector(census@data$SP_ID))
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
trueCentroids <- spTransform(trueCentroids, CRS(llprj))

ncf.cor <- correlog(trueCentroids$x, trueCentroids$y, census@data$IMU, increment=2, latlon=TRUE, resamp=1)
# plot(ncf.cor, xlab="Distance (km)")

# making a random variation graph

it <- 1
mc <- matrix(NA,nrow=it,ncol=length(ncf.cor$mean.of.class))
for(i in 1:it){
  print(i)
  #   census@data$rand <- sample(census@data$IMU,length(census@data$IMU),replace=F)  
  rand1 <- rnorm (length(census@data$IMU), mean(census@data$IMU), sd(census@data$IMU))
  clog <- correlog(trueCentroids$x, trueCentroids$y, rand1, increment=2, latlon=TRUE, resamp=1)
  mc[i,] <- clog$correlation 
}

ac1 <- ncf.cor$correlation
ac1 <- data.frame(cbind(ac1,ncf.cor$mean.of.class))
ac1 <- cbind(ac1,t(apply(mc,2,quantile, probs = c(0.005,0.995))))
names(ac1) <- c("ac","dist","lci","uci")

ggplot(ac1, aes(dist, ac)) +
  geom_point(colour = "darkblue", size = 3) +
  geom_line(colour = "red") +
  scale_x_continuous('Great Circle Distance (KM)',limits=c(0,60)) + 
  scale_y_continuous('Autocorrelation',limits=c(-0.4,0.7)) +
  theme_bw() + 
  geom_hline(yintercept=0) +   
  geom_smooth(aes(ymin = lci, ymax = uci), stat="identity",fill="blue",colour="darkblue")



write_csv(as.data.frame(mc), "data/baseline/mexico_random.csv")
write_csv(ac1, "data/baseline/mexico_moran.csv")

