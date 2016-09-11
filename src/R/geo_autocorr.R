library(readr)
library(maptools)
library(caret)
library(dplyr)
library(magrittr)
library(spdep)
library(ggthemes)
library(ggplot2)
library(rgdal)
library(ncf)
library(maps)
library(geosphere)
library(rgeos)
library(fields)

setwd("/Users/myeong/git/DSSG/DSSG2016-SensingTheCensus/")

# From-Scratch implementation

census = readOGR("data/GeoJSON/milano_census_sez.geojson", "OGRGeoJSON") 
trueCentroids = gCentroid(census,byid=TRUE, id = as.vector(census@data$SEZ2011))
census@data$deprivation[is.na(census@data$deprivation)] <- 0

popdists <- as.matrix(rdist.earth(cbind(trueCentroids$x, trueCentroids$y), miles = F, R = NULL))

aa <- ceiling(max(popdists)/0.5)
dists <- seq(0,aa*0.5,0.5)
size <- length(dists) 


# w: distance-based weights matrix
# x: the variable of intererst 
autocorr <- function(w,x,dist=1){  
  aa <- ceiling(max(w)/dist)
  dists <- seq(0,aa*dist,dist)
  cors <- NULL
  for(i in 1:aa){
    print(i)
    w1 <- ifelse(w > dists[i] & w <= dists[i+1], 1, 0) 
    w2 <- w1
    
    sums <- rowSums(w1)
    sums[sums==0] <- 1
    w2 <- w1/sums
    
    lag <- w2 %*% x
    cors <- c(cors,cor(x,lag))    
  }
  return(cors)
}

print("Milano SEZ")
ac2 <- autocorr(w=popdists,x=census@data$deprivation,dist=0.5)

it <- 50
mc <- matrix(NA,nrow=it,ncol=length(ac2))
for(i in 1:it){
  print(i)
#   census@data$rand <- sample(census@data$deprivation,length(census@data$deprivation),replace=F)
  rand1 <- rnorm (length(census@data$deprivation), mean(census@data$deprivation), sd(census@data$deprivation))
  temp <- autocorr(w=popdists,x=rand1,dist=0.5)
  mc[i,] <- temp
}

ac2 <- data.frame(cbind(ac2,seq(0, size/2 - 1, 0.5)))
ac2 <- cbind(ac2,t(apply(mc,2,quantile, probs = c(0.025,0.975))))
names(ac2) <- c("ac","dist","lci","uci")
 
ggplot(ac2, aes(dist, ac)) +
  geom_point(colour = "darkblue", size = 3) +
  geom_line(colour = "red") +
  scale_x_continuous('Great Circle Distance (KM)',limits=c(0,15)) + 
  scale_y_continuous('Autocorrelation',limits=c(-0.4,0.7)) +
  theme_bw() + 
  geom_hline(yintercept=0) +   
  geom_smooth(aes(ymin = lci, ymax = uci), stat="identity",fill="blue")

write_csv(ac2, "data/baseline/milano_sez_moran.csv")
write_csv(as.data.frame(mc), "data/baseline/milano_sez_random.csv")




#Mexico Version2 -- using scripts from scratch
library(fields)
census = readOGR("data/census/mexico_city/mexico_city_census.shp", layer="mexico_city_census")
# census = readOGR("data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") 
proj4string(census) = CRS("+proj=utm +zone=32 +datum=WGS84")

trueCentroids = gCentroid(census,byid=TRUE, id = as.vector(census@data$SP_ID))
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
trueCentroids <- spTransform(trueCentroids, CRS(llprj))

popdists <- as.matrix(rdist.earth(cbind(trueCentroids$x, trueCentroids$y), miles = F, R = NULL))

distance <- 2
aa <- ceiling(max(popdists)/distance)
dists <- seq(0,aa*distance,distance)
size <- length(dists) 

ac1 <- autocorr(w=popdists,x=census@data$IMU,dist=distance)


print("Mexico City")
it <- 50
mc <- matrix(NA,nrow=it,ncol=nrow(ac1))
for(i in 1:it){
  print(i)
#   census@data$rand <- sample(census@data$IMU,length(census@data$IMU),replace=F)
  rand1 <- rnorm (length(census@data$IMU), mean(census@data$IMU), sd(census@data$IMU))
  temp <- autocorr(w=popdists,x=rand1,dist=distance)
  mc[i,] <- temp
}

ac1 <- data.frame(cbind(ac1,seq(0, (size-1) * distance - 1, distance)))
ac1 <- cbind(ac1,t(apply(mc,2,quantile, probs = c(0.025,0.975))))
names(ac1) <- c("ac","dist","lci","uci")
 
ggplot(ac1, aes(dist, ac)) +
  geom_point(colour = "darkblue", size = 3) +
  geom_line(colour = "red") +
  scale_x_continuous('Great Circle Distance (KM)',limits=c(0,60)) + 
  scale_y_continuous('Autocorrelation',limits=c(-0.4,0.7)) +
  theme_bw() + 
  geom_hline(yintercept=0) +   
  geom_smooth(aes(ymin = lci, ymax = uci), stat="identity",fill="blue")


write_csv(as.data.frame(mc), "data/baseline/mexico_random.csv")
write_csv(ac1, "data/baseline/mexico_moran.csv")


```