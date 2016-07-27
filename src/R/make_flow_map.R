library(maps)
library(geosphere)
library(readr)
library(dplyr)
library(magrittr)
library(lubridate)
library(rgdal)
library(raster)
library(rgeos)


setwd("/Users/myeong/git/DSSG/DSSG2016-SensingTheCensus/")
census = readOGR("data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") 
#%>% spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))

trueCentroids = gCentroid(census,byid=TRUE, id = as.vector(census@data$ACE))
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
trueCentroids <- spTransform(trueCentroids, CRS(llprj))


cdr_network = read_delim("data/CDR/generated/region_network_final.csv", delim = ",",col_names = TRUE ) 

cdr_network$source <- as.factor(cdr_network$source)
cdr_network$dest <- as.factor(cdr_network$dest)

census_network =  cdr_network %>% left_join(census@data, by = c("source"="ACE")) %>%
  left_join(census@data, by = c("dest"="ACE")) %>% 
  dplyr::group_by(source, dest) %>% 
  summarize(call = sum(call, na.rm=TRUE))

llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
trans <- spTransform(trueCentroids, CRS(llprj))
census_network$source.x <- trans[census_network$source]$x
census_network$source.y <- trans[census_network$source]$y
census_network$dest.x <- trans[census_network$dest]$x
census_network$dest.y <- trans[census_network$dest]$y


# Drawing the map
par(mar=c(1,1,1,1)+0.1)
plot(census, col="#f2f2f2", bg="white",lwd=0.1)
points(trueCentroids,pch=1)

pal <- colorRampPalette(c("#FFF5F3", "red"))
colors <- pal(100)
maxcnt <- max(census_network$cnt)

for (i in 1:length(census_network$call)){  
  if (census_network$call < 200000){
    next
  }
  inter <- gcIntermediate(c(census_network$source.x[i], census_network$source.y[i]), 
                          c(census_network$dest.x[i], census_network$dest.y[i]), n=100, addStartEnd=TRUE)
  colindex <- round( (census_network[i,]$call / 10000) * length(colors) )  
  lines(inter, col=colors[colindex], lwd=0.2)
}


