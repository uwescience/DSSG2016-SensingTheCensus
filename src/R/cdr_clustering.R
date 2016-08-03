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


clusters = read_delim("data/CDR/generated/time-cluster.csv", delim = ",",col_names = TRUE ) 

clusters$region <- as.factor(clusters$region)
with_cluster <- census
with_cluster@data <-  census@data %>% left_join(clusters, by = c("ACE"="region")) 


# Drawing the map
par(mar=c(1,1,1,1)+0.1)

pal <- colorRampPalette(c("#FFF5F3", "red"))
pall <- rainbow(7, s = 1, v = 1, start = 0, end = 1, alpha = 1)
colors <- pal(7)

colindex <- round( (with_cluster@data$cluster ) )
plot(with_cluster, col=pall[colindex], bg="white",lwd=0.2)
text(trueCentroids@coords[,"x"],trueCentroids@coords[,"y"],row.names(trueCentroids@coords),cex=1,adj=0,col="#000000")

# make this as GeoJSON?
