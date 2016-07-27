library(maptools)
library(gdata)
library(classInt)
library(maps)
library(ggmap)
library(raster)  
library(rgdal)  
library(rgeos) 
library(spdep)
library(magrittr)
library(dplyr)
library(GISTools)
library(ggplot2)
library(gstat)
library(sp)

#read in street intersection node data
streets = readShapePoints("data/OSM/streets/street_intersections.shp")
proj4string(streets) = CRS("+init=epsg:4326")
projitaly <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m")
streets %<>% spTransform(.,projitaly)

#plot kernel density function
#breach.dens = kde.points(ameni,lims=census)
#level.plot(pt)


#generate grid to interpolate into from streets int
x.range <- as.integer(range(streets@coords[,1]))
y.range <- as.integer(range(streets@coords[,2]))
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=100), y=seq(from=y.range[1], to=y.range[2], by=100))
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE
#project grid
proj4string(grd) <- projitaly

#plot grid with streets 
plot(grd, cex=1.5)
points(streets, pch=1, col='red', cex=1)
title("interpolation streets + grid")

#inverse distance weighting for interpolation w/ betweenness 
idw<-idw(formula=streets$closeness ~ 1, locations=streets, newdata=grd)
idw.output=as.data.frame(idw)
names(idw.output)[1:3]<-c("long","lat","var1.pred")
#glimpse(idw.output)


#plot raster of interpolated network betweenness
plot<-ggplot(data=idw.output,aes(x=long,y=lat))
layer1<-c(geom_tile(data=idw.output,aes(fill=var1.pred)))
plot+layer1+scale_fill_gradient(low="#FFFFCC", high="#000044")+coord_equal()
