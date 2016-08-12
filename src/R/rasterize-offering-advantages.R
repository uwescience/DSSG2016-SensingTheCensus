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
library(GGally)


source("src/R/utils.R")
#' Read in street intersection node data
census = readShapePoly("data/census/mexico_city/mexico_city_census.shp")
offering_advantages = read_csv("data/OSM/mexico_city/mexico_amenity_pca.csv") %>% 
  dplyr::select(CVE_GEOAGE, arts_centre:waste)
# 
projmexico <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")
proj4string(census) = projmexico

#' Plot kernel density function
#breach.dens = kde.points(ameni,lims=census)
#level.plot(pt)

centroids =  gCentroid(census, byid =TRUE)
centroids = SpatialPointsDataFrame(centroids, census@data%>% dplyr::select(CVE_GEOAGE))
centroids@data %<>% left_join(offering_advantages, by="CVE_GEOAGE")
centroids@data[is.na(centroids@data)] = 0


#' Generate grid to interpolate into from streets int
x.range <- as.integer(range(census@bbox[1,]))
y.range <- as.integer(range(census@bbox[2,]))
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=100), y=seq(from=y.range[1], to=y.range[2], by=100))
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE
#project grid
proj4string(grd) <- projmexico

#' Inverse distance weighting for interpolation w/ betweenness 
for(name in names(dplyr::select(centroids@data, arts_centre:waste))){
  
  print(paste("Processing:", name))
  
  filtered_centroids = subset(centroids, unlist(centroids@data[name]) > 0)
  if(dim(filtered_centroids@data)[1] > 0){
    interpolation = idw(formula=unlist(filtered_centroids@data[name]) ~ 1, locations=filtered_centroids, newdata=grd)
    
    intersection = over(census, interpolation, fn = max)
    # intersection_bet = over(census, idw_bet, fn = median)
    
    
    census@data[paste("idw_", name, sep = "")] = intersection$var1.pred
    # census@data["betweenness"] = intersection_bet[,"betweenness"]    
  } 


}

census@data %<>% mutate_each(funs(ifelse(is.na(.), mean(., na.rm=TRUE),.)), idw_arts_centre:idw_waste)

census@data %>% dplyr::select(CVE_GEOAGE,idw_arts_centre:idw_waste) %>% write_csv("data/OSM/mexico_city/amenities_offering_advantage_idw.csv")
