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

source("src/R/utils.R")
#' Read in street intersection node data
streets = readShapePoints("data/OSM/streets/street_intersections.shp")
proj4string(streets) = CRS("+init=epsg:4326")
projitaly <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m")
streets %<>% spTransform(.,projitaly)

#' Plot kernel density function
#breach.dens = kde.points(ameni,lims=census)
#level.plot(pt)


#' Generate grid to interpolate into from streets int
x.range <- as.integer(range(streets@coords[,1]))
y.range <- as.integer(range(streets@coords[,2]))
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=50), y=seq(from=y.range[1], to=y.range[2], by=50))
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE
#project grid
proj4string(grd) <- projitaly


#' Inverse distance weighting for interpolation w/ betweenness 
idw_close = idw(formula=streets$closeness ~ 1, locations=streets, newdata=grd)
names(idw_close)[1] = "closeness"
idw.output_close = as.data.frame(idw_close)
names(idw.output_close)[1:3]<-c("long","lat","closeness")
#glimpse(idw.output)

idw_bet= idw(formula=streets$betweennes ~ 1, locations=streets, newdata=grd)
names(idw_bet)[1] = "betweenness"
idw.output_bet = as.data.frame(idw_bet)
names(idw.output_bet)[1:3]<-c("long","lat","betweenness")


#' Plot raster of interpolated network betweenness
plot_close = ggplot(data=idw.output_close,aes(x=long,y=lat)) +
  geom_tile(data=idw.output_close,aes(fill=closeness)) +
  scale_fill_gradient(low="#FFFFCC", high="#000044") +
  coord_equal() + theme_nothing()
plot_close

plot_bet = ggplot(data=idw.output_bet,aes(x=long,y=lat)) +
  geom_tile(data=idw.output_bet,aes(fill=betweenness)) +
  scale_fill_gradient(low="#FFFFCC", high="#000044") +
  coord_equal() + theme_nothing()
plot_bet



# Read Census Data
census = readOGR("data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") %>%
  spTransform(projitaly) 
census@data = get_deprivation_features(census)


# idw_raster = raster(idw)
# ext = extract(idw, census)
intersection_close = over(census, idw_close, fn = median)
intersection_bet = over(census, idw_bet, fn = median)


census@data["closeness"] = intersection_close[,"closeness"]
census@data["betweenness"] = intersection_bet[,"betweenness"]


leaflet_map(census, "closeness", "closeness")
leaflet_map(census, "betweenness", "betweenness")


summary(lm(deprivation~closeness, census@data))
summary(lm(deprivation~betweenness, census@data))

library(GGally)


ggpairs(dplyr::select(census@data,closeness,betweenness, deprivation, high_school:work_force))
