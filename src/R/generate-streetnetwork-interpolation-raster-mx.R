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
streets = readShapePoints("data/geography/mexico_city/streets/street_intersections.shp")
proj4string(streets) = CRS("+init=epsg:4326")
projmexico <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")

streets %<>% spTransform(projmexico)

#' Plot kernel density function
#breach.dens = kde.points(ameni,lims=census)
#level.plot(pt)


#' Generate grid to interpolate into from streets int
x.range <- as.integer(range(streets@coords[,1]))
y.range <- as.integer(range(streets@coords[,2]))
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=100), y=seq(from=y.range[1], to=y.range[2], by=100))
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE
#project grid
proj4string(grd) <- projmexico


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
census = readShapePoly("data/census/mexico_city/mexico_city_census.shp")

census@data %<>% mutate(density=as.numeric(as.character(POB_TOT))/area(census))

proj4string(census) = projmexico



# idw_raster = raster(idw)
# ext = extract(idw, census)
intersection_close = over(census, idw_close, fn = median)
intersection_bet = over(census, idw_bet, fn = median)


census@data["closeness"] = intersection_close[,"closeness"]
census@data["betweenness"] = intersection_bet[,"betweenness"]

census@data %<>% mutate_each(funs(ifelse(is.na(.), mean(., na.rm=TRUE),.)),closeness, betweenness)


leaflet_map(census, "closeness", "closeness")
leaflet_map(census, "betweenness", "betweenness")
leaflet_map(census, "IMU", "deprivation", n=10)
leaflet_map(census, "density", "density", n=10)


cor(dplyr::select(census,closeness,betweenness) ,
    dplyr::select(cenus,IMU))

summary(lm(IMU~closeness, census@data))
summary(lm(IMU~betweenness, census@data))
summary(lm(IMU~closeness+density, census@data))
summary(lm(IMU~betweenness+density, census@data))
summary(lm(IMU~betweenness*closeness+density, census@data))

# 
# library(ggplot2)
ggpairs(dplyr::select(census@data, closeness, betweenness, IMU) %>% 
          mutate(log_betweenness = log(betweenness),
                 log_closeness = log(closeness),
                 inv_closeness = 1/closeness,
                 inv_betweenness = 1/betweenness))

qplot(x = IMU, y = closeness, data = census@data)


qplot(x = betweenness, y = closeness, data = census@data)

census@data %>% select(CVE_GEOAGE,closeness, betweenness) %>% write_csv("data/census/mexico_city/centrality_ageb.csv")
