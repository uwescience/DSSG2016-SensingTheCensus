pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

#pkgTest("dplyr")
#pkgTest("geojsonio")
#pkgTest("lubridate")
#pkgTest("magrittr")
#pkgTest("maptools")
#pkgTest("raster")
#pkgTest("readr")
#pkgTest("rgdal")
#pkgTest("rgeos")
#pkgTest("sp")
#pkgTest(tidyr)


rm(list=ls())
setwd("/Users/kivan/repos/DSSG2016-SensingTheCensus/src/R")

library(dplyr)
library(geojsonio)
library(lubridate)
library(magrittr)
library(maptools)
library(raster)
library(readr)
library(rgdal)
library(rgeos)
library(sp)
library(tidyr)


# Convert Mexico City Airbnb data to .geojson file
mexico_city_airbnb_geocode=tbl_df(read.csv("../../data/airbnb/mexico_city_airbnb_geocode.csv"))
mexico_city_airbnb_polygon<-Polygon(data.frame(mexico_city_airbnb_geocode$longitude, mexico_city_airbnb_geocode$latitude)) 
mexico_city_airbnb_polygons<- Polygons(list(mexico_city_airbnb_polygon), ID=1) 
WGS84<-CRS("+proj=longlat +datum=WGS84")  
mexico_city_airbnb_spatial_polygons<- SpatialPolygons(list(mexico_city_airbnb_polygons),proj4string=WGS84) 
mexico_city_airbnb_locations<-data.frame(mexico_city_airbnb_geocode$longitude,
                                         mexico_city_airbnb_geocode$latitude,
                                         ID=1:nrow(mexico_city_airbnb_geocode))
mexico_city_airbnb_location_points<-SpatialPoints(coordinates(mexico_city_airbnb_locations[,-3]),proj4string=WGS84)
mexico_city_airbnb_location_points_df<-SpatialPointsDataFrame(coordinates(mexico_city_airbnb_locations[,-3]),mexico_city_airbnb_locations,proj4string=WGS84)
MERC<- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
mexico_city_airbnb_location_points_df<-spTransform(mexico_city_airbnb_location_points_df,MERC)

mexico_city_airbnb_location_poly_df<- SpatialPolygonsDataFrame(mexico_city_airbnb_spatial_polygons,
                                                               data.frame(ID=1))
writeOGR(mexico_city_airbnb_location_points_df,'../../data/airbnb/mexico_city_shape_files','mexico_city',
driver="ESRI Shapefile",overwrite_layer=TRUE)

#' create mexico_city_airbnb.geojson
shpfile <- file.path("../../data/airbnb/mexico_city_shape_files/mexico_city.shp")
file_to_geojson(shpfile, method='local', output='../../data/airbnb/mexico_city_airbnb')

'
mexico_city_airbnb_geocode=tbl_df(read.csv("../../data/airbnb/mexico_city_airbnb_geocode.csv"))
coordinates(mexico_city_airbnb_geocode)=~longitude+latitude
proj4string(mexico_city_airbnb_geocode)=CRS("+proj=longlat +datum=WGS84") 
mexico_city_airbnb_geocode.df <- SpatialPointsDataFrame(mexico_city_airbnb_geocode,
                                                data.frame(id=1:length(mexico_city_airbnb_geocode),
                                                           price=mexico_city_airbnb_geocode$price,
                                                           lat = mexico_city_airbnb_geocode$latitude,
                                                           lon = mexico_city_airbnb_geocode$longitude))
mexico_city_lon_lat<-spTransform(mexico_city_airbnb_geocode.df,
                                 CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
mexico_city_lon_lat.df=SpatialPointsDataFrame(mexico_city_lon_lat,  data.frame(id=1:length(mexico_city_airbnb_geocode),
                                                                               price=mexico_city_airbnb_geocode$price,
                                                                               lat = mexico_city_airbnb_geocode$latitude,
                                                                               lon = mexico_city_airbnb_geocode$longitude))

mexico_city_polygon = Polygon(mexico_city_lon_lat.df, hole=as.logical(NA))
mexico_city_polygons = Polygons(list(mexico_city_polygon),1)
mexico_city_spatial_polygons = SpatialPolygons(list(mexico_city_polygons))
proj4string(mexico_city_spatial_polygons) = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")

data = data.frame(f=99.9)
mexico_city_spatial_polygons_df = SpatialPolygonsDataFrame(mexico_city_spatial_polygons, data)
#spplot(mexico_city_spatial_polygons_df)

mexico_city_geojson = geojson_json(mexico_city_spatial_polygons_df)
geojson_write(mexico_city_geojson, file = "../../data/airbnb/mexico_city_airbnb.geojson")

#writeOGR(mexico_city_lon_lat.df, dsn="../../data/airbnb/" ,layer="mexico-city_airbnb",driver="ESRI Shapefile")
#writeSpatialShape(mexico_city_lon_lat.df,"../../data/airbnb/test")
#writeOGR(mexico_city_lon_lat.df, dsn="../../data/airbnb/mexico_city_airbnb.json" ,layer="OGRGeoJSON",driver="GeoJSON")
'

#' mexico city layers, can user shape files or geojson
mexico_city_airbnb_layer = readOGR("../../data/airbnb/mexico_city_shape_files/", layer ="mexico_city") %>% 
  spTransform(CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
#mexico_city_airbnb_layer = readOGR("../../data/airbnb/mexico_city_airbnb.geojson", layer ="OGRGeoJSON") %>%
#  spTransform(CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))

#' census layers, can user shape files or geojson
#census_layer = readOGR("../../data/census/mexicocity.geojson", layer ="OGRGeoJSON")
census_layer = readOGR("../../data/census/mexico_city/", layer ="mexico_city_census")

#' Intersect polygons
#' no intersection when using shape file layers
#' need to use identical CRS for intersection for geojson layers
proj4string(mexico_city_airbnb_layer)
proj4string(census_layer)<-CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
intersection = raster::intersect(x = census_layer, y = mexico_city_airbnb_layer)

#' Calcualte area of each polygon
intersection@data$area = area(intersection)

#' Calculate area of each polygon
square_size = max(intersection@data$area)

'
intersection@data %<>% dplyr::select(ACE,area, cellId)

# Filter by cells that intersect to a census tract
cdr_network %<>%
  filter(source %in% intersection@data$cellId, target %in% intersection@data$cellId)

# Aggregate data into census areas summing the CDR data proportionally to the size of the squares

census_network =  cdr_network %>% left_join(intersection@data, by = c("source"="cellId")) %>%
  left_join(intersection@data, by = c("target"="cellId")) %>% 
  dplyr::group_by(ACE.x, ACE.y, hour) %>% 
  summarize(calls = sum(((area.x)/(square_size))*((area.y)/(square_size)) *calls, na.rm=TRUE))
write_csv(census_network, save_path)
'

## Not run: 
togeojson <- function(file, writepath = "~") {
  url <- "http://ogre.adc4gis.com/convert"
  tt <- POST(url, body = list(upload = upload_file(file)))
  out <- content(tt, as = "text")
  fileConn <- file(writepath)
  writeLines(out, fileConn)
  close(fileConn)
}

file <- '../../data/airbnb/mexico_city_airbnb.json'

# KML type file - using the web method
#togeojson(file)

