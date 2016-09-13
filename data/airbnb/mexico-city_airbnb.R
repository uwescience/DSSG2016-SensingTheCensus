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
#mexico_city_airbnb=tbl_df(read.csv("../../data/airbnb/mexico_city_airbnb_geocode.csv"))
mexico_city_airbnb=read.csv("../../data/airbnb/mexico_city_airbnb_geocode.csv")
coordinates(mexico_city_airbnb)=~longitude+latitude
proj4string(mexico_city_airbnb)=CRS("+proj=utm +zone=32 +datum=WGS84") # set it to UTM
mexico_city_airbnb.df <- SpatialPointsDataFrame(mexico_city_airbnb, data.frame(id=1:length(mexico_city_airbnb)))
mexico_city_lon_lat<-spTransform(mexico_city_airbnb.df,CRS("+proj=longlat +datum=WGS84"))
mexico_city_lon_lat.df=SpatialPointsDataFrame(mexico_city_lon_lat, data.frame(id=1:length(mexico_city_lon_lat)))

mexico_city_polygon = Polygon(mexico_city_lon_lat, hole=as.logical(NA))
mexico_city_polygons = Polygons(list(mexico_city_polygon),1)
mexico_city_spatial_polygons = SpatialPolygons(list(mexico_city_polygons))
proj4string(mexico_city_spatial_polygons) = CRS("+proj=utm +zone=32 +datum=WGS84")
#proj4string(mexico_city_spatial_polygons) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

data = data.frame(f=99.9)
mexico_city_spatial_polygons_df = SpatialPolygonsDataFrame(mexico_city_spatial_polygons, data)
#spplot(mexico_city_spatial_polygons_df)

mexico_city_geojson = geojson_json(mexico_city_spatial_polygons_df)
#mexico_city_geojson = geojson_json(mexico_city_spatial_polygons)
#mexico_city_geojson = geojson_list(mexico_city_polygons)
#geojson_list(mexico_city_spatial_polygons_df)
geojson_write(mexico_city_geojson, file = "../../data/airbnb/mexico_city_airbnb.geojson")

#writeOGR(mexico_city_lon_lat.df, dsn="../../data/airbnb/" ,layer="mexico-city_airbnb",driver="ESRI Shapefile")
#writeSpatialShape(mexico_city_lon_lat.df,"../../data/airbnb/test")
#writeOGR(mexico_city_lon_lat.df, dsn="../../data/airbnb/mexico_city_airbnb.json" ,layer="OGRGeoJSON",driver="GeoJSON")

# file only has 1 feature/field. need to fix 
airbnb = readOGR("../../data/airbnb/mexico_city_airbnb.geojson", "OGRGeoJSON") %>%
  spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))

#' Load census and CDR geojson
census = readOGR("../../data/census/mexicocity.geojson", "OGRGeoJSON") %>%
  spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))

#' Intersect polygons
intersection = raster::intersect(x = census, y = airbnb)

#' Calcualte area of each polygon
intersection@data$area = area(intersection)

#' Calculate area of each polygon
square_size = max(intersection@data$area)

intersection@data %<>% dplyr::select(ACE,area, cellId)

# Filter by cells that intersect to a census tract
cdr_network %<>%
  filter(source %in% intersection@data$cellId, target %in% intersection@data$cellId)

#' Aggregate data into census areas summing the CDR data proportionally to the size of the squares

census_network =  cdr_network %>% left_join(intersection@data, by = c("source"="cellId")) %>%
  left_join(intersection@data, by = c("target"="cellId")) %>% 
  dplyr::group_by(ACE.x, ACE.y, hour) %>% 
  summarize(calls = sum(((area.x)/(square_size))*((area.y)/(square_size)) *calls, na.rm=TRUE))
write_csv(census_network, save_path)

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
togeojson(file)

