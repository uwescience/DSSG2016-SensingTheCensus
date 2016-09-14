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

'
mexico_city_airbnb=read.csv("../../data/airbnb/mexico_city_airbnb_geocode.csv")
p<-Polygon(data.frame(mexico_city_airbnb$lon, mexico_city_airbnb$lat)) 
#p1<- Polygons(list(p), ID=1:length(p)) 
p1<- Polygons(list(p), ID=1) 
WGS84<-CRS("+proj=longlat +datum=WGS84")  #ESRI GCS_WGS_1984 
LocPoly<- SpatialPolygons(list(p1),proj4string=WGS84) 
#Loc<-data.frame(lon=-99.1013,lat=19.2465,name='Mexico City',ID=1)
Loc<-data.frame(mexico_city_airbnb$lon, mexico_city_airbnb$lat,ID=1:nrow(mexico_city_airbnb))
#LocPt<-SpatialPoints(coordinates(mexico_city_airbnb[,(-c(1:4))]),proj4string=WGS84)
#LocPtDF<-SpatialPointsDataFrame(coordinates(mexico_city_airbnb[,(-c(1:4))],Loc,proj4string=WGS84)
LocPt<-SpatialPoints(coordinates(Loc[,-3]),proj4string=WGS84)
LocPtDF<-SpatialPointsDataFrame(coordinates(Loc[,-3]),Loc,proj4string=WGS84)
LocPtDF<-spTransform(LocPtDF,CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))

LocPolyDF<- SpatialPolygonsDataFrame(LocPoly,
data.frame(ID=1))
writeOGR(LocPtDF,'../../data/airbnb/mexico_city_shape_files','mexico_city',
driver="ESRI Shapefile",overwrite_layer=TRUE)

GPS.Points=data.frame(Longitude=c(23.85474, 23.85531, 23.85534))
GPS.Points=cbind(GPS.Points,Latitude=c(-19.52211, -19.52243, -19.52257))

19.69857	-98.742912	18.94665	-99.507149
test = data.frame(lon = mexico_city_airbnb$longitude, lat = mexico_city_airbnb$latitude)
test_subset = subset(test, lon<-98.742912 & lon>-99.507149)
test_subset = subset(test, lat<19.69857 & lat>18.94665)
test.Points.Spatial.Data <- SpatialPoints(test_subset, 
proj4string=CRS("+proj=longlat     +ellps=WGS84"))
GPS.Points.Spatial.Data <- SpatialPoints(GPS.Points, 
proj4string=CRS("+proj=longlat     +ellps=WGS84"))
test.Points.Spatial.Data[1]
GPS.Points.Spatial.Data[1]

class(GPS.Points.Spatial.Data)

GPS.Points.UTM.Spatial.Data <- spTransform(GPS.Points.Spatial.Data,
CRS("+proj=utm +south +zone=34 +ellps=WGS84"))

test.Points.UTM.Spatial.Data <- spTransform(test.Points.Spatial.Data,
CRS("+proj=utm +zone=14 +ellps=WGS84"))

test_lon_lat.df=SpatialPointsDataFrame(test.Points.UTM.Spatial.Data, data.frame(id=1:length(test.Points.UTM.Spatial.Data)))
test_polygon = Polygon(test_lon_lat.df, hole=as.logical(NA))
#test_polygon = Polygon(test_airbnb.df, hole=as.logical(NA))
test_polygons = Polygons(list(test_polygon),1)
test_spatial_polygons = SpatialPolygons(list(test_polygons))
#proj4string(test_spatial_polygons) = CRS("+proj=utm +zone=32 +datum=WGS84")
proj4string(test_spatial_polygons) = CRS("+proj=utm +zone=14 +datum=WGS84")
#proj4string(test_spatial_polygons) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

data = data.frame(f=99.9)
test_spatial_polygons_df = SpatialPolygonsDataFrame(test_spatial_polygons, data)
spplot(test_spatial_polygons_df)

test_geojson = geojson_json(test_spatial_polygons_df)
geojson_write(test_geojson, file = "../../data/airbnb/test_geojson.geojson")
'

#mexico_city_airbnb=tbl_df(read.csv("../../data/airbnb/mexico_city_airbnb_geocode.csv"))

mexico_city_airbnb=read.csv("../../data/airbnb/mexico_city_airbnb_geocode.csv")
coordinates(mexico_city_airbnb)=~longitude+latitude
proj4string(mexico_city_airbnb)=CRS("+proj=longlat +datum=WGS84") 
mexico_city_airbnb.df <- SpatialPointsDataFrame(mexico_city_airbnb,
                                                data.frame(id=1:length(mexico_city_airbnb),
                                                           price=mexico_city_airbnb$price,
                                                           lat = mexico_city_airbnb$latitude,
                                                           lon = mexico_city_airbnb$longitude))
mexico_city_lon_lat<-spTransform(mexico_city_airbnb.df,
                                 CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
#mexico_city_lon_lat<-spTransform(mexico_city_airbnb.df,CRS("+proj=merc +units=m +nadgrids=@null +no_defs"))
mexico_city_lon_lat.df=SpatialPointsDataFrame(mexico_city_lon_lat,  data.frame(id=1:length(mexico_city_airbnb),
                                                                               price=mexico_city_airbnb$price,
                                                                               lat = mexico_city_airbnb$latitude,
                                                                               lon = mexico_city_airbnb$longitude))

mexico_city_polygon = Polygon(mexico_city_lon_lat.df, hole=as.logical(NA))
#mexico_city_polygon = Polygon(mexico_city_airbnb.df, hole=as.logical(NA))
mexico_city_polygons = Polygons(list(mexico_city_polygon),1)
mexico_city_spatial_polygons = SpatialPolygons(list(mexico_city_polygons))
#proj4string(mexico_city_spatial_polygons) = CRS("+proj=utm +zone=32 +datum=WGS84")
#proj4string(mexico_city_spatial_polygons) = CRS("+proj=utm +zone=32 +datum=WGS84")
proj4string(mexico_city_spatial_polygons) = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
#proj4string(mexico_city_spatial_polygons) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#data = data.frame(f=99.9)
data =  data.frame(id=1:length(mexico_city_airbnb),
                   price=mexico_city_airbnb$price,
                   lat = mexico_city_airbnb$latitude,
                   lon = mexico_city_airbnb$longitude)
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
airbnb = readOGR("../../data/airbnb/mexico_city_shape_files/", layer = "mexico_city")

#' Load census and CDR geojson
census = readOGR("../../data/census/mexicocity.geojson", layer ="OGRGeoJSON")
census = readOGR("../../data/census/mexicocity.geojson", layer ="OGRGeoJSON") %>%
   spTransform(CRS("+proj=merc +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +units=m +no_defs"))

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

