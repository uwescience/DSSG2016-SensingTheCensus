library(sp)
library(readr)
library(dplyr)
library(maptools)
library(rgdal)
library(leaflet)
library(GGally)
library(ggplot2)
library(rgeos)
library(geojsonio)
library(magrittr)
library(stringr)
library(shp2graph)


source("src/R/utils.R")

mex_city_mun = c(9002,9003,9004,9005,9006,9007,9008,9009,9010,9011,9012,9013,
                 9014,9015,9016,9017,15002,15009,15010,15011,15013,15015,
                 15016,15017,15020,15022,15023,15024,15025,15028,15029,15030,
                 15031,15033,15034,15035,15036,15037,15038,15039,15044,15046,
                 15050,15053,15057,15058,15059,15060,15061,15065,15068,15069,
                 15070,15075,15081,15083,15084,15089,15091,15092,15093,15094,
                 15095,15096,15099,15100,15103,15104,15108,15109,15112,15120,
                 15121,15122,15125) %>% as.character()
census = readOGR("data/OSM/mexico_city_census/IMU_2010.shp", layer="IMU_2010")
# census %<>% spTransform(CRS("+init=epsg:4326"))

census@data %<>% mutate(IMU =  as.numeric(as.character(IMU)),
                        CVE_ENT = as.character(CVE_ENT), 
                        CVE_MUN =  str_pad(as.character(CVE_MUN) , 3, pad = "0"),
                        CVE_MUN_COMP = paste(CVE_ENT,CVE_MUN,sep=""))
census %<>% subset(CVE_MUN_COMP %in% mex_city_mun)


writeSpatialShape(census,"data/census/mexico_city/mexico_city_census.shp")
streets_cdmx = readOGR("data/OSM/mexico_city_streets/carreviali09/Red_de_Carreteras_09_DF.shp",
                       layer = "Red_de_Carreteras_09_DF")

streets_edomex = readOGR("data/OSM/mexico_city_streets/carreviali15/Red_de_Carreteras_15_Mex.shp",
                         layer = "Red_de_Carreteras_15_Mex")

#' Set unique ID's
streets_cdmx = spChFIDs(streets_cdmx, as.character(streets_cdmx@data$ID))
row.names(streets_cdmx@data) =  as.character(streets_cdmx@data$ID)

streets_edomex = spChFIDs(streets_edomex, as.character(streets_edomex@data$ID))
row.names(streets_edomex@data) =  as.character(streets_edomex@data$ID)

#' Bind streets
streets_raw = spRbind(streets_cdmx, streets_edomex)
rm(streets_cdmx, streets_edomex) # Remove old objects

##' Clip streets to CDMX census tracts
#' Compute Convex Hull for census tracts
census_chull = gConvexHull(census)

streets = raster::crop(streets_raw,census_chull) # Clip the streets to the convex hull

writeLinesShape(streets,"data/census/mexico_city/mexico_city_streets.shp")



# rm(streets_raw) # Remove raw object
# # proj4string(streets) = CRS("+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
# streets %<>% spTransform(CRS("+init=epsg:4326"))
# census %<>% spTransform(CRS("+init=epsg:4326"))
# 
# 
# # pal <- function(x) {colorBin("YlGnBu", x, bins=5)}
# leaflet_map(census, "IMU", "deprivation")
# 
# leaflet(census) %>% addProviderTiles("CartoDB.Positron") %>% 
#   addPolygons(fillColor = ~pal(IMU)(IMU), weight = .2, color="white",fillOpacity = 0.6) %>%
#   addLegend(pal = pal(census$IMU),
#             values = census$IMU,
#             position = "bottomleft",title = "Deprivation"
#   ) %>%
#   addPolylines(weight = 1, color="black", data=streets) 



