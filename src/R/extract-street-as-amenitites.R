library(sp)
library(readr)
library(dplyr)
library(maptools)
library(rgdal)
library(leaflet)
library(GGally)
library(ggplot2)
library(rgeos)
library(magrittr)
library(stringr)
library(raster)
library(RColorBrewer)
library(tidyr)

source("src/R/utils.R")

mexicoproj = CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")

# streets = readOGR("data/geography/mexico_city/streets/mexico_city_streets.shp", layer = "mexico_city_streets") 
census = readOGR("data/census/mexico_city/mexico_city_census.shp", layer="mexico_city_census")
# 
census@data$area = area(census)
proj4string(census) = mexicoproj
# # 
# # streets_simple = gSimplify(streets, tol=10, topologyPreserve=TRUE)
# # plot(streets_simple)
# streets = spChFIDs(streets, as.character(streets@data$ID))
# row.names(streets@data) =  as.character(streets@data$ID)
# 
# street_ends = plyr::ldply(streets@lines,.fun = function(line){
#   
#   coord = line@Lines[[1]]@coords
#   data.frame(x = coord[,1], y=  coord[,2], id = line@ID)
# 
# })
# # street_ends = streets@coords %>% as.data.frame() 
# 
# coordinates(street_ends) = ~ x + y
# proj4string(street_ends) = mexicoproj
# 
# # plot(a)
# 
# 
# # midpoints = SpatialLinesMidPoints(streets)
# intersection = over(street_ends, census)
# %>% bind_cols(midpoints@data %>% dplyr::select(TIPOVIA))

# intersection = gIntersection(streets, census) 

intersection = readOGR("data/OSM/mexico_city_streets/street_intersection_test.shp", 
                       layer = "street_intersection_test")

intersection@data$length = gLength(intersection, byid=TRUE)
#intersect amenity and census data
count_cat = intersection@data %>% group_by(CVE_GEOAGE, TIPOVIA) %>% summarize(count_cat = n()) 
count_geo = count_cat %>% group_by(CVE_GEOAGE) %>% summarize(count_geo = n()) 
total_poi = dim(intersection@data)[1]
#intersection@data[1]
total_poi_cat = intersection@data %>% group_by(TIPOVIA) %>% summarize(poi_cat = n()) 

#calculate offering advantage 
offer_advantage = count_cat %>% 
  left_join(count_geo, by ="CVE_GEOAGE") %>% 
  left_join(total_poi_cat, by ="TIPOVIA") %>%
  mutate(total_poi = total_poi) %>%
  mutate(offer_advantage = (count_cat/count_geo)*(total_poi/poi_cat))

offer_advantage_wide = offer_advantage%>% dplyr::select(CVE_GEOAGE, TIPOVIA, offer_advantage) %>% 
  spread(TIPOVIA, offer_advantage, fill = 0)

offer_advantage_wide %<>% na.omit()

#join census and amenity data 
census@data %<>% left_join(offer_advantage_wide, by = "CVE_GEOAGE")
census@data[is.na(census@data )] = 0
# corr_depriv_cat = cor(census@data %>% dplyr::select(IMU) %>% mutate(IMU = as.numeric(IMU)), 
#                       census@data %>% dplyr::select(AMPLIACION:VIADUCTO))
# corr_depriv_cat
# png(height=600, width=1200, pointsize=25, file="doc/plots/correlation_osm_deprivation.png")
# corrplot(corr_depriv_cat[,abs(corr_depriv_cat["well_being",] )>.1] , method="color", tl.col="black", tl.srt=60, number.font = 1,
#          col = colorRampPalette(c("red","white","blue"))(14))
# dev.off()


street_density = intersection@data %>% 
  left_join(dplyr::select(census@data,area,CVE_GEOAGE)) %>% 
  group_by(CVE_GEOAGE) %>% 
  summarize(street_density = sum(length)/unique(area))


census@data %<>% left_join(street_density, by = "CVE_GEOAGE") %>%
  mutate(street_density = ifelse(is.na(street_density),0, street_density))

census@data %>% dplyr::select(CVE_GEOAGE, AMPLIACION:street_density) %>% 
  write_csv("data/OSM/mexico_city/street_offering_advantage.csv")
# cor(census@data$IMU, census@data$street_density)
# plot(census@data$IMU, census@data$street_density)
# 
# summary(lm(IMU ~ street_density.y, census@data))
# 
# summary(lm(IMU ~ street_density.x, census@data))
# plot(census@data$IMU, census@data$street_density)
# 
# leaflet_map(census, "street_density.y", "street_density")
# leaflet_map(census, "IMU", "deprivation")
