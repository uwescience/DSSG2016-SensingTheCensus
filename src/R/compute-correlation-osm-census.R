library(readr)
library(magrittr)
library(sp)
library(spdep)
library(raster)
library(rgdal)
library(tidyr)
library(ggplot2)
library(ade4)
library(maptools)
library(dplyr)

amenities = read_csv("data/OSM/amenities.csv", col_types = list(
  id = col_character()
)) %>% as.data.frame()

coordinates(amenities) =  ~ lon + lat
proj4string(amenities) = CRS("+init=epsg:4326")
amenities %<>% spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))

census = readOGR("data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") %>%
  spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))


intersection = raster::intersect(y = census, x = amenities)

count_ace_cat = intersection@data %>% group_by(ACE, amenity) %>% summarize(count_ace_cat = n()) 
count_ace = count_ace_cat %>% group_by(ACE) %>% summarize(count_ace = n()) 
total_poi = dim(intersection@data)[1]
total_poi_cat = intersection@data %>% group_by(amenity) %>% summarize(poi_cat = n()) 

offer_advantage = count_ace_cat %>% 
  left_join(count_ace, by ="ACE") %>% 
  left_join(total_poi_cat, by ="amenity") %>% 
  mutate(total_poi = total_poi) %>%
  mutate(offer_advantage = (count_ace_cat/count_ace)*(poi_cat/total_poi))
# intersection  
# plot(intersection)


offer_advantage_wide = offer_advantage%>% select(ACE, amenity, offer_advantage) %>% spread(amenity, offer_advantage, fill = 0)


census@data %<>% left_join(offer_advantage_wide, by = "ACE")


# dudi = dudi.pca(census@data %>% select(deprivation, arts_centre:wifi),scannf = FALSE)

# nb = poly2nb(census)
# nblist = nb2listw(nb)
# sp_cor = multispati(dudi, listw = nblist)
# ggplot(t) + geom_point(aes(y = offer_advantage, x = amenity) ) + facet_wrap(~ACE, ncol = 3) 


# plot(census@data %>% select(deprivation), census@data %>% select(arts_centre:wifi))
cor(census@data %>% select(deprivation), census@data %>% select(arts_centre:wifi))
ggplot(census@data) +  geom_point(aes(x = bar, y = deprivation))+ 
  labs(x="bar offering", y = "well-being", title = "Bar offering vs well-being") + 
  theme_fivethirtyeight()
ggsave(filename = "doc/plots/bar_deprivation.png")

ggplot(census@data) +  geom_point(aes(x = marketplace, y = deprivation))

ggplot(census@data) +  geom_point(aes(x = bbq, y = deprivation))

ggplot(census@data) +  geom_point(aes(x = fast_food, y = deprivation))+ 
  labs(x="fast food offering", y = "well-being", title = "Fast food offering vs well-being") + 
  theme_fivethirtyeight()
ggsave(filename = "doc/plots/fast-food_deprivation.png")

ggplot(census@data) +  geom_point(aes(x = pub, y = deprivation))+ 
  labs(x="pub offering", y = "well-being", title = "Pub offering vs well-being") + 
  theme_fivethirtyeight()

ggplot(census@data) +  geom_point(aes(x = bicycle_parking, y = deprivation)) + labs(x="bicycle parking", y = "well-being")
