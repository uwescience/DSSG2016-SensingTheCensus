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
library(pander)
library(RColorBrewer)
library(corrplot)
library(leaflet)
library(ggthemes)

source("src/R/utils.R")
source("src/R/amenity-aggregation-info.R")

amenities = read_csv("data/OSM/amenities.csv", col_types = list(
  id = col_character()
)) %>% as.data.frame()

summary(amenities)

#aggregate and filter OSM amenity categories 
amenities %<>% filter(!amenity %in% remove) %>% 
  mutate(amenity=ifelse(amenity %in% bank, "bank", 
                        ifelse(amenity %in% casino, "casino",
                               ifelse(amenity %in% community_center, "community_center",
                                      ifelse(amenity %in% hospital, "hospital",
                                             ifelse(amenity %in% marketplace, "marketplace",
                                                    ifelse(amenity %in% parking, "parking",
                                                           ifelse(amenity %in% place_of_worship, "place_of_worship",
                                                                  ifelse(amenity %in% toilet, "toilet",
                                                                         ifelse(amenity %in% waste, "waste",
                                                                                ifelse(amenity %in% bar, "bar", 
                                                                                       ifelse(amenity %in% bicycle, "bicycle", amenity))))))))))))


coordinates(amenities) =  ~ lon + lat
proj4string(amenities) = CRS("+init=epsg:4326")
amenities %<>% spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))

census = readOGR("data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") %>%
  spTransform(CRS("+proj=utm +zone=32 +datum=WGS84")) 

census@data = get_deprivation_features(census)

intersection = raster::intersect(y = census, x = amenities)

count_ace_cat = intersection@data %>% group_by(ACE, amenity) %>% dplyr::summarize(count_ace_cat = n()) 
count_ace = count_ace_cat %>% group_by(ACE) %>% dplyr::summarize(count_ace = n()) 
total_poi = dim(intersection@data)[1]
total_poi_cat = intersection@data %>% group_by(amenity) %>% dplyr::summarize(poi_cat = n()) 

offer_advantage = count_ace_cat %>% 
  left_join(count_ace, by ="ACE") %>% 
  left_join(total_poi_cat, by ="amenity") %>% 
  mutate(total_poi = total_poi) %>%
  mutate(offer_advantage = (count_ace_cat/count_ace)*(total_poi/poi_cat))
# intersection  
# plot(intersection)

offer_advantage_wide = offer_advantage%>% dplyr::select(ACE, amenity, offer_advantage) %>% spread(amenity, offer_advantage, fill = 0)

census@data %<>% left_join(offer_advantage_wide, by = "ACE")

write_csv(census@data %>% dplyr::select(ACE, arts_centre:wifi),
          "data/OSM/offering_advantage.csv")
osm_pca = prcomp(census@data %>% dplyr::select(arts_centre:waste), center = TRUE, scale = TRUE)
osm_pca_df = as.data.frame(osm_pca$x[,1:20])
biplot(osm_pca)

#write pca amenity csv
census@data %>% dplyr::select(ACE, starts_with("PC"), arts_centre:waste) %>% 
  write_csv("data/OSM/milano_amenity_pca.csv")

correlationamenity = cor(offer_advantage_wide[,2:61])
corrplot(correlationamenity, method = "color", tl.col="black", number.font=1, col= wes_palette("Rushmore",n=5, type = c("discrete")))

corr_df = correlationamenity %>% as.data.frame()%>% add_rownames("var1") %>% gather(var2,cor,arts_centre:waste) %>% arrange(desc(cor))
write.csv(corr_df, "data/milano_corr_df.csv")

#extract osm public_transport data 
public_transport = read.csv("data/OSM/public_transport.csv")
census = readOGR("data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") 
census@data = get_deprivation_features(census)
census %<>% spTransform(CRS("+proj=longlat"))
coordinates(public_transport) = ~lon+lat
proj4string(public_transport) = CRS("+proj=longlat")
proj4string(census) = CRS("+proj=longlat")

#intersect census and public transport
intersection = raster::intersect(y = census, x = public_transport)
plot(intersection)

#calculate offering advantage
count_ace_cat = intersection@data %>% group_by(ACE, public_transport) %>% dplyr::summarize(count_ace_cat = n()) 
count_ace = count_ace_cat %>% group_by(ACE) %>% dplyr::summarize(count_ace = n()) 
total_poi = dim(intersection@data)[1]
total_poi_cat = intersection@data %>% group_by(public_transport) %>% dplyr::summarize(poi_cat = n()) 

offer_advantage = count_ace_cat %>% 
  left_join(count_ace, by ="ACE") %>% 
  left_join(total_poi_cat, by ="public_transport") %>% 
  mutate(total_poi = total_poi) %>%
  mutate(offer_advantage = (count_ace_cat/count_ace)*(total_poi/poi_cat))

offer_advantage_wide = offer_advantage%>% dplyr::select(ACE, public_transport, offer_advantage) %>% spread(public_transport, offer_advantage, fill = 0)
census@data %<>% left_join(offer_advantage_wide, by = "ACE")

#generate csv of public transportation offering advantage
write_csv(census@data %>% dplyr::select(ACE, platform, stop_position),
          "data/OSM/offering_advantage_public_transport_milano.csv")


#corr_df = corr_depriv_cat[,abs(corr_depriv_cat["well_being",] )>.1] %>% as.data.frame() %>% add_rownames("census") %>%
  
# nb = poly2nb(census)
# nblist = nb2listw(nb, style = "W")
# 
# ## Spatial autocrrelation test
# moran.mc(census$deprivation,listw = nblist, 9999)
# # sp_cor = multispati(dudi, listw = nblist)
# # ggplot(t) + geom_point(aes(y = offer_advantage, x = amenity) ) + facet_wrap(~ACE, ncol = 3) 
# 
# 
# # plot(census@data %>% select(deprivation), census@data %>% select(arts_centre:wifi))
# corr_depriv_cat = cor(census@data %>% mutate(well_being = deprivation) %>% dplyr::select(well_being, high_school:unemployment), 
#                       census@data %>% dplyr::select(arts_centre:wifi))
# 
# # png(height=600, width=1200, pointsize=25, file="doc/plots/correlation_osm_deprivation.png")
# # corrplot(corr_depriv_cat[,abs(corr_depriv_cat["well_being",] )>.1] , method="color", tl.col="black", tl.srt=60, number.font = 1,
# #          col = colorRampPalette(c("red","white","blue"))(14))
# # dev.off()
# 
# corr_df = corr_depriv_cat[,abs(corr_depriv_cat["well_being",] )>.1] %>% as.data.frame() %>% add_rownames("census") %>%
#   gather(osm, correlation, bar:waste)
# 
# corr_plot = ggplot(corr_df)+aes(x = osm, y=census) + geom_tile(aes(fill= correlation)) + coord_fixed()+
#   geom_text(aes(label = round(correlation,2)))+
#   scale_fill_gradient2(low="#e41a1c",mid = "white", high = "#377eb8", limits=c(-1, 1))+
#   labs(title="OSM Offering Advantage vs Census") + 
#                        # guide = guide_legend(direction = "vertical")) +
#   theme_fivethirtyeight() + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.title = element_blank(),
#         legend.position="right",legend.direction = "vertical")
# 
# corr_plot
# ggsave("doc/plots/correlation_osm_deprivation.png", corr_plot, scale=1.5)
# 
# ggplot(census@data) +  geom_point(aes(x = bar, y = deprivation))+ 
#   labs(x="bar offering", y = "well-being", title = "Bar offering vs well-being") + 
#   theme_fivethirtyeight()
# ggsave(filename = "doc/plots/bar_deprivation.png")
# 
# ggplot(census@data) +  geom_point(aes(x = marketplace, y = deprivation))
# 
# ggplot(census@data) +  geom_point(aes(x = bbq, y = deprivation))
# 
# ggplot(census@data) +  geom_point(aes(x = fast_food, y = deprivation))+ 
#   labs(x="fast food offering", y = "well-being", title = "Fast food offering vs well-being") + 
#   theme_fivethirtyeight()
# ggsave(filename = "doc/plots/fast-food_deprivation.png")
# 
# ggplot(census@data) +  geom_point(aes(x = pub, y = deprivation))+ 
#   labs(x="pub offering", y = "well-being", title = "Pub offering vs well-being") + 
#   theme_fivethirtyeight()
# 
# ggplot(census@data) +  geom_point(aes(x = bicycle_parking, y = deprivation)) + labs(x="bicycle parking", y = "well-being")
# 
# strongest_cor = colnames(corr_depriv_cat[,abs(corr_depriv_cat["well_being",] )>.2])
# intersection_df = intersection %>% 
#   spTransform(CRS("+init=epsg:4326")) %>% 
#   as.data.frame() %>% 
#   filter(amenity %in% strongest_cor[!strongest_cor %in% "telephone"])
# 
# 
# palDisc = colorFactor("Set1", intersection_df$amenity, na.color = "#808080")
# leaflet_map(census, "deprivation", "Well-being") %>% 
#   addCircleMarkers(lng = ~lon, lat = ~lat, data = intersection_df, 
#                    fillColor = ~palDisc(amenity), radius =3, stroke = FALSE,
#                    fillOpacity = 0.95, popup = ~amenity) %>%
#   addLegend(pal = palDisc,
#             values = intersection_df$amenity,
#             position = "bottomright",title = "POI's",
#             opacity = 0.95
#   )
# 
# leaflet_map(census, "bar", "Bar Offering Advantage", "PuBuGn", "equal")
# leaflet_map(census, "bicycle_parking", "Bicycle Parking <br>Offering Advantage", "PuBuGn", "equal")
# 
# # leaflet_map(census, "deprivation", "Bar offering Advantage", "PuBuGn")


