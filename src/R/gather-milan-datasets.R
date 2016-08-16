library(readr)
library(rgdal)
library(dplyr)
library(magrittr)

source("src/R/utils.R")

projitaly <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m")

#' Read milan datasets
milan_census = readOGR("data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") 
milan_census %<>% spTransform(projitaly)
offering_advantage = read_csv("data/OSM/offering_advantage.csv")
names(offering_advantage)[-1] = paste(names(offering_advantage), "osm", sep= "_")[-1]

# cdr = read_csv("data/CDR/cdr_temporal_total_features.csv")
street_centrality = read_csv("data/census/centrality_ace.csv")
  
#' Get deprivation features
milan_census@data = get_deprivation_features(milan_census)



#' Join datasets
temp_data = milan_census@data %>% mutate(ACE = as.numeric(as.character(ACE))) %>%
  dplyr::select(ACE, deprivation,census_area:work_force) %>%
  left_join(offering_advantage, by = "ACE") %>%
  # left_join(cdr, by = "ACE") %>%
  left_join(street_centrality, by = "ACE")


corr_depriv_cat = cor(temp_data %>% dplyr::select(deprivation, high_school:unemployment),
                      temp_data %>% dplyr::select(ends_with("osm")))

most_correlated_amenities = 
  corr_depriv_cat[,abs(corr_depriv_cat["deprivation",] )>.15] %>% as.data.frame() %>% names()
# corr_df = corr_depriv_cat[,abs(corr_depriv_cat["deprivation",] )>.1] %>% as.data.frame() %>% add_rownames("census") %>%
#   gather(osm, correlation, bar:university)

#' Join datasets
milan_census@data %<>% mutate(ACE = as.numeric(as.character(ACE))) %>%
  dplyr::select(ACE, deprivation,census_area:work_force) %>%
  left_join(offering_advantage %>% dplyr::select(ACE, one_of(most_correlated_amenities)), by = "ACE") %>%
  # left_join(cdr, by = "ACE") %>%
  left_join(street_centrality, by = "ACE")

milan_census = spChFIDs(milan_census, as.character(milan_census@data$ACE))




milan_census %>% spTransform(CRS("+proj=longlat"))  %>% saveRDS("app/milan.rds")


milan_census = readOGR("data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") 
milan_census %<>% spTransform(projitaly)

streets = readOGR("data/OSM/streets/streets.shp", "streets")
proj4string(streets) = CRS("+proj=longlat")
streets %<>% spTransform(projitaly)


street_intersection = raster::intersect(streets,milan_census)
street_intersection %>% spTransform(CRS("+proj=longlat"))  %>% saveRDS("app/milan_street_intersection.rds")

#' Amenities
amenities = read_csv("data/OSM/amenities.csv")
milan_census = readOGR("data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") 
milan_census %<>% spTransform(projitaly)

coordinates(amenities) = ~ lon + lat
proj4string(amenities) = CRS("+proj=longlat")
amenities %<>% spTransform(projitaly)


amenity_intersection_raw = raster::intersect(amenities,milan_census)
amenity_intersection_raw %<>% spTransform(CRS("+proj=longlat"))

cat_filter = names(rev(sort(abs(corr_depriv_cat[1,])))[1:6]) %>% gsub("\\_osm","", .)
amenity_intersection =  bind_cols(data.frame(lon = amenity_intersection_raw@coords[,1],
                                  lat = amenity_intersection_raw@coords[,2]),
                                  amenity_intersection_raw@data) %>% filter(amenity %in% cat_filter)

amenity_intersection %>% dplyr::select(ACE, lon, lat, amenity) %>% saveRDS("app/milan_amenities.rds")

classPal <- function(x) {
  #ffff33
  # palette = brewer.pal(name="Set1", n=5)
  palette = c("#E41A1C", "#377EB8", "#4DAF4A", "#ffff33", "#FF7F00")
  colorFactor(palette= palette, domain = x)
  
}
milan_census%<>% spTransform(CRS("+proj=longlat"))
leaflet() %>% addProviderTiles("CartoDB.DarkMatter")%>% 
  addPolygons(data = milan_census, fillColor = NA,, fillOpacity=0, color = "white",opacity = .6, weight=.4)%>%
  addCircles(lng= ~lon, lat= ~lat, weight = .5,radius = 100,color="white",
   fillColor = classPal(amenity_intersection$amenity)(amenity_intersection$amenity),
   fillOpacity =1, data= amenity_intersection)%>%
  addLegend(pal = classPal(amenity_intersection$amenity),
            values = amenity_intersection$amenity,
            position = "bottomleft",
            title = "amenity",
            opacity=.9
  )
# amenity_intersection %>% dplyr::select(ACE, lon, lat, amenity) %>% write_csv("data/OSM/most_correlated_amenities.csv")

# readShapeLines("data/OSM/mexico_city/amenities.shp")


 # library(geojsonio)

# milan_census %>% geojson_write(file = "app/milan.geojson")
# library(caret)
# 
# fitControl <- trainControl(
#   method = "cv",
#   number = 5)
# # 
# 
# # preProcValues <- preProcess(train, method = c("center", "scale"))
# # train.std <- predict(preProcValues, train)
# 
# set.seed(1)
# 
# glm.grid = expand.grid(alpha = seq(0,1,.2), lambda = exp(seq(-15, -2, 0.05))) # 
# glm.fit = train(deprivation ~ ., data = milan_census@data,
#                 method = "glmnet",
#                 trControl = fitControl,
#                 tuneGrid = glm.grid,
#                 preProcess = c("center", "scale"))
# glm.fit
# plot(glm.fit)
# glm.fit$bestTune
# 
# 
# 
# 
# rf.grid = expand.grid(mtry= seq(30,75,3)) #
# 
# rf.fit = train(deprivation ~ ., data = milan_census@data %>% dplyr::select(-ACE,-(density:work_force)), ntree = 1500,
#                method = "rf",
#                trControl = fitControl,
#                tuneGrid = rf.grid,
#                preProcess = c("center", "scale"))
# min(rf.fit$results$RMSE)
# importance(rf.fit$finalModel) %>% as.data.frame()%>%add_rownames("var") %>% ggplot() +
#   geom_bar(aes(x=reorder(var, IncNodePurity) , y =  IncNodePurity), stat = "identity") + coord_flip()
# 
# plot(rf.fit)
