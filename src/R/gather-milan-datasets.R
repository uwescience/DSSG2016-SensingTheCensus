library(readr)
library(rgdal)
library(dplyr)

source("src/R/utils.R")

#' Read milan datasets
milan_census = readOGR("data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") 
offering_advantage = read_csv("data/OSM/offering_advantage.csv")
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
                      temp_data %>% dplyr::select(arts_centre:waste))

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

milan_census %>% saveRDS("app/milan.rds")


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
