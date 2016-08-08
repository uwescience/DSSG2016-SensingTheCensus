library(readr)
library(maptools)
library(caret)
library(dplyr)
library(magrittr)

census = readOGR("data/census/mexico_city/mexico_city_census.shp", layer="mexico_city_census")
proj4string(census) = CRS("+proj=utm +zone=32 +datum=WGS84")
centrality = read_csv("data/census/mexico_city/centrality_ageb.csv")
amenities = read_csv("data/OSM/mexico_city/mexico_amenity_pca.csv") %>% 
  dplyr::select(-arts_centre:-waste)
  # dplyr::select(-starts_with("PC"))

amenities[is.na(amenities)] = 0
data = census@data %>% dplyr::select(CVE_GEOAGE, IMU) %>% 
  left_join(centrality, by = "CVE_GEOAGE") %>%
  left_join(amenities, by = "CVE_GEOAGE") 
row.names(data) = data$CVE_GEOAGE
data %<>% dplyr::select(-CVE_GEOAGE) 

set.seed(1)
train = data %>% sample_frac(.75)
test =  data %>% setdiff(train)

fitControl <- trainControl(
  method = "cv",
  number = 5)
# 

# preProcValues <- preProcess(train, method = c("center", "scale"))
# train.std <- predict(preProcValues, train)

set.seed(1)

glm.grid = expand.grid(alpha = seq(0,1,.2), lambda = exp(seq(-10, -1, 0.1))) # 
glm.fit = train(IMU ~ ., data = train,
                method = "glmnet",
                trControl = fitControl,
                tuneGrid = glm.grid,
                preProcess = c("center", "scale"))
glm.fit
plot(glm.fit)
glm.fit$bestTune

model = glm.fit$finalModel
summary(model)
glm.fit2 = train(IMU ~ .*., data = train,
                method = "glmnet",
                trControl = fitControl,
                tuneGrid = glm.grid,
                preProcess = c("center", "scale"))
plot(glm.fit2)

test = glmnet(x = dplyr::select(train,closeness, betweenness) %>% as.matrix(),
              y =  train$IMU,
              alpha = 1 ,nlambda=1000)

plot(test)

glm.fit2$bestTune


rf.grid = expand.grid(mtry= seq(1,24,4)) #

rf.fit = train(IMU ~ ., data = train,
               method = "rf",
               trControl = fitControl,
               tuneGrid = rf.grid,
               preProcess = c("center", "scale"))
min(rf.fit$results$RMSE)
importance(rf.fit$finalModel) %>% as.data.frame()%>%add_rownames("var") %>% ggplot() +
  geom_bar(aes(x=reorder(var, IncNodePurity) , y =  IncNodePurity), stat = "identity") + coord_flip()

rf.fit2 = train(IMU ~ .*., data = train,
               method = "rf",
               trControl = fitControl,
               ntree = 1000,
               tuneGrid = rf.grid,
               preProcess = c("center", "scale"))
plot(rf.fit2) 
min(rf.fit2$results$RMSE)


svm.grid = expand.grid(C= seq(-1,2,.2), sigma = c(1,10,20)) #

svm.fit = train(IMU ~ ., data = train,
               method = "svmRadial",
               trControl = fitControl,
               tuneGrid = svm.grid,
               preProcess = c("center", "scale"))

plot(svm.fit)
min(rf.fit2$results$RMSE)

