library(readr)
library(maptools)
library(caret)
library(dplyr)
library(magrittr)
library(rgdal)

census = readOGR("data/census/mexico_city/mexico_city_census.shp", layer="mexico_city_census")
proj4string(census) = CRS("+proj=utm +zone=32 +datum=WGS84")
streetoa = read_csv("data/OSM/mexico_city/street_offering_advantage.csv")
centrality = read_csv("data/census/mexico_city/centrality_ageb.csv")
public_transport = read_csv("data/OSM/mexico_city/offering_advantage_public_transport_mexicocity.csv")
amenities = read_csv("data/OSM/mexico_city/mexico_amenity_pca.csv") %>% 
  dplyr::select(-arts_centre:-waste)
  # dplyr::select(-starts_with("PC"))

amenities[is.na(amenities)] = 0
public_transport[is.na(public_transport)] = 0

data = census@data %>% dplyr::select(CVE_GEOAGE, IMU) %>% 
  left_join(centrality, by = "CVE_GEOAGE") %>%
  left_join(amenities, by = "CVE_GEOAGE") %>%
  left_join(streetoa, by = "CVE_GEOAGE") %>%
  left_join(public_transport, by = "CVE_GEOAGE")
row.names(data) = data$CVE_GEOAGE
data %<>% dplyr::select(-CVE_GEOAGE) 
data[is.na(data)] = 0 
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


rf.grid = expand.grid(mtry= seq(1,30,3)) #

rf.fit = train(IMU ~ ., data = train, ntree = 1000,
               method = "rf",
               trControl = fitControl,
               tuneGrid = rf.grid,
               preProcess = c("center", "scale"))
min(rf.fit$results$RMSE)
importance(rf.fit$finalModel) %>% as.data.frame()%>%add_rownames("var") %>% ggplot() +
  geom_bar(aes(x=reorder(var, IncNodePurity) , y =  IncNodePurity), stat = "identity") + coord_flip()

plot(rf.fit)

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











#####################################################
########### PENDING: Spatial Regressions ############
#####################################################

library(spdep)
library(ggthemes)
library(ggplot2)


nb <- poly2nb(census,census$IMU)
weights_list <- nb2listw(nb, style="W", zero.policy = TRUE)
W = as.matrix(weights_list)
ptrW = trW(W)

census$IMU_lag = lag.listw(weights_list, census$IMU)

moran_plot = ggplot(data = census@data, aes(x=IMU, y = IMU_lag)) + 
  geom_vline(xintercept = 0,color="black", size=.5, alpha=.6, linetype="longdash")+
  geom_hline(yintercept = 0,color="black", size=.5, alpha=.6, linetype="longdash")+
  geom_point(color="#f8766d", size=1.3) +
  geom_smooth(method = "lm") +
  xlab("Marginalization Index") + ylab("Lagged Marginalization Index")  + theme_fivethirtyeight() + 
  labs(title="Moran Plot Social Deprivation Index")
moran_plot

moran.test(census$IMU, weights_list, zero.policy = T)
moran.test(census$IMU, weights_list,randomisation = FALSE, zero.policy=T)

localmoran(census$IMU, weights_list, zero.policy=T)
lm.morantest(lm(IMU~closeness+betweenness+PC1+PC2+PC3+PC4, data = scale(data)%>% as.data.frame(), listw = weights_list, zero.policy = T))

lm_i = lm(IMU~closeness+betweenness+PC1+PC2+PC3+PC4, data = scale(data)%>% as.data.frame())
res = sp.correlogram(nb, residuals(lm_i), order = 8, method = "I", style = "B", zero.policy = T)
plot(res)

moran.plot(residuals(lm_i), weights_list, quiet = TRUE, zero.policy = T)

# 
# lag.fit <- lagsarlm(IMU~closeness+betweenness+PC1+PC2+PC3+PC4, data = scale(data)%>% as.data.frame(), weights_list, trs = ptrW, zero.policy = T)
# summary(lag.fit)
# 
# err.fit <- errorsarlm(IMU~closeness+betweenness+PC1+PC2+PC3+PC4, data = census, listw = weights_list, zero.policy = T)
# summary(lag.fit)
# 

