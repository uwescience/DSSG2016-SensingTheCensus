library(readr)
library(maptools)
library(caret)
library(dplyr)
library(magrittr)
library(rgdal)
library(randomForest)


census = readOGR("data/census/mexico_city/mexico_city_census.shp", layer="mexico_city_census")
proj4string(census) = CRS("+proj=utm +zone=32 +datum=WGS84")
streetoa = read_csv("data/OSM/mexico_city/street_offering_advantage.csv")
centrality = read_csv("data/census/mexico_city/centrality_ageb.csv")
public_transport = read_csv("data/OSM/mexico_city/publictrans_offering_advantage_idw.csv")
amenities = read_csv("data/OSM/mexico_city/amenities_offering_advantage_idw.csv")
  # dplyr::select(-starts_with("PC"))

nb <- poly2nb(cens us,census$IMU)
weights_list <- nb2listw(nb, style = "W", zero.policy = TRUE)
print(weights_list, zero.policy=T)
W = as.matrix(weights_list)
ptrW = trW(W)

census$IMU_lag = lag.listw(weights_list, census$IMU, zero.policy = T)
lag.plot(census$IMU, lags =1)
imulag = as.data.frame(census$IMU_lag)



public_transport %<>% dplyr::select(CVE_GEOAGE:idw_stop_position)
amenities %<>% dplyr::select(CVE_GEOAGE:idw_waste)


amenities[is.na(amenities)] = 0
public_transport[is.na(public_transport)] = 0

data = census@data %>% dplyr::select(CVE_GEOAGE, IMU, IMU_lag) %>% 
  left_join(centrality, by = "CVE_GEOAGE") %>%
  left_join(amenities, by = "CVE_GEOAGE") %>%
  left_join(streetoa, by = "CVE_GEOAGE") %>%
  left_join(public_transport, by = "CVE_GEOAGE")


row.names(data) = data$CVE_GEOAGE
data[is.na(data)] = 0 
data %<>% dplyr::select(IMU:idw_stop_position)

set.seed(1)
train = data %>% sample_frac(.75)
test =  data %>% setdiff(train)



fitControl <- trainControl(
  method = "cv",
  number = 5)
# 

class(data)



# preProcValues <- preProcess(train, method = c("center", "scale"))
# train.std <- predict(preProcValues, train)

set.seed(1)

glm.grid = expand.grid(alpha = seq(0,1,.2), lambda = exp(seq(-10, -1, 0.1))) # 
glm.fit = train(IMU ~ , data = train,
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

baseline1 = lm(IMU ~ .-IMU_lag, data = train)
baseline2 = lm(IMU ~IMU_lag, data = train)
summary(baseline1)

predictions <- predict(baseline1, train)
# summarize accuracy
rmse <- mean((train$IMU - predictions)^2)
print(rmse)


predictions2 <- predict(baseline2, train)
# summarize accuracy
rmse <- mean((train$IMU - predictions2)^2)
print(rmse)


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




#

#####################################################
########### PENDING: Spatial Regressions ############
#####################################################

library(spdep)
library(ggthemes)
library(ggplot2)
library(spgwr)
library(nlme)
library(gstat)
library(car)
library(sp)

census@data = data.frame(census@data, data[match(census@data[,"CVE_GEOAGE"], data[,"CVE_GEOAGE"]),])
nb <- poly2nb(census,census$IMU)
weights_list <- nb2listw(nb, style = "W", zero.policy = TRUE)
print(weights_list, zero.policy=T)
W = as.matrix(weights_list)
ptrW = trW(W)

census$IMU_lag = lag.listw(weights_list, census$IMU, zero.policy = T)
lag.plot(census$IMU, lags =1)
imulag = as.data.frame(census$IMU_lag)

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
#lm.morantest(lm(IMU~closeness+betweenness+PC1+PC2+PC3+PC4, data = scale(data)%>% as.data.frame(), W, zero.policy = T))

#calc plot correlogram for spatial autocorrelation
lm_i = lm(IMU~closeness+betweenness+PC1+PC2+PC3+PC4, census@data)
lm.morantest(lm_i, weights_list, zero.policy = TRUE)
res = sp.correlogram(nb, census@data$IMU, order = 8, method = "I", style = "B", zero.policy = T)
boxplot(res$res)
plot(res)

md = as.data.frame(census@data)
scatterplotMatrix(census@data[,29:34])
##Test for proper spatial weights model ?

lm1 <- localmoran(census$IMU, listw=weights_list, zero.policy=T)
census@data$lm1 <- abs(lm1[,4]) ## Extract z-scores

lm3 <- localmoran.sad(lm(IMU~closeness+betweenness+PC1+PC2+PC3+PC4,census@data), nb=nb, style="W", zero.policy=T)
head(lm3)

censusdata.lagrange<-lm.LMtests(lm_i, weights_list, zero.policy = T,test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
print(censusdata.lagrange)

census_lag <- lagsarlm(IMU ~ betweenness + PC1 + PC2 + PC3, data = census@data, weights_list, zero.policy = T, na.action=na.omit)
summary(census_lag)

?lagsarlm


?scale
test1$close = md$closeness
test1$betw = md$betweenness3
test1$PC1 = md$PC1

corrl = cor(test1[,4:6])
corrplot(corrl, method = "color", tl.col="black", number.font=1, col= colorRampPalette(c("green", "white", "blue"))((14)))


# lag.fit <- lagsarlm(IMU~closeness+betweenness+PC1+PC2+PC3+PC4, data = scale(data)%>% as.data.frame(), weights_list, trs = ptrW, zero.policy = T)

# summary(lag.fit)
# 
# err.fit <- errorsarlm(IMU~closeness+betweenness+PC1+PC2+PC3+PC4, data = census, listw = weights_list, zero.policy = T)
# summary(lag.fit)

# bwg = gwr.sel(IMU ~ closeness + betweenness + street_density + PC1 + PC2 + PC3 + PC4, data = census, gweight = gwr.Gauss, verbose = FALSE, adapt = T)
# gwrG <- gwr(IMU ~closeness + betweenness + street_density + PC1 + PC2 + PC3 + PC4, data = census, bandwidth = bwg, gweight = gwr.Gauss, hatmatrix = TRUE)
# gwrG



