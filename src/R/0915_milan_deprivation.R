library(readr)
library(rgdal)
library(dplyr)
library(magrittr)
library(geojsonio)


setwd("/Users/myeong/git/DSSG/")

milano_sez = readShapePoly("DSSG2016-SensingTheCensus/data/milan_sez_2011/Milano_SezC_2011.shp")
proj4string(milano_sez) = CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
milano_sez = spTransform(milano_sez, CRS("+init=epsg:4326"))

projitaly <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m")


source("DSSG2016-SensingTheCensus/src/R/utils.R")

census11 = multmerge("Project/dati-cpa_2011/Sezioni di Censimento", "R\\d", read_census_data)
census01 =multmerge("Project/dati-cpa_2001", "*.xls", read_census_data_old)


# df=read_census_data_old("Project/dati-cpa_2001/dati-cpa_2001_R13_DatiCPA_2001.xls")
# head(df$SEZ2001)
census01$SEZ2001=as.character(census01$SEZ2001)

unit="SEZ" #"AREA/SEZ"

###########2001 poverty#################
census_depriv01 = census01 %>% 
  transmute(high_school = P48/P1, 
            illiteracy = P52/P1, 
            sixtyfive_plus = (P27 + P28 + P29)/P1,
            #foreigners = ST15/P1, data not available in 2001
            #rented_dwelling = ifelse(A46 + A47+ A48 > 0, A46/(A46 + A47+ A48), NA),data not available in 2001
            unemployment = P62/P60,
            SEZ2001 = SEZ2001
  )

census_depriv01$illiteracy <- log(census_depriv01$illiteracy+10^-9)
census_depriv01$unemployment <- log(census_depriv01$unemployment +10^-9)


row.names(census_depriv01) <- census_depriv01$SEZ2001

depriv_pca = prcomp(~+ (high_school +illiteracy +unemployment + sixtyfive_plus), census_depriv01, center = TRUE, scale = TRUE, na.action = na.omit)
variance <- apply(depriv_pca$x, 2, var)  
props <- variance / sum(variance)
cum_variance = cumsum(props)
summary(depriv_pca)
data.frame(proportion = props, cumulative_proportion = cum_variance) %>% filter(row_number() %in% 1:10)

feature_importance = depriv_pca$rotation %>% as.data.frame() %>% dplyr::select(PC1) %>% add_rownames()%>% arrange(desc(abs(PC1)))
feature_importance 
depriv_index = depriv_pca$x[,1]


depriv_df01 =  data.frame(SEZ2001 = names(depriv_index), dep01 = unname(depriv_index))
depriv_df01$SEZ2001=as.character(depriv_df01$SEZ2001)

milano_sez@data <- milano_sez@data %>%
  mutate(SEZ2011 = as.character(SEZ2011)) %>%
  left_join(depriv_df01, by = c("SEZ2011" = "SEZ2001"))

# View(depriv_df01)
#############



census=census11#[census11$PROVINCIA=='Milano',]
#census=merge(census,abnbSEZ,by="SEZ2011")
census_depriv = census %>% 
  transmute(high_school = P48/P1, 
            illiteracy = P52/P1, sixtyfive_plus = (P27 + P28 + P29)/P1,
#             foreigners = ST15/P1,
#             rented_dwelling = ifelse(A46 + A47+ A48 > 0, A46/(A46 + A47+ A48), NA),
            unemployment = P62/P60,
         # airbnbMeanPrice= meanAirbnbPrice/(max(abnbSEZ$meanAirbnbPrice)),
          #  work_force = P60/(P17 + P18 + P19 + P20 + P21 + P22 + P23 + P24 + P25 + P26 + P27 + P28 + P29),
            uparea= as.numeric(ACE),
            P1 = P1
            )
census_depriv$illiteracy=log(census_depriv$illiteracy+10^-9)
# census_depriv$foreigners=log(census_depriv$foreigners +10^-9)
# census_depriv$rented_dwelling=log(census_depriv$rented_dwelling +10^-9 )
census_depriv$unemployment=log(census_depriv$unemployment +10^-9)
#census_depriv$airbnbMeanPrice=log(census_depriv$airbnbMeanPrice)
row.names(census_depriv) = census$SEZ2011
#View(census_depriv)
depriv_pca = prcomp(~+ (high_school +illiteracy  +unemployment + sixtyfive_plus), census_depriv, center = TRUE, scale = TRUE, na.action = na.omit)
variance <- apply(depriv_pca$x, 2, var)  
props <- variance / sum(variance)
cum_variance = cumsum(props)
# summary(depriv_pca)
data.frame(proportion = props, cumulative_proportion = cum_variance) %>% filter(row_number() %in% 1:10)

feature_importance = depriv_pca$rotation %>% as.data.frame() %>% dplyr::select(PC1) %>% add_rownames()%>% arrange(desc(abs(PC1)))
feature_importance 
depriv_index = depriv_pca$x[,1]
depriv_df =  data.frame(SEZ2011 = names(depriv_index), dep11 = unname(depriv_index))
depriv_df$SEZ2011=as.character(depriv_df$SEZ2011)


# final=merge(depriv_df01,depriv_df,by.x='SEZ2001',by.y='SEZ2011' )
# names(final)[names(final)=="SEZ2001"] <- "SEZ"
# names(final)[names(final)=="deprivation.x"] <- "dep01"
# names(final)[names(final)=="deprivation.y"] <- "dep11"

milano_sez@data <- milano_sez@data %>% left_join(depriv_df, by = c("SEZ2011"))

deprivation <- subset(milano_sez@data, select=c(SEZ2011, dep01, dep11))

# geojson_write(milano_sez, file = "DSSG2016-SensingTheCensus/data/GeoJSON/milano_deprivation_sez.geojson")

write_csv(deprivation, "DSSG2016-SensingTheCensus/data/census/temporal_deprivation.csv")

# final=NULL
# final=merge(census,depriv_df,by='SEZ2011' ,na.action = na.omit)
# final$SEZ2011=as.numeric(final$SEZ2011)
# summary(lm(final$dep11~final$dep01))
# 
# 
# if(unit=="AREA"){
# depfinal=aggregate(final, by=list(final$ACE), FUN=mean, na.rm=TRUE)
# depfinal <- depfinal[ -c(0,3:153) ]
# }else{
# depfinal=final
# }

#depfinal$deprivation=depfinal$deprivation - min(depfinal$deprivation) +10^-7
 
#summary(lm(depfinal$meanAirbnbPrice~log(depfinal$deprivation+10^-7 )))






##airbnb code####

# milanoSEZ = readOGR("data/GeoJSON/milano_census_sez.geojson", "OGRGeoJSON") 
# proj4string(milanoSEZ) = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
#  
# 
# airbnb=read.csv(file="~/Downloads/Milano Airbnb Listings (November 2014)-filtered.csv",sep=",",header=TRUE)
# #airbnb=readOGR("~/Downloads/Milano Airbnb Listings (November 2014)-filtered.kml")
# 
# proj4string(abnbsp) = CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=0.9996 +x_0=1500000 +y_0=0 
#                           +ellps=intl +units=m +no_defs ")
#  pts = cbind(airbnb$lat, airbnb$lon)
# 
#  
# df = data.frame(price = airbnb$price)
# abnbsp=SpatialPointsDataFrame(pts, df,match.ID = FALSE,proj4string =p4j ) # warn
# abnbsp=spTransform(abnbsp, CRS(" +proj=tmerc +lat_0=0 +lon_0=9 +k=0.9996 +x_0=1500000 +y_0=0 +ellps=intl +units=m +no_defs "))
#  
#  
# p4j <- CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0  ")
# 
# row.names(milano_ace) = milano_ace$ACE
#  df$id=sp::over(as(abnbsp,"SpatialPoints"),as(milano_ace,"SpatialPolygons"))

