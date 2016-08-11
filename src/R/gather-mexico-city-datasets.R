library(readr)
library(rgdal)
library(dplyr)
library(sp)


source("src/R/utils.R")

#' Read milan datasets
mexicoproj = CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")

#' Read mexico  datasets
census = readShapePoly("data/census/mexico_city/mexico_city_census.shp")
proj4string(census) = mexicoproj

offering_advantage = read_csv("data/OSM/mexico_city/mexico_amenity_pca.csv")%>% 
  dplyr::select(CVE_GEOAGE,arts_centre:waste)
street_centrality = read_csv("data/census/mexico_city/centrality_ageb.csv")


#' Join datasets
temp_data = census@data %>% 
  dplyr::select(CVE_GEOAGE, P6A14NAE:VSREFRI,IMU, GMU) %>%
  left_join(offering_advantage, by = "CVE_GEOAGE") %>%


temp_data[is.na(temp_data)] = 0
variance = apply(temp_data,2,var)
variance[is.na(variance)] = -1
temp_data = temp_data[,variance!=0]

corr_depriv_cat = cor(temp_data %>% dplyr::select(P6A14NAE:VSREFRI,IMU) %>%mutate_each(funs(as.numeric(as.character(.)))),
                      temp_data %>% dplyr::select(arts_centre:waste))

most_correlated_amenities = 
  corr_depriv_cat[,abs(corr_depriv_cat["IMU",] )>.065] %>% as.data.frame() %>% names()


#' Join datasets
census@data %<>% 
  dplyr::select(CVE_GEOAGE, P6A14NAE:VSREFRI,IMU, GMU) %>%
  left_join(offering_advantage %>% dplyr::select(CVE_GEOAGE, one_of(most_correlated_amenities)), by = "CVE_GEOAGE") %>%
  # left_join(cdr, by = "ACE") %>%
  left_join(street_centrality, by = "CVE_GEOAGE")

census %>% spTransform(CRS("+proj=longlat")) %>% saveRDS("app/mexico_city.rds")
