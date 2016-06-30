library(raster)
library(dplyr)
library(rgdal)
library(leaflet)
library(GGally)

source("src/R/utils.R")

#' Load census and CDR geojson
census = readOGR("data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") %>%
  spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))
cdr = readOGR("data/GeoJSON/CDR_join_output.geojson", "OGRGeoJSON") %>%
  spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))

#' Intersect polygons
intersection = raster::intersect(x = census, y = cdr)

#' Calcualte area of each polygon
intersection@data$area = area(intersection)

#' Calculate area of each polygon
square_size = max(intersection@data$area)

#' Aggregate data into census areas summing the CDR data proportionally to the size of the squares
aggr_cdr = intersection@data %>% dplyr::select(ACE, internet:area, -cellId) %>% dplyr::group_by(ACE) %>% 
  summarise_each(funs(sum((area/square_size)*., na.rm=TRUE)), -area) %>% as.data.frame()

#' Join data to census polygons
census@data = census@data %>% left_join(aggr_cdr, by = "ACE")

census = spTransform(census, CRS("+init=epsg:4326"))

#' Map aggregated data
leaflet_map(census, "call_out", "Call Out")
leaflet_map(census, "call_in", "Call In")
leaflet_map(census, "internet", "Internet")


#' Create correlation plot between relevant features
census@data = census@data %>% mutate(high_school = P48/P1, 
                                 illiteracy = P52/P1, sixtyfive_plus = (P27 + P28 + P29)/P1,
                                 foreigners = ST15/P1,
                                 rented_dwelling = ifelse(A46 + A47+ A48 > 0, A46/(A46 + A47+ A48), NA),
                                 unemployment = P62/P60,
                                 work_force = P60/(P17 + P18 + P19 + P20 + P21 + P22 + P23 + P24 + P25 + P26 + P27 + P28 + P29),
                                 call_ratio = callIn/callOut
)


png("/data/doc/plots/census_cdr_corr.png", height = 1000, width = 1000)
corr_plot = ggpairs(census@data %>% select(internet:callOut, deprivation, high_school:work_force))
print(corr_plot)
dev.off()

## Try some regresions
summary(lm(deprivation ~  call_ratio, census@data))
summary(lm(deprivation ~ callIn, census@data))
summary(lm(deprivation ~ callIn + callOut + smsIn + smsOut + internet, census@data))
