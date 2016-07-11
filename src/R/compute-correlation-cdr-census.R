library(raster)
library(dplyr)
library(rgdal)
library(leaflet)
library(GGally)
library(ggplot2)

source("src/R/utils.R")

census = readOGR("data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") %>%
  spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))

time_day = c("morning", "day", "evening") 
week_day = c("weekend", "weekday")
names = c("smsIn", "smsOut","callIn","callOut","internet")

cdr_week = list()
for(day in time_day){
  for(week in week_day){
    cdr_name = paste(day, "_", week, sep = "")
    print(cdr_name)
    cdr_path = paste("data/GeoJSON/CDR_", cdr_name, ".geojson", sep ="")
    temp_cdr = readOGR(cdr_path, "OGRGeoJSON") %>%
      spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))
    names(temp_cdr@data)[names(temp_cdr@data) %in% names] = paste(names, cdr_name, sep="_")
    
    intersection = raster::intersect(x = census, y = temp_cdr)
    
    #' Calcualte area of each polygon
    intersection@data$area = area(intersection)
    
    #' Calculate area of each polygon
    square_size = max(intersection@data$area)
    
    #' Aggregate data into census areas summing the CDR data proportionally to the size of the squares
    aggr_cdr = intersection@data %>% dplyr::select(ACE, area , ends_with(cdr_name), -cellId) %>% dplyr::group_by(ACE) %>% 
      summarise_each(funs(sum((area/square_size)*., na.rm=TRUE)), -area) %>% as.data.frame()
    
    #' Join data to census polygons
    census@data = census@data %>% left_join(aggr_cdr, by = "ACE")
  }
}


census@data = census@data %>% 
  mutate(census_area = area(census),
    high_school = P48/P1, 
    illiteracy = P52/P1, sixtyfive_plus = (P27 + P28 + P29)/P1,
    foreigners = ST15/P1,
    rented_dwelling = ifelse(A46 + A47+ A48 > 0, A46/(A46 + A47+ A48), NA),
    unemployment = P62/P60,
    work_force = P60/(P17 + P18 + P19 + P20 + P21 + P22 + P23 + P24 + P25 + P26 + P27 + P28 + P29)
  ) %>%
  mutate_each(funs(norm(., P1), dens(., P1, census_area)), smsIn_morning_weekend:internet_evening_weekday)


census = spTransform(census, CRS("+init=epsg:4326"))



corr_plot = ggpairs(census@data %>% select(high_school:internet_evening_weekday_dens, deprivation),
                    lower = list(continuous = wrap("points", alpha = 0.3)))
ggsave("doc/plots/census_cdr_corr_2.png", corr_plot, dpi = 400, scale=4,height = 15, width = 15, limitsize = FALSE)

cor(census@data$deprivation, census@data %>% select(smsIn_morning_weekend_norm:internet_evening_weekday_dens))
# corr[,which.max(corr)]
