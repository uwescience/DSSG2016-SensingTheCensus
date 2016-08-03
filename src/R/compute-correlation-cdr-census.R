library(raster)
library(dplyr)
library(rgdal)
library(leaflet)
library(GGally)
library(ggplot2)
library(ggthemes)
library(tidyr)

source("src/R/utils.R")

census = readOGR("data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") %>%
  spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))

cdr = readOGR("data/GeoJSON/CDR_join_output.geojson", "OGRGeoJSON") %>%
  spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))

time_day = c("morning", "day", "evening") 
week_day = c("weekend", "weekday")
names = c("smsIn", "smsOut","callIn","callOut","internet")


for(day in time_day){
  for(week in week_day){
    cdr_name = paste(day, "_", week, sep = "")
    print(cdr_name)
    cdr_path = paste("data/GeoJSON/CDR_", cdr_name, ".geojson", sep ="")
    temp_cdr = readOGR(cdr_path, "OGRGeoJSON") %>%
      spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))
    names(temp_cdr@data)[names(temp_cdr@data) %in% names] = paste(names, cdr_name, sep="_")
    
    #' Compute intersection polygons
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


census@data = get_deprivation_features(census) %>%
  mutate_each(funs(norm(., P1), dens(., P1, census_area)), 
              smsIn_morning_weekend:internet_evening_weekday)

census@data %>% 
  dplyr::select(ACE,smsIn_morning_weekend:internet_evening_weekday, 
                smsIn_morning_weekend_norm:internet_evening_weekday_dens) %>%
  write_csv("data/CDR/cdr_temporal_total_features.csv")


census = spTransform(census, CRS("+init=epsg:4326"))

# corr_plot = ggpairs(census@data %>% dplyr::select(high_school:internet_evening_weekday_dens, deprivation),
                    # lower = list(continuous = wrap("points", alpha = 0.3)))
# ggsave("doc/plots/census_cdr_corr_2.png", corr_plot, dpi = 400, scale=4,height = 15, width = 15, limitsize = FALSE)

corr_df = cor(census@data %>% mutate(well_being = deprivation) %>% dplyr::select(well_being, high_school:unemployment),  
              dplyr::select(census@data,smsIn_morning_weekend:internet_evening_weekday_dens)) %>% 
  as.data.frame() %>% add_rownames("census") %>%
  gather(cdr, correlation, smsIn_morning_weekend_dens:internet_evening_weekday_dens)
# corr[,which.max(corr)]


corr_plot = ggplot(corr_df)+aes(x = cdr, y=census) + geom_tile(aes(fill= correlation)) + coord_fixed()+
  geom_text(aes(label = round(correlation,2)))+
  scale_fill_gradient2(low="#e41a1c",mid = "white", high = "#377eb8", limits=c(-1, 1))+
  labs(title="CDR vs Census Correlation") + 
  # guide = guide_legend(direction = "vertical")) +
  theme_fivethirtyeight() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank(),
        legend.position="right",legend.direction = "vertical")

corr_plot
ggsave("doc/plots/correlation_cdr_deprivation.png", corr_plot, scale=2)


census_corr_plot = ggpairs(census@data %>% mutate(well_being = deprivation) %>% 
                             dplyr::select(well_being, high_school:unemployment),
                           title = "Well-being index vs Census Variables")+
  theme_fivethirtyeight() 
census_corr_plot

ggsave("doc/plots/correlation_deprivation_census.png", census_corr_plot, scale=1.5)


density_corr_plot = ggpairs(census@data %>% 
                             dplyr::select(density, smsIn_morning_weekend:callOut_morning_weekday),
                           title = "Well-being index vs Census Variables")+
  theme_fivethirtyeight() 

ggsave("doc/plots/correlation_density.png", density_corr_plot, scale=1.5)

