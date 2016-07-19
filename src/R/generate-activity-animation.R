library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(rgdal)
library(tidyr)
library(magrittr)
library(ggmap)
library(maptools)
library(gganimate)
library(gtools)
library(ggthemes)
library(stringr)

source("src/R/utils.R")
cdr = read_delim("data/CDR/all-december.txt", delim = "\t",col_names = FALSE ) %>% 
  bind_rows(read_delim("data/CDR/all-november.txt", delim = "\t",col_names = FALSE ))

cdr_shape = readOGR("data/GeoJSON/CDR_join_output.geojson", "OGRGeoJSON") 

census = readOGR("data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") %>%
  spTransform(CRS("+proj=utm +zone=32 +datum=WGS84 +units=m"))


cdr %<>% transmute(cell_id = X1, 
                   date = as.POSIXct(as.numeric(as.character(X2))/1000,origin="1970-01-01",tz = "GMT"),
                   # date =X2,
                   day = day(date),
                   hour = hour(date),
                   weekday = ifelse(wday(date)%in%2:6, "weekday", "weekend"),
                   smsIn = X4,
                   smsOut = X5,
                   callIn = X6,
                   callOut = X7,
                   internet = X8
) 


wday_tower_activity_hour = cdr %>% group_by(day, hour, cell_id, weekday) %>%
  summarise_each(funs(sum(., na.rm=TRUE)), smsIn:internet) %>%
  group_by(hour, cell_id, weekday) %>% 
  summarise_each(funs(median(., na.rm=TRUE)), smsIn:internet)


temp = cdr_shape %>% spTransform(CRS("+proj=utm +zone=32 +datum=WGS84 +units=m"))
temp_intersect = raster::intersect(x = census, y = temp) %>% 
  spTransform(CRS("+init=epsg:4326"))

bbox = {temp_intersect %>% summary()}$bbox %>% c()


fortified_df = temp_intersect %>% fortify(region = "cellId") %>% mutate(id = as.numeric(id)) %>%
  left_join(wday_tower_activity_hour %>% select(cell_id, callOut, weekday), by=c("id" = "cell_id")) %>% 
  mutate(cat_callOut = quantcut(callOut, q=9))


map = get_map(bbox,source="osm", color="bw")

activity_animation = ggmap(map) + 
  geom_polygon(aes(x = long, y = lat, fill=cat_callOut, group = id, frame = paste("Call Out",str_pad(hour, 2, pad = "0"), "hr")), data = fortified_df, alpha =.7 )+
  scale_fill_brewer(palette = "Blues") + facet_grid(.~weekday) + theme_fivethirtyeight()
gg_animate(activity_animation,"doc/plots/activity_by_weekday.gif", interval = .5, ani.width = 1350, ani.height = 700) 




