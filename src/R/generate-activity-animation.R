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

# fortified_df = temp_intersect %>% fortify(region = "cellId") %>% mutate(id = as.numeric(id)) %>%
#   left_join(wday_tower_activity_hour %>% select(cell_id, callOut, weekday), by=c("id" = "cell_id")) %>%
#   mutate(cat_callOut = quantcut(callOut, q=9))
# 
# 
# map = get_map(bbox,source="osm", color="bw")
# 
# activity_animation = ggmap(map) + 
#   geom_polygon(aes(x = long, y = lat, fill=cat_callOut, group = id, frame = paste("Call Out",str_pad(hour, 2, pad = "0"), "hr")), data = fortified_df, alpha =.7 )+
#   scale_fill_brewer(palette = "Blues") + facet_grid(.~weekday) + theme_fivethirtyeight()
# gg_animate(activity_animation,"doc/plots/activity_by_weekday.gif", interval = .5, ani.width = 1350, ani.height = 700)

map = get_map(bbox, source="osm", color="bw")

generate_gif_by_var = function(var, title){
  print("Fortifying and adding data")
  fortified_df = 
    temp_intersect %>% 
    fortify(region = "cellId") %>% 
    mutate(id = as.numeric(id)) %>%
    left_join(wday_tower_activity_hour %>% select_("cell_id", var, "weekday"), by=c("id" = "cell_id")) %>% 
    mutate_(var = var) %>% 
    mutate(cat_var = quantcut(var, q=9),
           hour_str = paste("Median", title, str_pad(hour, 2, pad = "0"), "hr"))
  
  print("Creating map")
  activity_animation = ggmap(map) + 
    geom_polygon(aes(x = long, y = lat, fill=cat_var, group = id, frame = hour_str), data = fortified_df, alpha =.7 )+
    scale_fill_brewer(title, palette = "Blues") + facet_grid(.~weekday) + theme_fivethirtyeight()
  
  print("Saving Gif")
  gg_animate(activity_animation, paste("doc/plots/", var, "_activity_by_weekday.gif", sep = ""), interval = .5, ani.width = 1300, ani.height = 700)
  
}

comm_cat = list(list("callOut", "Call Out"), list("callIn", "Call In"), 
         list("smsIn", "Sms In"), list("smsOut", "Sms Out"),
         list("internet", "Internet"))

for(cat in comm_cat){ generate_gif_by_var(cat[[1]], cat[[2]]) }




fortified_df =
  temp_intersect %>%
  fortify(region = "cellId") %>%
  mutate(id = as.numeric(id)) %>%
  left_join(wday_tower_activity_hour %>% gather(metric, value, smsIn:internet), by=c("id" = "cell_id")) %>%
  group_by(metric) %>%
  mutate(cat_var = quantcut(value, q=9)) %>%
  ungroup() %>%
  mutate(hour_str = paste("Median Activity", str_pad(hour, 2, pad = "0"), "hr"))


arrangeplot = ggmap(map) +
  geom_polygon(aes(x = long, y = lat, fill=cat_var, group = id, frame = hour_str), data = fortified_df, alpha =.7 )+
  scale_fill_brewer(title, palette = "Blues") + 
  facet_grid(weekday~metric) + theme_fivethirtyeight() + 
  theme(legend.position="none")
gg_animate(arrangeplot, paste("doc/plots/activity_by_weekday.gif", sep = ""), interval = .5, ani.width = 700, ani.height = 1600)
gg_animate(arrangeplot, paste("doc/plots/activity_by_weekday.gif", sep = ""), interval = .5, ani.width = 1200, ani.height = 525, ani.res =600)

