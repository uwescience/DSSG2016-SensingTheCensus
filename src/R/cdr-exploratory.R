library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(rgdal)
library(tidyr)
library(magrittr)

source("src/R/utils.R")
cdr = read_delim("data/CDR/sms/all-december.txt", delim = "\t",col_names = FALSE )
cdr_shape = readOGR("data/GeoJSON/CDR_join_output.geojson", "OGRGeoJSON") 

# dates = first + (1:(144*32))*hours(6)
cdr = cdr %>% transmute(cell_id = X1, 
                        date = as.POSIXct(as.numeric(as.character(X2))/1000,origin="1970-01-01"),
                        # date =X2,
                        day = day(date),
                        sms_in = X4,
                        sms_out = X5,
                        call_in = X6,
                        call_out = X7,
                        internet = X8
)



activity_by_tower = cdr %>% group_by(day, cell_id) %>% summarise_each(funs(sum(., na.rm=TRUE)), sms_in:internet)

call_in_long = activity_by_tower %>% ungroup() %>% dplyr::select(day, call_in, cell_id) %>% 
  spread(cell_id, call_in)  %>% dplyr::select(-day)


call_in_prcomp = prcomp(call_in_long[,apply(call_in_long, 2, var, na.rm=TRUE) != 0], scale=TRUE, center=TRUE)
call_in_pca = call_in_prcomp$rotation[,1]

cdr_shape@data = cdr_shape@data %>% left_join(data.frame(call_in_pca = unname(call_in_pca), cellId = as.numeric(names(call_in_pca))), by = "cellId")


leaflet_map(cdr_shape, "call_in_pca", "Call In Variation")
# activity = cdr %>% group_by(day) %>% summarise_each(funs(sum(., na.rm=TRUE)), sms_in:internet)

activity %>% mutate(date = dates)

ggplot(activity) + geom_line(aes(x = day, y = internet))
