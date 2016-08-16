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

# offering_advantage = read_csv("data/OSM/mexico_city/mexico_amenity_pca.csv")%>% 
#   dplyr::select(CVE_GEOAGE,arts_centre:waste)
offering_advantage = read_csv("data/OSM/mexico_city/amenities_offering_advantage_idw.csv") 
names(offering_advantage)[-1] = paste(names(offering_advantage), "osm", sep= "_")[-1]

public_transport =  read_csv("data/OSM/mexico_city/publictrans_offering_advantage_idw.csv") %>%
  dplyr::select(CVE_GEOAGE, idw_platform, idw_station, idw_stop_area)
names(public_transport)[-1] = paste(names(public_transport), "osm", sep= "_")[-1]



street_centrality = read_csv("data/census/mexico_city/centrality_ageb.csv")


#' Join datasets
temp_data = census@data %>% 
  dplyr::select(CVE_GEOAGE, P6A14NAE:VSREFRI,IMU, GMU) %>%
  left_join(offering_advantage, by = "CVE_GEOAGE")


temp_data[is.na(temp_data)] = 0
variance = apply(temp_data,2,var)
variance[is.na(variance)] = -1
temp_data = temp_data[,variance!=0]

corr_depriv_cat = cor(temp_data %>% dplyr::select(P6A14NAE:VSREFRI,IMU) %>%mutate_each(funs(as.numeric(as.character(.)))),
                      temp_data %>% dplyr::select(ends_with("osm")))

most_correlated_amenities = 
  corr_depriv_cat[,abs(corr_depriv_cat["IMU",] )>.205] %>% as.data.frame() %>% names()


#' Join datasets
census@data %<>% 
  dplyr::select(CVE_GEOAGE, P6A14NAE:VSREFRI,IMU, GMU) %>%
  left_join(offering_advantage %>% dplyr::select(CVE_GEOAGE, one_of(most_correlated_amenities)), by = "CVE_GEOAGE") %>%
  # left_join(cdr, by = "ACE") %>%
  left_join(street_centrality, by = "CVE_GEOAGE") %>%
  left_join(public_transport, by = "CVE_GEOAGE")


census = spChFIDs(census, as.character(census@data$CVE_GEOAGE))

#' Save output
census %>% spTransform(CRS("+proj=longlat")) %>% saveRDS("app/mexico_city.rds")


# streets = readOGR("data/geography/mexico_city/streets/mexico_city_streets.shp", "mexico_city_streets")
intersection = readOGR("data/OSM/mexico_city_streets/street_intersections_census.shp", 
                       layer = "street_intersections_census")
proj4string(intersection) = mexicoproj
intersection %<>% spTransform(CRS("+proj=longlat")) %>% saveRDS("app/mexico_city_street_intersection.rds")



#' Amenities

amenities = readOGR("data/OSM/mexico_city/amenities.shp","amenities")

amenities %<>% spTransform(mexicoproj)


amenity_intersection_raw = raster::intersect(amenities,census)
amenity_intersection_raw %<>% spTransform(CRS("+proj=longlat"))

cat_filter = names(rev(sort(abs(corr_depriv_cat[1,])))[1:6]) %>% gsub("\\_osm","", .)%>% gsub("idw\\_","", .)
amenity_intersection =  bind_cols(data.frame(lon = amenity_intersection_raw@coords[,1],
                                             lat = amenity_intersection_raw@coords[,2]),
                                  amenity_intersection_raw@data) %>% filter(amenity %in% cat_filter)

amenity_intersection %>% dplyr::select(CVE_GEOAGE, lon, lat, amenity) %>% saveRDS("app/mexico_city_amenities.rds")
amenity_intersection %>% dplyr::select(CVE_GEOAGE, lon, lat, amenity) %>% write_csv("data/OSM/mexico_city/most_corralted_amenities.csv")

