library(raster)

source("src/R/utils.R")

census = readOGR("data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") %>%
  spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))
cdr = readOGR("data/GeoJSON/Nov1_CDR_aggregation.json", "OGRGeoJSON") %>%
  spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))

intersection = raster::intersect(x = census, y = cdr)
intersection@data$area = area(intersection)

aggr_cdr = intersection@data %>% dplyr::select(ACE, call_out:area, -cellId) %>% dplyr::group_by(ACE) %>% 
  summarise_each(funs(weighted.mean(., area, na.rm=TRUE)), -area) %>% as.data.frame()

aggr_cdr

census@data = census@data %>% left_join(aggr_cdr, by = "ACE")

census = spTransform(census, CRS("+init=epsg:4326"))

leaflet(census) %>% addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(fillColor = ~pal(call_in)(call_in), weight = .2, color="white",fillOpacity = 0.6) %>%
  addLegend(pal = pal(census$call_in),
            values = ~call_in,
            position = "bottomleft",title = "Call in"
  )


leaflet(census) %>% addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(fillColor = ~pal(call_out)(call_out), weight = .2, color="white",fillOpacity = 0.6) %>%
  addLegend(pal = pal(census$call_out),
            values = ~call_out,
            position = "bottomleft",title = "Call out"
  )
