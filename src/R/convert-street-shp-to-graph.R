library(readr)
library(dplyr)
library(maptools)
library(sp)
library(rgdal)
# library(leaflet)
# library(GGally)
# library(ggplot2)
library(rgeos)
library(shp2graph)
# library(geojsonio)

source("src/R/utils.R")

streets = readOGR("data/census/milroads/milroads.shp",layer = "milroads" ) %>%
  spTransform(CRS("+proj=utm +zone=32 +datum=WGS84 +units=m"))



# Plot street shapefile
# streets%>% spTransform(CRS("+init=epsg:4326")) %>% leaflet() %>% 
#   addProviderTiles("CartoDB.Positron") %>% 
#   addPolylines(weight = .2, color="black")


street_graph_list = readshpnw(streets,ELComputed=TRUE) 
street_graph_ = nel2igraph(street_graph_list[[2]],street_graph_list[[3]],weight=.[[4]])

street_graph %>% 
  simplify() %>% 
  plot(vertex.label=NA, vertex.size=.1,vertex.size2=.1, edge.curved = FALSE)

eig_cent = eigen_centrality(street_graph, weight=E(street_graph)$weight)$vector


bet = betweenness(street_graph, weight=E(street_graph)$weight)

# fine = 500
# palette = colorRampPalette(c('red','green'))

# graphCol = palette(fine)[as.numeric(cut(bet,breaks = fine))]

graphCol = colorBin("YlOrRd", bet, n=10)


plot(street_graph, vertex.label=NA, vertex.size=.1,vertex.size2=.1, edge.curved = FALSE)


plot(street_graph)
fine = 500 # this will adjust the resolving power.
palette = colorRampPalette(c('red','green'))

#this gives you the colors you want for every point
graphCol = palette(fine)[as.numeric(cut(bet,breaks = fine))]
