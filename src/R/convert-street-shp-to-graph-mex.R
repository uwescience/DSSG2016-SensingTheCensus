library(sp)
library(readr)
library(dplyr)
library(maptools)
library(rgdal)
library(leaflet)
library(GGally)
library(ggplot2)
library(rgeos)
library(geojsonio)
library(magrittr)
library(stringr)
library(shp2graph)

library(htmlwidgets)

library(RColorBrewer)


source("src/R/utils.R")


streets = readOGR("data/geography/mexico_city/streets/mexico_city_streets.shp", layer = "mexico_city_streets") 
# proj4string(streets) = CRS("+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

street_graph_list = readshpnw(streets, ELComputed=TRUE, longlat=FALSE) 
street_graph = nel2igraph(street_graph_list[[2]],street_graph_list[[3]],
                          weight=street_graph_list[[4]])
# eadf = street_graph_list[[5]])

#' Remove self loops
street_graph %<>% simplify()

#' Plot results
# street_graph %>%
# simplify() %>% 
# plot(vertex.label=NA, vertex.size=.1,vertex.size2=.1, edge.curved = FALSE)
# 

#' COmpute some centrality measures
eig = eigen_centrality(street_graph, weight=E(street_graph)$weight)$vector
deg = degree(street_graph)
bet = betweenness.estimate(street_graph, cutoff=20000, directed=FALSE)
close = closeness.estimate(street_graph, cutoff=20000)

V(street_graph)$degree = deg
V(street_graph)$closeness = close
V(street_graph)$betweenness = bet
V(street_graph)$eigen = eig


#' Create SpatialPoints from nodes
#' 
intersections_data_frame = get.data.frame(street_graph, what="vertices")

coordinates(intersections_data_frame)= ~ x +y
proj4string(intersections_data_frame ) = CRS("+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
intersections_data_frame %<>% spTransform(CRS("+init=epsg:4326"))

#' Save SpatialPointsDataFrame
writePointsShape(intersections_data_frame, "data/geography/mexico_city/streets/street_intersections.shp")

##' Convert back the graph object to a shapefile
#' Create data frame with the information for all edges
street_data_frame = as_data_frame(street_graph)
street_data_frame %<>% 
  mutate(
          closeness = (vertex_attr(street_graph, "closeness", to)+
          vertex_attr(street_graph, "closeness", from))/2,
         betweenness = (vertex_attr(street_graph, "betweenness", to)+
                          vertex_attr(street_graph, "betweenness", from))/2,
         # degree = (vertex_attr(street_graph, "degree", to)+
         #             vertex_attr(street_graph, "degree", from))/2,
         # eigen = (vertex_attr(street_graph, "eigen", to)+
         #            vertex_attr(street_graph, "eigen", from))/2,
         from.lon =vertex_attr(street_graph, "x", from),
         from.lat =vertex_attr(street_graph, "y", from),
         to.lon =vertex_attr(street_graph, "x", to),
         to.lat =vertex_attr(street_graph, "y", to)
  )%>% 
  add_rownames("id")

#' Create SpatialLinesDataFrame
lines_list = apply(street_data_frame, 1,function(row){
  Lines(
    Line(
      rbind(as.numeric(c(row["from.lon"], row["from.lat"])),
            as.numeric(c(row["to.lon"], row["to.lat"])))
    ),
    ID= row["id"])
})

spatial_lines = SpatialLines(lines_list)
lines_df = SpatialLinesDataFrame(SpatialLines(lines_list), data=as.data.frame(street_data_frame))
proj4string(lines_df) = CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")

lines_df %<>% spTransform(CRS("+init=epsg:4326"))


palette <- rev(brewer.pal(10, "RdYlBu")) #Spectral #RdYlBu

roadPal = function(x) {colorQuantile(palette = palette, domain = x, n=10)}


bet_map = leaflet(lines_df) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolylines(weight = .5, color=~roadPal(betweenness)(betweenness)) 
bet_map

close_map = leaflet(lines_df) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolylines(weight = .5, color=~roadPal(closeness)(closeness)) 
close_map
saveWidget(bet_map, paste(getwd(),"/doc/plots/mex_street_betweeness.html", sep=""), selfcontained = TRUE)
