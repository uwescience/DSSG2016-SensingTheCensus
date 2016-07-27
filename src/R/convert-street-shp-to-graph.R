library(readr)
library(dplyr)
library(maptools)
library(sp)
library(rgdal)
library(leaflet)
# library(GGally)
# library(ggplot2)
library(rgeos)
library(shp2graph)
library(magrittr)
# library(geojsonio)
library(RColorBrewer)

source("src/R/utils.R")

#' Read Street network shapefile
streets = readOGR("data/census/milroads/milroads.shp",layer = "milroads" ) %>%
  spTransform(CRS("+proj=utm +zone=32 +datum=WGS84 +units=m")) 

#' Remove unwanted paths
streets %<>%subset(!type %in% c("footway", "pedestrian", "cycleway", "elevator", "rest_area", "steps"))


# Plot street shapefile
# streets%>% spTransform(CRS("+init=epsg:4326")) %>% leaflet() %>%
  # addProviderTiles("CartoDB.Positron") %>%
  # addPolylines(weight = .4, color="black")

#' Create graph object from shapefile
street_graph_list = readshpnw(streets, ELComputed=TRUE, longlat=FALSE) 
street_graph = nel2igraph(street_graph_list[[2]],street_graph_list[[3]],
                          weight=street_graph_list[[4]],
                          eadf = street_graph_list[[5]])

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
bet = betweenness(street_graph, weight=E(street_graph)$weight)
close = closeness(street_graph, weight=E(street_graph)$weight)

V(street_graph)$degree = deg
V(street_graph)$closeness = close
V(street_graph)$betweenness = bet
V(street_graph)$eigen = eig


#' Plot closeness centrality
closeness_discrete = cut(close, 
                         breaks =c(quantile(close, probs = seq(0, 1, .1), max(close)), na.rm=TRUE),
                         right = FALSE,include.lowest=T) 

pallete = rev(brewer.pal(n = length(levels(closeness_discrete)), "RdYlBu"))
i = 1
for (level in levels(closeness_discrete)){
  print(i)
  print(level)
  level_nodes =  V(street_graph)[closeness_discrete == level]
  E(street_graph)[ from(level_nodes) ]$color <- pallete[i]
  i = i + 1
}

png("doc/plots/road_network_centrality_filtered.png",width = 3000, height=3000, res=500, bg = "black")
plot(street_graph, vertex.label=NA, vertex.size=.1,vertex.size2=.1, vertex.frame.color=NA, 
     edge.curved = FALSE, edge.color = E(street_graph)$color, edge.width = .4)
     # vertex.color = graphCol(close)(close), )
dev.off()


#' Create SpatialPoints from nodes
#' 
intersections_data_frame = get.data.frame(street_graph, what="vertices")

coordinates(intersections_data_frame)= ~ x +y
proj4string(intersections_data_frame ) = CRS("+proj=utm +zone=32 +datum=WGS84 +units=m")
intersections_data_frame %<>% spTransform(CRS("+init=epsg:4326"))

#' Save SpatialPointsDataFrame
writePointsShape(intersections_data_frame, "data/census/streets/street_intersections.shp")

##' Convert back the graph object to a shapefile
#' Create data frame with the information for all edges
street_data_frame = as_data_frame(street_graph)
street_data_frame %<>% 
  mutate(closeness = (vertex_attr(street_graph, "closeness", to)+
          vertex_attr(street_graph, "closeness", from))/2,
          betweenness = (vertex_attr(street_graph, "betweenness", to)+
                         vertex_attr(street_graph, "betweenness", from))/2,
          degree = (vertex_attr(street_graph, "degree", to)+
                           vertex_attr(street_graph, "degree", from))/2,
          eigen = (vertex_attr(street_graph, "eigen", to)+
                     vertex_attr(street_graph, "eigen", from))/2,
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
lines_df = SpatialLinesDataFrame(SpatialLines(lines_list), data=street_data_frame)
lines_df@data %<>% dplyr::select(-name)
proj4string(lines_df) = CRS("+proj=utm +zone=32 +datum=WGS84 +units=m")

lines_df %<>% spTransform(CRS("+init=epsg:4326"))

#' Save SpatialLinesDataFrame
writeLinesShape(lines_df, "data/census/streets/streets.shp")
 
#' Plot result into a map
palette <- rev(brewer.pal(10, "RdYlBu")) #Spectral #RdYlBu

roadPal = function(x) {colorQuantile(palette = palette, domain = x, n=10)}

leaflet(lines_df) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolylines(weight = 1, color=~roadPal(closeness)(closeness))

leaflet(lines_df) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolylines(weight = 1, color=~roadPal(betweenness)(betweenness)) 

