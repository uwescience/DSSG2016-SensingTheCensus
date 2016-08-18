library(shiny)
library(leaflet)
library(maptools)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(ggthemes)
library(shinyBS)
library(shinyjs)
library(tidyr)
library(RColorBrewer)


milan_streets = readRDS("milan_street_intersection.rds")
mexico_streets = readRDS("mexico_city_street_intersection.rds")

milan_census = readRDS("milan.rds") 
mexico_census = readRDS("mexico_city.rds")

milan_amenities = readRDS("milan_amenities.rds")
mexico_amenities = readRDS("mexico_city_amenities.rds")

poly_ids = list("milan" = "ACE", "mexico_city" = "CVE_GEOAGE")
census = list("milan" = milan_census, "mexico_city" = mexico_census)
streets =  list("milan" = milan_streets, "mexico_city" = mexico_streets )
amenities = list("milan" = milan_amenities, "mexico_city" = mexico_amenities)


pal <- function(x) {
  n = 10
  quant_list = quantile(x, probs = seq(0, 1, 1/n))
  if (length(unique(quant_list)) == n+1)
    colorQuantile(c("black", "#99FFCC"), x, n = n, na.color = "black")
  else
    colorBin(c("black", "#99FFCC"), x, bins = n, na.color = "black")
  
  
}

pal2 <- function(x) {
  n = 10
  quant_list = quantile(x, probs = seq(0, 1, 1/n))
  
  if (length(unique(quant_list)) == n+1) 
    colorBin(c("black", "#EE82EE"), x, bins=quant_list, na.color = "black")
  else
    colorBin(c("black", "#EE82EE"), x, bins = n, na.color = "black")
  
}

classPal <- function(x) {
  #ffff33
  # palette = brewer.pal(name="Set1", n=5)
  palette = c("#E41A1C", "#377EB8", "#4DAF4A", "#ffff33", "#FF7F00")
  colorFactor(palette= palette, domain = x)
  
}

reverseList = function(hash_list){
  revMap = list()
  for(key in names(hash_list)){ revMap[[hash_list[[key]]]] = key }
  return(revMap)
}
# palette <- rev(brewer.pal(10, "RdYlBu")) #Spectral #RdYlBu
palette <- brewer.pal(10, "YlGnBu") #Spectral #RdYlBu

roadPal = function(x) {colorBin(palette = palette, domain = x, bins=quantile(x, probs = seq(0, 1, 0.1), na.rm=TRUE))}


milan_extent = milan_census@bbox
mexico_extent = mexico_census@bbox


cityMap = list("Milan" = "milan",
               "Mexico City" = "mexico_city")

milanCensusMap = list("deprivation" = "deprivation",
                      "unemployment" = "unemployment",
                      "% high school" =  "high_school",
                      "% illiteracy" = "illiteracy",
                      "% age > 65" =  "sixtyfive_plus",
                      "% foreigners" = "foreigners",
                      "% rented dwellings" = "rented_dwelling")

mexicoCensusMap = list("deprivation" = "IMU")

censusMap = list("milan" = milanCensusMap, "mexico_city" = mexicoCensusMap)

milanOSMMap = list()
milanOSMMap[["Closeness Centrality"]] = "closeness"
milanOSMMap[["Betweenness Centrality"]] = "betweenness"
for(name in milan_census@data%>% dplyr::select(ends_with("osm")) %>% names()){
  clean_name =name %>% gsub("_osm", "",.) %>% gsub("\\_", "",.) 
  key = paste(clean_name, "offering advantage")
  milanOSMMap[[key]] = name
}


mexicoOSMMap = list()

mexicoOSMMap[["Betweenness Centrality"]] = "betweenness"
mexicoOSMMap[["Closeness Centrality"]] = "closeness"
# for(name in mexico_census@data%>% dplyr::select(bank:school) %>% names()){
for(name in mexico_census@data%>% dplyr::select(ends_with("osm")) %>% names()){
  clean_name = name %>% gsub("idw_", "",.)  %>% gsub("_osm", "",.)  %>% gsub("\\_", "",.) 
  key = paste(clean_name, "offering advantage")
  
  mexicoOSMMap[[key]] = name 
}

osmMap = list("milan" = milanOSMMap, "mexico_city" = mexicoOSMMap)

defaults = list("milan" = "deprivation", "mexico_city" = "IMU")
defaults_osm = list("milan" = "closeness", "mexico_city" = "closeness")



shinyUI(
  bootstrapPage(
    tags$head(
      includeCSS("styles.css"),
      includeScript("L.Map.Sync.js"),
      includeScript("sync_maps.js")
    ),
    fluidRow(class="map-container",
             # h4("pending")
             column(6, class="left-side",
                    leafletOutput("deprivation_map", width = "100%", height = "100%"),
                    absolutePanel(top = 50, left = 45,h3(textOutput("census_map_title")))),
             column(6, class="right-side",
                    leafletOutput("variable_map", width = "100%", height = "100%"),
                    absolutePanel(top = 50, left = 45,h3(textOutput("variable_map_title"))))
             #        # absolutePanel(top = 50, left = 45,h3("Street-Network")))
             #
             
    ),
    absolutePanel(top = 130, id = "controls", class = "panel panel-default middle",
                  fixed = TRUE, draggable = TRUE,height = "auto", width =  "350px",
                  h4("city"),
                  selectInput("city_feature", NA, choices = cityMap, selected="milan"),
                  # conditionalPanel( condition = "input.city_feature == 'Milan'",
                  h4("census"),
                  selectInput("census_feature", NA, choices = milanCensusMap, selected="deprivation"),
                  # uiOutput("census_input"),
                  h5("density"),
                  plotOutput('census_density', height = 125),
                  # tableOutput('census_statistics'),
                  h4("OSM"),
                  selectInput("osm_feature", NA, choices = milanOSMMap, selected="deprivation"),
                  # checkboxInput("show_netwrok", "Show network?", value = TRUE),
                  # selectInput("categories", "Select descriptors to display",multiple = TRUE, selected = descriptors,
                  # choices = descriptors, selectize = FALSE, size=3),
                  
                  # scatterD3Output("scatter_plot", height = 200)
                  
                  h5("scatterplot"),
                  plotOutput("scatter_plot", height = 175)
                  # plotOutput("variable_distribution", height = 200)
                  
                  # plotOutput("response_season", height = 200)
                  # )
                  
                  
    ),
    absolutePanel(top = 0, left = 35,headerPanel("Crowdsensing the Census")),
    bsModal("detail-modal", textOutput("modal_title"), "tabBut", size = "large",
            useShinyjs(),
            leafletOutput("modal_map", width = "100%", height = "300px"),
            fluidRow(class="some-class",
                     # h4("pending")
                     column(6, class="left-side",
                            h4("OSM Distribution"),
                            plotOutput("selected_distribution", width = "100%", height = "250px")),
                     column(6, class="right-side",
                            h4("Deprivation rank"),
                            tableOutput("selected_rank"))
                     #        # absolutePanel(top = 50, left = 45,h3("Street-Network")))
                     #
            )
    )
  )
)