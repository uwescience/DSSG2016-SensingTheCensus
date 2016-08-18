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


source("app-utils.R")

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