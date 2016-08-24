shinyUI(
  bootstrapPage(
    tags$head(
      includeCSS("www/css/styles.css"),
      includeScript("www/js/L.Map.Sync.js"),
      includeScript("www/js/sync_maps.js")
    ),
    fluidRow(class="map-container",
             #' Left map panel
             column(6, class="left-side",
                    leafletOutput("deprivation_map", width = "100%", height = "100%"),
                    absolutePanel(top = 50, left = 45,h3(textOutput("census_map_title")))),
             #' Right map panel
             column(6, class="right-side",
                    leafletOutput("variable_map", width = "100%", height = "100%"),
                    absolutePanel(top = 50, left = 45,h3(textOutput("variable_map_title"))))

    ),
    #' Controls panel
    absolutePanel(top = 130, id = "controls", class = "panel panel-default middle",
                  fixed = TRUE, draggable = TRUE,height = "auto", width =  "350px",
                  h4("city"),
                  selectInput("city_feature", NA, choices = cityMap, selected="milan"),
                  h4("census"),
                  selectInput("census_feature", NA, choices = milanCensusMap, selected="deprivation"),
                  h5("density"),
                  plotOutput('census_density', height = 125),
                  h4("OSM"),
                  selectInput("osm_feature", NA, choices = milanOSMMap, selected="deprivation"),
                  h5("scatterplot"),
                  plotOutput("scatter_plot", height = 175)
                  
                  
    ),
    #' Modal panel. It opens after a polygon click
    absolutePanel(top = 0, left = 35, headerPanel("Crowdsensing the Census")),
    bsModal("detail-modal", textOutput("modal_title"), "tabBut", size = "large",
            useShinyjs(),
            leafletOutput("modal_map", width = "100%", height = "300px"),
            fluidRow(class="some-class",
                     column(6, class="left-side",
                            h4("OSM Distribution"),
                            plotOutput("selected_distribution", width = "100%", height = "250px")),
                     column(6, class="right-side",
                            h4("Deprivation rank"),
                            tableOutput("selected_rank"))
            )
    )
  )
)