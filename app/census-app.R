library(shiny)
library(leaflet)
library(maptools)
library(spatialEco)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(rgdal)


# setwd("app")

milan_streets = readRDS("milan_street_network.rds") 
milan_streets = SpatialLinesDataFrame(simple_streets,milan_streets@data)

milan_census = readOGR("milano_census_ace.geojson", "OGRGeoJSON") 


pal <- function(x) {colorBin(c("black", "#99FFCC"), x, bins=quantile(x, probs = seq(0, 1, 0.1), na.rm=TRUE),na.color = "black")}
palette <- rev(brewer.pal(10, "RdYlBu")) #Spectral #RdYlBu

roadPal = function(x) {colorQuantile(palette = palette, domain = x, n=10)}





extent = milan_streets@bbox

cityMap = list("Milan" = "milan",
               "Mexico City" = "mexico_city")

censusMap = list("well-being" = "deprivation",
                "Median Rent" = "median_rent")

osmMap = list("Closeness Centrality" = "closeness_centrality",
                 "Median Rent" = "median_rent")

# namesMap = data.frame(var= c("median_income", "median_rent"), name = c("Median Income", "Median Rent"), short_name =c("Income", "Rent"))

ui = bootstrapPage(
  tags$head(
    includeCSS("styles.css"),
    includeScript("L.Map.Sync.js"),
    includeScript("sync_maps.js")
  ),
  fluidRow(class="map-container",
           column(6, class="left-side",
                  leafletOutput("deprivation_map", width = "100%", height = "100%"),
                  absolutePanel(top = 50, left = 45,h3("Well-being"))),
           column(6, class="right-side",
                  leafletOutput("variable_map", width = "100%", height = "100%"),
                  # absolutePanel(top = 50, left = 45,h3(textOutput("variable_map_title"))))
                  absolutePanel(top = 50, left = 45,h3("Street-Network")))
           
           
  ),
  absolutePanel(top = 130, right = "35%", id = "controls", class = "panel panel-default",
                fixed = TRUE, draggable = TRUE,height = "350px", width =  "300px",
                h4("city"),
                selectInput("city_feature", NA,
                            choices = names(cityMap)),
                h4("census"),
                selectInput("census_feature", NA,
                            choices = names(censusMap)),
                h4("OSM"),
                selectInput("osm_feature", NA,
                            choices = names(osmMap)),
                checkboxInput("show_netwrok", "Show network?", value = TRUE)
                # selectInput("categories", "Select descriptors to display",multiple = TRUE, selected = descriptors,
                # choices = descriptors, selectize = FALSE, size=3),
                
                # plotOutput("response_variable_points", height = 200)
                # plotOutput("response_distribution", height = 200),
                # plotOutput("variable_distribution", height = 200)
                # 
                # plotOutput("response_season", height = 200)
  ),
  absolutePanel(top = 0, left = 35,headerPanel("Crowdsensing the Census"))
)


server <- function(input, output, session) {
  # Reactive expression to subset data
  # selectedFeature <- reactive({
  #   selected = input$feature
  #   
  #   return(list(feature = namesMap[[selected]], name = selected))
  #   
  #   
  # })
  # selectedCategories <- reactive({
  #   cat = input$categories
  # 
  #   # return(list(feature = namesMap[[selected]], name = selected))
  # 
  # })
  output$deprivation_map <- renderLeaflet({
    # Aaspects of the map that  won't need to change dynamically
    
    leaflet(milan_census) %>% 
      addProviderTiles("CartoDB.DarkMatter") %>%
      setView(lng= mean(extent[1,]), lat = mean(extent[2,]),zoom =12) %>%
      addPolygons(fillColor = pal(milan_census$deprivation)(milan_census$deprivation),fillOpacity = 0.8, weight = 0, color="white")%>%
      addLegend(pal = pal(milan_census$deprivation),
                values = milan_census$deprivation,
                position = "bottomleft",
                title = "well-being",
                opacity=.9
                # labFormat = labelFormat(suffix = " days")
      )
    
    
  })
  
  
  
  output$variable_map <- renderLeaflet({
    # Aaspects of the map that  won't need to change dynamically
    leaflet(milan_streets) %>% 
      setView(lng= mean(extent[1,]), lat = mean(extent[2,]),zoom =12) %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addPolylines(weight = 1, color=~roadPal(closeness)(closeness))

  })
  # observe({
  #   # feature = unlist(nycZip@data[selectedFeature()[["feature"]]])
  #   # featureName = selectedFeature()[["name"]]
  #   # 
  #   # leafletProxy("variable_map") %>%
  #   #   clearControls() %>%
  #   #   clearShapes() %>%
  #   #   addPolygons(data = nycZip, color = pal(feature)(feature),stroke = TRUE, weight = 1,
  #   #               popup = paste(featureName,feature, sep=": "), fillOpacity = 0.7)%>%
  #   #   addLegend(pal = pal(feature),
  #   #             values = feature,
  #   #             position = "bottomright",
  #   #             title = featureName,
  #   #             labFormat = labelFormat(prefix = "$"))
  #   
  # })
  # 
  # output$response_variable_points <- renderPlot({
  #   feature = unlist(nycZip@data[selectedFeature()[["feature"]]])
  #   featureName = selectedFeature()[["name"]]
  #   
  #   plot_df = data.frame(var =feature, response_time = nycZip@data$response.Time, descriptor = nycZip@data$Descriptor)
  #   
  #   ggplot(data = plot_df) + 
  #     aes(x=var,y=response_time) + 
  #     scale_x_log10(breaks=c(100,1000,10000))+
  #     # geom_point(aes(color=descriptor)) +
  #     geom_point() +
  #     # scale_color_tableau(guide=FALSE)+
  #     geom_smooth(method = "lm") +
  #     theme_fivethirtyeight() + 
  #     theme(plot.title = element_text(hjust = 1)) +
  #     labs(title = paste("Response vs ",featureName, sep=""), x = featureName, y="Response Time")
  #   
  #   
  # })
  # output$variable_map_title <- renderText({
  #   feature = unlist(nycZip@data[selectedFeature()[["feature"]]])
  #   selectedFeature()[["name"]]
  # })
  # output$response_distribution <- renderPlot({
  #   
  #   
  #   # ggplot(data = nycZip@data) + 
  #   #   aes(x=Descriptor,y=response.Time) +
  #   #   # geom_boxplot(aes(fill=Descriptor)) + 
  #   #   geom_density(aes(fill=Descriptor)) + 
  #   #   theme_fivethirtyeight() + 
  #   #   # scale_fill_tableau(guide=FALSE)+
  #   #   labs(title = paste("Response Time Distribution"), x = "Descriptor", y="Response Time")+
  #   #   theme(axis.text.x=element_text(angle=15, vjust=1, hjust=1),
  #   #         plot.title = element_text(hjust = 1)) 
  #   
  #   ggplot(data = nycZip@data) + 
  #     aes(x=response.Time) +
  #     # geom_boxplot(aes(fill=Descriptor)) + 
  #     geom_density(fill="grey", alpha=.3) + 
  #     theme_fivethirtyeight() + 
  #     # scale_fill_tableau(guide=FALSE)+
  #     labs(title = "Response Time Distribution", x="Response Time")+
  #     theme(plot.title = element_text(hjust = 1)) 
  #   
  # })
  # output$variable_distribution <- renderPlot({
  #   feature = unlist(nycZip@data[selectedFeature()[["feature"]]])
  #   featureName = selectedFeature()[["name"]]
  #   
  #   ggplot() + 
  #     aes(x=feature) +
  #     # geom_boxplot(aes(fill=Descriptor)) + 
  #     geom_density(fill="grey", alpha=.3) + 
  #     theme_fivethirtyeight() + 
  #     # scale_fill_tableau(guide=FALSE)+
  #     labs(title = paste(featureName, "Distribution",sep=" "), x=featureName)+
  #     scale_x_log10(breaks=c(100,1000,10000))+
  #     theme(plot.title = element_text(hjust = 1)) 
  #   
  # })
  # 
  
  # plotOutput("response_distribution", height = 200),
  # plotOutput("response_variable_points", height = 200),
  # plotOutput("response_season", height = 200)
}

shinyApp(ui, server)



