library(shiny)
library(leaflet)
library(maptools)
library(spatialEco)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(rgdal)
# library(RColorBrewer)
library(scatterD3)
library(ggthemes)
library(data.table)
library(plotly)
# setwd("app")

milan_streets = readRDS("milan_street_network.rds")
milan_census = readRDS("milan.rds") 
# mexico_census = readRDS("mexico_city.rds") 

# pal <- function(x) {
#   n = 9
#   cuts = cut(x, breaks = n)
#   # labels = c("low", NA, NA, NA, "medium", NA, NA, NA ,"high"))
#   print(cuts)
#   colors = colorRampPalette(c("black", "#99FFCC"))(n)
#   
#   colorFactor(colors, cuts, na.color = "black")}
pal <- function(x) {colorBin(c("black", "#99FFCC"), x,bins =10, na.color = "black")}
pal2 <- function(x) {colorBin(c("black", "#EE82EE"), x, bins=quantile(x, probs = seq(0, 1, 0.1), na.rm=TRUE),na.color = "black")}
reverseList = function(hash_list){
  revMap = list()
  for(key in names(hash_list)){ revMap[[hash_list[[key]]]] = key }
  return(revMap)
}
palette <- rev(brewer.pal(10, "RdYlBu")) #Spectral #RdYlBu

roadPal = function(x) {colorQuantile(palette = palette, domain = x, n=10)}


milan_extent = milan_streets@bbox

cityMap = list("Milan" = "milan",
               "Mexico City" = "mexico_city")

milanCensusMap = list("well-being" = "deprivation",
                "unemployment" = "unemployment",
                "% high school" =  "high_school",
                "% illiteracy" = "illiteracy",
                "% age > 65" =  "sixtyfive_plus",
                "% foreigners" = "foreigners",
                "% rented dwellings" = "rented_dwelling")

milanOSMMap = list()
milanOSMMap[["Closeness Centrality"]] = "closeness"
milanOSMMap[["Betweenness Centrality"]] = "betweenness"
for(name in milan_census@data%>% dplyr::select(bar:university) %>% names()){
  clean_name = gsub("\\_", " ", name)
  key = paste(clean_name, "offering advantage")
  milanOSMMap[[key]] = name
}


mexCensusMap = list("deprivation" = "IMU")

  # list("Closeness Centrality" = "closeness", "bar" = "bar")

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
                  absolutePanel(top = 50, left = 45,h3(textOutput("census_map_title")))),
           column(6, class="right-side",
                  leafletOutput("variable_map", width = "100%", height = "100%"),
                  absolutePanel(top = 50, left = 45,h3(textOutput("variable_map_title"))))
                  # absolutePanel(top = 50, left = 45,h3("Street-Network")))
           #
           
  ),
  absolutePanel(top = 130, id = "controls", class = "panel panel-default middle",
                fixed = TRUE, draggable = TRUE,height = "auto", width =  "350px",
                h4("city"),
                selectInput("city_feature", NA,
                            choices = names(cityMap)),
                conditionalPanel( condition = "input.city_feature == 'Milan'",
                  h4("census"),
                  selectInput("census_feature", NA,
                              choices = milanCensusMap),
                  tableOutput('census_statistics'),
                  h4("OSM"),
                  selectInput("osm_feature", NA,
                              choices = milanOSMMap),
                  checkboxInput("show_netwrok", "Show network?", value = TRUE),
                  # selectInput("categories", "Select descriptors to display",multiple = TRUE, selected = descriptors,
                  # choices = descriptors, selectize = FALSE, size=3),
                  
                  # scatterD3Output("scatter_plot", height = 200)
                  
                  h5("Scatterplot"),
                  plotlyOutput("scatter_plot", height = 200)
                  # plotOutput("variable_distribution", height = 200)
                  # 
                  # plotOutput("response_season", height = 200)
                )
                
  ),
  absolutePanel(top = 0, left = 35,headerPanel("Crowdsensing the Census"))
)


server <- function(input, output, session) {
  # Reactive expression to subset data
  selectedCensusFeature <- reactive({
    selected = input$census_feature

    return(selected)


  })
  
  selectedOSMFeature <- reactive({
    selected = input$osm_feature
    
    return(selected)
    
    
  })
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
      setView(lng= mean(milan_extent[1,]), lat = mean(milan_extent[2,]),zoom =12)
    
    
  })
  
  
  
  output$variable_map <- renderLeaflet({
    # Aaspects of the map that  won't need to change dynamically

    leaflet(milan_census) %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      setView(lng= mean(milan_extent[1,]), lat = mean(milan_extent[2,]),zoom =12)

  })
  observe({
    selected = selectedCensusFeature()
    feature = unlist(milan_census@data[selected])
    legend = unlist(reverseList(milanCensusMap)[[selected]])
    # featureName = selectedFeature()[["name"]]
    leafletProxy("deprivation_map") %>%
      clearControls() %>%
      clearShapes() %>%
      addPolygons(fillColor = pal2(feature)(feature),fillOpacity = 0.8, weight = .4, color="white",
                  data = milan_census)%>%
      addLegend(pal = pal2(feature),
                values = feature,
                position = "bottomleft",
                title = legend,
                # labels = c("Low",NA,NA,NA, "Medium",NA , NA ,NA ,NA,  "High"),
                opacity=.9,
                labFormat = labelFormat(digits = 2)
      )
  })
  
  observe({
    selected = selectedOSMFeature()
    feature = unlist(milan_census@data[selected])
    legend =unlist(reverseList(milanOSMMap)[[selected]])
    # featureName = selectedFeature()[["name"]]
    leafletProxy("variable_map") %>%
      clearControls() %>%
      clearShapes() %>%
      addPolygons(fillColor = pal(feature)(feature),fillOpacity = 0.8, weight = .4, color="white",
                  data = milan_census) %>%
      addLegend(pal = pal(feature),
                values = feature,
                position = "bottomright",
                title = legend,
                opacity=.9
                # labels = c("Low",NA,NA,NA, "Medium",NA , NA ,NA ,NA,  "High")
                # labFormat = labelFormat(digits= 0,transform = function(x){
                #   NA
                # })
      )
  })
  # 
  # output$scatter_plot <- renderScatterD3(
  #   scatterD3(x = milan_census@data$deprivation, y = milan_census@data$closeness, 
  #             # lab = ACE,
  #             # col_var = cyl, symbol_var = am,
  #             xlab = "Weight", ylab = "Mpg", col_lab = "Cylinders",
  #             symbol_lab = "Manual transmission")
  # )
  output$scatter_plot <- renderPlotly({
    selected_census = selectedCensusFeature()
    feature_census = unlist(milan_census@data[selected_census])
    featureName_census =  unlist(reverseList(milanCensusMap)[[selected_census]])

    selected_osm = selectedOSMFeature()
    feature_osm = unlist(milan_census@data[selected_osm])
    featureName_osm =  unlist(reverseList(milanOSMMap)[[selected_osm]])
    
    plot_df = data.frame(selected_osm  = feature_osm, selected_census = feature_census)
    names(plot_df) = c(selected_osm, selected_census)
    print(names(plot_df))
    plot = ggplot(data = plot_df) +
      geom_point( aes_string(x = selected_osm, y = selected_census), size = .8) +
      geom_smooth( aes_string(x = selected_osm, y = selected_census),method = "lm") +
      theme_bw()+
      # theme(plot.title = element_text(hjust = 1)) +
      labs(x = featureName_osm, y=featureName_census) +
      theme(axis.text=element_text(size=8),
            axis.title=element_text(size=8))
    ggplotly(plot)# %>%
      # layout(plot_bgcolor='rgb(254, 247, 234, .5)') %>%
      # layout(paper_bgcolor='rgb(254, 247, 234, .5)')
      # labs(title = paste(selected_osm," vs ",selected_census, sep=""), x = featureName_osm, y=featureName_census)
  })
  
  output$census_map_title <- renderText({
    selected = selectedCensusFeature()
    unlist(reverseList(milanCensusMap)[[selected]])
  })
  output$variable_map_title <- renderText({
    selected = selectedOSMFeature()
    unlist(reverseList(milanOSMMap)[[selected]])
  })

  output$census_statistics <- renderTable({
    selected = selectedCensusFeature()
    feature = unlist(milan_census@data[selected])
    summary(feature) %>% broom::tidy() %>% 
      dplyr::select(minimum, median, mean, maximum)%>% 
      setNames(c("min", "median", "mean", "max")) 
  }, 
  include.rownames=FALSE)

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


# highchart() %>% 
#   hc_add_theme(hc_theme_538()) %>% 
#   hc_add_serie(data = ds, name = "data", type = "scatter") 

