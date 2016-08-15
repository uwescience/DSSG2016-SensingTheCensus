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
library(plotly)
library(shinyBS)
library(shinyjs)
library(tidyr)
library(RColorBrewer)
# setwd("app")

# milan_streets = readRDS("milan_street_network.rds")
milan_streets = readRDS("milan_street_intersection.rds")
mexico_streets = readRDS("mexico_city_street_intersection.rds")

milan_census = readRDS("milan.rds") 
mexico_census = readRDS("mexico_city.rds")

poly_ids = list("milan" = "ACE", "mexico_city" = "CVE_GEOAGE")
census = list("milan" = milan_census, "mexico_city" = mexico_census)
streets =  list("milan" = milan_streets, "mexico_city" = mexico_streets )
# pal <- function(x) {
#   n = 9
#   cuts = cut(x, breaks = n)
#   # labels = c("low", NA, NA, NA, "medium", NA, NA, NA ,"high"))
#   print(cuts)
#   colors = colorRampPalette(c("black", "#99FFCC"))(n)
#   
#   colorFactor(colors, cuts, na.color = "black")}
pal <- function(x) {colorBin(c("black", "#99FFCC"), x,bins =10, na.color = "black")}
pal <- function(x) {colorQuantile(c("black", "#99FFCC"), x,n = 10, na.color = "black")}

pal2 <- function(x) {colorBin(c("black", "#EE82EE"), x, bins=quantile(x, probs = seq(0, 1, 0.1), na.rm=TRUE),na.color = "black")}
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

milanCensusMap = list("well-being" = "deprivation",
                "unemployment" = "unemployment",
                "% high school" =  "high_school",
                "% illiteracy" = "illiteracy",
                "% age > 65" =  "sixtyfive_plus",
                "% foreigners" = "foreigners",
                "% rented dwellings" = "rented_dwelling")

mexicoCensusMap = list("deprivation index" = "IMU")

censusMap = list("milan" = milanCensusMap, "mexico_city" = mexicoCensusMap)

milanOSMMap = list()
milanOSMMap[["Closeness Centrality"]] = "closeness"
milanOSMMap[["Betweenness Centrality"]] = "betweenness"
for(name in milan_census@data%>% dplyr::select(bar:university) %>% names()){
  clean_name = gsub("\\_", " ", name)
  key = paste(clean_name, "offering advantage")
  milanOSMMap[[key]] = name
}


mexicoOSMMap = list()

mexicoOSMMap[["Betweenness Centrality"]] = "betweenness"
mexicoOSMMap[["Closeness Centrality"]] = "closeness"
# for(name in mexico_census@data%>% dplyr::select(bank:school) %>% names()){
for(name in mexico_census@data%>% dplyr::select(idw_arts_centre:idw_townhall) %>% names()){
  clean_name = gsub("\\_", " ", name)
  key = paste(clean_name, "offering advantage")
  mexicoOSMMap[[key]] = name
}

osmMap = list("milan" = milanOSMMap, "mexico_city" = mexicoOSMMap)

defaults = list("milan" = "deprivation", "mexico_city" = "IMU")
defaults_osm = list("milan" = "closeness", "mexico_city" = "closeness")

ui = bootstrapPage(
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
                plotlyOutput('census_density', height = "150"),
                tableOutput('census_statistics'),
                h4("OSM"),
                selectInput("osm_feature", NA, choices = milanOSMMap, selected="deprivation"),
                # checkboxInput("show_netwrok", "Show network?", value = TRUE),
                # selectInput("categories", "Select descriptors to display",multiple = TRUE, selected = descriptors,
                # choices = descriptors, selectize = FALSE, size=3),

                # scatterD3Output("scatter_plot", height = 200)

                h5("Scatterplot"),
                plotlyOutput("scatter_plot", height = 200)
                  # plotOutput("variable_distribution", height = 200)

                  # plotOutput("response_season", height = 200)
                # )

                
  ),
  absolutePanel(top = 0, left = 35,headerPanel("Crowdsensing the Census")),
  bsModal("detail-modal", "Detail", "tabBut", size = "large",
          useShinyjs(),
          leafletOutput("modal_map", width = "100%", height = "300px"),
          fluidRow(class="some-class",
                   # h4("pending")
                   column(6, class="left-side",
                          h4("Distribution")),
                          # plotOutput("selected_distribution", width = "100%", height = "250px")),
                   column(6, class="right-side",
                          h4("To Fill"))
                   #        # absolutePanel(top = 50, left = 45,h3("Street-Network")))
                   #
                  )
          )
)


server <- function(input, output, session) {
  
  
  # Reactive expression to subset data
  
  selectedCityFeature <- reactive({
    selected = input$city_feature
    return(selected)
  })

  
  selectedCensusFeature <- reactive({
    city = isolate({input$city_feature})
    selected = if(is.null(input$census_feature))
      defaults[[city]]
    else
      input$census_feature

    map = census[[city]]
    data = unlist(map@data[selected])
    
    legend = unlist(reverseList(censusMap[[city]])[[selected]])
    
    return(list(data = data, selected = selected, legend = legend, census_map = map))
  })
  
  selectedOSMFeature <- reactive({
    city = isolate({input$city_feature})
    selected = if(is.null(input$osm_feature))
      defaults_osm[[city]]
    else
      input$osm_feature
    
    map = census[[city]]
    data = unlist(map@data[selected])
    
    legend = unlist(reverseList(osmMap[[city]])[[selected]])
    
    return(list(data = data, selected = selected, legend = legend, census_map = map))
  })
  
  observe({
    selected = selectedCityFeature()
    choiceMap = censusMap[[selected]]
    updateSelectInput(session, "census_feature", choices = choiceMap)
  })
  
  # selectedCategories <- reactive({
  #   cat = input$categories
  # 
  #   # return(list(feature = namesMap[[selected]], name = selected))
  # 
  # })
  
  observe({
    selected = selectedCityFeature()

        choiceMap = osmMap[[selected]]
    updateSelectInput(session,"osm_feature", choices = choiceMap)
  })
  
  output$deprivation_map <- renderLeaflet({
    # Aaspects of the map that  won't need to change dynamically
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter") 

  })
  
  output$variable_map <- renderLeaflet({
    # Aaspects of the map that  won't need to change dynamically
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter")

  })
  
  output$modal_map <- renderLeaflet({
    # Aaspects of the map that  won't need to change dynamically
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter")
    
  })
  
  observe({
    selectedMap = selectedCensusFeature()
    selected =  selectedMap[["selected"]]
    feature = selectedMap[["data"]]
    legend = selectedMap[["legend"]]
    # featureName = selectedFeature()[["name"]]
    map = selectedMap[["census_map"]]
    extent = map@bbox
    
    
    city = isolate({input$city_feature})
    if(city == "milan"){
      weight = .4
      zoom = 12
    } else {
      weight = .2
      zoom = 10    
    }
    
    ids = unlist(unname(map@data[poly_ids[[city]]]))
    
    leafletProxy("deprivation_map") %>%
      clearControls() %>%
      clearShapes() %>%
      addPolygons(fillColor = pal2(feature)(feature),fillOpacity = 0.8, weight = weight, color="white",
                  layerId = ids,
                  data = map)%>%
      addLegend(pal = pal2(feature),
                values = feature,
                position = "bottomleft",
                title = legend,
                # labels = c("Low",NA,NA,NA, "Medium",NA , NA ,NA ,NA,  "High"),
                opacity=.9,
                labFormat = labelFormat(digits = 2)
      )%>%
      setView(lng= mean(extent[1,]), lat = mean(extent[2,]),zoom =zoom)
      
  })

  observe({
    selectedMap = selectedOSMFeature()
    selected =  selectedMap[["selected"]]
    feature = selectedMap[["data"]]
    legend = selectedMap[["legend"]]
    map = selectedMap[["census_map"]]
    
    
    extent = map@bbox
    
    city = isolate({input$city_feature})
    if(city == "milan"){
      weight = .4
      zoom = 12
    } else {
      weight = .2
      zoom = 10    
    }
    
    ids = unlist(unname(map@data[poly_ids[[city]]]))
    # featureName = selectedFeature()[["name"]]
    leafletProxy("variable_map") %>%
      clearControls() %>%
      clearShapes() %>%
      addPolygons(fillColor = pal(feature)(feature), fillOpacity = 0.8, weight = weight, color="white",
                  layerId = ids,
                  data = map) %>%
      addLegend(pal = pal(feature),
                values = feature,
                position = "bottomright",
                title = legend,
                opacity=.9
      ) %>%
      setView(lng= mean(extent[1,]), lat = mean(extent[2,]), zoom = zoom)
  })
  # input$MAPID_click
  observeEvent(input$variable_map_shape_click, {
    event = input$variable_map_shape_click
    map_click_event_handler_update_map(event)
    
  })
  
  observeEvent(input$variable_map_shape_click, {
      toggleModal(session, "detail-modal", "open")
  })

  observeEvent(input$variable_map_shape_click, {
    output$selected_distribution <- renderPlot({
      event = input$variable_map_shape_click
      map_click_event_handler_plot(event)

      
    })
  })
  
  observeEvent(input$deprivation_map_shape_click, {
    event = input$deprivation_map_shape_click
    map_click_event_handler_update_map(event)
    
  })
  
  observeEvent(input$deprivation_map_shape_click, {
    toggleModal(session, "detail-modal", "open")
  })
  
  observeEvent(input$deprivation_map_shape_click, {
    output$selected_distribution <- renderPlot({
      event = input$deprivation_map_shape_click
      map_click_event_handler_plot(event)
    })
  })  
  
  map_click_event_handler_update_map = function(event){
    poly_id = event$id
    
    city = isolate({input$city_feature})
    map = census[[city]]
    street_map = streets[[city]]
    
    filtered_map = subset(map, unlist(map@data[poly_ids[[city]]]) == poly_id)
    
    selected = isolate({input$census_feature})
    feature = unlist(map@data[selected])
    
    filtered_feature = unlist(filtered_map@data[selected])
    
    filtered_streets = subset(street_map, unlist(street_map@data[poly_ids[[city]]]) == poly_id)
    
    # legend = reverseList()
    # map = selectedMap[["census_map"]]
    # # 
    extent = filtered_map@bbox
    # 
    # city = isolate({input$city_feature})
    if(city == "milan"){
      weight = .4
      zoom = 14
    } else {
      weight = .2
      zoom = 17
    }
    # 
    # featureName = selectedFeature()[["name"]]
    delay(700, {
      leafletProxy("modal_map") %>%
        clearControls() %>%
        clearShapes() %>%
        setView(lng= mean(extent[1,]), lat = mean(extent[2,]), zoom = 14)%>%
        addPolygons(fillColor = pal2(feature)(filtered_feature), 
                    fillOpacity = 0.8, 
                    weight = weight, 
                    color="white",
                    data = filtered_map)  %>%
        addPolylines(weight = 2, 
                     color= roadPal(street_map$closeness)(filtered_streets$closeness), 
                     data=filtered_streets)%>%
        addLegend(pal = roadPal(street_map$closeness),
                  values = street_map$closeness,
                  position = "bottomright",
                  title = "betwenness <br> centrality",
                  opacity=.9
        )
    })
  }
  map_click_event_handler_plot = function(event) {
    poly_id = event$id
    
    city = isolate({input$city_feature})
    map = census[[city]]
    street_map = streets[[city]]
    
    filtered_map = subset(map, unlist(map@data[poly_ids[[city]]]) == poly_id)
    
    selected = isolate({input$census_feature})
    feature = unlist(map@data[selected])
    
    filtered_feature = unlist(filtered_map@data[selected])
    
    df = map@data %>% dplyr::select(deprivation, bar:university) %>%
      gather(key, value)
    
    df_selected = subset(map, ACE== poly_id)@data%>% dplyr::select(deprivation, bar:university) %>%
      gather(key, value)
    
    ggplot(df) + geom_boxplot(aes(1, value)) +
      geom_hline(aes(yintercept=value), color="red", data= df_selected)+
      facet_grid(key~., scales="free_y") + coord_flip() +
      theme_fivethirtyeight()+
      theme(strip.text.y = element_text(angle=0), axis.text = element_blank(),
            axis.title = element_blank(), axis.ticks.y = element_blank() )
  }
  #############################
  # output$scatter_plot <- renderScatterD3(
  #   scatterD3(x = milan_census@data$deprivation, y = milan_census@data$closeness, 
  #             # lab = ACE,
  #             # col_var = cyl, symbol_var = am,
  #             xlab = "Weight", ylab = "Mpg", col_lab = "Cylinders",
  #             symbol_lab = "Manual transmission")
  # )
  output$scatter_plot <- renderPlotly({
    selectedCensus = selectedCensusFeature()
    
    selected_census =  selectedCensus[["selected"]]
    feature_census  = selectedCensus[["data"]]
    featureName_census = selectedCensus[["legend"]]
    
    selectedOSM = selectedOSMFeature()
    
    selected_osm =  selectedOSM[["selected"]]
    feature_osm  = selectedOSM[["data"]]
    featureName_osm = selectedOSM[["legend"]]
    
    if(length(feature_osm) != length(feature_census)){
      ggplotly(ggplot())
    } else {
      plot_df = data.frame(selected_osm  = feature_osm, selected_census = feature_census)
      names(plot_df) = c(selected_osm, selected_census)
  
      plot = ggplot(data = plot_df) +
        geom_point( aes_string(x = selected_osm, y = selected_census), size = .8) +
        geom_smooth( aes_string(x = selected_osm, y = selected_census),method = "lm") +
        theme_bw()+
        # theme(plot.title = element_text(hjust = 1)) +
        labs(x = featureName_osm, y=featureName_census) +
        theme(axis.text=element_text(size=8),
              axis.title=element_text(size=8))
      ggplotly(plot)# %>%
    }
      # layout(plot_bgcolor='rgb(254, 247, 234, .5)') %>%
      # layout(paper_bgcolor='rgb(254, 247, 234, .5)')
      # labs(title = paste(selected_osm," vs ",selected_census, sep=""), x = featureName_osm, y=featureName_census)
  })
  
  output$census_map_title <- renderText({
    selectedMap = selectedCensusFeature()
    
    selectedMap[["legend"]]
  })
  output$variable_map_title <- renderText({
    selectedMap = selectedOSMFeature()
    
    selectedMap[["legend"]]
  })

  output$census_statistics <- renderTable({
    
    selectedMap = selectedCensusFeature()
    
    selected =  selectedMap[["selected"]]
    feature = selectedMap[["data"]]
    legend = selectedMap[["legend"]]
    

    summary(feature) %>% broom::tidy() %>% 
      dplyr::select(minimum, median, mean, maximum)%>% 
      setNames(c("min", "median", "mean", "max")) 
  }, 
  include.rownames=FALSE)
  
  output$census_density <- renderPlotly({
    
    selectedMap = selectedCensusFeature()
    
    selected =  selectedMap[["selected"]]
    feature = selectedMap[["data"]]
    legend = selectedMap[["legend"]]
    
    
    p = ggplot() + geom_density(aes(x = feature), fill = "#EE82EE", alpha=.7) + 
      labs(x = legend) + theme_fivethirtyeight()
    ggplotly(p)
    
  })
}

shinyApp(ui, server)


# highchart() %>% 
#   hc_add_theme(hc_theme_538()) %>% 
#   hc_add_serie(data = ds, name = "data", type = "scatter") 
# df = milan_census@data %>% dplyr::select(deprivation, bar:university) %>% 
#   gather(key, value)
# df_selected = subset(milan_census, ACE=="1")@data%>% dplyr::select(deprivation, bar:university) %>% 
# 
#   gather(key, value)

# ggplot(df%>%filter(key == "university")) + geom_boxplot(aes(1,value)) +  
  # geom_hline(aes(yintercept=value, color="selected"), data= df_selected%>%filter(key == "university"))
# ggplot(df) + geom_boxplot(aes(key, value))  + coord_flip()
