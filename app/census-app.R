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

milan_amenities = readRDS("milan_amenities.rds")
mexico_amenities = readRDS("mexico_city_amenities.rds")


source("app-utils.R")

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
    delay(700, {
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
    delay(700, {
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
  
  observeEvent(input$variable_map_shape_click, {
    output$selected_rank <- renderTable({
      event = input$variable_map_shape_click
      map_click_event_handler_table(event)
    },include.rownames=FALSE)
    output$modal_title <- renderText({
      event = input$variable_map_shape_click
      paste("Detail", event$id)
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
  
  observeEvent(input$deprivation_map_shape_click, {
    output$selected_rank <- renderTable({
      event = input$deprivation_map_shape_click
      map_click_event_handler_table(event)
    },include.rownames=FALSE)
    output$modal_title <- renderText({
      event = input$variable_map_shape_click
      paste("Detail", event$id)
    })
  })
  
  map_click_event_handler_update_map = function(event){
    poly_id = event$id
    
    city = isolate({input$city_feature})
    
    map = census[[city]]
    street_map = streets[[city]]
    amenity_map = amenities[[city]]
    
    filtered_map = subset(map, unlist(map@data[poly_ids[[city]]]) == poly_id)
    
    selected = isolate({input$census_feature})
    feature = unlist(map@data[selected])
    
    filtered_feature = unlist(filtered_map@data[selected])
    
    filtered_streets = subset(street_map, unlist(street_map@data[poly_ids[[city]]]) == poly_id)

    filtered_amenities = amenity_map[amenity_map[poly_ids[[city]]] == poly_id,]

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
        addCircles(lng= ~lon, lat= ~lat, weight = 1,radius = 20,color="white",
                                 fillColor = classPal(amenity_map$amenity)(filtered_amenities$amenity),
                                 fillOpacity =1, data= filtered_amenities) %>%
        addLegend(pal = classPal(amenity_map$amenity),
                  values = amenity_map$amenity,
                  position = "bottomleft",
                  title = "amenity",
                  opacity=.9
        ) %>%
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
    
    df = map@data %>% dplyr::select(ends_with("osm")) %>%
      gather(key, value) %>% 
      mutate(key = {key %>% gsub("idw_", "",.)  %>% gsub("_osm", "",.) %>% gsub("_", " ",.)})
    
    # plot_names = unique(df$key) %>% gsub("idw_", "",.)  %>% gsub("_osm", "",.) %>% gsub("_", " ",.) 
    # print(plot_names)
    # subset(street_map, unlist(street_map@data[poly_ids[[city]]]) == poly_id)
    df_selected = filtered_map@data %>% dplyr::select(ends_with("osm")) %>%
      gather(key, value) %>% 
      mutate(key = {key %>% gsub("idw_", "",.)  %>% gsub("_osm", "",.) %>% gsub("_", " ",.)})
    
    ggplot(df) + geom_boxplot(aes(1, value), fill = "#303030",size=.5,color = "darkgrey") +
      geom_hline(aes(yintercept=value), color="#EE82EE", size=1.2, data= df_selected)+
      facet_grid(key~., scales="free_y") + 
      coord_flip() +
      theme_fivethirtyeight()+
      theme(strip.text.y = element_text(angle=0), axis.text = element_blank(),
            axis.title = element_blank(), axis.ticks.y = element_blank(),
            panel.background = element_rect(fill = "#303030"),
            plot.background = element_rect(fill = "#303030"),
            axis.title = element_text(colour = "white"),
            axis.text = element_text(colour = "white"),
            panel.grid = element_blank(),
            strip.background = element_rect(fill = "black"),
            strip.text = element_text(colour = "white")
      )
  }

  output$scatter_plot <- renderPlot({
    selectedCensus = selectedCensusFeature()
    
    selected_census =  selectedCensus[["selected"]]
    feature_census  = selectedCensus[["data"]]
    featureName_census = selectedCensus[["legend"]]
    
    selectedOSM = selectedOSMFeature()
    
    selected_osm =  selectedOSM[["selected"]]
    feature_osm  = selectedOSM[["data"]]
    featureName_osm = selectedOSM[["legend"]]
    
    if(length(feature_osm) != length(feature_census)){
      plot = ggplot()
    } else {
      plot_df = data.frame(selected_osm  = feature_osm, selected_census = feature_census)
      names(plot_df) = c(selected_osm, selected_census)
  
      plot = ggplot(data = plot_df) +
        geom_point(aes_string(x = selected_osm, y = selected_census), size =.8, color="white") +
        geom_smooth(aes_string(x = selected_osm, y = selected_census),method = "lm", fill="lightgrey", color = "#6FDCF1") +
        theme_fivethirtyeight() +
        # theme(plot.title = element_text(hjust = 1)) +
        labs(x = featureName_osm, y=featureName_census) +
        theme(panel.background = element_rect(fill = "black"),
              plot.background = element_rect(fill = "black"),
              axis.title = element_text(colour = "white"),
              axis.text = element_text(colour = "white"))
    }
    plot
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
  
  output$census_density <- renderPlot({
    
    selectedMap = selectedCensusFeature()
    
    selected =  selectedMap[["selected"]]
    feature = selectedMap[["data"]]
    legend = selectedMap[["legend"]]
    
    
    p = ggplot() + geom_density(aes(x = feature), fill = "#EE82EE", alpha=.7) + 
      labs(x = legend) + theme_fivethirtyeight() + 
      theme(axis.title=element_blank(),
            panel.background = element_rect(fill = "black"),
            plot.background = element_rect(fill = "black"),
            axis.title = element_text(colour = "white"),
            axis.text = element_text(colour = "white"))
    p
    
  })
  
  # output$selected_rank <- renderTable({
  map_click_event_handler_table = function(event){
    poly_id = event$id
    
    city = isolate({input$city_feature})
    map = census[[city]]

    ranked = map@data %>% arrange_(defaults[[city]])
    ranked$percentile = percent_rank(ranked[defaults[[city]]])
    ranked$rank = 1:dim(ranked)[1]
    
    index = which(ranked[poly_ids[[city]]] == poly_id)
    
    print(index)
    surroundings = (index - 2):(index + 2)
    while( sum(surroundings <=0) > 0 ){surroundings = surroundings + 1}
    while( sum(surroundings > dim(map@data)[1]) > 0 ){surroundings = surroundings - 1}
    
    table = ranked %>% slice(surroundings) %>% dplyr::select_("rank", "percentile", poly_ids[[city]],  defaults[[city]]) 
    names(table) = c("rank", "percentile", "id",  reverseList(censusMap[[city]])[[defaults[[city]]]])
    
    table
  }
  
}

shinyApp(ui, server)



# df_selected = filtered_map@data %>% dplyr::select(ends_with("osm")) %>%
  # gather(key, value) %>% 
  # mutate(key = {key %>% gsub("idw_", "",.)  %>% gsub("_osm", "",.) %>% gsub("_", " ",.)})
# df = milan_census@data %>% dplyr::select(ends_with("osm")) %>%
#   gather(key, value) %>% 
#   mutate(key = {key %>% gsub("idw_", "",.)  %>% gsub("_osm", "",.) %>% gsub("_", " ",.)})
# 
# 
# ggplot(df) + geom_boxplot(aes(1, value), fill = "#303030",size=.5,color = "darkgrey") +
#   # geom_hline(aes(yintercept=value), color="#EE82EE", size=1.2, data= df_selected)+
#   facet_grid(key~., scales="free_y") + 
#   coord_flip() +
#   theme_fivethirtyeight()+
#   theme(strip.text.y = element_text(angle=0), axis.text = element_blank(),
#         axis.title = element_blank(), axis.ticks.y = element_blank(),
#         panel.grid = element_blank()
#         )+
#   labs(title = "Amenity OA Distributions - Milan")
#         # panel.background = element_rect(fill = "#303030"),
#         # plot.background = element_rect(fill = "#303030"),
#         # axis.title = element_text(colour = "white"),
#         # axis.text = element_text(colour = "white"),
# ggsave("milan-oa-sitributions.png")
#         # strip.background = element_rect(fill = "black"),
#         # strip.text = element_text(colour = "white")



