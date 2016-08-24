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


#' Read datasets
milan_streets <<- readRDS("milan_street_intersection.rds")
mexico_streets <<- readRDS("mexico_city_street_intersection.rds")

milan_census <<- readRDS("milan.rds") 
mexico_census <<- readRDS("mexico_city.rds")

milan_amenities <<- readRDS("milan_amenities.rds")
mexico_amenities <<- readRDS("mexico_city_amenities.rds")


source("app-utils.R")