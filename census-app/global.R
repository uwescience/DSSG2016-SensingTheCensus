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
milan_streets <<- readRDS("data/milan_street_intersection.rds")
mexico_streets <<- readRDS("data/mexico_city_street_intersection.rds")

milan_census <<- readRDS("data/milan.rds") 
mexico_census <<- readRDS("data/mexico_city.rds")

milan_amenities <<- readRDS("data/milan_amenities.rds")
mexico_amenities <<- readRDS("data/mexico_city_amenities.rds")


source("app-utils.R")