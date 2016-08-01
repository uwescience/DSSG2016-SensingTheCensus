pkgTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(pkgs=x,repos="http://cran.r-project.org")
      if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }

#pkgTest("readr")
#pkgTest("dplyr")
#pkgTest("magrittr")
#pkgTest("lubridate")
#pkgTest("rgdal")
#pkgTest("raster")

library(readr)
library(dplyr)
library(magrittr)
library(lubridate)
library(rgdal)
library(raster)

setwd("/Users/myeong/git/DSSG/DSSG2016-SensingTheCensus/")

#' Load census and CDR geojson
census = readOGR("../../data/GeoJSON/milano_census_ace.geojson", "OGRGeoJSON") %>%
  spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))
cdr = readOGR("../../data/GeoJSON/CDR_join_output.geojson", "OGRGeoJSON") %>%
  spTransform(CRS("+proj=utm +zone=32 +datum=WGS84"))

#' Intersect polygons
intersection = raster::intersect(x = census, y = cdr)

#' Calcualte area of each polygon
intersection@data$area = area(intersection)

#' Calculate area of each polygon
square_size = max(intersection@data$area)

intersection@data %<>% dplyr::select(ACE,area, cellId)

days = seq(ymd("2013-11-01"), ymd("2014-01-01"), by="days")

for(day in format(days, "%Y-%m-%d")){
  #file_path = paste("https://s3-us-west-2.amazonaws.com/census-cdr/mi-to-mi/MItoMI-", day, ".txt", sep = "")
  local_path =  paste("../../data/CDR/cdr_nov/MItoMI-", day, ".txt", sep = "")
  save_path = paste("../../data/CDR/generated/ACEtoACE-", day, ".csv", sep = "")
  
  print(day)

  print("Reading")
  
  #' Download netwrok dataset
  #download.file(file_path, local_path, mode="wb")
  #' Load netwrok dataset
  cdr_network = read_delim(local_path, delim = "\t",col_names = FALSE )%>%
    transmute(date = as.POSIXct(X1/1000,origin="1970-01-01",tz = "Europe/Rome"),
                      # date =X2,
                      hour = hour(date),
                      source = X2,
                      target = X3,
                      calls = X4
              ) 
  print("Done Reading")
  
  # Filter by cells that intersect to a census tract
  cdr_network %<>%
    filter(source %in% intersection@data$cellId, target %in% intersection@data$cellId)
  
  #' Aggregate data into census areas summing the CDR data proportionally to the size of the squares
  
  census_network =  cdr_network %>% left_join(intersection@data, by = c("source"="cellId")) %>%
    left_join(intersection@data, by = c("target"="cellId")) %>% 
    dplyr::group_by(ACE.x, ACE.y, hour) %>% 
    summarize(calls = sum(((area.x)/(square_size))*((area.y)/(square_size)) *calls, na.rm=TRUE))
  write_csv(census_network, save_path)
}
