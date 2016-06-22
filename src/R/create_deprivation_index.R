library(readr)
library(dplyr)
library(maptools)
library(sp)
library(rgdal)
library(leaflet)

setwd("~/Documents/git/DSSG2016-SensingTheCensus/")
source("src/R/utils.R")

## Load Data
census = multmerge("data/census/Sezioni di Censimento", "R\\d", read_census_data)
# lombardia_shp = readShapePoly("data/R03_11_WGS84/R03_11_WGS84.shp")
milano_shp = readShapePoly("data/milan_sez_2011/Milano_SezC_2011.shp")

proj4string(milano_shp) = CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
milano_shp = spTransform(milano_shp, CRS("+init=epsg:4326"))
# proj4string(lombardia_shp) = CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
# lombardia_shp = spTransform(lombardia_shp, CRS("+init=epsg:4326"))
# lombardia_shp@data = lombardia_shp@data %>% 
#   mutate(SEZ2011 = as.character(SEZ2011)) %>%
#   left_join(census, by = c("SEZ2011"))



#### Build Deprivation Index using PCA 
## Read all census dataset
# census = multmerge("data/dati-cpa_2011_/Aree di Censimento", "R\\d")

census_depriv = census %>% 
  transmute(P47 = P47/P1, P48 = P48/P1, P49 = P49/P1, P50 = P50/P1, P51 = P51/P1, 
            P52 = P52/P1, P27 = P27/P1, P28 = P28/P1, P29 = P29/P1, ST15 = ST15/P1,
            A46 = ifelse(A2 > 0, A46/A2, NA), A47 = ifelse(A2 > 0, A47/A2, NA), 
            P62 = P62/P60, P61 = P61/P60, P60 = P60/P1, E21 = E21/E3, E22 = E22/E3, 
            E23 = E23/E3, E24 = E24/E3, E25 = E25/E3,E26 = E26/E3) 
row.names(census_depriv) = census$SEZ2011

## Compute cumulative variance explained
depriv_pca = prcomp(~+. , census_depriv, center = TRUE, scale = TRUE, na.action = na.omit)
variance <- apply(depriv_pca$x, 2, var)  
props <- variance / sum(variance)
cum_variance = cumsum(props)

## Print PCA results
data.frame(proportion = props, cumulative_proportion = cum_variance) %>% filter(row_number() %in% 1:10)
# biplot(depriv_pca)
# ggbiplot(depriv_pca) + theme_fivethirtyeight()
feature_importance = depriv_pca$rotation %>% as.data.frame() %>% select(PC1:PC3) %>% add_rownames()%>% arrange(desc(PC1))
feature_importance 
depriv_index = depriv_pca$x[,1]
depriv_df =  data.frame(SEZ2011 = names(depriv_index), deprivation = unname(depriv_index))


## Add index to milano polygon
milano_shp@data = milano_shp@data %>%
  mutate(SEZ2011 = as.character(SEZ2011)) %>%
  left_join(depriv_df, by = c("SEZ2011"))

## Map the index 
pal <- function(x) {colorBin("YlGnBu", x, bins=quantile(x, probs = seq(0, 1, 0.1), na.rm=TRUE))}

leaflet(milano_shp) %>% addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(fillColor = ~pal(deprivation)(deprivation), weight = .2, color="white",fillOpacity = 0.6) %>%
  addLegend(pal = pal(milano_shp$deprivation),
            values = ~deprivation,
            position = "bottomleft"
  )
