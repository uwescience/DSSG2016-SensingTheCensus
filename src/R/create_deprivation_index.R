library(readr)
library(dplyr)
library(maptools)
library(sp)
library(rgdal)
library(leaflet)
library(GGally)
library(ggplot2)
library(rgeos)

source("src/R/utils.R")

## Load Data

granularity_level = "Census Area" ## Choose 'Block' or 'Census Area'
# region = "Italy" ## Choose 'Milano', 'Trento' or 'Milan'

# census = multmerge("data/census/Sezioni di Censimento", "R\\d", read_census_data)
census = multmerge("data/census/Aree di Censimento", "R\\d", function(x){read_delim(file=x, delim = ";", na = "null")})

milano_sez = readShapePoly("data/milan_sez_2011/Milano_SezC_2011.shp")
proj4string(milano_sez) = CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
milano_sez = spTransform(milano_sez, CRS("+init=epsg:4326"))

milano_ace = readShapePoly("data/milan_ace_2011/ACE_maggio_2011.shp")
proj4string(milano_ace) = CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=0.9996 +x_0=1500000 +y_0=0 
                          +ellps=intl +units=m +no_defs ")
milano_ace = spTransform(milano_ace, CRS("+init=epsg:4326"))


# italy_sez = readShapePoly("data/Italy_SezC_2011/Italy_SezC_2011.shp")
# proj4string(italy) = CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
# italy = spTransform(italy, CRS("+init=epsg:4326"))



#### Build Deprivation Index using PCA 
census_depriv = census %>% 
  transmute(high_school = P48/P1, 
            illiteracy = P52/P1, sixtyfive_plus = (P27 + P28 + P29)/P1,
            foreigners = ST15/P1,
            rented_dwelling = ifelse(A46 + A47+ A48 > 0, A46/(A46 + A47+ A48), NA),
            unemployment = P62/P60,
            work_force = P60/(P17 + P18 + P19 + P20 + P21 + P22 + P23 + P24 + P25 + P26 + P27 + P28 + P29)
            ) 

if(granularity_level == "Block"){
  row.names(census_depriv) = census$SEZ2011
}else if(granularity_level == "Census Area"){
  row.names(census_depriv) -> census$ACE
  }



png("corr_plot.png", height = 1000, width = 1000)
corr_plot = ggpairs(census_depriv, lower = list(continuous = wrap("points", alpha = 0.15)))
print(corr_plot)
dev.off()


## Compute cumulative variance explained
depriv_pca = prcomp(~+. , census_depriv, center = TRUE, scale = TRUE, na.action = na.omit)
variance <- apply(depriv_pca$x, 2, var)  
props <- variance / sum(variance)
cum_variance = cumsum(props)

## Print PCA results
data.frame(proportion = props, cumulative_proportion = cum_variance) %>% filter(row_number() %in% 1:10)
biplot(depriv_pca)
# ggbiplot(depriv_pca) + theme_fivethirtyeight()
feature_importance = depriv_pca$rotation %>% as.data.frame() %>% select(PC1) %>% add_rownames()%>% arrange(desc(abs(PC1)))
feature_importance 
depriv_index = depriv_pca$x[,1]

census$deprivation = census_depriv
if(granularity_level == "Block"){
  depriv_df =  data.frame(SEZ2011 = names(depriv_index), deprivation = unname(depriv_index))
}else if(granularity_level == "Census Area"){
  depriv_df =  data.frame(ACE = names(depriv_index), deprivation = unname(depriv_index))
}



## Add index to milano polygon

if(granularity_level == "Block"){
  milano_sez@data = milano_sez@data %>%
    mutate(SEZ2011 = as.character(SEZ2011)) %>%
    left_join(depriv_df, by = c("SEZ2011"))
} else if(granularity_level == "Census Area"){
  milano_ace@data = milano_ace@data %>%
    mutate(ACE = as.character(ACE)) %>%
    left_join(depriv_df, by = c("ACE"))
}



# italy@data = italy@data %>%
#   mutate(SEZ2011 = as.character(SEZ2011)) %>%
#   left_join(depriv_df, by = c("SEZ2011"))

# writePolyShape(italy, "italy.shp")

## Map the index 
pal <- function(x) {colorBin("YlGnBu", depriv_index, bins=quantile(depriv_index, probs = seq(0, 1, 0.15), na.rm=TRUE))}


if(granularity_level == "Block"){
  leaflet(milano_sez) %>% addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(fillColor = ~pal(deprivation)(deprivation), weight = .2, color="white",fillOpacity = 0.6) %>%
    addLegend(pal = pal(milano_sez$deprivation),
              values = ~deprivation,
              position = "bottomleft"
    )
}else if(granularity_level == "Census Area"){
  leaflet(milano_ace) %>% addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(fillColor = ~pal(deprivation)(deprivation), weight = .2, color="white",fillOpacity = 0.6) %>%
    addLegend(pal = pal(milano_ace$deprivation),
              values = ~deprivation,
              position = "bottomleft"
    )
}

milano_ace@data = milano_ace@data %>% left_join(depriv_df, by = "ACE")
milano_sez@data = milano_sez@data %>% mutate(ACE = as.character(ACE)) %>% 
  left_join(depriv_df, by = "ACE")


writeOGR(milano_ace, "data/GeoJSON/milano_ace", layer="census", driver="GeoJSON")
writeOGR(milano_sez, "data/GeoJSON/milano_sez", layer="census", driver="GeoJSON")
