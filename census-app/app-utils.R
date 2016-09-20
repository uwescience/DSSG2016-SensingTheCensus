poly_ids = list("milan" = "ACE", "mexico_city" = "CVE_GEOAGE")
census = list("milan" = milan_census, "mexico_city" = mexico_census)
streets =  list("milan" = milan_streets, "mexico_city" = mexico_streets )
amenities = list("milan" = milan_amenities, "mexico_city" = mexico_amenities)


pal <- function(x) {
  n = 7
  quant_list = quantile(x, probs = seq(0, 1, 1/n))
  if (length(unique(quant_list)) == n+1)
    colorQuantile(c("black", "#99FFCC"), x, n = n, na.color = "black")
  else
    colorBin(c("black", "#99FFCC"), x, bins = n, na.color = "black")

  
  }

pal2 <- function(x) {
  n = 7
  quant_list = quantile(x, probs = seq(0, 1, 1/n))
  
  if (length(unique(quant_list)) == n+1) 
    colorBin(c("black", "#EE82EE"), x, bins=quant_list, na.color = "black")
  else
    colorBin(c("black", "#EE82EE"), x, bins = n, na.color = "black")
  
}

classPal <- function(x) {
  #ffff33
  # palette = brewer.pal(name="Set1", n=5)
  palette = c("#E41A1C", "#377EB8", "#4DAF4A", "#ffff33", "#FF7F00")
  colorFactor(palette= palette, domain = x)

}

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

milanCensusMap = list("deprivation" = "deprivation",
                      "unemployment" = "unemployment",
                      "% high school" =  "high_school",
                      "% illiteracy" = "illiteracy",
                      "% age > 65" =  "sixtyfive_plus",
                      "% foreigners" = "foreigners",
                      "% rented dwellings" = "rented_dwelling")

mexicoCensusMap = list("deprivation" = "IMU")

censusMap = list("milan" = milanCensusMap, "mexico_city" = mexicoCensusMap)

milanOSMMap = list()
milanOSMMap[["Closeness Centrality"]] = "closeness"
milanOSMMap[["Betweenness Centrality"]] = "betweenness"
for(name in milan_census@data%>% dplyr::select(ends_with("osm")) %>% names()){
  clean_name =name %>% gsub("_osm", "",.) %>% gsub("\\_", "",.) 
  key = paste(clean_name, "offering advantage")
  milanOSMMap[[key]] = name
}


mexicoOSMMap = list()

mexicoOSMMap[["Betweenness Centrality"]] = "betweenness"
mexicoOSMMap[["Closeness Centrality"]] = "closeness"
# for(name in mexico_census@data%>% dplyr::select(bank:school) %>% names()){
for(name in mexico_census@data%>% dplyr::select(ends_with("osm")) %>% names()){
  clean_name = name %>% gsub("idw_", "",.)  %>% gsub("_osm", "",.)  %>% gsub("\\_", "",.) 
  key = paste(clean_name, "offering advantage")
  
  mexicoOSMMap[[key]] = name 
}

osmMap = list("milan" = milanOSMMap, "mexico_city" = mexicoOSMMap)

defaults = list("milan" = "deprivation", "mexico_city" = "IMU")
defaults_osm = list("milan" = "closeness", "mexico_city" = "closeness")

