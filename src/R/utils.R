multmerge = function(mypath, pattern, read_function){
  filenames = list.files(path=mypath, full.names=TRUE)
  filenames = filenames[grepl(pattern,filenames)]
  datalist = lapply(filenames, function(x){
    print(paste("Reading.....", x))
    read_function(x)
  })
  print(paste("Merging datasets..."))
  Reduce(function(x,y) {bind_rows(x,y)}, datalist) %>% tbl_df()
}

read_census_data = function(file_path) {
  read_delim(file=file_path, delim = ";", na = "null", col_types = cols(
    "CODREG" = col_character(),
    "REGIONE" = col_character(),
    "CODPRO" = col_character(),
    "PROVINCIA" = col_character(),
    "CODCOM" = col_character(),
    "COMUNE" = col_character(),
    "PROCOM" = col_character(),
    "SEZ2011" = col_character(),
    "NSEZ" = col_character(),
    "ACE" = col_character(),
    "CODLOC" = col_character(),
    "CODASC"= col_character()
  ))
}


pal <- function(x) {colorBin("YlGnBu", x, bins=quantile(x, probs = seq(0, 1, 0.20), na.rm=TRUE))}
# pal <- function(x) {colorBin("YlGnBu", x, bins=5)}


leaflet_map = function(spatialDf, var, legend_title){
  library(rgdal)

  feature = unlist(spatialDf@data[var])
  
  spatialDf = spTransform(spatialDf, CRS("+init=epsg:4326"))

  leaflet(spatialDf) %>% addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(fillColor = pal(feature)(feature), weight = .2, color="white",fillOpacity = 0.6) %>%
    addLegend(pal = pal(feature),
              values = feature,
              position = "bottomleft",title = legend_title
    )
}


grid.draw.gg <- function(x){print(x)}

norm = function(x,p){x/p}

dens = function(x,p,a){x/(p/a)}