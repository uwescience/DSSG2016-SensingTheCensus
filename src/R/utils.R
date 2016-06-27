multmerge = function(mypath, pattern, read_function){
  filenames = list.files(path=mypath, full.names=TRUE)
  filenames = filenames[grepl(pattern,filenames)]
  datalist = lapply(filenames, function(x){
    print(paste("Reading.....", x))
    read_function(x)
  })
  # datalist
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