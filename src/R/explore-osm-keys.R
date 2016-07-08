library(xml2)
library(XML)
library(magrittr)
library(dplyr)

xmlfile <- xmlTreeParse(xml.url)

raw_xml <- read_xml("planet_8.623,45.245_9.656,45.712.osm", options = c("HUGE"))

tags = raw_xml %>% xml_find_all("//tag")

# tags %>% xml_find_all(".//tag[k='amenity']")

keys = raw_xml %>% xml_find_all("//tag") %>% xml_attr("k")
unique(keys)

key_count = keys %>% data.frame(key = .) %>% group_by(key) %>% summarize(n = n())

filtered_keys = key_count %>% filter(n >20)

# amenity_value =  raw_xml %>% xml_find_all("//tag[k='amenity']")

unique_keys = unique(keys)

# node_df = nodes %>% {data.frame(id = xml_attr(., "id"), lon = xml_attr(., "lon"), lat = xml_attr(., "lat"))}
# xml_name(x)
# xml_children(x)
# xml_text(x)

# ids$