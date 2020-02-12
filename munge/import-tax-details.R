# Go to Arc-GIS Server for Public Map
# Using <https://stackoverflow.com/questions/50161492/how-do-i-scrape-data-from-an-arcgis-online-map>
a <- readLines("https://www.arcgis.com/sharing/rest/content/items/1a96853598c3460191ca465834b3456d/data")

a <- jsonlite::fromJSON(a)

# Find the Map
a[["map"]][["itemId"]]

# Pull the Data
b <- jsonlite::fromJSON(readLines("https://www.arcgis.com/sharing/rest/content/items/479bf10e23a846959ecf445b53e7b112/data"))


# find the Server Addresses
map_details <- tibble::tibble(
title = b[["operationalLayers"]][["title"]],
utl = b[["operationalLayers"]][["url"]]
)
# Read some of the data
out <- jsonlite::fromJSON(readLines("https://services1.arcgis.com/5Yf8nIJWE7cxpd3N/arcgis/rest/services/TPV_Main_Hosted/FeatureServer/1/query?where=0%3D0&outFields=%2A&f=json"))
taxes <- jsonlite::fromJSON(readLines("https://services1.arcgis.com/5Yf8nIJWE7cxpd3N/arcgis/rest/services/TPV_Main_Hosted/FeatureServer/2/query?where=0%3D0&outFields=%2A&f=json"))
try_2 <- jsonlite::fromJSON(readLines("https://services1.arcgis.com/5Yf8nIJWE7cxpd3N/arcgis/rest/services/TPV_Main_Hosted/FeatureServer/0/query?where=0%3D0&outFields=%2A&f=json"))

features <- out$features[[1]]
tax_details <- try_2$features$attributes %>% 
  mutate(PROPERTYADDRESS = stringr::str_to_upper(PROPERTYADDRESS))

library(dplyr)

tax_details

readr::write_rds(tax_details, here::here("data", "forsyth_property_taxes.rds"))
readr::write_rds(features, here::here("data", "forsyth_property_features.rds"))
