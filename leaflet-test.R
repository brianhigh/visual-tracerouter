# Will not show leaflet map in "Viewer" tab of RStudio if script is "sourced".

library(maps)
library(leaflet)

ipinfo.csv <- "data/www_cubagob_cu/ipinfo.csv"

if (file.exists(ipinfo.csv) == TRUE) {
  ipinfo <- read.csv(ipinfo.csv, stringsAsFactors=FALSE)
  
  world <- map("world", fill = TRUE, plot = FALSE) 
  
  leaflet(data=world) %>% 
    addTiles() %>% 
    addPolylines(ipinfo$longitude, ipinfo$latitude) %>%
    addCircleMarkers(ipinfo$longitude, ipinfo$latitude, 
                     color = '#ff0000', popup=ipinfo$city)
}