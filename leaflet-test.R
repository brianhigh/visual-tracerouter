# Will not show leaflet map in "Viewer" tab of RStudio if script is "sourced".

library(leaflet)

ipinfo.csv <- "data/www_cubagob_cu/ipinfo.csv"

if (file.exists(ipinfo.csv) == TRUE) {
    ipinfo <- read.csv(ipinfo.csv, stringsAsFactors=FALSE)
  
    attach(ipinfo)
    leaflet() %>% 
        addTiles() %>% 
        addPolylines(longitude, latitude) %>%
        addCircleMarkers(longitude, latitude, color = '#ff0000', 
                       popup=paste(city, region_code, country_code, "-",
                                   ip, "(", round(mean_rtt, 0), "ms )"))
  
    detach(ipinfo)
}
