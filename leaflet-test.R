# This is a test of the leaflet package using data from `traceroute`. Run this
# code in RStudio. It may not show the leaflet map in the "Viewer" tab of 
# RStudio if script is "sourced". If so, either paste this code into the 
# console or highlight the code in the editor window and "Run" (Ctrl/Cmd-enter).

# Install packages and load into memory
for (pkg in c("magrittr","leaflet", "pander")) {
    if(pkg %in% rownames(installed.packages()) == FALSE) {
        install.packages(pkg, quiet = TRUE, 
                         repos="http://cran.fhcrc.org",
                         dependencies=TRUE)
    }
    require(pkg, character.only = TRUE, quietly = TRUE)
}

# Load data into a data.frame
ipinfo <-
    structure(list(ip = c("128.95.21.102", "205.175.118.2", "209.124.188.132",
                          "198.104.202.6", "198.104.202.5", "64.86.123.41", 
                          "64.86.124.25", "64.86.124.30", "66.198.96.45", 
                          "66.198.96.34", "200.0.16.85", "200.0.16.196", 
                          "200.0.16.182", "169.158.128.54"), 
                   country_code = c("US", "US", "US", "US", "US", "US", "US", 
                                    "US", "CA", "CA", "CU", "CU", "CU", "CU"), 
                   region_code = c("WA", "WA", "WA", "CO", "CO", "DE", "DE", 
                                   "DE", "QC", "QC", "", "", "", "08"), 
                   city = c("Seattle","Seattle", "Seattle", "Englewood", 
                            "Englewood", "Wilmington", "Wilmington", 
                            "Wilmington", "Montreal", "Montreal", "", "", "", 
                            "Habana"), 
                   latitude = c(47.6606, 47.6606, 47.6606, 39.6237, 39.6237, 
                                39.7351, 39.7351, 39.7351, 45.498, 45.498, 
                                21.5, 21.5, 21.5, 22.2514), 
                   longitude = c(-122.2919, -122.2919, -122.2919, -104.8738, 
                                 -104.8738, -75.6684, -75.6684, -75.6684, 
                                 -73.5472, -73.5472, -80, -80, -80, -78.9131), 
                   mean_rtt = c(1, 1, 1.3, 1.3, 1.7, 1.3, 2, 70.7, 70.3, 261.7, 
                                213.3, 210, 220.7, 226.7)), 
              .Names = c("ip", "country_code", "region_code", "city", 
                         "latitude", "longitude", "mean_rtt"), 
              row.names = c(NA, -14L), class = "data.frame")

# Create the leaflet and display in the RStudio Viewer tab
attach(ipinfo)

leaflet() %>%
    addTiles() %>%
    addPolylines(longitude, latitude) %>%
    addCircleMarkers(longitude, latitude, color = '#ff0000',
                     popup=paste(city, region_code, country_code, "-",
                                 ip, "(", round(mean_rtt, 0), "ms )"))

ipinfo %>% pandoc.table(style="rmarkdown")
detach(ipinfo)
