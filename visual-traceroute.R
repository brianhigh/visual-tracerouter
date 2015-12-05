# -------------------------------------------------------------------------
# title: visual-tracerouter
# description: Plot a map of the route of internet traffic to a remote host
# author: Brian High
# date: 4 Dec. 2015
# -------------------------------------------------------------------------

# -------------
# Configuration
# -------------

# Enter the remote address (a domain name or IP address)
domain <- "www.cubagob.cu"

# TRUE will save time if you just want to plot old data
use.cache <- TRUE

# TRUE will save plots to PNG files.
save.plot <- TRUE

# Choose "maps" or "ggmap" to specify the package to use for mapping
map.pkg <- "maps"

# ------------------
# Package Management
# ------------------

# Install packages (if necessary)
for (pkg in c("rjson", "dplyr", "ggmap", "maps")) {
    if (! suppressWarnings(require(pkg, character.only=TRUE)) ) {
        install.packages(pkg, repos="http://cran.fhcrc.org", dependencies=TRUE)
        if (! suppressWarnings(require(pkg, character.only=TRUE)) ) {
            stop(paste0(c("Can't load package: ", pkg, "!"), collapse = ""))
        }
    }
}

# ---------
# Functions
# ---------

# https://heuristically.wordpress.com/2013/05/20/geolocate-ip-addresses-in-r/
# http://www.dataanalysistools.net/geocode-ip-addresses-in-r/
freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
    library(rjson)
    if (1 == length(ip)) {
        # a single IP address
        require(rjson)
        url <- paste(c("http://freegeoip.net/json/", ip), collapse='')
        ret <- fromJSON(readLines(url, warn=FALSE))
        if (format == 'dataframe') {
            ret <- data.frame(t(unlist(ret)))
            return(ret)
        } else {
            ret <- data.frame()
            for (i in 1:length(ip)) {
                r <- freegeoip(ip[i], format="dataframe")
                ret <- rbind(ret, r)
            }
        }
        return(ret)
    }
}  

# Error handling freegeoip function supplement
try.ip <- function(ip) suppressWarnings(try(freegeoip(ip), silent = TRUE))

trace_router <- function(x) {
    # Run the system traceroute utility on the supplied address
    route <- c()
    sysname <- Sys.info()["sysname"]
    
    if (sysname == "Windows") {
        route <- try(system(paste("tracert", "-d", x), intern = TRUE))
    }
    else {
        route <- try(system(paste("traceroute", "-n", x), intern = TRUE))
    }
    
    if (length(route) > 0) {
        pattern <- "(?:[0-9]{1,3}\\.){3}[0-9]{1,3}"
        route <- gsub(
            pattern = paste0(c(".* (", pattern, ") .*"), collapse=""), 
            x=route, replacement = "\\1")
        route <- route[grepl(
            pattern = paste0(c("^", pattern, "$"), collapse=""), 
            x=route)]
        write.csv(route, "route.csv", row.names = FALSE)
    }

    return(route)
}

get_ipinfo <- function (route) {
    # Get geolocation info for all IP addresses in route
    library(dplyr)
    ipinfo <- rbind_all(
        lapply(route, function(x) suppressWarnings(as.data.frame(try.ip(x), 
                                                stringsAsFactors=FALSE))))
    
    ipinfo <- ipinfo[ipinfo$latitude != 0 & ipinfo$longitude != 0, ]
    write.csv(ipinfo, "ipinfo.csv", row.names = FALSE)
    
    return(ipinfo)
}

plot_ggmap <- function(ipinfo) {
    # Plot using the ggmap package.
    
    # Find end points of each segment by copying lat/lon and shifting up a row.
    ipinfo$next_latitude <- c(ipinfo[-1, "latitude"], ipinfo[1, "latitude"])
    ipinfo$next_longitude <- c(ipinfo[-1, "longitude"], ipinfo[1, "longitude"])
    
    # Limit dataset to valid segments by removing last row
    points <- ipinfo[-nrow(ipinfo), c("latitude", "longitude", 
                                      "next_latitude", "next_longitude")] 

    # Plot map
    library(ggmap)
    q <- qmplot(longitude, latitude, data = points, 
           maptype = "toner-lite", color = I("red"), 
           geom = "segment", xend=next_longitude, yend=next_latitude)
    
    if (save.plot == TRUE) {
        dev.copy(png,'route_ggmap.png')
        dev.off()
    }
    
    return(q)
}

plot_map <- function(ipinfo) {
    # Plot using the maps package.
    
    # Calculate map boundaries
    ipinfo$latitude <- as.numeric(ipinfo$latitude)
    ipinfo$longitude <- as.numeric(ipinfo$longitude)
    maxlat <- ceiling(max(ipinfo$latitude))
    minlat <- floor(min(ipinfo$latitude))
    maxlon <- ceiling(max(ipinfo$longitude))
    minlon <- floor(min(ipinfo$longitude))
    deltalat <- maxlat - minlat
    deltalon <- maxlon - minlon
    maxlat <- maxlat + deltalat/4
    minlat <- minlat - deltalat/4
    maxlon <- maxlon + deltalon/4
    minlon <- minlon - deltalon/4
    
    # Plot map
    library(maps)
    map("world", xlim=c(minlon,maxlon), ylim=c(minlat,maxlat), 
        col="gray90", fill=TRUE)
    points(x = points$longitude, y = points$latitude, col = "red")
    lines(x = points$longitude, y = points$latitude, col = "blue")
    
    if (save.plot == TRUE) {
        dev.copy(png,'route_map.png')
        dev.off()
    }
}

# ------------
# Main Routine
# ------------

if (use.cache == TRUE & file.exists("route.csv")) {
    route <- read.csv("route.csv", stringsAsFactors=FALSE)
} else {
    # This may take a while...
    route <- trace_router(domain)
}

if (length(route) > 0) {
    
    if (use.cache == TRUE & file.exists("ipinfo.csv")) {
        ipinfo <- read.csv("ipinfo.csv", stringsAsFactors=FALSE)
    } else {
        # This may take a while...
        ipinfo <- get_ipinfo(route)
    }
    
    if (length(ipinfo) > 0) {
        if (map.pkg == "ggmap") plot_ggmap(ipinfo)
        if (map.pkg == "maps") plot_map(ipinfo)
    }
}