# -------------------------------------------------------------------------
# title: visual-tracerouter
# description: Plot a map of the route of internet traffic to a remote host
# author: Brian High
# date: 5 Dec. 2015
# -------------------------------------------------------------------------

# -------------
# Configuration
# -------------

# Enter the remote address (a domain name or an IPv4 address)
# Example-1: addr <- "www.cubagob.cu"
# Example-2: addr <- 169.158.128.86
addr <- "www.cubagob.cu"

# TRUE will save time if you just want to plot old data (or FALSE)
use.cache <- FALSE

# TRUE will save plots to PNG files (or FALSE)
save.plot <- TRUE

# Choose "maps" or "ggmap" to specify the package to use for mapping
map.pkg <- "maps"

# These folders will be used to store data (text) and images
datadir <- "data"
imagesdir <- "images"

# ------------------
# Package Management
# ------------------

# Install packages (if necessary)
for (pkg in c("stringr", "rjson", "dplyr", "ggmap", "maps")) {
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

create_folders_and_filenames <- function(file_addr, datadir, imagesdir) {
    # File and folder management
    
    # Create folders if not already present
    dir.create(file.path(datadir), showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(imagesdir), showWarnings = FALSE, recursive = TRUE)
    
    # Construct paths to files
    files <- data.frame(
        routeTxtFile <- paste0(c(datadir, "/", file_addr, "_route.txt"), 
                               collapse = ""),
        routeCsvFile = paste0(c(datadir, "/", file_addr, "_route.csv"), 
                              collapse = ""),
        ipinfoCsvFile = paste0(c(datadir, "/", file_addr, "_ipinfo.csv"), 
                               collapse = ""),
        mapPngFile = paste0(c(imagesdir, "/", file_addr, "_map.png"), 
                            collapse = ""),
        ggmapPngFile = paste0(c(imagesdir, "/", file_addr, "_ggmap.png"), 
                              collapse = ""),
        stringsAsFactors=FALSE
    )
    
    return(files)
}

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
    
    if (use.cache == FALSE | file.exists(files$routeTxtFile) == FALSE) {
        if (sysname == "Windows") {
            res <- try(system(
                paste("tracert", "-d", x, ">", files$routeTxtFile), 
                intern = TRUE))
        }
        else {
            res <- try(system(
                paste("traceroute", "-n", x, ">", files$routeTxtFile), 
                intern = TRUE))
        }        
    }
    
    if (file.exists(files$routeTxtFile) == TRUE) {
        routeString <- paste(readLines(files$routeTxtFile), collapse=" ")
        pattern <- "(?:[0-9]{1,3}\\.){3}[0-9]{1,3}"
        route <- unlist(str_extract_all(routeString, pattern))[-1]
    
        if (length(route) > 0) {
            write.csv(route, files$routeCsvFile, row.names = FALSE)
        }
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
    ipinfo$latitude <- as.numeric(ipinfo$latitude)
    ipinfo$longitude <- as.numeric(ipinfo$longitude)
    write.csv(ipinfo, files$ipinfoCsvFile, row.names = FALSE)
    
    return(ipinfo)
}

get_endpoints <- function(ipinfo) {
    # Find end points of each segment by copying lat/lon and shifting up a row.
    ipinfo$next_latitude <- as.vector(
        c(ipinfo$latitude[-1], last(ipinfo$latitude)), mode="numeric")
    ipinfo$next_longitude <- as.vector(
        c(ipinfo$longitude[-1], last(ipinfo$longitude)), mode="numeric")
    return(ipinfo)
}

get_bbox <- function(ipinfo) {   
    # Calculate map boundaries
    maxlat <- ceiling(max(ipinfo$latitude))
    minlat <- floor(min(ipinfo$latitude))
    maxlon <- ceiling(max(ipinfo$longitude))
    minlon <- floor(min(ipinfo$longitude))
    
    # Make sides of the box one quarter larger as a border
    deltalat <- maxlat - minlat
    deltalon <- maxlon - minlon    
    bbox <- data.frame(
        maxlat = c(maxlat + deltalat/4),
        minlat = c(minlat - deltalat/4),
        maxlon = c(maxlon + deltalon/4),
        minlon = c(minlon - deltalon/4))
    return(bbox)
}

plot_ggmap <- function(ipinfo) {
    # Plot using the ggmap package.
    library(ggmap)
    p <- qmplot(longitude, latitude, data = ipinfo, 
                maptype = "toner-lite", color = I("red"), 
                geom = "segment", xend=next_longitude, yend=next_latitude)
    print(p)
    
    if (save.plot == TRUE) {
        dev.copy(png, files$ggmapPngFile)
        dev.off()
    }
}

plot_maps <- function(ipinfo, bbox) {
    # Plot using the maps package.
    library(maps)
    map("world", xlim=c(bbox$minlon,bbox$maxlon), 
        ylim=c(bbox$minlat,bbox$maxlat), 
        col="gray90", fill=TRUE)
    points(x = ipinfo$longitude, y = ipinfo$latitude, col = "red")
    lines(x = ipinfo$longitude, y = ipinfo$latitude, col = "blue")
    
    if (save.plot == TRUE) {
        dev.copy(png, files$mapPngFile)
        dev.off()
    } 
}

# ------------
# Main Routine
# ------------

file_addr <- gsub("\\.", "_", addr)
files <- create_folders_and_filenames(file_addr, datadir, imagesdir)

if (use.cache == TRUE & file.exists(files$routeCsvFile) == TRUE) {
    route <- read.csv(files$routeCsvFile, stringsAsFactors=FALSE)
} else {
    # This may take a while...
    route <- trace_router(addr)
}

if (length(route) > 0) {
    
    if (use.cache == TRUE & file.exists(files$ipinfoCsvFile) == TRUE) {
        ipinfo <- read.csv(files$ipinfoCsvFile, stringsAsFactors=FALSE)
    } else {
        # This may take a while...
        ipinfo <- get_ipinfo(route)
    }
    
    if (length(ipinfo) > 0) {
        if (map.pkg == "ggmap") plot_ggmap(get_endpoints(ipinfo))
        if (map.pkg == "maps") plot_maps(ipinfo, get_bbox(ipinfo))
    }
}