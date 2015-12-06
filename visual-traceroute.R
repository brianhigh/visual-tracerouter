# -------------------------------------------------------------------------
# title: visual-tracerouter
# description: Plot a map of the route of internet traffic to a remote host
# author: Brian High
# date: 5 Dec. 2015
# -------------------------------------------------------------------------

# You can also run from a command-line shell (Terminal) prompt in the form:
#   Rscript visual-traceroute.R "arg1='value'; arg2='value'; arg3='...'"
#
# Example: Trace a route to moosylvania.com and plot with ggmap.
#   Rscript visual-traceroute.R "addr='moosylvania.com'; map.pkg='ggmap'"
#
# Any option not listed on the command-line will use the defaults (below).

# -------------
# Configuration
# -------------

# Configure default parameters here, or supply them as command-line arguments.

# Enter the remote address (a domain name or an IPv4 address)
# Example-1: addr <- "www.cubagob.cu"
# Example-2: addr <- 169.158.128.86
addr <- "www.cubagob.cu"

# TRUE will save time if you just want to plot old data (or FALSE)
use.cache <- TRUE

# TRUE will save plots to PNG files (or FALSE)
save.plot <- TRUE

# Choose "maps" or "ggmap" to specify the package to use for mapping
map.pkg <- "maps"

# These folders will be used to store data (text) and images
data.dir <- "data"
images.dir <- "images"

# --------------------------
# Parse command-line options
# --------------------------
# Modified from: http://stackoverflow.com/questions/14167178

# Read in the arguments listed at the command line
args=(commandArgs(TRUE))

# Check to see if arguments are passed.
if(length(args) > 0) {
    # Cycle through each element of the list and evaluate the expressions.    
    for(i in 1:length(args)) {
        eval(parse(text=args[[i]]))
    }
}

# ------------------
# Package Management
# ------------------

# Install packages (if necessary)
for (pkg in c("stringr", "rjson", "dplyr", "ggmap", "maps", "pander")) {
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

create_folders_and_filenames <- function(file.addr, data.dir, images.dir) {
    # File and folder management
    
    # Create folders if not already present
    dir.create(file.path(data.dir, file.addr), 
               showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(images.dir, file.addr), 
               showWarnings = FALSE, recursive = TRUE)
    
    # Construct paths to files
    files <- data.frame(
        route.txt.file <- file.path(data.dir, file.addr, "route.txt"),
        route.csv.file = file.path(data.dir, file.addr, "route.csv"),
        ipinfo.csv.file = file.path(data.dir, file.addr, "ipinfo.csv"),
        map.png.file = file.path(images.dir, file.addr, "map.png"),
        ggmap.png.file = file.path(images.dir, file.addr, "ggmap.png"),
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
try_ip <- function(ip) suppressWarnings(try(freegeoip(ip), silent = TRUE))

trace_router <- function(x) {
    # Run the system traceroute utility on the supplied address
    route <- c()
    sysname <- Sys.info()["sysname"]
    
    if (use.cache == FALSE | file.exists(files$route.txt.file) == FALSE) {
        if (sysname == "Windows") {
            res <- try(system(
                paste('cmd /c "tracert', '-d', x, '>', files$route.txt.file, '"'), 
                intern = TRUE))
        }
        else {
            res <- try(system(
                paste("traceroute", "-n", x, ">", files$route.txt.file), 
                intern = TRUE))
        }        
    }
    
    if (file.exists(files$route.txt.file) == TRUE) {
        route.string <- paste(readLines(files$route.txt.file), collapse=" ")
        pattern <- "(?:[0-9]{1,3}\\.){3}[0-9]{1,3}"
        route <- unlist(str_extract_all(route.string, pattern))[-1]
        
        if (length(route) > 0) {
            write.csv(route, files$route.csv.file, row.names = FALSE)
        }
    }
    return(route)
}

get_ipinfo <- function (route) {
    # Get geolocation info for all IP addresses in route
    library(dplyr)
    ipinfo <- rbind_all(
        lapply(route, function(x) suppressWarnings(as.data.frame(try_ip(x), 
                                                                 stringsAsFactors=FALSE))))
    
    ipinfo <- ipinfo[ipinfo$latitude != 0 & ipinfo$longitude != 0, ]
    ipinfo$latitude <- as.numeric(ipinfo$latitude)
    ipinfo$longitude <- as.numeric(ipinfo$longitude)
    write.csv(ipinfo, files$ipinfo.csv.file, row.names = FALSE)
    
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
    x11()
    library(ggmap)
    p <- qmplot(longitude, latitude, data = ipinfo, 
                maptype = "toner-lite", color = I("red"), 
                geom = "segment", xend=next_longitude, yend=next_latitude)
    print(p)
    
    if (save.plot == TRUE) {
        dev.copy(png, files$ggmap.png.file)
        dev.off()
    }
}

plot_maps <- function(ipinfo, bbox) {
    # Plot using the maps package.
    x11()
    library(maps)
    map("world", xlim=c(bbox$minlon,bbox$maxlon), 
        ylim=c(bbox$minlat,bbox$maxlat), 
        col="gray90", fill=TRUE)
    points(x = ipinfo$longitude, y = ipinfo$latitude, col = "red")
    lines(x = ipinfo$longitude, y = ipinfo$latitude, col = "blue")
    locator(1)
    
    if (save.plot == TRUE) {
        dev.copy(png, files$map.png.file)
        dev.off()
    } 
}

# ------------
# Main Routine
# ------------

file.addr <- gsub("\\.", "_", addr)
files <- create_folders_and_filenames(file.addr, data.dir, images.dir)

if (use.cache == TRUE & file.exists(files$route.csv.file) == TRUE) {
    route <- read.csv(files$route.csv.file, stringsAsFactors=FALSE)
} else {
    # This may take a while...
    route <- trace_router(addr)
}

if (length(route) > 0) {
    
    if (use.cache == TRUE & file.exists(files$ipinfo.csv.file) == TRUE) {
        ipinfo <- read.csv(files$ipinfo.csv.file, stringsAsFactors=FALSE)
    } else {
        # This may take a while...
        ipinfo <- get_ipinfo(route)
    }
    
    if (nrow(ipinfo) > 0) {
        if (map.pkg == "ggmap") plot_ggmap(get_endpoints(ipinfo))
        if (map.pkg == "maps") plot_maps(ipinfo, get_bbox(ipinfo))
        
        # Print out a table of the route
        ipinfo[, c("ip", "city", "region_name", "country_name")] %>% 
            pandoc.table(style="simple")
    }
}