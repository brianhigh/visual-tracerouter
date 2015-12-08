# --------------------------------------------------------------------------
# title:  visual-tracerouter
# descr:  Plot a map of the route of internet traffic to a remote host
# author: Brian High
# date:   7 Dec. 2015
# 
# Copyright (c) 2015 Brian High (https://github.com/brianhigh). MIT LICENSE.
# --------------------------------------------------------------------------

# You can also run from a command-line shell (Terminal) prompt in the form:
#   Rscript visual-traceroute.R "arg1='value'; arg2='value'; arg3='...'"
#
# Example: Trace a route to www.gov.za and show the plot in a new window.
#   Rscript visual-traceroute.R "addr='www.gov.za'; new.win='TRUE'"
#
# Any option not listed on the command-line will use the defaults (below).
# Aside from specifying the "addr" to route, the other defaults were chosen 
# to maximize speed (e.g. use of a cache and the "maps" package over "ggmap").

# -------------
# Configuration
# -------------

# Configure default parameters here, or supply them as command-line arguments.

# Enter the remote address (a domain name or an IPv4 address).
# Example-1: addr <- "www.cubagob.cu"
# Example-2: addr <- "169.158.128.86"
addr <- "www.cubagob.cu"

# TRUE will use old data if present (or FALSE will not, regenerating it).
use.cache <- TRUE

# TRUE will save plots to PNG files (or FALSE will not).
save.plot <- TRUE

# TRUE will open a separate window to show the map (or FALSE will not).
# Note: If FALSE, RStudio and RGUI on Windows will show the map anyway.
#       As such, this feature is mainly to control behavior from a Terminal.
new.win <- FALSE

# Choose "maps" or "ggmap" to specify the package to use for mapping.
map.pkg <- "maps"

# These folders will be used to store data (text) and images.
data.dir <- "data"
images.dir <- "images"

# --------------------------
# Parse command-line options
# --------------------------

args=(commandArgs(TRUE))
if(length(args) > 0) {
    for(i in 1:length(args)) {
        eval(parse(text=args[[i]]))
    }
}

# ---------
# Functions
# ---------

load_packages <- function(pkgs) {
    # Install packages (if necessary) and load them into memory.
    
    for (pkg in pkgs) {
        if (! suppressWarnings(suppressPackageStartupMessages(require(
            pkg, character.only=TRUE, quietly=TRUE))) ) {
            install.packages(pkg, repos="http://cran.fhcrc.org", 
                             dependencies=TRUE)
            if (! suppressWarnings(suppressPackageStartupMessages(require(
                pkg, character.only=TRUE, quietly=TRUE))) ) {
                stop(paste0(c("Can't load package: ", pkg, "!"), 
                            collapse = ""))
            }
        }
    }
}

create_folders_and_filenames <- function(file.addr, data.dir, images.dir) {
    # File and folder management
    
    # Create folders if not already present.
    dir.create(file.path(data.dir, file.addr), 
               showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(images.dir, file.addr), 
               showWarnings = FALSE, recursive = TRUE)
    
    # Construct paths to files.
    files <- data.frame(
        route.txt <- file.path(data.dir, file.addr, "route.txt"),
        route.csv = file.path(data.dir, file.addr, "route.csv"),
        ipinfo.csv = file.path(data.dir, file.addr, "ipinfo.csv"),
        map.png = file.path(images.dir, file.addr, "map.png"),
        ggmap.png = file.path(images.dir, file.addr, "ggmap.png"),
        stringsAsFactors=FALSE
    )
    
    return(files)
}

# https://heuristically.wordpress.com/2013/05/20/geolocate-ip-addresses-in-r/
# http://www.dataanalysistools.net/geocode-ip-addresses-in-r/
# This function is ? Andrew Ziem and DataAnalysisTools.net, respectively.
freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe')) {
    # Look up information about an IP address using an online service.
  
    if (1 == length(ip)) {
        # A single IP address
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

# Error handling freegeoip function ? Andrew Ziem
try_ip <- function(ip) suppressWarnings(try(freegeoip(ip), silent = TRUE))

trace_router <- function(x) {
    # Run the system traceroute utility on the supplied address.
    
    pattern <- ""
    route <- c()
    
    if (use.cache == FALSE | file.exists(files$route.txt) == FALSE) {
        if (Sys.info()["sysname"] == "Windows") {
            pattern <- "(?:<?[0-9.]+ ms[ *]+)*(?:[0-9]{1,3}\\.){3}[0-9]{1,3}"
            res <- try(
                system(paste(
                    'cmd /c "tracert -d -h 30', x, '>', files$route.txt, '"'), 
                intern = TRUE))
        }
        else {
            pattern <- "(?:[0-9]{1,3}\\.){3}[0-9]{1,3}(?:[ *]+<?[0-9.]+ ms)*"
            res <- try(
                system(paste(
                    "traceroute -n -m 30", x, ">", files$route.txt), 
                intern = TRUE))
        }        
    }
    
    if (file.exists(files$route.txt) == TRUE) {
        route.string <- paste(readLines(files$route.txt), collapse=" ")
        route <- unlist(str_extract_all(route.string, pattern))[-1]
        rtt <- strapply(route, "([0-9]+) ms", as.numeric)
        addrs <- strapply(route, "(?:[0-9]{1,3}\\.){3}[0-9]{1,3}")
        route <- data.frame(unlist(addrs), sapply(rtt, mean), 
                            stringsAsFactors=FALSE)
        names(route) <- c("addr", "mean_rtt")
        
        if (nrow(route) > 0) {
            write.csv(route, files$route.csv, row.names = FALSE)
        }
    }
    return(route)
}

get_ipinfo <- function (route) {
    # Get geolocation info for all IP addresses in route.
  
    library(dplyr)
    ipinfo <- rbind_all(
        lapply(route$addr, function(x) suppressWarnings(
            as.data.frame(try_ip(x), stringsAsFactors=FALSE))))
    
    ipinfo$mean_rtt <- route$mean_rtt
    ipinfo <- ipinfo[ipinfo$latitude != 0 & ipinfo$longitude != 0, ]
    ipinfo$latitude <- as.numeric(ipinfo$latitude)
    ipinfo$longitude <- as.numeric(ipinfo$longitude)
    rownames(ipinfo) <- NULL
    write.csv(ipinfo, files$ipinfo.csv, row.names = FALSE)
    
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
    # Calculate map boundaries.
  
    maxlat <- ceiling(max(ipinfo$latitude))
    minlat <- floor(min(ipinfo$latitude))
    maxlon <- ceiling(max(ipinfo$longitude))
    minlon <- floor(min(ipinfo$longitude))
    
    # Make sides of the box one quarter larger as a border.
    deltalat <- maxlat - minlat
    deltalon <- maxlon - minlon
    
    # To keep the box from being too skinny, use greatest delta.
    delta <- ifelse(abs(deltalon) > abs(deltalat), deltalon, deltalat)
    
    bbox <- data.frame(
        maxlat = c(maxlat + delta/4),
        minlat = c(minlat - delta/4),
        maxlon = c(maxlon + delta/4),
        minlon = c(minlon - delta/4))
    return(bbox)
}

plot_ggmap <- function(ipinfo) {
    # Plot using the ggmap package.
  
    library(ggmap)
    p <- qmplot(longitude, latitude, data = ipinfo, source="stamen", 
                maptype = "toner-lite", mapcolor="bw", color = I("red"), 
                geom = "segment", xend=next_longitude, yend=next_latitude)
    
    # Show plot in separate graphics device window.
    if (new.win == TRUE) x11()

    print(p)
    if (interactive() == FALSE) gglocator(1)

    # Save the plot as a PNG image file.
    if (save.plot == TRUE) {
        ggsave(file=files$ggmap.png, plot=p)
    }
}

plot_maps <- function(ipinfo, bbox) {
    # Get unique locations to minimize the overwriting of labels.
    ipinfo <- unique(ipinfo[,c(3, 5, 6, 8, 9, 10)])
    
    # Plot using the maps package.
    library(maps)
    make_plot <- function(ipinfo, bbox) {
        map("world", xlim=c(bbox$minlon,bbox$maxlon), 
            ylim=c(bbox$minlat,bbox$maxlat), 
            col="gray90", fill=TRUE)
        points(x = ipinfo$longitude, y = ipinfo$latitude, col = "red")
        lines(x = ipinfo$longitude, y = ipinfo$latitude, col = "blue")
        text(ipinfo$longitude, ipinfo$latitude, ipinfo$city, 
             cex=.7, adj=0, pos=1, col="gray30")
        if (interactive() == FALSE & new.win == TRUE) locator(1)
    }
    
    # Show plot in separate graphics device window.
    if (new.win == TRUE) x11()
    
    x11()
    make_plot(ipinfo, bbox)
    if (interactive() == FALSE) locator(1)
    
    # Save the plot as a PNG image file.
    if (save.plot == TRUE) {
        png(files$map.png)
        make_plot(ipinfo, bbox)
        dev.off()
    } 
}

view_image <- function(image) {
    # Load a PNG image from a file and view it.
  
    if (new.win == TRUE) x11()
    plot.new()
    img <- readPNG(image)
    grid::grid.raster(img)
    
    if (interactive() == FALSE) {
        locator(1)
        dev.off()
    }
}

print_route_table <- function(ipinfo) {
    # Print out a table of the route.
    ipinfo[, c("ip", "mean_rtt", "city", "region_name", "country_name")] %>% 
        pandoc.table(style="simple")
}

# ------------
# Main Routine
# ------------

pkgs <- c("stringr", "rjson", "dplyr", "ggmap", "maps", "pander", "png", 
          "gsubfn")
load_packages(pkgs)

files <- create_folders_and_filenames(gsub("\\.", "_", addr), 
                                      data.dir, images.dir)

cat(paste(c("Tracing route to:", addr, "...", "\n")))

if (use.cache == TRUE & file.exists(files$route.csv) == TRUE) {
    route <- read.csv(files$route.csv, stringsAsFactors=FALSE)
} else {
    cat(paste(c("This may take a while ..."), "\n"))
    route <- trace_router(addr)
}

if (nrow(route) > 0) {
    
    if (use.cache == TRUE & file.exists(files$ipinfo.csv) == TRUE) {
        ipinfo <- read.csv(files$ipinfo.csv, stringsAsFactors=FALSE)
    } else {
        cat(paste(c("Geocoding addresses ... This may take a while ..."), "\n"))
        ipinfo <- get_ipinfo(route)
    }
    
    if (nrow(ipinfo) > 0) {    
        if (map.pkg == "ggmap") {
            if (use.cache == TRUE & file.exists(files$ggmap.png) == TRUE) {
                view_image(files$ggmap.png)
            }
            else plot_ggmap(get_endpoints(ipinfo))
        }
        
        if (map.pkg == "maps") {
            if (use.cache == TRUE & file.exists(files$map.png) == TRUE) {
                view_image(files$map.png)
            }
            else plot_maps(ipinfo, get_bbox(ipinfo))
        }
        
        print_route_table(ipinfo)
    }
}