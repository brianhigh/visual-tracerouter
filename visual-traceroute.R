# --------------------------------------------------------------------------
# title:  visual-tracerouter
# descr:  Plot a map of the route of internet traffic to a remote host
# author: Brian High
# date:   10 Jan. 2016
# --------------------------------------------------------------------------
#
# Copyright (C) 2016 Brian High (https://github.com/brianhigh)
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# --------------------------------------------------------------------------
#
# NOTES:
#
# You can also run from a command-line shell (Terminal) prompt in the form:
#   Rscript visual-traceroute.R "arg1='value'; arg2='value'; arg3='...'"
#
# Example: Trace a route to www.gov.bb and show the plot in a new window.
#   Rscript visual-traceroute.R "addr='www.gov.bb'; new.win='TRUE'"
#
# Any option not listed on the command-line will use the defaults (below).
# Aside from specifying the "addr" to route, the other defaults were chosen 
# to maximize speed (e.g. use of a cache and the "maps" package over "ggmap").
#
# --------------------------------------------------------------------------

# Clear R's workspace memory to start with a fresh workspace.
rm(list=ls())

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
# NOTE: If FALSE, RStudio (and RGUI on Windows) will show the map anyway.
#       As such, this feature is mainly to control behavior from a Terminal.
new.win <- FALSE

# TRUE will print a text table of route data (or FALSE will not).
show.table <- TRUE

# Choose "maps", "ggmap", or "leaflet" to specify the mapping package.
# NOTE: "leaflet" opens the map only when running from within RStudio.
map.pkg <- "maps"

# TRUE will use ICMP ECHO Request messages for traceroute (or FALSE will not).
# This option may require administrative rights on Linux when set to TRUE.
# However, this option may be required to get through certain firewalls.
# Ignore this for Windows, since ICMP ECHO is the default in Windows (tracert).
use.icmp <- TRUE

# These folders will be used to store data (text) and images.
data.dir <- "data"
images.dir <- "images"

# Set .all_aesthetics to avoid error: "object '.all_aesthetics' not found"
.all_aesthetics <- suppressWarnings(unlist(getAnywhere(.all_aesthetics)[1:42]))

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
# This function is (c) Andrew Ziem and DataAnalysisTools.net, respectively.
freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe')) {
  # Look up information about an IP address using an online service.
  
  # Only load these packages if this function is called.
  load_packages(c("rjson"))
  
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

# Error handling freegeoip function 
# (by flodel http://stackoverflow.com/questions/17536221)
try_ip <- function(ip) suppressWarnings(try(freegeoip(ip), silent = TRUE))

trace_parser <- function(pattern) {
  # Read, parse, and clean the output of traceroute from a text file.
  
  # Only load these packages if this function is called.
  load_packages(c("stringr", "gsubfn"))
  
  # Using supplied pattern, parse traceroute text, grabbing matching strings.
  if (pattern != "" & file.exists(files$route.txt) == TRUE) {
    route.string <- paste(readLines(files$route.txt), collapse=" ")
    route <- unlist(str_extract_all(route.string, pattern))[-1]
    rtt <- strapply(route, "([0-9.]+) ms", as.numeric)
    addrs <- strapply(route, "(?:[0-9]{1,3}\\.){3}[0-9]{1,3}")
    route <- data.frame(unlist(addrs), sapply(rtt, mean), 
                        stringsAsFactors=FALSE)
    names(route) <- c("addr", "mean_rtt")
    route <- route[complete.cases(route), ]
    
    # Store the individual rtt values, or NA if missing.
    m <- max(sapply(rtt, length))
    route$rtt.1 <- mapply(function(x, y, z) { 
      length(x) <- z; x[y] }, rtt, 1, m)
    route$rtt.2 <- mapply(function(x, y, z) { 
      length(x) <- z; x[y] }, rtt, 2, m)
    route$rtt.3 <- mapply(function(x, y, z) { 
      length(x) <- z; x[y] }, rtt, 3, m)
    
    if (nrow(route) > 0) {
      write.csv(route, files$route.csv, row.names = FALSE)
    }
    
    return(route)
    
  } else {
    # Return an empty data frame if this fails.
    return(data.frame(x=NULL))
  }
}

trace_router <- function(addr) {
  # Run the system traceroute utility on the address and save output as text.
  
  pattern <- ""
  
  # Contruct a pattern-match string (regular expression) and shell command.
  if (Sys.info()["sysname"] == "Windows") {
    # Windows uses a `tracert` command to trace internet routes.
    pattern <- "(?:<?[0-9.]+ ms[ *]+)*(?:[0-9]{1,3}\\.){3}[0-9]{1,3}"
    traceroute <- paste(
      'cmd /c "tracert -d -h 30', addr, '>', files$route.txt, '"')
  } else {
    # POSIX systems (OSX, Linux, Unix) use a similar `traceroute` command.
    pattern <- "(?:[0-9]{1,3}\\.){3}[0-9]{1,3}(?:[ *]+<?[0-9.]+ ms)*"
    traceroute <- "traceroute -n -m 30"
    
    # Use the command-line option "-I" if we configured for ICMP ECHO.
    if (use.icmp == TRUE) { 
      traceroute <- paste0(c(traceroute, " -I"), collapse = "")
    }
    
    traceroute <- paste(traceroute, addr, ">", files$route.txt)
  }
  
  if (use.cache == FALSE | file.exists(files$route.txt) == FALSE) {
    # Run the shell command contructed above to run traceroute.
    cat(paste("\n", "Running command:", "\n", traceroute, "\n"))
    res <- try(system(traceroute, intern = TRUE))
  }
  
  # Convert text output into a data frame and return it.
  return(trace_parser(pattern))
}

get_ipinfo <- function (route) {
  # Get geolocation info for all IP addresses in route.
  
  # Only load these packages if this function is called.
  load_packages(c("data.table"))
  
  # Use rbindlist function of datatable package to avoid coersion warnings.
  ipinfo <- as.data.frame(rbindlist(lapply(route$addr, function(x) 
    as.data.table(try_ip(x))), fill=TRUE))
  
  ipinfo$mean_rtt <- route$mean_rtt
  ipinfo <- ipinfo[ipinfo$latitude != 0 & ipinfo$longitude != 0, ]
  ipinfo$latitude <- as.numeric(as.character(ipinfo$latitude))
  ipinfo$longitude <- as.numeric(as.character(ipinfo$longitude))
  rownames(ipinfo) <- NULL
  write.csv(ipinfo, files$ipinfo.csv, row.names = FALSE)
  
  return(ipinfo)
}

get_endpoints <- function(ipinfo) {
  # Find end points of each segment by copying lat/lon and shifting up a row.
  
  ipinfo$next_latitude <- as.vector(
    c(ipinfo$latitude[-1], tail(ipinfo$latitude, 1)), mode="numeric")
  ipinfo$next_longitude <- as.vector(
    c(ipinfo$longitude[-1], tail(ipinfo$longitude, 1)), mode="numeric")
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
  
  # Only load these packages if this function is called.
  load_packages(c("ggmap"))
  
  # Create the plot
  p <- qmplot(x = longitude, y = latitude, data = ipinfo, source = "stamen", 
              maptype = "toner-lite", mapcolor = "bw", color = I("red"), 
              xend = next_longitude, yend = next_latitude, geom = "segment")
  
  # BUG: If you see these errors, run the script a second time from R.
  #   Error: geom_segment requires the following missing aesthetics: x, y
  #   Error in eval(expr, envir, enclos) : object 'next_longitude' not found
  #
  # > source("./visual-traceroute.R")
  # 
  # Tracing route to: www.gov.za ... 
  # Using zoom = 3...
  # Error in eval(expr, envir, enclos) : object 'next_longitude' not found
  #
  # > source("./visual-traceroute.R")
  #
  # Tracing route to: www.gov.za ... 
  # Using zoom = 3...
  # Saving 6.99 x 7 in image
  #
  # Note: Until this bug is fixed, map.pkg='ggmap' will not work from Rscript.
  
  # Show plot in separate graphics device window.
  if (new.win == TRUE) x11()
  
  print(p)
  if (interactive() == FALSE) gglocator(1)
  
  # Save the plot as a PNG image file.
  if (save.plot == TRUE) {
    ggsave(file=files$ggmap.png, plot=p)
  }
}

make_plot <- function(ipinfo, bbox) {
  # Make plot using the maps package and base plotting.
  
  # Only load these packages if this function is called.
  load_packages(c("maps"))
  
  attach(ipinfo)
  attach(bbox)
  map("world", xlim=c(minlon,maxlon), 
      ylim=c(minlat,maxlat), 
      col="gray90", fill=TRUE)
  points(x = longitude, y = latitude, col = "red")
  lines(x = longitude, y = latitude, col = "blue")
  text(longitude, latitude, city, 
       cex=.7, adj=0, pos=1, col="red")
  if (interactive() == FALSE & new.win == TRUE) locator(1)
  detach(bbox)
  detach(ipinfo)
}

make_leaflet <- function(ipinfo) {
  # Plot using the leaflet package.
  
  # Only shows the map in "Viewer" tab if run manually from RStudio console.
  # So, we will instead save the leaflet as a web page (HTML).  The map will 
  # then show in browser window (automatically) if running from RStudio.
  
  # Only load these packages if this function is called.
  load_packages(c("magrittr", "leaflet", "htmlwidgets", "rstudioapi"))
  
  # Create the leaflet.
  attach(ipinfo)
  l <- leaflet() %>% 
    addTiles() %>% 
    addPolylines(longitude, latitude) %>%
    addCircleMarkers(longitude, latitude, color = '#ff0000', 
                     popup=paste(city, region_code, country_code, "-",
                                 ip, "(", round(mean_rtt, 0), "ms )"))
  
  detach(ipinfo)
  
  # Store leaflet in an HTML file. (Will be overwritten if already exists.)
  leaflet.html <- "leaflet.html"
  saveWidget(l, file=leaflet.html)
  
  # Only open HTML file in web browser if running this script from RStudio.
  if (Sys.getenv("RSTUDIO") == "1") {
    viewer <- getOption("viewer")
    viewer(leaflet.html) 
  }
  
  # NOTE: We will not save this output with other cached map output,
  # but will, instead, regenerate this HTML file as needed if rerun.
}

plot_maps <- function(ipinfo, bbox) {
  # Plot a route on a map using the maps package.
  
  # Get unique locations to minimize the overwriting of labels.
  ipinfo <- unique(ipinfo[,c(3, 5, 6, 8, 9, 10)])
  
  # Show plot in separate graphics device window.
  if (new.win == TRUE) x11()
  
  # Show a base plot with the "world" map from "maps".
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
  
  # Only load these packages if this function is called.
  load_packages(c("png"))
  
  if (new.win == TRUE) x11()
  plot.new()
  img <- readPNG(image)
  grid::grid.raster(img)
  
  if (interactive() == FALSE) {
    locator(1)
    dev.off()
  }
}

make_rtt_plot <- function(route) {
  # Produce a line plot of mean rtt by route hop using base graphics.
  
  plot.new()
  
  plot(rownames(route), route$mean_rtt, 
       xlab = "Route Hops", ylab = "Mean RTT (ms)", 
       main="Mean RTT by Route Hop")
  lines(rownames(route), route$mean_rtt)
}

print_route_table <- function(ipinfo) {
  # Print out a table of the route.
  
  # Only load these packages if this function is called.
  load_packages(c("magrittr", "knitr"))
  
  ipinfo[, c("ip", "mean_rtt", "city", "region_name", "country_name")] %>% 
    kable()
}

# ------------
# Main Routine
# ------------

# Clean up addr if it is actually a url instead of a domain name.
addr <- gsub(pattern = "^(?:[a-zA-Z]*:\\/\\/)?([^\\/:]+).*", 
             replacement = "\\1", addr)

# Create a data frame of files to use for input and output.
files <- create_folders_and_filenames(gsub("\\.", "_", addr), 
                                      data.dir, images.dir)

# Send message to user.
cat(paste(c("\n", "Tracing route to:", addr, "...", "\n")))

# Get route.
if (use.cache == TRUE & file.exists(files$route.csv) == TRUE) {
  route <- read.csv(files$route.csv, stringsAsFactors=FALSE)
} else {
  cat(paste(c(" This may take a while ..."), "\n"))
  route <- trace_router(addr)
}

if (nrow(route) > 0) {
  
  # Get geocoded route.
  if (use.cache == TRUE & file.exists(files$ipinfo.csv) == TRUE) {
    ipinfo <- read.csv(files$ipinfo.csv, stringsAsFactors=FALSE)
  } else {
    cat(paste(
      c("\n", "Geocoding addresses ... This may take a while ..."), "\n"))
    ipinfo <- get_ipinfo(route)
  }
  
  # Deliver output maps: and tables.
  if (nrow(ipinfo) > 0) {    
    if (map.pkg == "ggmap") {
      if (use.cache == TRUE & file.exists(files$ggmap.png) == TRUE) {
        view_image(files$ggmap.png)
      } else plot_ggmap(get_endpoints(ipinfo))
    }
    
    if (map.pkg == "maps") {
      if (use.cache == TRUE & file.exists(files$map.png) == TRUE) {
        view_image(files$map.png)
      }
      else plot_maps(ipinfo, get_bbox(ipinfo))
    }
    
    if (map.pkg == "leaflet") make_leaflet(ipinfo)
    
    if (show.table == TRUE) print_route_table(ipinfo)
    
    # Plot the mean round-trip-times (rtt) for each hop in the route.
    # Not yet fully incorporated in script, but you can run from R console.
    #make_rtt_plot(route)
  }
}
