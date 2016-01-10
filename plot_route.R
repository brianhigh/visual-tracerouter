# --------------------------------------------------------------------------
# title:  plot_route.R
# descr:  Plot hops to reach hosts by round-trip time (rtt) with error bars.
# author: Brian High
# date:   07 Jan. 2016
# --------------------------------------------------------------------------
#
# Note: Only those hops with rtt data will be plotted; others are ignored.
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

# Function to install packages as needed then load them into R.
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

# Function to import and tidy route hop data from CSV file.
import.route <- function(fname, host) {
    if (file.exists(fname) == TRUE) {
        route <- read.table(fname, sep=",", header=T, stringsAsFactors=F)
        route$hop <- row.names(route)
        #route$mean_rtt <- NULL
        route$hop <- sprintf("%02d", as.numeric(route$hop))
        route$hop <- as.factor(route$hop)
        route.long <- melt(route, id=c("hop", "addr", "mean_rtt"))
        names(route.long) <- c("hop", "addr", "mean_rtt", "query", "rtt")
        route.long$host <- rep(host, nrow(route.long))
        route.long$query <- as.factor(gsub("rtt.", "", route.long$query, 
                                           fixed = TRUE))
        return(na.omit(as.data.table(route.long)))
    } else {
        return(as.data.table(NULL))
    }
}

# Load packages needed for this script.
load_packages(c("reshape", "Rmisc", "ggplot2", "data.table"))

# Search the "data" folder for "route.csv" files.
hosts <- gsub("_", ".", list.dirs('data', recursive=FALSE, full.names=FALSE))
files <- gsub("$", "/route.csv", list.dirs('data', recursive=FALSE))

# Process the routes using the route info.
routes <- rbindlist(lapply(1:length(files), 
                        function(n) import.route(files[n], hosts[n])))

# Combine all route data into a singe data table.
route.summary <- summarySE(routes, measurevar="rtt", 
                           groupvars=c("host", "hop", "mean_rtt"))

# Make a basic line and point graph with error bars.

# Plotting code (below) based on this example:
# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)

# Use 95% confidence interval for error bars.
g <- ggplot(route.summary, aes(x=hop, y=rtt, colour=host)) + 
    geom_errorbar(aes(ymin=rtt-ci, ymax=rtt+ci), colour="black", width=.1) +
    geom_line(aes(group=host), size=2) +
    theme_grey(base_size = 20) +
    xlab("Network hop") + ylab("Round-trip time (ms)") +
    ggtitle("Round-trip time (ms) per network hop")
plot(g)