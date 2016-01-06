# Plot network hops to reach hosts by round-trip time (rtt) with error bars.

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
        route$mean_rtt <- NULL
        route$hop <- sprintf("%02d", as.numeric(route$hop))
        route$hop <- as.factor(route$hop)
        route.long <- melt(route, id=c("hop","addr"))
        names(route.long) <- c("hop", "addr", "query", "rtt")
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
route.summary <- summarySE(routes, measurevar="rtt", groupvars=c("host", "hop"))

# Make a basic line and point graph with error bars.

# Plotting code (below) based on this example:
# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)

# Use 95% confidence interval for error bars.
g <- ggplot(route.summary, aes(x=hop, y=rtt, colour=host)) + 
    geom_errorbar(aes(ymin=rtt-ci, ymax=rtt+ci), width=.1, size=2) +
    geom_line(aes(group=host), size=2) +
    theme_bw() + 
    xlab("network hop") + ylab("round-trip time (ms)") +
    ggtitle("Round-trip time (ms) per network hop")
plot(g)