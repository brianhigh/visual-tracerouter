# Plot network hops to reach hosts by round-trip time (rtt) with error bars.

# Data files were created with these commands from the Terminal (console):
#
#  DOS> tracert -d example.com > "route.txt"
#
#  bash-linux$ sudo traceroute -I \
#          -n example.com 2>&1 | sed 's/\*/* ms/g' | tr -s " " "|" | \
#          tail -n +2 > "route.txt"
#          
#  bash-osx$ traceroute -I \
#          -n example.com 2>&1 | sed 's/\*/* ms/g' | tr -s " " "|" | \
#          tail -n +3 > "route.txt"

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

# Function to import route hop data from text files.
import.route <- function(fname, troute.type, host) {
    # We have two different formats, so parse differently for each type.
    if (troute.type == "tracert") {
        route <- read.fwf(fname, 
                          widths=c(3, 6, 3, 6, 3, 6, 3, 16), 
                          header=F, skip=4, strip.white=TRUE, 
                          stringsAsFactors=F)
        route <- route[,c(1, 2, 4, 6, 8)]
        route <- route[complete.cases(route),]
        names(route) <- c("hop", "query.1", "query.2", "query.3", "ip.addr")
        route[,2:4] <- as.numeric(gsub("[<*]", "", as.matrix(route[2:4])))
    } else {
        route <- read.table(fname, sep="|", header=F, stringsAsFactors=F)
        route <- route[,c(2, 3, 4, 6, 8)]
        route <- route[complete.cases(route),]
        names(route) <- c("hop", "ip.addr", "query.1", "query.2", "query.3")
        route[,3:5] <- as.numeric(gsub("<", "", as.matrix(route[3:5])))
        route <- route[, c(1, 3:5, 2)]
    }
    
    route$hop <- sprintf("%02d", as.numeric(route$hop))
    route$hop <- as.factor(route$hop)
    route.long <- melt(route, id=c("hop","ip.addr"))
    names(route.long) <- c("hop", "ip.addr", "query", "rtt")
    route.long$host <- rep(host, nrow(route.long))
    route.long$query <- as.factor(gsub("query.", "", route.long$query, 
                                       fixed = TRUE))
    return(na.omit(as.data.table(route.long)))
}

# Load packages needed for this script.
load_packages(c("reshape", "Rmisc", "ggplot2", "data.table"))

# Define information about the data files to be used as route data.
files <- c("data/www_gov_za/route.txt", 
           "data/www_gov_bb/route.txt",
           "data/www_gov_ro/route.txt",
           "data/www_cubagob_cu/route.txt")
types <- c("tracert", "tracert", "tracert", "tracert")
hosts <- c("www.gov.za", "www.gov.bb", "www.gov.ro", "www.cubagob.cu")
route.info <- cbind(file=files, type=types, host=hosts)

# Process the routes using the route info.
routes <- rbindlist(lapply(1:nrow(route.info), 
                        function(n) import.route(files[n], types[n], hosts[n])))

# Combine all route data into a singe data table.
route.summary <- summarySE(routes, measurevar="rtt", groupvars=c("host", "hop"))


# Make a basic line and point graph.

# Plotting code (below) based on this example:
# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)

# Use 95% confidence interval for error bars.
g <- ggplot(route.summary, aes(x=hop, y=rtt, colour=host)) + 
    geom_errorbar(aes(ymin=rtt-ci, ymax=rtt+ci), width=.1) +
    geom_line(aes(group=host)) +
    geom_point() + theme_bw() + 
    xlab("network hop") + ylab("round-trip time (ms)") +
    ggtitle("Round-trip time (ms) per network hop")
plot(g)