# visual-tracerouter

This is a simple demo of running a shell utility from within R and using the output from the utility to plot a route on a map.

The utility used will be "traceroute" on a Windows system and "tracert" otherwise (Mac, Linux, Unix, etc.).

Mapping can be done with `ggmap` or `maps`, depending on the configuration of variables early in the R script.

Example data in CSV files and output in PNG files are provided.

Compare to: http://www.yougetsignal.com/tools/visual-tracert/ ... with the button "Proxy Trace" pressed after you enter the domain as "Remote Address".
