# visual-tracerouter

This is a simple demo of running a shell utility from within R and using the output from the utility to plot a route on a map.

The utility used will be "traceroute" on a Windows system and "tracert" otherwise (Mac, Linux, Unix, etc.).

Mapping can be done with `ggmap` or `maps`, depending on the configuration of variables early in the R script.

Example data in CSV files and output in PNG files are provided.

Compare to: http://www.yougetsignal.com/tools/visual-tracert/ ... with the button "Proxy Trace" pressed after you enter the domain as "Remote Address".

### Route Example

For reference, [this is example output](https://raw.githubusercontent.com/brianhigh/visual-tracerouter/master/data/www_cubagob_cu/route.txt) from `tracert` on a Windows system:

`C:\Users\nobody>tracert -d www.cubagob.cu`

```
Tracing route to vhunix.ceniai.inf.cu [169.158.128.86]
over a maximum of 30 hops:

  1    <1 ms    <1 ms    <1 ms  128.95.230.102 
  2    <1 ms    <1 ms    <1 ms  10.132.1.65 
  3    <1 ms    <1 ms    <1 ms  10.132.1.66 
  4     1 ms     1 ms     1 ms  209.124.188.132 
  5     1 ms     1 ms     1 ms  198.104.202.6 
  6     1 ms     1 ms     1 ms  198.104.202.5 
  7     1 ms     1 ms     1 ms  64.86.123.41 
  8     1 ms     1 ms     1 ms  64.86.124.25 
  9    70 ms    70 ms    71 ms  64.86.124.22 
 10    69 ms    69 ms    69 ms  66.198.96.45 
 11   220 ms   220 ms   220 ms  66.198.96.34 
 12   189 ms   189 ms   189 ms  200.0.16.85 
 13   189 ms   189 ms   189 ms  200.0.16.196 
 14   192 ms   193 ms   192 ms  200.0.16.182 
 15   190 ms   190 ms   190 ms  169.158.128.54 
 16     *        *        *     Request timed out.
 17     *        *        *     Request timed out.
 18     *        *        *     Request timed out.
 19     *        *        *     Request timed out.
 20     *        *        *     Request timed out.
 21     *        *        *     Request timed out.
 22     *        *        *     Request timed out.
 23     *        *        *     Request timed out.
 24     *        *        *     Request timed out.
 25     *        *        *     Request timed out.
 26     *        *        *     Request timed out.
 27     *        *        *     Request timed out.
 28     *        *        *     Request timed out.
 29     *        *        *     Request timed out.
 30     *        *        *     Request timed out.

Trace complete.
```

Here are the resulting plots:

#### `ggmap` package plot using `qmplot` function

![ggmap plot](https://raw.githubusercontent.com/brianhigh/visual-tracerouter/master/images/www_cubagob_cu/ggmap.png)

#### `maps` package plot using `map`, `points` and `lines` functions

![maps plot](https://raw.githubusercontent.com/brianhigh/visual-tracerouter/master/images/www_cubagob_cu/map.png)
