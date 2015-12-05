# visual-tracerouter

This is a simple demo of running a shell utility from within R and using the output from the utility to plot a route on a map.

The utility used will be "traceroute" on a Windows system and "tracert" otherwise (Mac, Linux, Unix, etc.).

Mapping can be done with `ggmap` or `maps`, depending on the configuration of variables early in the R script.

Example data in CSV files and output in PNG files are provided.

Compare to: http://www.yougetsignal.com/tools/visual-tracert/ ... with the button "Proxy Trace" pressed after you enter the domain as "Remote Address".

### Route Example

For reference, this is example output from `tracert` on a Windows system:

```
C:\Users\nobody>tracert www.cubagob.cu

Tracing route to vhunix.ceniai.inf.cu [169.158.128.86]
over a maximum of 30 hops:

  1    <1 ms    <1 ms    <1 ms  ae50--657.uwar-atg-1.infra.washington.edu [128.95.230.102]
  2    <1 ms    <1 ms    <1 ms  irb--10.uwcr-ads-1.infra.washington.edu [10.132.1.65]
  3    <1 ms    <1 ms    <1 ms  ae1--24.uwbr-ads-1.infra.washington.edu [10.132.1.66]
  4    <1 ms    <1 ms    <1 ms  ae0--4000.icar-sttl1-1.infra.pnw-gigapop.net [209.124.188.132]
  5    <1 ms    <1 ms     1 ms  xe-0-5-0-5.r05.sttlwa01.us.ce.gin.ntt.net [198.104.202.6]
  6     1 ms     1 ms     1 ms  xe-0-5-0-5.r05.sttlwa01.us.bb.gin.ntt.net [198.104.202.5]
  7     1 ms    <1 ms    <1 ms  ix-10-0.tcore1.00S-Seattle.as6453.net [64.86.123.41]
  8     1 ms     1 ms    <1 ms  if-11-0-0.core1.00S-Seattle.as6453.net [64.86.124.25]
  9    69 ms    69 ms    70 ms  if-8-1-2-4.tcore2.CT8-Chicago.as6453.net [64.86.124.22]
 10    69 ms    70 ms    69 ms  if-3-2.tcore1.W6C-Montreal.as6453.net [66.198.96.45]
 11   220 ms   220 ms   220 ms  66.198.96.34
 12   204 ms   202 ms   201 ms  200.0.16.85
 13   201 ms   201 ms   201 ms  200.0.16.196
 14   203 ms   219 ms   207 ms  200.0.16.182
 15   205 ms   203 ms   204 ms  core-aa4.bb-cap.citmatel.cu [169.158.128.54]
 16     *        *        *     Request timed out.
 17     *        *        *     Request timed out.
 18     *        *     ^C
```

Here are the resulting plots:

#### `ggmap` package plot using `qmplot` function

![ggmap plot](https://raw.githubusercontent.com/brianhigh/visual-tracerouter/master/route_ggmap.png)

#### `maps` package plot using `map`, `points` and `lines` functions

![maps plot](https://raw.githubusercontent.com/brianhigh/visual-tracerouter/master/route_map.png)
