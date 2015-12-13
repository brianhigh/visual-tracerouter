# Whois Demo

We will first install a [whois](https://en.wikipedia.org/wiki/WHOIS) utility 
if we are running Windows, as this operating system does not come with this 
tool by default. Other popular operating systems like OSX and Linux come with
`whois` pre-installed, so we will not perform this step on those systems.


```r
if (Sys.info()["sysname"] == "Windows") {
    # Get the whois.exe file for Windows from SysInternals.
    exefile <- "whois.exe"
    
    # Only download and extract the utility if we don't already have it.
    if (! file.exists(exefile) == TRUE) {
        zipfile <- "WhoIs.zip"
        if (! file.exists(zipfile) == TRUE) {
            url <- "https://download.sysinternals.com/files/WhoIs.zip"
            download.file(url, zipfile)
        }
        unzip(zipfile)
    }
}
```

Run the `whois` command with the domain `www.worldbank.org` as a test.


```r
# Try out the whois utility, removing any carriage-return (\r) characters.
whois.data <- gsub(pattern = "\\r", replacement = "", 
                   x = system("whois www.worldbank.org", intern = TRUE))

# Take a quick look at the results.
length(whois.data)
```

```
## [1] 66
```

```r
head(whois.data, 10)
```

```
##  [1] ""                                               
##  [2] "Whois v1.12 - Domain information lookup utility"
##  [3] "Sysinternals - www.sysinternals.com"            
##  [4] "Copyright (C) 2005-2014 Mark Russinovich"       
##  [5] ""                                               
##  [6] "Connecting to ORG.whois-servers.net..."         
##  [7] ""                                               
##  [8] "Domain ID: D5166969-LROR"                       
##  [9] "WHOIS Server:"                                  
## [10] "Referral URL: http://www.networksolutions.com"
```

Read the data into a `data.frame`.


```r
# clean up the the whois data and read into a data.frame.

# Use the vertical bar symbol (|) as the delimiter instead of the colon (:).
whois.data <- gsub(pattern = ":[[:space:]]", replacement = "|", x = whois.data)

# Remove lines which do not contain the delimiter.
whois.data <- whois.data[grepl(x = whois.data, pattern = "\\|")]

# Take a quick look at the results.
length(whois.data)
```

```
## [1] 44
```

```r
head(whois.data, 10)
```

```
##  [1] "Domain ID|D5166969-LROR"                                                                  
##  [2] "Referral URL|http://www.networksolutions.com"                                             
##  [3] "Updated Date|2012-03-05T18:59:29Z"                                                        
##  [4] "Creation Date|1991-08-14T04:00:00Z"                                                       
##  [5] "Registry Expiry Date|2021-08-13T04:00:00Z"                                                
##  [6] "Sponsoring Registrar|Network Solutions, LLC"                                              
##  [7] "Sponsoring Registrar IANA ID|2"                                                           
##  [8] "Domain Status|clientTransferProhibited https://www.icann.org/epp#clientTransferProhibited"
##  [9] "Registrant ID|15962757-NSI"                                                               
## [10] "Registrant Name|World Bank"
```

```r
# Take a look at the last few lines.
tail(whois.data, 10)
```

```
##  [1] "Tech Postal Code|20043"                                    
##  [2] "Tech Country|US"                                           
##  [3] "Tech Phone|+1.2024588503"                                  
##  [4] "Tech Email|dnsadmin@worldbank.org"                         
##  [5] "Name Server|DNS2.WORLDBANK.ORG"                            
##  [6] "Name Server|DNS1.WORLDBANK.ORG"                            
##  [7] "Name Server|DNS3.WORLDBANK.ORG"                            
##  [8] "Name Server|DNS4.WORLDBANK.ORG"                            
##  [9] "DNSSEC|unsigned"                                           
## [10] ">>> Last update of WHOIS database|2015-12-11T14:59:02Z <<<"
```

```r
# Remove the '>>>' and '<<<' strings from the last line.
whois.data[length(whois.data)] <- gsub("[[:space:]]?[<>]{3}[[:space:]]?", "", 
                                       x = whois.data[length(whois.data)])

# Check the last line again.
whois.data[length(whois.data)]
```

```
## [1] "Last update of WHOIS database|2015-12-11T14:59:02Z"
```

```r
# Read the file into a data.frame.
whois.df <- read.table(text = whois.data, sep = "|", as.is = TRUE)
names(whois.df) <- c("variable", "value")
```

Load the `knitr` package so that we can use the `kable()` function.


```r
library(knitr)
```

Produce a formatted table from the `data.frame` using `kable()`.


```r
kable(whois.df)
```



variable                        value                                              
------------------------------  ---------------------------------------------------
Domain ID                       D5166969-LROR                                      
Referral URL                    http://www.networksolutions.com                    
Updated Date                    2012-03-05T18:59:29Z                               
Creation Date                   1991-08-14T04:00:00Z                               
Registry Expiry Date            2021-08-13T04:00:00Z                               
Sponsoring Registrar            Network Solutions, LLC                             
Sponsoring Registrar IANA ID    2                                                  
Domain Status                   clientTransferProhibited https://www.icann.org/epp 
Registrant ID                   15962757-NSI                                       
Registrant Name                 World Bank                                         
Registrant Organization         World Bank                                         
Registrant Street               1818 H Street NW                                   
Registrant City                 Washington                                         
Registrant State/Province       DC                                                 
Registrant Postal Code          20433                                              
Registrant Country              US                                                 
Registrant Phone                +1.2024588503                                      
Registrant Email                dnsadmin@worldbank.org                             
Admin ID                        43166795-NSI                                       
Admin Name                      DNS Admin                                          
Admin Organization              World Bank                                         
Admin Street                    1818 H St N.W.                                     
Admin City                      Washington                                         
Admin State/Province            DC                                                 
Admin Postal Code               20043                                              
Admin Country                   US                                                 
Admin Phone                     +1.2024588503                                      
Admin Email                     dnsadmin@worldbank.org                             
Tech ID                         43166795-NSI                                       
Tech Name                       DNS Admin                                          
Tech Organization               World Bank                                         
Tech Street                     1818 H St N.W.                                     
Tech City                       Washington                                         
Tech State/Province             DC                                                 
Tech Postal Code                20043                                              
Tech Country                    US                                                 
Tech Phone                      +1.2024588503                                      
Tech Email                      dnsadmin@worldbank.org                             
Name Server                     DNS2.WORLDBANK.ORG                                 
Name Server                     DNS1.WORLDBANK.ORG                                 
Name Server                     DNS3.WORLDBANK.ORG                                 
Name Server                     DNS4.WORLDBANK.ORG                                 
DNSSEC                          unsigned                                           
Last update of WHOIS database   2015-12-11T14:59:02Z                               
