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

Run the `whois` command with the domain `who.int` as a test.


```r
# Try out the whois utility, removing any carriage-return (\r) characters.
whois.data <- gsub(pattern = "\\r", replacement = "", 
                   x = system("whois who.int", intern = TRUE))

# Take a quick look at the results.
length(whois.data)
```

```
## [1] 42
```

```r
head(whois.data, 10)
```

```
##  [1] "% IANA WHOIS server"                                      
##  [2] "% for more information on IANA, visit http://www.iana.org"
##  [3] "% This query returned 1 object"                           
##  [4] ""                                                         
##  [5] "domain:       WHO.INT"                                    
##  [6] ""                                                         
##  [7] "organisation: World Health Organization (WHO)"            
##  [8] "address:      20, Avenue Appia"                           
##  [9] "address:      Geneva 27"                                  
## [10] "address:      Geneva Geneva CH-1211"
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
## [1] 32
```

```r
head(whois.data, 10)
```

```
##  [1] "domain|      WHO.INT"                        
##  [2] "organisation|World Health Organization (WHO)"
##  [3] "address|     20, Avenue Appia"               
##  [4] "address|     Geneva 27"                      
##  [5] "address|     Geneva Geneva CH-1211"          
##  [6] "address|     Switzerland"                    
##  [7] "contact|     administrative"                 
##  [8] "name|        WHO-HQ-NOC (at ITS/NTS)"        
##  [9] "address|     20, Avenue Appia"               
## [10] "address|     Geneva 27"
```

```r
# Take a look at the last few lines.
tail(whois.data, 10)
```

```
##  [1] "fax-no|      +41 22 791 4779"                
##  [2] "e-mail|      hostmaster@who.int"             
##  [3] "nserver|     EXT-DNS-2.CERN.CH 192.91.245.85"
##  [4] "nserver|     NS1.WPRO.WHO.INT 123.176.64.11" 
##  [5] "nserver|     WHQDNS1.WHO.INT 158.232.12.5"   
##  [6] "nserver|     WHQDNS2.WHO.INT 158.232.12.6"   
##  [7] "nserver|     WHQDNS3.WHO.INT 202.188.122.155"
##  [8] "created|     1998-06-05"                     
##  [9] "changed|     2015-10-05"                     
## [10] "source|      IANA"
```

```r
# Remove any '>>>' and '<<<' strings, if present.
whois.data <- gsub("[[:space:]]?[<>]{3}[[:space:]]?", "", whois.data)

# Check the last line again.
whois.data[length(whois.data)]
```

```
## [1] "source|      IANA"
```

```r
# Read the file into a data.frame (df).
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



variable       value                           
-------------  --------------------------------
domain         WHO.INT                         
organisation   World Health Organization (WHO) 
address        20, Avenue Appia                
address        Geneva 27                       
address        Geneva Geneva CH-1211           
address        Switzerland                     
contact        administrative                  
name           WHO-HQ-NOC (at ITS/NTS)         
address        20, Avenue Appia                
address        Geneva 27                       
address        Geneva  CH-1211                 
address        Switzerland                     
phone          +41 22 791 2411                 
fax-no         +41 22 791 4779                 
e-mail         hostmaster@who.int              
contact        technical                       
name           WHO-HQ-NOC (at ITS/NTS)         
address        20, Avenue Appia                
address        Geneva 27                       
address        Geneva  CH-1211                 
address        Switzerland                     
phone          +41 22 791 2411                 
fax-no         +41 22 791 4779                 
e-mail         hostmaster@who.int              
nserver        EXT-DNS-2.CERN.CH 192.91.245.85 
nserver        NS1.WPRO.WHO.INT 123.176.64.11  
nserver        WHQDNS1.WHO.INT 158.232.12.5    
nserver        WHQDNS2.WHO.INT 158.232.12.6    
nserver        WHQDNS3.WHO.INT 202.188.122.155 
created        1998-06-05                      
changed        2015-10-05                      
source         IANA                            
