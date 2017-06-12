[![cran version](http://www.r-pkg.org/badges/version/EventStudy)](https://cran.rstudio.com/web/packages/zoon) 
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/EventStudy?)](https://cran.r-project.org/web/packages/EventStudy/)

# EventStudyTools (EST) API R Wrapper

This software library provides the capability to easily deploy the EST API.

* More detailed documentation about available applications can be found on [http://wwww.eventstudytools.com](http://wwww.eventstudytools.com)
* The full API documentation is presented on our website: [http://wwww.eventstudytools.com/API-ARC](http://wwww.eventstudytools.com/API-ARC)

## Installation

Developer Version
```
library(devtools)
install_github("EventStudyTools/api-wrapper.r")
```

CRAN Version
```
install.packages("EventStudy")
```

**Attention**

You need the developer verision of `purrr` ([github](https://github.com/tidyverse/purrr)) 

## Ask a Question

- [Facebook](https://www.facebook.com/EventStudy/)
- [Commercial](http://muon-stat.com/dienstleistungen/kapitalmarktanalyse/)
- [Website](http://eventstudytools.com)

## Simple Example of an Abnormal Returns Calculator (ARC) launch

You can find a **free** API key on our website: [https://www.eventstudytools.com/api-key](https://www.eventstudytools.com/api-key)
```
apiUrl <- "http://api.eventstudytools.com"
apiKey <- "Insert API key"

library(EventStudy)
# Setup API Connection
estSetup <- EventStudyAPI$new(apiUrl)
estSetup$authentication(apiKey)

# Type of Analysis
estType <- "arc"

# CSV files
dataFiles <- c("request_file" = "01_RequestFile.csv", 
               "firm_data"    = "02_firmData.csv", 
               "market_data"  = "03_MarketData.csv")

# Path of result files
resultPath <- "results"

# Perform standard Event Study
estSetup$performDefaultEventStudy(estType    = estType,
                                  dataFiles  = dataFiles, 
                                  resultPath = resultPath)
```

## Details can be found in our vignettes

- [Introduction in EventStudy R Package](https://cran.rstudio.com/web/packages/EventStudy/vignettes/introduction_eventstudy.html)
- Use Case: Dieselgate
- [Parameters](https://cran.rstudio.com/web/packages/EventStudy/vignettes/parameters_eventstudy.html)
- Plotting
- Reports
