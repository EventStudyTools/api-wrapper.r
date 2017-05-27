[![cran version](http://www.r-pkg.org/badges/version/EventStudy)](https://cran.rstudio.com/web/packages/zoon) 
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/EventStudy?)](https://github.com/metacran/cranlogs.app)

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

## Example of an Abnormal Returns Calculator (ARC) launch

```
# Coming Soon
apiUrl <- "Insert API URL"
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

Links will be provided after first release on CRAN.
