# EventStudyTools (EST) API R Wrapper

This software library provides the capability to easily deploy the EST API.

* More detailed documentation about available applications can be found [here](http://wwww.eventtudytools.com)
* The full API documentation is presented [here](http://wwww.eventtudytools.com/API-ARC)

## Installation
```
library(devtools)
install_github("EventStudyTools/api-wrapper.r")
```

## Example of an Abnormal Returns Calculator (ARC) launch

```
key <- "xxx"
url <- "xxx"

library(EventStudy)
est <- EventStudyAPI$new(url)
est$getApiVersion()
est$authentication(apiKey = key)

# get & set parameters
esaParams <- ESTARCParameters$new()
# esaParams$setEMail("info@muon-stat.com")
arcParams <- esaParams$getParameters()
arcSetup <- ArcApplicationInput$new(arcParams)

est$configureTask(arcSetup)
est$uploadFile(fileKey = "request_file", fileName = "01_RequestFile.csv")
est$uploadFile(fileKey = "firm_data", fileName = "02_firmData.csv")
est$uploadFile(fileKey = "market_data", fileName = "03_marketData.csv")
est$commitData()
est$processTask()

if (est$getTaskStatus() == 3) {
  est$getTaskResults()
}
```
