## Notes

- Fixing date issue: dates before 01.01.1970 are currently not working

## 0.36.900

### Bugfixes

- CAAR result parsing
- aarPlot & arPlot: more userfriendly error messages


## 0.36

- Result files
  - Downloaded files have meaningful names, e.g. ar_results.csv
  - Download will be performed quietly
  - Result folder will be created if it not exists

- API key can be set as a global option
- Minor updates in Parameters Vignette


## 0.35

- Added some more message to output during the process

### Bugfixes

- result files loading is fixed
- result file parser is fixed
- fixed task checks
- input file checker is fixed

- examples
    - Github example was not working

## 0.34

### Bugfixes

- EventStudyAddin
    - setTestStatistics did not worked proberly
    - added all types of event studies
    - ResultPraser object is now returned to the console


## Version 0.33

- add Excel export functionality to ResultParser
- add new file ckecks
    - check event window is in firm and market data
    - check estimation window is in firm and market data
    - dates before 01.01.1970 are actually not allowed
- add function estAPIKey for setting EventStudy API key
- ResultParser now parses CAR values

### Bugfixes

- fix EventStudy Addin 
    - tradding days options
    - statistics options
    - result object is now returned to console


## Version 0.32

- redesign vignettes

### Bugfixes 

- file parser bugfix
- aarPlot bugfix CI interval
- spelling fixes


## Version 0.31

- add vignette
    - Event Study RStudio Addin
    - Howto: Dieselgate as an Example
- Check data files in prior of file upload


### Bugfixes

- ResultParser was not correctly called in EventStudyAPI
- ResultParser was not able to download files from url
- getTaskStatus did not work properly in EventStudyAPI 
- fix av and avyc parsing


## Version 0.30

- add examples
- minor bugfixing


## Version 0.20

- refactoring class hierarchy
- adding RStudio addins for performing a Return Event Study
- Vignettes
    - Introduction
    - Parameters
- add abnormal volume Event Study R6 class
- add abnormal volatility Event Study R6 class


## Version 0.11

- automatic ARC parameter setup
- ggplot and highchart graphics:
    - abnormal returns
    - averaged abnormal returns
    - pointwise cumulative abnormal returns


## Version 0.10

Initial version of Event Study Tools API

- perform an Event Study from R through the eventstudytools.com API
- abnormal return and averaged abnormal return result parser and highchart plots
