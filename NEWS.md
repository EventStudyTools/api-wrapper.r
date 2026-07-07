## 0.40.0

- Fix: file upload now works with curl >= 5. `httr::upload_file()` failed with
  "unsupported seekfunction"; the raw request body is posted through curl instead.
- Fix: `authentication()` now returns `FALSE` on failure instead of reporting success.
- Fix: default AVC / AVyC studies now use the correct application key (they were
  silently running an ARC analysis).
- Fix: `commitData()` no longer emits a spurious "response is invalid" message.
- Fix: `getTaskStatus()` guards against empty result sets; `retryRequest()` now
  re-checks the status inside its backoff loop and guards against request errors.
- Fix: several parser/plotting bugs (`confidence_interval()`, `CAResults$print()`,
  `get_car()` without an analysis report).
- Add the Fama-French 5-factor (`ff5fm`) and CAPM (`capm`) benchmark models.
- Declare `R6` and `tibble` in Imports; remove committed API keys from the vignette.
- Docs: update deprecated links (archived-CRAN and moved eventstudytools.com URLs),
  remove the dead ShinyApp reference, and align example file names with the data
  written by `getSP500ExampleFiles()`.

## Notes

- Fixing date issue: dates before 01.01.1970 are currently not working

## 0.39

- Parsed outputs are now data classes
- Re-align to CRAN 
- Adapt to changes of EventStudy API
- More code documentation.


## 0.38

- Remove strong package dependencies
- Change licence to MIT

### Bugfixes

- CAAR result parsing
- aarPlot & arPlot: more user-friendly error messages


## 0.36.900

### Bugfixes

- CAAR result parsing
- aarPlot & arPlot: more user-friendly error messages


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
