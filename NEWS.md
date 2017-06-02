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
