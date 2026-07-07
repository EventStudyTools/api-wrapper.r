[![cran version](https://www.r-pkg.org/badges/version/EventStudy)](https://cran.r-project.org/package=EventStudy) [![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/EventStudy)](https://cran.r-project.org/package=EventStudy) [![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/EventStudy?)](https://cran.r-project.org/package=EventStudy)

# EventStudyTools (EST) API R Wrapper


# **[Get Started](https://github.com/EventStudyTools/api-wrapper.r/blob/main/vignettes/get_started.Rmd)**

<p align="center">
<img src='vignettes/landing_page.png' width='350' style="display: block; margin: 0 auto">
</p>

## Purpose of this Package

-   Prepare data for an Event Study in R.

    -   Some pre-tests on the data are applied such that you do not get some strange errors from the API.

-   Perform an Event Study using the <https://www.eventstudytools.com> API.

-   Parse results to R and do additional analysis and plotting with results.

## Further Information & Help

-   The description of test statistics and available models can be found on [Significance Tests](https://www.eventstudytools.com/significance-tests) and [Expected Return Models](https://www.eventstudytools.com/expected-return-models).

-   If another consultancy on event study or panel data analysis is necessary, you may contact the EST Team (no-reply@eventstudytools.com).

-   Don't hesitate to contact EST Team if you want to perform Event Studies On-Premise, with low latency or large scale.


## Installation

From GitHub (current, recommended):

    # install.packages("remotes")
    remotes::install_github("EventStudyTools/api-wrapper.r")

The package was previously published on CRAN but is currently archived
there, so `install.packages("EventStudy")` no longer works. Install from
GitHub instead.

## Simple Example of an Abnormal Returns Calculator (ARC) launch

    apiKey <- "Insert API key"

    # Generate Example Data
    EventStudy::getSP500ExampleFiles()

    library(EventStudy)
    # Setup API Connection
    estSetup <- EventStudyAPI$new()
    estSetup$authentication(apiKey)

    # Type of Analysis
    estType <- "arc"

    # CSV files
    dataFiles <- c("request_file" = "01_RequestFile.csv", 
                   "firm_data"    = "02_FirmData.csv", 
                   "market_data"  = "03_MarketData.csv")

    # Path of result files
    resultPath <- "results"

    # Perform standard Event Study
    estSetup$performDefaultEventStudy(estType   = estType,
                                      dataFiles = dataFiles, 
                                      destDir   = resultPath)
                            
    # Parse Results                        
    estParser <- ResultParser$new()
    request_data = estParser$get_request_file("01_RequestFile.csv")
    analysis_report = estParser$get_analysis_report("results/analysis_report.csv")


    ar_result = estParser$get_ar("results/ar_results.csv", analysis_report, request_data)
    ar_result$plot()

    aar_result = estParser$get_aar("results/aar_results.csv", analysis_report)
    aar_result$plot(ci_statistics = "Generalized Sign Z")
    aar_result$plot_cumulative()
    aar_result$plot_test_statistics(p=.99)

    car_result = estParser$get_car("results/car_results.csv", analysis_report)
    car_result$car_tbl

    caar_result = estParser$get_caar("results/caar_results.csv")
    caar_result$caar_tbl
    caar_result$statistics_tbl




## Details can be found in our vignettes

-   [Get Started](vignettes/get_started.Rmd)
-   [Use Case: Dieselgate](vignettes/use_case_dieselgate.Rmd)
-   [Parameters](vignettes/parameters.Rmd)
-   [RStudio Addin](vignettes/addin_eventstudy.Rmd)
