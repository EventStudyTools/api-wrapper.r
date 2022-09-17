# // Copyright (C) 2017 Simon Müller
# // This file is part of EventStudy
# //
# // EventStudy is free software: you can redistribute it and/or modify it
# // under the terms of the GNU General Public License as published by
# // the Free Software Foundation, either version 2 of the License, or
# // (at your option) any later version.
# //
# // EventStudy is distributed in the hope that it will be useful, but
# // WITHOUT ANY WARRANTY; without even the implied warranty of
# // MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# // GNU General Public License for more details.
# //
# // You should have received a copy of the GNU General Public License
# // along with EventStudy  If not, see <http://www.gnu.org/licenses/>.
#' @name AVyCApplicationInput
#' 
#' @title Abnormal Volatility Calculation Parameters
#' 
#' @description 
#' This R6 class defines the parameters for the Abnormal Volatility Volume Event 
#' Study. We recommend to use the \code{set} functionality to setup your Event  
#' Study, as we check input parameters.
#' 
#' For more details see the help vignette:
#' \code{vignette("parameters_eventstudy", package = "EventStudy")}
#' 
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{$new()}}{Constructor for AVyCApplicationInput}
#'   \item{\code{$setEMail(eMail)}}{Set the e-Mail address for reporting. This 
#'   functionality is currently not working.}
#'   \item{\code{$setBenchmarkModel(model = 'mm')}}{Setter for the benchmark
#'   models}
#'   \item{\code{$setReturnType(returnType)}}{Setter for the return type (log 
#'   or simple)}
#'   \item{\code{$setTestStatistics(testStatistics)}}{Setter for the test 
#'   statistics. Per default all available test statistics are applied. You may
#'   find all test statistics in the vignette 'parameter_eventstudy'}
#'}
#' 
#' 
#' @section Arguments:
#' \describe{
#'  \item{AVyCApplicationInput}{An \code{AVyCApplicationInput} object}
#'  \item{eMail}{An E-Mail address in \code{String} format}
#'  \item{model}{A benchmark model in \code{String} format}
#'  \item{returnType}{A return type in \code{String} format}
#'  \item{testStatistics}{A \code{String} vector with test statistics}
#' }
#' 
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @return a ESTParameters R6 object
#' 
#' @examples 
#' \dontrun{
#' # get files for our S&P500 example; 3 files are written in the current 
#' # working directory
#' getSP500ExampleFiles()
#' 
#' # Generate a new parameter object
#' avycParams <- AVyCApplicationInput$new()
#' 
#' # set test statistics
#' avycParams$setTestStatistics(c("aarptlz", "aarrankz"))
#' 
#' # Setup API object
#' apiKey <- "{Your API key}"
#' estSetup <- EventStudyAPI$new()
#' estSetup$authentication(apiKey)
#'
#' # Perform Event Study
#' estSetup$performEventStudy(estParams = avycParams, 
#'                            dataFiles = c("request_file" = "01_RequestFile.csv",
#'                                          "firm_data"    = "02_firmData.csv",
#'                                          "market_data"  = "03_marketData.csv"))
#'
#' # Download task results and save them in the actiual working directory
#' estSetup$getTaskResults()
#' }
#' 
#' @export
AVyCApplicationInput <- R6::R6Class(classname = "AVyCApplicationInput",
                                  inherit = EventStudyApplicationInput,
                                  public = list(
                                    #' @field key Key of the Parameter set.
                                    key              = "avyc",
                                    #' @field test_statistics Available test statistics.
                                    test_statistics  = list(
                                      "aarcsvyt",
                                      "aarcsvyt",
                                      "aarcsart",
                                      "aarcscart"
                                    )
                                  ),
                                  private = list(
                                    allowedTestStatistics = c(
                                      "aarcsvyt",
                                      "aarcsvyt",
                                      "aarlambdaz",
                                      "aarcsart",
                                      "aarcscart"
                                    ),
                                    allowedBenchmarkModel = c("Garch(1, 1)" = "garch")
                                  )
)
