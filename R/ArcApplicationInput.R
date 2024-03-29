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
#' @name ARCApplicationInput
#' 
#' @title Abnormal Return Calculation Parameters
#' 
#' @description 
#' This R6 class defines the parameters for the Return Event Study. We recommend
#' to use the \code{set} functionality to setup your Event Study, as we check
#' input parameters.
#' 
#' For more details see the help vignette:
#' \code{vignette("parameters_eventstudy", package = "EventStudy")}
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Constructor for ARCApplicationInput.}
#'   \item{\code{$setEMail(eMail)}}{Set the e-Mail address for reporting. This 
#'   functionality is currently not working.}
#'   \item{\code{$setBenchmarkModel(model = 'mm')}}{Setter for the benchmark
#'   model.s}
#'   \item{\code{$setReturnType(returnType)}}{Setter for the return type (log 
#'   or simple)}
#'   \item{\code{$setTestStatistics(testStatistics)}}{Setter for the test 
#'   statistics.}
#'}
#' 
#' 
#' @section Arguments:
#' \describe{
#'  \item{ESTARCParameters}{An \code{ARCApplicationInput} object}
#'  \item{eMail}{An E-Mail address in \code{String} format}
#'  \item{model}{A benchmark model in \code{String} format}
#'  \item{returnType}{A return type in \code{String} format}
#'  \item{testStatistics}{A \code{String} vector with test statistics.}
#' }
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
#' arcParams <- ARCApplicationInput$new()
#' 
#' # set test statistics
#' arcParams$setBenchmarkModel("garch")
#' 
#' # Setup API object
#' apiKey <- "{Your API key}"
#' estSetup <- EventStudyAPI$new()
#' estSetup$authentication(apiKey)
#'
#' # Perform Event Study
#' estSetup$performEventStudy(estParams = arcParams, 
#'                            dataFiles = c("request_file" = "01_RequestFile.csv",
#'                                          "firm_data"    = "02_firmData.csv",
#'                                          "market_data"  = "03_marketData.csv"))
#'
#' # Download task results and save them in the actiual working directory
#' estSetup$getTaskResults()
#' }
#' 
#' @export
ARCApplicationInput <- R6::R6Class(classname = "ARCApplicationInput",
                                   inherit = EventStudyApplicationInput,
                                   public = list(
                                     #' @field task Task description
                                     task             = list(locale = 'en'),
                                     #' @field key Key
                                     key              = "arc",
                                     #' @field benchmark_model Benchmark model
                                     benchmark_model  = list(benchmark_model = "mm"),
                                     #' @field return_type Return type
                                     return_type      = list(return_type = "log"),
                                     #' @field non_trading_days How to handle non-trading days
                                     non_trading_days = list(non_trading_days = "later"),
                                     #' @field test_statistics Test statistics 
                                     test_statistics  = list("art", "cart", 
                                                             "aart", "caart", "abhart", 
                                                             "aarptlz", "caarptlz", 
                                                             "aaraptlz", "caaraptlz", 
                                                             "aarbmpz", "caarbmpz", 
                                                             "aarabmpz", "caarabmpz", 
                                                             "aarskewadjt", "caarskewadjt", "abharskewadjt", 
                                                             "aarrankz", "caarrankz", 
                                                             "aargrankt", "caargrankt",
                                                             "aargrankz", "caargrankz", 
                                                             "aargsignz", "caargsignz"
                                     ),
                                     #' @field request_file Request file 
                                     request_file = list(
                                       key  = "request_file",
                                       type = "csv"
                                     ),
                                     #' @field firm_data Firm data
                                     firm_data    = list(
                                       key  = "firm_data",
                                       type = "csv"
                                     ),
                                     #' @field market_data Market data
                                     market_data  = list(
                                       key  = "market_data",
                                       type = "csv"
                                     ),
                                     #' @description  set email
                                     #' 
                                     #' @param eMail Your E-mail address
                                     setEMail = function(eMail) {
                                       stop("This parameter is currently not working.")
                                       self$task[["email"]] <- eMail
                                     },
                                     #' @description set benchmark model
                                     #' 
                                     #' @param model benchmark model
                                     setBenchmarkModel = function(model) {
                                       model <- match.arg(model, unname(private$allowedBenchmarkModel))
                                       self$benchmark_model[["benchmark_model"]] <- model
                                     },
                                     #' @description Set return type
                                     #' 
                                     #' @param returnType return type
                                     setReturnType = function(returnType) {
                                       returnType <- match.arg(returnType, c("log", "simple"))
                                       self$return_type[["return_type"]] <- returnType
                                     },
                                     #' @description Set non trading days
                                     #' 
                                     #' @param nonTradingDays how to handle non traing days
                                     setNonTradingDays = function(nonTradingDays = "later") {
                                       nonTradingDays <- match.arg(nonTradingDays, unname(private$allowedNonTradingDays))
                                       self$non_trading_days[["non_trading_days"]] <- nonTradingDays
                                     },
                                     #' @description Set test statistics
                                     #' 
                                     #' @param testStatistics Test statistic
                                     setTestStatistics = function(testStatistics = NULL) {
                                       if (is.null(testStatistics)) {
                                         testStatistics <- private$allowedTestStatistics
                                       }
                                       testthat::expect(all(testStatistics %in% private$allowedTestStatistics), "Some non valid test statistics!")
                                       self$test_statistics <- as.list(testStatistics)
                                     },
                                     #' @description Set reques, firm, and market data file 
                                     #' 
                                     #' @param dataFiles Named vector of data files.
                                     setDataFiles = function(dataFiles  = c("request_file" = "01_RequestFile.csv", 
                                                                            "firm_data"    = "02_firmData.csv", 
                                                                            "market_data"  = "03_MarketData.csv")) {
                                       check <- match.arg(names(dataFiles), c("request_file", "firm_data", "market_data"))
                                     }
                                   ),
                                   private = list(
                                     allowedTestStatistics = c(
                                       "art", "cart",
                                       "aart", "caart", "abhart",
                                       "aarptlz", "caarptlz", 
                                       "aaraptlz", "caaraptlz",
                                       "aarbmpz", "caarbmpz",
                                       "aarabmpz", "caarabmpz",
                                       "aarskewadjt", "caarskewadjt", "abharskewadjt",
                                       "aarrankz", "caarrankz",
                                       "aargrankt", "caargrankt",
                                       "aargrankz", "caargrankz",
                                       "aargsignz", "caargsignz"
                                     ),
                                     allowedBenchmarkModel = c("Market Model"                        = "mm", 
                                                               "Scholes/Williams Model"              = "mm-sw", 
                                                               "Market Adjusted"                     = "mam", 
                                                               "Comparison Period Mean Adjusted"     = "cpmam",
                                                               "Fama-French 3 Factor Model"          = "ff3fm",
                                                               "Fama-French-Momentum 4 Factor Model" = "ffm4fm",
                                                               "GARCH(1,1)"                          = "garch", 
                                                               "EGARCH(1, 1)"                        = "egarch"),
                                     allowedNonTradingDays = c("Take earlier trading day"     = "earlier", 
                                                               "Take later trading day"       = "later", 
                                                               "Keep non-trading day"         = "keep", 
                                                               "Skip respective observations" = "skip")
                                   )
)
