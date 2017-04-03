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
#' @name AVCApplicationInput
#' 
#' @title Abnormal Volume Calculation Parameters
#' 
#' @description 
#' This R6 class defines the parameters for the Abnormal Volume Event Study. 
#' We recommend to use the \code{set} functionality to setup your Event Study, 
#' as we check input parameters.
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Constructor for AVCApplicationInput}
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
#'  \item{AVCApplicationInput}{An \code{AVCApplicationInput} object}
#'  \item{eMail}{An E-Mail address in \code{String} format}
#'  \item{model}{A benchmark model in \code{String} format}
#'  \item{returnType}{A return type in \code{String} format}
#'  \item{testStatistics}{A \code{String} vector with test statistics.}
#' }
#' 
#' 
#' @section Class Members:
#' @param task Actually, just \code{locale} is defined. E-Mail-Address is added
#' in a later version.
#' @param return_type return type calculation: log (default), simple
#' @param non_trading_days handler for non-trading days: later (default), 
#' earlier, keep, and skip
#' @param test_statistics test statistics that will be used in the Event Study
#' @param request_file list of request file key and type
#' @param firm_data list of firm data file key and type
#' @param market_data list of market data file key and type 
#' @param allowedTestStatistics allowed test statistics
#' @param allowedBenchmarkModel available market models
#' @param allowedNonTradingDays available market models
#' 
#' @format \code{\link{R6Class}} object.
#' 
#' @seealso \url{https://www.eventstudytools.com/axc/upload}
#' 
#' @return a ESTParameters R6 object
#'
#' @export
AVCApplicationInput <- R6::R6Class(classname = "AVCApplicationInput",
                                   inherit = EventStudyApplicationInput,
                                   public = list(
                                     task             = list(locale = 'en'),
                                     benchmark_model  = list(benchmark_model = "mm"),
                                     return_type      = list(return_type = "log"),
                                     non_trading_days = list(non_trading_days = "later"),
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
                                     request_file = list(
                                       key  = "request_file",
                                       type = "csv"
                                     ),
                                     firm_data    = list(
                                       key  = "firm_data",
                                       type = "csv"
                                     ),
                                     market_data  = list(
                                       key  = "market_data",
                                       type = "csv"
                                     ),
                                     # set email
                                     setEMail = function(eMail) {
                                       stop("This parameter is currently not working.")
                                       self$task[["email"]] <- eMail
                                     },
                                     # set benchmark model
                                     setBenchmarkModel = function(model) {
                                       model <- match.arg(model, unname(private$allowedBenchmarkModel))
                                       self$benchmark_model[["benchmark_model"]] <- model
                                     },
                                     # set return type
                                     setReturnType = function(returnType) {
                                       returnType <- match.arg(returnType, c("log", "simple"))
                                       self$return_type[["return_type"]] <- returnType
                                     },
                                     # set non trading days
                                     setNonTradingDays = function(nonTradingDays = "later") {
                                       nonTradingDays <- match.arg(nonTradingDays, unname(private$allowedNonTradingDays))
                                       self$non_trading_days[["non_trading_days"]] <- nonTradingDays
                                     },
                                     # set test statistics
                                     setTestStatistics = function(testStatistics = NULL) {
                                       if (is.null(testStatistics)) {
                                         testStatistics <- private$allowedTestStatistics
                                       }
                                       testStatistics <- match.arg(testStatistics, private$test_statistics)
                                       self$test_statistics <- as.list(testStatistics)
                                     },
                                     # set reques, firm, and market data file 
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
                                     allowedBenchmarkModel = c("Market Model"                    = "mm", 
                                                               "Scholes/Williams Model"          = "mm-sw", 
                                                               "Market Adjusted"                 = "mam", 
                                                               "Comparison Period Mean Adjusted" = "cpmam"),
                                     allowedNonTradingDays = c("Take earlier trading day"     = "earlier", 
                                                               "Take later trading day"       = "later", 
                                                               "Keep non-trading day"         = "keep")
                                   )
)
