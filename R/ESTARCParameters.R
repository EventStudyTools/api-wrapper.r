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
#' @name ESTARCParameters
#' 
#' @title Abnormal Return Calculation Parameters
#' 
#' @description 
#' This R6 class defines the parameters for the Return Event Study. We recommend
#' to use the \code{set} functionality to setup your Event Study, as we check
#' input parameters.
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Constructor for ESTARCParameters.}
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
#' @section Arguments:
#' 
#' \describe{
#'  \item{ESTARCParameters}{An \code{ESTARCParameters} object}
#'  \item{eMail}{An E-Mail address in \code{String} format}
#'  \item{model}{A benchmark model in \code{String} format}
#'  \item{returnType}{A return type in \code{String} format}
#'  \item{testStatistics}{A \code{String} vector with test statistics.}
#' }
#' 
#' @section Class Members:
#' 
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
ESTARCParameters <- R6::R6Class(classname = "ESTParameters",
                                inherit = ApplicationInputInterface,
                                public = list(
                                  task = list(locale = 'en'),
                                  benchmark_model  = list(benchmark_model = "mm"),
                                  return_type      = list(return_type = "log"),
                                  non_trading_days = list(non_trading_days = "later"),
                                  test_statistics  = list("art", "cart", "aart", 
                                                          "caart", "abhart", 
                                                          "aarptlz", "caarptlz", 
                                                          "aaraptlz", "caaraptlz", 
                                                          "aarbmpz", "caarbmpz", 
                                                          "aarabmpz", "caarabmpz", 
                                                          "aarskewadjt", 
                                                          "caarskewadjt", 
                                                          "abharskewadjt", 
                                                          "aarrankz", "caarrankz", 
                                                          "aargrankt", "caargrankt",
                                                          "aargrankz", "caargrankz", 
                                                          "aargsignz", "caargsignz",
                                                          "aarcdat", "aarjackknivet", 
                                                          "caarjackknivet"),
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
                                    model <- match.arg(model, unname(private$market_model))
                                    self$benchmark_model <- model
                                  },
                                  # set return type
                                  setReturnType = function(returnType) {
                                    returnType <- match.arg(returnType, c("log", "simple"))
                                    self$return_type <- returnType
                                  },
                                  # set test statistics
                                  setTestStatistics = function(testStatistics) {
                                    testStatistics <- match.arg(testStatistics, private$test_statistics)
                                    
                                  }
                                ),
                                private = list(
                                  allowedTestStatistics = c(
                                    "art",
                                    "cart",
                                    "aart",
                                    "caart",
                                    "abhart",
                                    "aarptlz",
                                    "caarptlz",
                                    "aaraptlz",
                                    "caaraptlz",
                                    "aarbmpz",
                                    "caarbmpz",
                                    "aarabmpz",
                                    "caarabmpz",
                                    "aarskewadjt",
                                    "caarskewadjt",
                                    "abharskewadjt",
                                    "aarrankz",
                                    "caarrankz",
                                    "aargrankt",
                                    "caargrankt",
                                    "aargrankz",
                                    "caargrankz",
                                    "aargsignz",
                                    "caargsignz",
                                    "aarcdat",
                                    "caarcdat",
                                    "aarjackknivet",
                                    "caarjackknivet"
                                  ),
                                  allowedBenchmarkModel = c("Market Model"                        = "mm", 
                                                            "Scholes/Williams Model"              = "mm-sw", 
                                                            "Market Adjusted"                     = "mam", 
                                                            "Comparison Period Mean Adjusted"     = "cpmam",
                                                            "Fama-French 3 Factor Model"          = "ff3fm",
                                                            "Fama-French-Momentum 4 Factor Model" = "ffm4fm",
                                                            "GARCH"                               = "garch", 
                                                            "EGARCH"                              = "egarch"),
                                  allowedNonTradingDays = c("Take earlier trading day"     = "csv", 
                                                            "Take later trading day"       = "later", 
                                                            "Keep non-trading day"         = "keep", 
                                                            "Skip respective observations" = "ods")
                                )
)
