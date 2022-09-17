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
#' @name EventStudyApplicationInput
#' 
#' @title Abnormal Return Calculation (ARC) API Wrapper
#'
#' @description 
#' This R6 class serialzes an Event Study parameter class to a list structure.
#' This is an abstract class for Event Study applications (Return, Volatility, 
#' and Volume Event Studies). It is not intended to use this class directly. 
#' Please use: \link{ARCApplicationInput}.
#' 
#'
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Constructor for EventStudyApplicationInput}
#'   \item{\code{$setup()}}{Setup the parameter list}
#'}
#' 
#' @format \code{\link[R6]{R6Class}} object.
EventStudyApplicationInput <- R6::R6Class(classname = "EventStudyApplicationInput",
                                          inherit = ApplicationInputInterface,
                                          public = list(
                                            #' @description Initialize parameters of an event study
                                            setup = function() {
                                              self$parameters <- NULL
                                              parameters <- self$serialize()
                                              # Task ----
                                              # check default for locale
                                              if (is.null(parameters[["task"]][["locale"]]))
                                                parameters[["task"]][["locale"]] <- "en"
                                              
                                              self$setNamedList(parentLevel   = "task",
                                                                secondLevel   = NULL,
                                                                thirthLevel   = NULL,
                                                                parameterList = parameters[["task"]])
                                              
                                              # Application ----
                                              # set key
                                              if (is.null(parameters[["application"]][["key"]]))
                                                parameters[["key"]] <- self$key
                                              self$setNamedList(parentLevel   = "application",
                                                                secondLevel   = "key",
                                                                thirthLevel   = NULL,
                                                                parameterList = parameters[["key"]])
                                              
                                              # set data sources
                                              self$setNamedList(parentLevel   = "application",
                                                                secondLevel   = "data_sources",
                                                                thirthLevel   = "request_file",
                                                                parameterList = as.list(parameters[["request_file"]]))
                                              self$setNamedList(parentLevel   = "application",
                                                                secondLevel   = "data_sources",
                                                                thirthLevel   = "firm_data",
                                                                parameterList = as.list(parameters[["firm_data"]]))
                                              self$setNamedList(parentLevel   = "application",
                                                                secondLevel   = "data_sources",
                                                                thirthLevel   = "market_data",
                                                                parameterList = as.list(parameters[["market_data"]]))
                                              self$parameters$application$data_sources <- unname(self$parameters$application$data_sources)
                                              
                                              # Parameters ----
                                              params <- c(parameters[["return_type"]],
                                                parameters[["result_file_type"]],
                                                parameters[["non_trading_days"]],
                                                parameters[["benchmark_model"]],
                                                test_statistics = list(parameters[["test_statistics"]]))
                                              if (self$key == "av") {
                                                # Abnormal Volume
                                                params <- params[-1]
                                              }
                                              self$parameters[["parameters"]] <- params
                                            }
                                          ))
