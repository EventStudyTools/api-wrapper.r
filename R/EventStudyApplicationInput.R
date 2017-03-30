#' Abnormal Return Calculation (ARC) API Wrapper
#'
#' This R6 class serialzes an ARC parameter class to a list structure 
#'
#' @export
EventStudyApplicationInput <- R6::R6Class(classname = "EventStudyApplicationInput",
                                          inherit = ApplicationInputInterface,
                                          public = list(
                                            setup = function(key = "arc") {
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
                                                parameters[["key"]] <- key
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
                                              self$parameters[["parameters"]] <- c(
                                                parameters[["return_type"]],
                                                parameters[["result_file_type"]],
                                                parameters[["non_trading_days"]],
                                                parameters[["benchmark_model"]],
                                                test_statistics = list(parameters[["test_statistics"]]))
                                            }
                                          ))
