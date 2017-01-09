#' Abnormal Return Calculation API Wrapper
#'
#'
#' @export
ArcApplicationInput <- R6::R6Class(classname = "ArcApplicationInput",
                                   inherit = ApplicationInputInterface,
                                   public = list(
                                     initialize = function(parameters, key = "arc") {

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
                                       self$parameters$application[["parameters"]] <- c(
                                         parameters[["return_type"]],
                                         parameters[["result_file_type"]],
                                         parameters[["non_trading_days"]],
                                         parameters[["benchmark_model"]],
                                         test_statistics = list(parameters[["test_statistics"]]))

                                        # set test statistics
                                       self$setNamedList(parentLevel   = "application",
                                                         secondLevel   = "parameters",
                                                         thirthLevel   = "test_statistics",
                                                         parameterList = unlist(parameters[["test_statistics"]]))
                                     }
                                   ))
