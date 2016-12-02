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
                                                         parameterList = parameters[["request_file"]])
                                       self$setNamedList(parentLevel   = "application",
                                                         secondLevel   = "data_sources",
                                                         thirthLevel   = "firm_data",
                                                         parameterList = parameters[["firm_data"]])
                                       self$setNamedList(parentLevel   = "application",
                                                         secondLevel   = "data_sources",
                                                         thirthLevel   = "market_data",
                                                         parameterList = parameters[["market_data"]])


                                       # Parameters ----
                                       self$setNamedList(parentLevel   = "application",
                                                         secondLevel   = "parameters",
                                                         thirthLevel   = "non_trading_days",
                                                         parameterList = parameters[["non_trading_days"]])
                                       self$setNamedList(parentLevel   = "application",
                                                         secondLevel   = "parameters",
                                                         thirthLevel   = "result_file_type",
                                                         parameterList = parameters[["result_file_type"]])
                                       self$setNamedList(parentLevel   = "application",
                                                         secondLevel   = "parameters",
                                                         thirthLevel   = "return_type",
                                                         parameterList = parameters[["return_type"]])
                                       self$setNamedList(parentLevel   = "application",
                                                         secondLevel   = "parameters",
                                                         thirthLevel   = "benchmark_model",
                                                         parameterList = parameters[["benchmark_model"]])

                                       # set test statistics
                                       self$setNamedList(parentLevel   = "application",
                                                         secondLevel   = "parameters",
                                                         thirthLevel   = "test_statistics",
                                                         parameterList = parameters[["test_statistics"]])
                                     }
                                   ))
