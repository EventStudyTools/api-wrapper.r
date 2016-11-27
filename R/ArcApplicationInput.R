#' @export
ArcApplicationInput <- R6::R6Class(classname = "ArcApplicationInput",
                                   inherit = ApplicationInputInterface,
                                   public = list(
                                     task        = NULL,
                                     application = NULL,
                                     parameters  = NULL,
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
                                       if (is.null(parameters[["application"]][["key"]]))
                                         parameters[["key"]] <- key

                                       self$setNamedList(parentLevel   = "application",
                                                         secondLevel   = "key",
                                                         thirthLevel   = NULL,
                                                         parameterList = parameters[["key"]])
                                       self$setNamedList(parentLevel   = "application",
                                                         secondLevel   = "datasources",
                                                         thirthLevel   = "request_file",
                                                         parameterList = parameters[["request_file"]])
                                       self$setNamedList(parentLevel   = "application",
                                                         secondLevel   = "datasources",
                                                         thirthLevel   = "firm_data",
                                                         parameterList = parameters[["firm_data"]])
                                       self$setNamedList(parentLevel   = "application",
                                                         secondLevel   = "datasources",
                                                         thirthLevel   = "market_data",
                                                         parameterList = parameters[["market_data"]])


                                       # Parameters ----
                                       self$setNamedList(parentLevel   = "parameters",
                                                         secondLevel   = NULL,
                                                         thirthLevel   = NULL,
                                                         parameterList = parameters[["parameters"]])

                                       # set test statistics
                                       self$setNamedList(parentLevel   = "parameters",
                                                         secondLevel   = "test_statistics",
                                                         thirthLevel   = NULL,
                                                         parameterList = parameters[["test_statistics"]])
                                     }
                                   ))
