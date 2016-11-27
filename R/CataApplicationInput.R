#' @export
CataApplicationInput <- R6::R6Class(classname = "CataApplicationInput",
                                   inherit = ApplicationInputInterface,
                                   public = list(
                                     task        = NULL,
                                     application = NULL,
                                     parameters  = NULL,
                                     initialize = function(parameters) {

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
                                         parameters[["key"]] <- "cata"

                                       self$setNamedList(parentLevel   = "application",
                                                         secondLevel   = "key",
                                                         thirthLevel   = NULL,
                                                         parameterList = parameters[["key"]])
                                       self$setNamedList(parentLevel   = "application",
                                                         secondLevel   = "datasources",
                                                         thirthLevel   = "text_data",
                                                         parameterList = parameters[["text_data"]])
                                       self$setNamedList(parentLevel   = "application",
                                                         secondLevel   = "datasources",
                                                         thirthLevel   = "keywords_data",
                                                         parameterList = parameters[["keywords_data"]])
                                     }
                                   ))
