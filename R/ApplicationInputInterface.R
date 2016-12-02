#' @export
ApplicationInputInterface <- R6::R6Class(classname = "ApplicationInputInterface",
                                         lock_objects = F,
                                         public = list(
                                           parameters = NULL,
                                           setNamedList = function(parentLevel,
                                                                   secondLevel = NULL,
                                                                   thirthLevel = NULL,
                                                                   parameterList) {
                                             if (is.null(self$parameters[[parentLevel]])) {
                                               tmpList <- list(var = list())
                                               self$parameters[[parentLevel]] <- list()
                                             }

                                             if (is.null(secondLevel)) {
                                               if(is.null(parameterList)) {
                                                 self$parameters[[parentLevel]] <- list()
                                               } else {
                                                 self$parameters[[parentLevel]] <- parameterList
                                               }
                                             } else if (is.null(thirthLevel)) {
                                               if(is.null(parameterList)) {
                                                 self$parameters[[parentLevel]][[secondLevel]] <- list()
                                               } else {
                                                 if (length(parameterList) == 1) {
                                                   self$parameters[[parentLevel]][[secondLevel]] <- parameterList
                                                 } else {
                                                   self$parameters[[parentLevel]][[secondLevel]] <- list(unlist(parameterList))
                                                 }
                                               }
                                             }

                                             if (!is.null(thirthLevel)) {
                                               if (length(parameterList) == 1) {
                                                 self$parameters[[parentLevel]][[secondLevel]][[thirthLevel]] <- parameterList
                                               } else {
                                                 self$parameters[[parentLevel]][[secondLevel]][[thirthLevel]] <- list(unlist(parameterList))
                                               }

                                             }
                                           },
                                           toJson = function() {
                                             self$getMember() %>%
                                               jsonlite::toJSON(auto_unbox = T)
                                           },
                                           getMember = function() {
                                             setdiff(x = ls(self),
                                                     y = lsf.str(self)) %>%
                                               mget(envir = self)
                                           }
                                         )
                                         )
