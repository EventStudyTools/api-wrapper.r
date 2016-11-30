#' @export
ApplicationInputInterface <- R6::R6Class(classname = "ApplicationInputInterface",
                                         lock_objects = F,
                                         public = list(
                                           setNamedList = function(parentLevel,
                                                                   secondLevel = NULL,
                                                                   thirthLevel = NULL,
                                                                   parameterList) {
                                             if (is.null(self[[parentLevel]])) {
                                               tmpList <- list(var = list())
                                               names(tmpList) <- parentLevel
                                               self[[parentLevel]] <- tmpList
                                             }

                                             if (is.null(secondLevel)) {
                                               if(is.null(parameterList)) {
                                                 self[[parentLevel]] <- list()
                                               } else {
                                                 self[[parentLevel]] <- parameterList
                                               }
                                             } else {
                                               if(is.null(parameterList)) {
                                                 self[[parentLevel]][[secondLevel]] <- list()
                                               } else {
                                                 self[[parentLevel]][[secondLevel]] <- parameterList
                                               }
                                             }

                                             if (!is.null(thirthLevel)) {
                                               self[[parentLevel]][[secondLevel]][[thirthLevel]] <- parameterList
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
