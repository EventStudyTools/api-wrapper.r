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
#' @name ApplicationInputInterface
#' @title Abstract Application Input Interface for Event Study R Package
#' 
#' @description
#' Abstract class no description.
#'
#' @format \code{\link[R6]{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Constructor for ApplicationInputInterface. 
#'   This class should not used directly.}
#'   \item{\code{$setNamedList()}}{Function to setup hierarchical \code{lists}.}
#'   \item{\code{$serialize()}}{Seriealize a \code{R6-Object}.}
#'   \item{\code{$serializeToJson(level)}}{Seriealize a \code{R6-Object} as a 
#'   \code{JSON-Object}.}
#'}
#'
#' @section Arguments:
#' \describe{
#'   \item{\code{type}}{Set the result file type.}
#'   \item{\code{level}}{Level to seriealize.}
#'}
#'
#' @keywords internal
ApplicationInputInterface <- R6::R6Class(classname = "ApplicationInputInterface",
                                         lock_objects = F,
                                         public = list(
                                           #' @field parameters Parameters
                                           parameters = NULL,
                                           #' @field result_file_type Result file type.
                                           result_file_type = list(result_file_type = "csv"),
                                           #' @description Set result file type
                                           #' 
                                           #' @param type Possible are csv, xls, xlsx, ods
                                           setResultFileType = function(type = "csv") {
                                             type <- match.arg(type, choices = private$allowedResultFileType)
                                             self$result_file_type[["result_file_type"]] <- type
                                           },
                                           #' @description Function to setup hierarchical \code{lists}.
                                           #' For internal usage.
                                           #' 
                                           #' @param parentLevel parent level
                                           #' @param secondLevel parent level
                                           #' @param thirthLevel parent level
                                           #' @param parameterList parent level
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
                                                 self$parameters[[parentLevel]][[secondLevel]][[thirthLevel]] <- parameterList
                                               }
                                               
                                             }
                                           },
                                           #' @description Serialize a list to json. For internal use.
                                           #' 
                                           #' @param level The level that should be serialized.
                                           serializeToJson = function(level = NULL) {
                                             if (is.null(level)) {
                                               self$serialize() %>%
                                                 jsonlite::toJSON(auto_unbox = T)
                                             } else {
                                               self$serialize()[[level]] %>%
                                                 jsonlite::toJSON(auto_unbox = T)
                                             }
                                           },
                                           #' @description Serialize. For internal use.
                                           serialize = function() {
                                             setdiff(x = ls(self),
                                                     y = lsf.str(self)) %>%
                                               mget(envir = self)
                                           }
                                         ),
                                         private = list(
                                           allowedResultFileType = c("CSV"  = "csv", 
                                                                     "XLS"  = "xls", 
                                                                     "XLSX" = "xlsx", 
                                                                     "ODS"  = "ods")
                                         )
)
