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
#' Abstract class no description
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Constructor for ApplicationInputInterface. This class should not used directly.}
#'   \item{\code{$setNamedList()}}{Function to setup hierarchical \code{lists}.}
#'   \item{\code{$serialize()}}{Seriealize a \code{R6-Object}.}
#'   \item{\code{$serializeToJson(level)}}{Seriealize a \code{R6-Object} as a \code{JSON-Object}.}
#'}
#'
#' @section Arguments:
#' \describe{
#'   \item{\code{type}}{Set the result file type.}
#'   \item{\code{level}}{LEvel to seriealize.}
#'}
#'
#' @export
ApplicationInputInterface <- R6::R6Class(classname = "ApplicationInputInterface",
                                         lock_objects = F,
                                         public = list(
                                           parameters = NULL,
                                           result_file_type = list(result_file_type = "csv"),
                                           setResultFileType = function(type = "csv") {
                                             type <- match.arg(type, choices = c("csv", "xls", "xlsx", "ods"))
                                             self$result_file_type[["result_file_type"]] <- type
                                           },
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
                                           serializeToJson = function(level = NULL) {
                                             if (is.null(level)) {
                                               self$serialize() %>%
                                                 jsonlite::toJSON(auto_unbox = T)
                                             } else {
                                               self$serialize()[[level]] %>%
                                                 jsonlite::toJSON(auto_unbox = T)
                                             }

                                           },
                                           serialize = function() {
                                             setdiff(x = ls(self),
                                                     y = lsf.str(self)) %>%
                                               mget(envir = self)
                                           }
                                         )
                                         )
