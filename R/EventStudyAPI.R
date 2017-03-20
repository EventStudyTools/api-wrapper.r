# // Copyright (C) 2015 - 2016  Dmitriy Selivanov
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
#' EventStudyAPI
#' 
#' API for \url{www.eventstudytools.com}
#' 
#' @description R interface for performing Event Studies on 
#' \url{www.eventstudytools.com}.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{new(apiServerUrl)}}{This method is used to create 
#'   object of this class with \code{apiServerUrl} as the url to the 
#'   EventStudyTools server.}
#'   \item{\code{authentication(apiKey)}}{This method is used to 
#'   authenticate at \code{apiServerUrl}. A valid \code{apiKey} is 
#'   required. You can download a free key on our website: 
#'   \url{www.eventstudytools.com}}
#'   \item{\code{processTask()}}{This method starts the Event Study 
#'   calculation on the server (after files are uploaded.}
#'   \item{\code{configureTask(input)}}{This method configures the 
#'   Event Study. \code{input} is an \code{ApplicationInputInterface} 
#'   R6 object, e.g. ARC configuration class}
#'   \item{\code{uploadFile(fileKey, fileName)}}{This method links to the 
#'   file to upload. \code{fileKey} is the key of the file. Valid values 
#'   are: \code{request_file}, \code{firm_data}, and \code{market_data}. 
#'   \code{fileName} file name to upload.}
#'   \item{\code{commitData()}}{This method commit the data to the server.}
#'   \item{\code{getTaskStatus()}}{Check if calcualtion is finished.}
#'   \item{\code{getTaskResults(destDir = getwd())}}{Downloads the 
#'   result files of the Event Study to \code{destDir} (Default: current 
#'   working directory).}
#'   }
#'
#' @section Arguments:
#' \describe{
#'  \item{eventstudyapi}{An \code{EventStudyAPI} object}
#'  \item{apiServerUrl}{URL to the API endpoint}
#'  \item{apiKey}{Key for authentication}
#'  \item{input}{An \code{ApplicationInputInterface} object.}
#'  \item{fileKey}{Type of input file: \code{request_file}, \code{firm_data}, and \code{market_data}.}
#'  \item{fileName}{Data filename.}
#'  \item{destDir}{Directory for saving result files.}
#' }
#', fileName
#' @export
EventStudyAPI <- R6::R6Class(classname = "EventStudyAPI",
                             public = list(
                               resultFiles = NULL,
                               initialize = function(apiServerUrl) {
                                 private$apiServerUrl <- apiServerUrl
                               },
                               # @param apiKey
                               # @return boolean
                               authentication = function(apiKey, debug=F) {

                                 new_handle() %>%
                                   handle_setopt(customrequest = "POST") %>%
                                   handle_setopt(postfields = "") %>%
                                   handle_setheaders("Content-Type" = "application/json",
                                                     "X-Customer-Key" = apiKey) -> handle

                                 # fetch result
                                 ch <- curl_fetch_memory(url    = paste0(private$apiServerUrl, "/task/create"),
                                                         handle = handle)

                                 # get token & check it
                                 rawToChar(ch$content) %>%
                                   jsonlite::fromJSON() %>%
                                   private$checkAndNormalizeResponse(httpcode = ch$status_code,
                                                                     method   = "authentication") -> result

                                 if (!is(result, "list") || is.null(result$token)) {
                                   message("Error: authentication failed")
                                   return(FALSE)
                                 }

                                 private$token <- result$token
                                 return(TRUE)
                               },
                               defaultRun = function(estType    = "arc", 
                                                     dataFiles  = c("request_file" = "01_RequestFile.csv", "firm_data" = "02_firmData.csv", "market_data" = "03_MarketData.csv"), 
                                                     resultPath = "results") {
                                 estType <- match.arg(estType, c("arc"))
                                 
                                 # Perform Study
                                 self$configureTask()
                                 
                                 self$uploadFile(fileKey  = "request_file", 
                                                     fileName = dataFiles["request_file"])
                                 self$uploadFile(fileKey  = "firm_data", 
                                                     fileName = dataFiles["firm_data"])
                                 self$uploadFile(fileKey  = "market_data", 
                                                     fileName = dataFiles["market_data"])
                                 self$commitData()
                                 
                                 self$processTask()
                                 
                                 waiting <- T
                                 iter <- 0
                                 maxIter <- 15
                                 while(iter < maxIter) {
                                   sleep(1)
                                   status <- self$getTaskStatus()
                                   if (status %in% c(3, 4)) {
                                     break()
                                   }
                                   iter <- iter + 1
                                 }
                                 
                                 if (!status %in% c(3, 4)) {
                                   message("Calculation is not finished. Please try to get results later.")
                                   return(T)
                                 }
                                 
                                 if (status == 4) {
                                   message("Application Error")
                                   return(F)
                                 }
                                 
                                 self$getTaskResults()
                                 return(T)
                               },
                               processTask = function() {
                                 if (is.null(private$token) || is.null(private$apiServerUrl))
                                   stop("Error in uploadFile: configuration error")

                                 new_handle() %>%
                                   handle_setopt(customrequest = "POST") %>%
                                   handle_setopt(postfields = "") %>%
                                   handle_setheaders("Content-Type" = "application/json",
                                                     "X-Task-Key"   = private$token) -> handle

                                 ch <- curl_fetch_memory(url    = paste0(private$apiServerUrl, "/task/process"),
                                                         handle = handle)

                                 rawToChar(ch$content) %>%
                                   jsonlite::fromJSON() %>%
                                   private$checkAndNormalizeResponse(httpcode = ch$status_code,
                                                                     method   = "processTask") -> result

                                 self$resultFiles <- result$results
                                 result
                               },
                               configureTask = function(input = NULL) {

                                 # Setup Standard Parameters
                                 if (is.null(input)) {
                                   message("Parameters are not set. Setup ARC parameters.")
                                   estParameters <- ESTARCParameters$new()
                                   estParameters$getParameters() %>% 
                                     ArcApplicationInput$new() -> input
                                 }
                                 
                                 if (!is(input, "ApplicationInputInterface") || is.null(private$token))
                                   stop("Error in configureTask: token is not set")

                                 new_handle() %>%
                                   handle_setopt(customrequest = "POST") %>%
                                   handle_setopt(postfields = "") %>%
                                   handle_setheaders("Content-Type" = "application/json",
                                                     "X-Task-Key"   = private$token) -> handle

                                 json <- input$toJson(level = "parameters")

                                 handle %>%
                                   handle_setopt(postfields = json) -> handle

                                 # fetch result
                                 ch <- curl_fetch_memory(url    = paste0(private$apiServerUrl, "/task/conf"),
                                                         handle = handle)

                                 rawToChar(ch$content) %>%
                                   jsonlite::fromJSON() %>%
                                   private$checkAndNormalizeResponse(httpcode = ch$status_code,
                                                                     method   = "configureTask") -> result

                                 if (!result)
                                   stop(paste0("Error in configureTask: configuration error"))

                                 return(TRUE)
                               },
                               uploadFile = function(fileKey, fileName, partNumber = 0) {

                                 if (is.null(private$token) || is.null(fileKey) || is.null(fileName))
                                   stop("Error in uploadFile: configuration error")

                                 if (!file.exists(fileName))
                                   stop("Error: file do not exist")

                                 fSize <- file.size(fileName)
                                 fd <- file(fileName, "r", method = "libcurl")

                                 ch <- POST(url=paste0(private$apiServerUrl, "/task/content/", fileKey, "/0"),
                                            body = upload_file(path =  fileName),
                                            add_headers('Content-Type' = "application/octet-stream",
                                                        "X-Task-Key"   = private$token)
                                 )

                                 rawToChar(ch$content) %>%
                                   jsonlite::fromJSON() %>%
                                   private$checkAndNormalizeResponse(httpcode = ch$status_code,
                                                                     method   = "uploadFile") -> result

                                 if (!result)
                                   stop(paste0("Error in uploadFile: configuration error"))

                                 return(TRUE)
                               },
                               deleteFileParts = function(parts) {
                                 # TODO
                               },
                               splitFile = function(fileName, maxChunkSize) {
                                 # TODO
                               },
                               # Parameters
                               get_token = function() {
                                 return(private$token)
                               },
                               commitData = function() {

                                 if (is.null(private$token))
                                   stop("Error: Configuration validation error")

                                 new_handle() %>%
                                   handle_setopt(customrequest = "POST") %>%
                                   handle_setopt(postfields = "") %>%
                                   handle_setheaders("Content-Type" = "application/json",
                                                     "X-Task-Key"   = private$token) -> handle

                                 ch <- curl_fetch_memory(url    = paste0(private$apiServerUrl, "/task/commit"),
                                                         handle = handle)

                                 rawToChar(ch$content) %>%
                                   jsonlite::fromJSON() %>%
                                   private$checkAndNormalizeResponse(httpcode = ch$status_code,
                                                                     method   = "commitData") -> result

                                 if (!is(result, "list") || is.null(result$log))
                                   stop("Error in commitData: response is invalid")

                                 return(result)

                               },
                               getTaskStatus = function(exceptionOnError = FALSE) {

                                 if (is.null(private$token))
                                   stop("Error: Configuration validation error")

                                 new_handle() %>%
                                   handle_setheaders("Content-Type" = "application/json",
                                                     "X-Task-Key"   = private$token) -> handle

                                 ch <- curl_fetch_memory(url    = paste0(private$apiServerUrl, "/task/status"),
                                                         handle = handle)

                                 rawToChar(ch$content) %>%
                                   jsonlite::fromJSON() %>%
                                   private$checkAndNormalizeResponse(httpcode = ch$status_code,
                                                                     method   = "getTaskResults") -> result

                                 # TODO
                                 return(result)
                               },
                               getTaskResults = function(destDir = getwd()) {
                                 if (is.null(private$token))
                                   stop("Error: Configuration validation error")

                                 if (is.null(self$resultFiles))
                                   stop("Error: No result files")

                                 # fetch data
                                 # destDir <- stringr::str_replace_all(destDir, "[/\\]", "")
                                 l <- lapply(self$resultFiles, function(x) {
                                   destFile <- unlist(stringr::str_split(x, "/"))
                                   destFile <- destFile[length(destFile)]
                                   curl::curl_download(url = x, destfile = paste0(destDir, "/", destFile))
                                 })
                               },
                               getApiVersion = function() {

                                 new_handle() %>%
                                   handle_setopt(customrequest = "GET") -> handle

                                 # fetch result
                                 ch <- curl_fetch_memory(url    = paste0(private$apiServerUrl, "/version"),
                                                         handle = handle)

                                 version <- ""
                                 if (ch$status_code == 200) {
                                   ch$content %>%
                                     base::rawToChar() %>%
                                     jsonlite::fromJSON() -> response
                                   if (!is.null(response$version))
                                     version <- response$version
                                 }
                                 return(version)
                               }
                             ),
                             private = list(
                               # private members
                               token        = NULL,
                               apiServerUrl = NULL,
                               checkAndNormalizeResponse = function(response, httpcode, method, exceptionOnError = T) {
                                 if ("error" %in% names(response)) {
                                   if (exceptionOnError) {
                                     stop(paste0("Error in ", method, ': ', response$error))
                                   } else {
                                     return(FALSE)
                                   }
                                 }
                                 if (httpcode != 200) {
                                   if (exceptionOnError) {
                                     if (!is(result, "list") && !is.null(response$error)) {
                                       stop(paste0("Error in ", method, ': ', response$error))
                                     } else {
                                       stop(paste0("Error in ", method, ': Application error'))
                                     }
                                   } else {
                                     return(FALSE)
                                   }
                                 }
                                 return(response)
                               }
                             )
                             )
