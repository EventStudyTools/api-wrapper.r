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
#' @name EventStudyAPI
#' 
#' @title API for \url{www.eventstudytools.com}
#' 
#' @description R interface for performing Event Studies on 
#' \url{www.eventstudytools.com}. Please get a free API key from our website:
#' \url{https://www.eventstudytools.com/api-key}.
#'
#' For more details see the help vignette:
#' \code{vignette("introduction_eventstudy", package = "EventStudy")}
#'
#' @format \code{\link[R6]{R6Class}} object
#'
#' @section Usage:
#' For usage details see \bold{Methods, Arguments, and Examples} sections.
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{new(apiServerUrl)}}{This method is used to create an
#'   object of this class with \code{apiServerUrl} as the url to the 
#'   EventStudyTools server}
#'   \item{\code{authentication(apiKey)}}{This method is used to 
#'   authenticate at \code{apiServerUrl}. A valid \code{APIkey} is 
#'   required. You can download a free key on our website: 
#'   \url{www.eventstudytools.com}}
#'   \item{\code{performEventStudy(estParam)}}{This method starts an Event Study. 
#'   This method does all the analysis work for you}
#'   \item{\code{performDefaultEventStudy()}}{This method starts a default 
#'   Event Study. It is a wrapper around \code{performEventStudy}}
#'   \item{\code{processTask()}}{This method starts the Event Study 
#'   calculation on the server (after files are uploaded.}
#'   \item{\code{configureTask(input)}}{This method configures the 
#'   Event Study. \code{input} is an \code{ApplicationInputInterface} 
#'   R6 object, e.g. ARC configuration class}
#'   \item{\code{uploadFile(fileKey, fileName)}}{This method links to the 
#'   file to upload. \code{fileKey} is the key of the file. Valid values 
#'   are: \code{request_file}, \code{firm_data}, and \code{market_data}. 
#'   \code{fileName} file name to upload.}
#'   \item{\code{commitData()}}{This method commits the data to the server}
#'   \item{\code{getTaskStatus()}}{Check if calculation is finished}
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
#'  \item{input}{An \code{ApplicationInputInterface} object.
#'  \item{fileKey}{Type of input file: \code{request_file}, \code{firm_data}, 
#'  and \code{market_data}}
#'  \item{fileName}{Data filename}
#'  \item{destDir}{Directory for saving result files}
#' }
#' 
#' @examples
#' \dontrun{
#' apiKey <- "{Please insert your aPI key here}"
#' 
#' The URL is already set by default
#' options(EventStudy.KEY = apiKey)
#'
#' # initialize object
#' estSetup <- EventStudyAPI$new()
#' 
#' # get S&P500 example data
#' getSP500ExampleFiles()
#' 
#' # set Event Study parameters
#' estType <- "arc"
#' dataFiles <- c("request_file" = "01_RequestFile.csv", 
#'                "firm_data"    = "02_firmData.csv", 
#'                "market_data"  = "03_MarketData.csv")
#' resultPath <- "results"
#' 
#' # Perform Event Study 
#' estResult <- estSetup$performDefaultEventStudy(estType    = estType,
#'                                                dataFiles  = dataFiles, 
#'                                                destDir    = resultPath)
#'}
#'
#' @export
EventStudyAPI <- R6::R6Class(classname = "EventStudyAPI",
                             public = list(
                               resultFiles = NULL,
                               dataFiles   = NULL,
                               initialize = function(apiServerUrl = NULL) {
                                 # if API key is null try to fetch it from options
                                 if (is.null(apiServerUrl)) {
                                   apiServerUrl <- getOption("EventStudy.URL")
                                 }
                                 
                                 # Else throw an error
                                 if (is.null(apiServerUrl)) {
                                   stop("An API server url is required!")
                                 }
                                 
                                 private$apiServerUrl <- apiServerUrl
                               },
                               # @param apiKey
                               # @return boolean
                               authentication = function(apiKey = NULL) {
                                 # if API key is null try to fetch it from options
                                 if (is.null(apiKey)) {
                                   apiKey <- getOption("EventStudy.KEY")
                                 }
                                 
                                 # Else throw an error
                                 if (is.null(apiKey)) {
                                   stop("An API key is required!")
                                 }
                                 
                                 ch <- doHttrRequest(url          = httr::modify_url(private$apiServerUrl, path = "/task/create"), 
                                                     request_type = "POST", 
                                                     config = httr::add_headers(c("Content-Type"   = "application/json", 
                                                                                             "X-Customer-Key" = apiKey))
                                 )
                                 
                                 # get token & check it
                                 result <- ch$content
                                 if (!is(result, "list") || is.null(result$token)) {
                                   myMessage("Error: authentication failed", level = 1)
                                   FALSE
                                 }
                                 private$token <- result$token
                                 TRUE
                               },
                               performEventStudy = function(estParams     = NULL,
                                                            dataFiles     = c("request_file" = "01_RequestFile.csv", 
                                                                           "firm_data"   = "02_firmData.csv", 
                                                                           "market_data" = "03_MarketData.csv"), 
                                                            destDir       = "results",
                                                            downloadFiles = T,
                                                            checkFiles    = F) {
                                 estParams$setup()
                                 self$dataFiles <- dataFiles
                                 
                                 # check files
                                 if (checkFiles) {
                                   EventStudy::checkFiles(dataFiles)
                                 }
                                 
                                 # Perform Study
                                 self$configureTask(estParams)
                                 
                                 self$uploadFile(fileKey  = "request_file", 
                                                 fileName = unname(dataFiles["request_file"]))
                                 self$uploadFile(fileKey  = "firm_data", 
                                                 fileName = unname(dataFiles["firm_data"]))
                                 self$uploadFile(fileKey  = "market_data", 
                                                 fileName = unname(dataFiles["market_data"]))
                                 self$commitData()
                                 
                                 self$processTask()
                                 
                                 waiting <- T
                                 iter <- 0
                                 while(iter < getOption("EventStudy.tryAttempts")) {
                                   print(paste0("Check batch process: Step ", iter))
                                   Sys.sleep(1)
                                   status <- self$getTaskStatus()
                                   if (status) {
                                     break()
                                   }
                                   iter <- iter + 1
                                 }
                                 
                                 if (!status) {
                                   myMessage("Calculation is not finished. Please try to get results later.", level = 1)
                                   myMessage("Check status with: .$getTaskStatus()", level = 1)
                                   myMessage("Get results with: .$getTaskResults()", level = 1)
                                   return(T)
                                 }
                                 
                                 # create result path if not exist
                                 if (downloadFiles && !dir.exists(destDir)) {
                                   dir.create(destDir)
                                 }
                                 
                                 return(self$getTaskResults(downloadFiles, destDir))
                               },
                               performDefaultEventStudy = function(estType       = "arc", 
                                                                   dataFiles     = c("request_file" = "01_RequestFile.csv", 
                                                                                     "firm_data"    = "02_firmData.csv", 
                                                                                     "market_data"  = "03_MarketData.csv"), 
                                                                   destDir       = "results",
                                                                   downloadFiles = T,
                                                                   checkFiles    = F) {
                                 estType <- match.arg(estType, c("arc", "avc", "avyc"))
                                 if (estType == "arc") {
                                   defaultParams <- ARCApplicationInput$new()
                                 } else if (estType == "avc") {
                                   defaultParams <- ARCApplicationInput$new()
                                 } else if (estType == "avyc") {
                                   defaultParams <- ARCApplicationInput$new()
                                 }
                                 
                                 self$performEventStudy(defaultParams, dataFiles, destDir)
                               },
                               processTask = function() {
                                 if (is.null(private$token) || is.null(private$apiServerUrl))
                                   stop("Error in uploadFile: configuration error")

                                 ch <- doHttrRequest(url          = httr::modify_url(private$apiServerUrl, path = "/task/process"), 
                                                     request_type = "POST", 
                                                     config       = httr::add_headers(c("Content-Type" = "application/json", 
                                                                                        "X-Task-Key"   = private$token))
                                 )
                                 
                                 result <- ch$content
                                 self$resultFiles <- result$results
                                 result
                               },
                               configureTask = function(estParams = NULL) {

                                 # Setup Standard Parameters
                                 if (is.null(estParams) || !inherits(estParams,"ApplicationInputInterface"))
                                   stop("Parameters are not set. Please set parameter object or run .$performDefaultEventStudy")
                                 
                                 if (is.null(private$token))
                                   stop("Error in configureTask: token is not set")
                                 
                                 json <- estParams$serializeToJson(level = "parameters")
                                 curl::new_handle() %>%
                                   curl::handle_setopt(customrequest = "POST") %>%
                                   curl::handle_setopt(postfields = json) %>%
                                   curl::handle_setheaders("Content-Type" = "application/json",
                                                           "X-Task-Key"   = private$token) -> handle
                                 
                                 # fetch result
                                 ch <- curl::curl_fetch_memory(url    = paste0(private$apiServerUrl, "/task/conf"),
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

                                 ch <- doHttrRequest(url          = httr::modify_url(private$apiServerUrl, path = paste0("/task/content/", fileKey, "/0")), 
                                                     request_type = "POST", 
                                                     the_body     = httr::upload_file(path = fileName),
                                                     config       = httr::add_headers(c("Content-Type" = "application/octet-stream", 
                                                                                        "X-Task-Key"   = private$token))
                                 )
                                 if (!ch$content)
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
                                 
                                 # ch <- doHttrRequest(url          = httr::modify_url(private$apiServerUrl, path = "/task/commit"), 
                                 #                     request_type = "VERB", 
                                 #                     config = httr::add_headers(c("Content-Type" = "application/json", 
                                 #                                                  "X-Task-Key"   = private$token))
                                 # )
                                 
                                 curl::new_handle() %>%
                                   curl::handle_setopt(customrequest = "POST") %>%
                                   curl::handle_setopt(postfields = "") %>%
                                   curl::handle_setheaders("Content-Type" = "application/json",
                                                           "X-Task-Key"   = private$token) -> handle
                                 
                                 ch <- curl::curl_fetch_memory(url    = paste0(private$apiServerUrl, "/task/commit"),
                                                               handle = handle)
                                 
                                 rawToChar(ch$content) %>%
                                   jsonlite::fromJSON() %>%
                                   private$checkAndNormalizeResponse(httpcode = ch$status_code,
                                                                     method   = "commitData") -> result  
                                 
                                 if (!inherits(ch$content, "list") || is.null(ch$content$log))
                                   myMessage("Error in commitData: response is invalid")

                                 return(ch$content)

                               },
                               getTaskStatus = function(exceptionOnError = FALSE) {

                                 if (is.null(private$token))
                                   stop("Error: Configuration validation error")

                                 # check if file exists
                                 return(!httr::http_error(self$resultFiles[1]))
                                 
                                 # TODO
                                 # curl::new_handle() %>%
                                 #   curl::handle_setheaders("Content-Type" = "application/json",
                                 #                           "X-Task-Key"   = private$token) -> handle
                                 # 
                                 # ch <- curl::curl_fetch_memory(url    = paste0(private$apiServerUrl, "/task/status"),
                                 #                               handle = handle)
                                 
                                 # rawToChar(ch$content) %>%
                                 #   jsonlite::fromJSON() %>%
                                 #   private$checkAndNormalizeResponse(httpcode = ch$status_code,
                                 #                                     method   = "getTaskStatus") -> result
                               },
                               getTaskResults = function(downloadFiles = T, destDir = getwd()) {
                                 if (is.null(private$token))
                                   stop("Error: Configuration validation error")

                                 if (is.null(self$resultFiles))
                                   stop("Error: No result files")

                                 # fetch data
                                 if (downloadFiles) {
                                   # destDir <- stringr::str_replace_all(destDir, "[/\\]", "")
                                   l <- lapply(self$resultFiles, function(x) {
                                     destFile <- unlist(stringr::str_split(x, "/"))
                                     destFile <- destFile[length(destFile)]
                                     curl::curl_download(url = x, destfile = paste0(destDir, "/", destFile))
                                   })
                                 } 
                                 
                                 # Parse data
                                 estParser <- ResultParser$new()
                                 estParser$parseRequestFile(self$dataFiles[["request_file"]])
                                 id <- which(stringr::str_detect(self$resultFiles, "/analysis_report"))
                                 if (length(id)) {
                                    estParser$parseReport(self$resultFiles[id])
                                 }
                                 
                                 # arc parsing
                                 id <- which(stringr::str_detect(self$resultFiles, "/ar_"))
                                 if (length(id)) {
                                    estParser$parseAR(self$resultFiles[id])
                                 }
                                 id <- which(stringr::str_detect(self$resultFiles, "/aar_"))
                                 if (length(id)) {
                                   estParser$parseAAR(self$resultFiles[id])
                                 }
                                 
                                 # avyc parsing
                                 id <- which(stringr::str_detect(self$resultFiles, "/avy_"))
                                 if (length(id)) {
                                   estParser$parseAR(self$resultFiles[id], analysisType = "AVy")
                                 }
                                 id <- which(stringr::str_detect(self$resultFiles, "/aavy_"))
                                 if (length(id)) {
                                   estParser$parseAAR(self$resultFiles[id])
                                 }
                                 
                                 # av paring
                                 
                                 
                                 estParser
                               },
                               getApiVersion = function() {
                                 ch <- doHttrRequest(url          = httr::modify_url(private$apiServerUrl, path = "/version"), 
                                                     request_type = "GET"
                                 )
                                 version <- ch$content
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
