#' API for EventStudyTools.com
#'
#' \describe{
#'   \item{\code{new(apiServerUrl)}}{This method is used to create object of this class with \code{apiServerUrl} as the url to the est server.}
#'   \item{\code{authentication(apiKey)}}{This method is used to authenticate at \code{apiServerUrl}. A valid \code{apiKey} is required.}
#'   \item{\code{processTask()}}{This method starts the EST calculation on the server (after files are uploaded.}
#'   \item{\code{configureTask(input)}}{This method configures the Event Study. \code{input} is an \code{ApplicationInputInterface} R6 object, e.g. ARC configuration class}
#'   \item{\code{uploadFile(fileKey, fileName, partNumber = 0)}}{This method links to the file to upload. \code{fileKey} is the key of the file, e.g. request_file. \code{fileName} file name to upload.}
#'   \item{\code{commitData()}}{This method commit the data to the server.}
#'   \item{\code{getTaskStatus()}}{Todo}
#'   \item{\code{getTaskResults()}}{Downloads the result files of the Event Study.}
#'   }
#'
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

                                 if (!is(result, "list") || is.null(result$token))
                                   stop("Error: authentication failed")

                                 private$token <- result$token
                                 return(TRUE)
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
