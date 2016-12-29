#' API for EventStudyTools.com
#'
#' @export
EventStudyAPI <- R6::R6Class(classname = "EventStudyAPI",
                             public = list(
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

                                 if (!result)
                                   stop(paste0("Error in processTask: Application launch error"))

                                 return(TRUE)
                               },
                               configureTask = function(input) {

                                 if (!is(input, "ApplicationInputInterface") || is.null(private$token))
                                   stop("Error in configureTask: required parameters are not set")

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


                                 # TODO: split file
                                 # new_handle() %>%
                                 #   handle_setopt(customrequest = "POST") %>%
                                 #   handle_setheaders("Content-Type" = "application/octet-stream",
                                 #                     "X-Task-Key"   = private$token) -> handle
                                 #
                                 # handle %>%
                                 #   handle_setopt(upload = TRUE) %>%
                                 #   handle_setopt(infilesize = file.size(fileName)) %>%
                                 #   handle_setopt(readdata = "fd") -> handle
                                 #
                                 # ch <- curl_fetch_memory(url    = paste0(private$apiServerUrl, "/task/content/", fileKey, "/0"),
                                 #                         handle = handle)

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
                               getTaskResults = function() {

                                 if (is.null(private$token))
                                   stop("Error: Configuration validation error")

                                 new_handle() %>%
                                   handle_setheaders("") -> handle

                                 ch <- curl_fetch_memory(url    = paste0(private$apiServerUrl, "/results/", private$token),
                                                         handle = handle)

                                 rawToChar(ch$content) %>%
                                   jsonlite::fromJSON() %>%
                                   private$checkAndNormalizeResponse(httpcode = ch$status_code,
                                                                     method   = "getTaskResults") -> result

                                 if (is.null(result) || is.null(result$results))
                                   stop("Error in getTaskResults: result is empty")

                                 return(result)
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
