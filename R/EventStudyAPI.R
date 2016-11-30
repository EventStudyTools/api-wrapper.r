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
                               configureTask = function(input) {

                                 if (!is(input, "ArcApplicationInput") || !is(input, "CataApplicationInput") || is.null(private$token))
                                   stop("Error in configureTask: required parameters are not set")

                                 new_handle() %>%
                                   handle_setopt(customrequest = "POST") %>%
                                   handle_setopt(postfields = "") %>%
                                   handle_setheaders("Content-Type" = "application/json",
                                                     "X-Task-Key"   = private$token) -> handle

                                 json <- input$toJson()

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
                               uploadFile = function(fileKey, fileName, partNumber) {

                                 if (is.null(private$token) || is.null(fileKey) || is.null(fileName))
                                   stop("Error: configuration error")

                                 if (!file.exists(filename))
                                   stop("Error: file do not exist")


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
                                                                     method   = "getTaskResults") -> result

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
                               deleteFileParts = function(parts) {

                               },
                               splitFile = function(fileName, maxChunkSize) {

                                 partFileNames <- c()
                                 i <- 0

                               },
                               checkAndNormalizeResponse = function(response, httpcode, method, exceptionOnError = T) {

                                 if ("error" %in% names(response)) {
                                   if (exceptionOnError) {
                                     stop(paste0("Error in ", method, ': request to api failed'))
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
