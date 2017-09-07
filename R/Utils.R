#' This functionality is borrowed from: https://github.com/MarkEdmondson1234/googleAuthR

#' ReTry API requests for certain errors using exponential backoff.
#'
#' @param f A function of a http request
#'
#' @keywords internal
retryRequest <- function(f) {
  
  verbose <- getOption("EventStudy.verbose")
  
  if(verbose <= 1) {
    the_request <- try(httr::with_verbose(f))
  } else {
    the_request <- try(f)
  }
  
  if (isError(the_request)) {
    warning("Request failed before finding status code. Retrying.")
    status_code <- "500"
  } else {
    status_code <- as.character(the_request$status_code)
  }
  
  if (!(grepl("^20", status_code))) {
    myMessage("Request Status Code: ", status_code, level = 3)
    
    content <- try(jsonlite::fromJSON(httr::content(x        = the_request,
                                                    as       = "text",
                                                    type     = "application/json",
                                                    encoding = "UTF-8")))
    if (isError(content)) {
      warning("No JSON content found in request", call. = FALSE)
      error <- "Could not fetch response"
    } else if(exists("error", where=content)) {
      error <- content$error$message
    } else {
      error <- "Unspecified Error"
    }
    myMessage("JSON fetch error: ", paste(error), level = 2)
    
    if(grepl("^5|429",status_code)){
      for(i in 1:getOption("EventStudy.tryAttempts")){
        myMessage("Trying again: ", i, " of ", 
                  getOption("EventStudy.tryAttempts"), 
                  level = 3)
        Sys.sleep((2 ^ i) + stats::runif(n = 1, min = 0, max = 1))
        the_request <- try(f)
        if(grepl("^20",status_code)) break
      }
      myMessage("All attempts failed.", level = 3)
    } else {
      myMessage("No retry attempted: ", error, level = 2)
    }
  }
  the_request
}


#' Customer message log level
#' 
#' @param ... The message(s)
#' @param level The severity
#' 
#' @details 0 = everything, 1 = debug, 2=normal, 3=important
#' 
#' @keywords internal
myMessage <- function(..., level = 2){
  compareLevel <- getOption("EventStudy.verbose")
  if(level >= compareLevel) {
    message(...)
  }
}


#' Is this a try error?
#' 
#' Utility to test errors
#' 
#' @param test_me an object created with try()
#' 
#' @return Boolean
#' 
#' @keywords internal
isError <- function(x){
  inherits(x, "try-error")
}


#' Get the error message
#'
#' @param test_me an object that has failed is.error
#'
#' @return The error message
#'
#' @keywords internal
errorMessage <- function(x) {
  if(isError(x)) attr(x, "condition")$message
}


#' Set eventStudy API Key
#' 
#' @param key EventStudy API Key
#' 
#' @export
estAPIKey <- function(key) {
  options(EventStudy.KEY = apiKey)
}


#' @title Returns default parameters for an EventStudy type
#' 
#' @param type type of event study (default: arc)
#' 
#' @keywords internal
getDefaultApplicationInput <- function(type = "arc") {
  if (estType == "arc") {
    defaultParams <- ARCApplicationInput$new()
  } else if (estType == "avc") {
    defaultParams <- ARCApplicationInput$new()
  } else if (estType == "avyc") {
    defaultParams <- ARCApplicationInput$new()
  }
  defaultParams
}
