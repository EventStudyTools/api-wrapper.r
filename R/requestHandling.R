#' ReTry API requests for certain errors using exponential backoff.
#'
#' @param f A function of a http request
#'
#' @keywords internal
retryRequest <- function(f) {
  
  verbose <- getOption("EventStudy.verbose")
  if(verbose <= 1){
    the_request <- try(httr::with_verbose(f))
  } else {
    the_request <- try(f)
  }
  
  status_code <- as.character(the_request$status_code)
  
  if(!(grepl("^20", status_code))) {
    myMessage("Request Status Code: ", status_code, level = 3)
    
    content <- jsonlite::fromJSON(httr::content(x        = the_request,
                                                as       = "text",
                                                type     = "application/json",
                                                encoding = "UTF-8"))
    
    if (exists("error", where=content)) {
      error <- content$error
      myMessage("JSON fetch error: ", paste(error), level = 3)
    } else {
      error <- "Unspecified Error"
    }
    
    if (grepl("^5|429",status_code)) {
      for (i in 1:getOption("EventStudy.tryAttempts")) {
        myMessage("Trying again: ", i, " of ", 
                  getOption("EventStudy.tryAttempts"), 
                  level = 3)
        Sys.sleep((2 ^ i) + stats::runif(n = 1, min = 0, max = 1))
        the_request <- try(f)
        if(grepl("^20", status_code)) break
      }
      myMessage("All attempts failed.", level = 3)
    } else {
      myMessage("No retry attempted: ", error, level = 3)
    }
  }
  
  the_request
}


#' Get URL content based on local
#'
#' @description
#' This changes the auth type depending on if its local
#'
#' @param url the url of the page to retrieve
#' @param request_type the type of httr request function: GET, POST, PUT, DELETE etc.
#' @param the_body body of POST request
#' @param params A named character vector of other parameters to add to request.
#' @param customConfig list of httr options such as \code{httr::use_proxy}
#'   or \code{httr::add_headers} that will be added to the request.
#' @param simplifyVector Passed to jsonlite::fromJSON
#'
#' @details Example of params: c(param1="foo", param2="bar")
#'
#' @importFrom utils packageVersion
#' @keywords internal
doHttrRequest <- function(url,
                          request_type   = "GET",
                          the_body       = NULL,
                          encode         = "json",
                          config         = NULL,
                          simplifyVector = getOption("EventStudy.jsonlite.simplifyVector")){
  
  
  arg_list <- list(url    = url,
                   body   = the_body,
                   encode = if(!is.null(encode)) encode else "json"
  )
  
  if (!is.null(config)) {
    arg_list[["config"]] <- config
  }
  
  if (!is.null(the_body) && arg_list$encode == "json") {
    tt <- try(myMessage("Body JSON parsed to: ", jsonlite::toJSON(the_body, auto_unbox=T), level = 2))
    if (isError(tt)) {
      myMessage("Could not parse body JSON", level = 2)
    } 
  }
  req <- retryRequest(f = do.call(what  = request_type, 
                                  args  = arg_list, 
                                  envir = asNamespace("httr")))
  rawResponse <- getOption("EventStudy.rawResponse")
  if (!rawResponse) {
    if (checkESTAPIError(req)) {
      content <- httr::content(x        = req, 
                               as       = "text", 
                               type     = "application/json",
                               encoding = "UTF-8")
      content <- jsonlite::fromJSON(content,
                                    simplifyVector = simplifyVector)
      req$content <- content
    }
  } else {
    myMessage("No checks on content due to option EventStudy.rawResponse, returning raw", level=2)
    req
  }
  req
}


#' Get Google API errors
#'
#' @param req a httr request
#' @param ok_content_types Expected content type of request
#' @param batched called from gar_batch or not
#'
#' @keywords internal
checkESTAPIError <- function(req,
                             ok_content_types=getOption("EventStudy.ok_content_types"),
                             batched=FALSE) {
  # from a batched request, we already have content
  skip_checks <- FALSE
  if (batched) {
    myMessage("Skipping API checks for batch content_type", level=2)
    skip_checks <- TRUE
  }
  
  if(!skip_checks){
    est.json <- httr::content(x        = req, 
                              as       = "text", 
                              type     = "application/json", 
                              encoding = "UTF-8")
    if(is.null(est.json)) {
      warning('JSON parsing was NULL')
      return(FALSE)
    }
    
    if(nchar(est.json) > 0) {
      est.json <- jsonlite::fromJSON(est.json)
    } else {
      warning("No JSON content detected")
      return(FALSE)
    }
    
    if(!is.null(req$headers$`content-type`)){
      if(!(req$headers$`content-type` %in% ok_content_types)) {
        stop(sprintf(paste("Not expecting content-type to be:\n%s"),
                     req$headers[["content-type"]]))
      }
    } else {
      myMessage("No content-type returned.", level=1)
      return(FALSE)
    }
    
    ## get error message from API
    # if (!is.null(est.json$error$message)) {
    #   stop("JSON fetch error: ", paste(est.json$error$message))
    # }
    httr::stop_for_status(req)
  } else {
    est.json <- req
  }
  TRUE
}
