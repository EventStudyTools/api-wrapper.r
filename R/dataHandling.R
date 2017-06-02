#' @title Chheck input data files
#' 
#' @description Check correct column, date, and shape of the input 
#' data files
#' 
#' @param path path to the input data file
#'
#' @return data.frame
#'  
#' @export
checkFile <- function(path, type = "request_file") {
  type <- match.arg(type, c("request_file", "firm_data", "market_data"))
  testthat::expect_true(file.exists(path))

  cat(paste("Checking", type))
  dtData <- readr::read_delim(file      = path, 
                              delim     = ";", 
                              col_names = F)  
  if (type == "request_file") {
    columnLabels <- c("id", "firm", "market", "eventDate", "grouping", "startEventWindow", "endEventWindow", "endEstimationWindow", "lengthEstimationWindow")
    names(dtData) <- columnLabels
    
    # Check on class Integer
    idInteger <- c(1, 6:9)
    dtData %>% 
      dplyr::select(idInteger) %>% 
      purrr::map2(.y = columnLabels[idInteger], 
                  .f = checkClass, class = "integer") -> ret
        
    # Check on class Character
    idCharacter <- c(2:5)
    dtData %>% 
      dplyr::select(idCharacter) %>% 
      purrr::map2(.y = columnLabels[idCharacter], 
                  .f = checkClass, class = "character") -> ret
    
    # check event id (column1)
    dtData[[1]] %>% 
      dplyr::n_distinct() -> n
    m <- nrow(dtData)
    testthat::expect_true(object = n == m, 
                          label  = "Unique IDs", 
                          info   = "Please ensure that all event IDs are unique.")
    
    # check start event window: 
    # - start event window <= 0
    # - start event window <= end event window
    dtData %>% 
      purrr::by_row(..f = function(x) {
        msg <- paste("Event ID:", x[1], "The event window start should be <= 0 and smaller equals event window end.")
        testthat::expect(x[6] <= 0 && x[6] <= x[7], msg)
      })
    
    # check date format
    checkDate(dtData[[4]])
  } else if (type %in% c("firm_data", "market_data")) {
    # check character vectors
    columnLabels <- c("firm", "date", "value")
    if (ncol(dtData) > 3) {
      columnLabels <- c(columnLabels, "")
    }
    idCharacter <- 1:2
    dtData %>% 
      dplyr::select(idCharacter) %>% 
      purrr::map2(.y = columnLabels[idCharacter], 
                  .f = checkClass, class = "character") -> ret
    
    # check numeric values; there may be additional columns
    idNumeric <- 3:ncol(dtData)
    dtData %>% 
      dplyr::select(idNumeric) %>% 
      purrr::map2(.y = columnLabels[idNumeric], 
                  .f = checkClass, class = "numeric") -> ret
    
    # check date format
    checkDate(dtData[[2]])
  } 
  
  return(dtData)
}


#' @title Check EventStudy input files
#' 
#' @description check each input file plus inter file relations, e.g. is market
#' index in request file has correct index in market_data and firm index in 
#' request_file has correct index in firm_data.
#' 
#' @param dataFiles a named character vector. The names must be request_file, 
#' firm_data, and market_data
#' @param returnData returns the data as list of data.frames
#' 
#' @expamles
#' \dontrun{
#' # save example files to current working directory
#' getSP500ExampleFiles()
#' 
#' dataFiles <- c("request_file" = "01_RequestFile.csv", 
#'                "firm_data"    = "02_firmData.csv", 
#'                "market_data"  = "03_MarketData.csv")
#'                
#' checkFiles(dataFiles)
#' }
#' 
#' @export
checkFiles <- function(dataFiles = c("request_file" = "01_RequestFile.csv", 
                                     "firm_data"    = "02_firmData.csv", 
                                     "market_data"  = "03_MarketData.csv"),
                       returnData = F) {
  # check file names
  fileNames <- names(dataFiles)
  testthat::expect("request_file" %in% fileNames, "checkFiles: filename request_file is not correct")
  testthat::expect("firm_data" %in% fileNames, "checkFiles: filename firm_data is not correct")
  testthat::expect("market_data" %in% fileNames, "checkFiles: filename market_data is not correct")
  
  # load and check data
  requestData <- EventStudy::checkFile(dataFiles["request_file"], type = "request_file")
  firmData <- EventStudy::checkFile(dataFiles["firm_data"], type = "firm_data")
  marketData <- EventStudy::checkFile(dataFiles["market_data"], type = "market_data")
  
  # check if market index in request file is in market_data
  marketIndex <- unique(requestData[[3]])
  marketIndexData <- unique(marketData[[1]])
  marketIndex %>% 
    purrr::map(.f = function(x, y) {
      testthat::expect(x %in% y, paste(x, "not in market data"))
    }, y = marketIndexData) -> ret
  
  # check if firm names are firm data
  firmIndex <- unique(requestData[[2]])
  firmIndexData <- unique(firmData[[1]])
  firmIndex %>% 
    purrr::map(.f = function(x, y) {
      testthat::expect(x %in% y, paste(x, "not in firm data"))
    }, y = firmIndexData) -> ret
  
  if (returnData) {
    return(list(
      request_file = requestData,
      firm_data    = firmData,
      marketData   = marketData
    ))
  }
}


#' @title Check column type
#' 
#' @description Function for map2 function from purrr. This function checks 
#' columns on the correct type
#' 
#' @param x vector
#' @param y column label
#' @param class variable type to check
#' 
#' @keywords internal
checkClass <- function(x, y, class = "integer") {
  testthat::expect_is(object = x, 
                      class  = class,
                      label  = paste("Column", y, "is not of type Integer!"))
  NULL
}


#' @title Check on correct date format
#' 
#' @description The date format must be in the form %d.%m.%Y
#' 
#' @param x a character vector
#' 
#' @keywords internal
checkDate <- function(x) {
  x <- as.Date(x, format = "%d.%m.%Y")
  testthat::expect(!any(is.na(x)), "Incorrect date format. Correct format is: %d.%m.%Y, e.g. 28.10.2011")
}


