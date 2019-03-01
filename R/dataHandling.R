#' @title Check input data files
#' 
#' @description Check correct column, date, and shape of the input 
#' data files
#' 
#' @param path path to the input data file
#' @param type the type of file to ckeck
#'
#' @return data.frame
#' 
#' @examples 
#' \dontrun{
#' # save example files to current working directory
#' getSP500ExampleFiles()
#'                
#' checkFile("01_RequestFile.csv", "request_file")
#' }
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
    int_cols <- c('id', "startEventWindow", "endEventWindow", "endEstimationWindow", "lengthEstimationWindow")
    dtData %>% 
      dplyr::select(int_cols) %>% 
      purrr::map2(.y = int_cols, 
                  .f = checkClass, class = "numeric") -> ret
    
    # Check on integer
    dtData %>% 
      dplyr::select(int_cols) %>% 
      purrr::map2(.y = int_cols, 
                 .f = function(x, y) {
                    testthat::expect_true(all.equal(x, as.integer(x)), label = y)
                  }) -> ret
    
    # Check on class Character
    char_class <- c("firm", "market", "eventDate", "grouping")
    dtData %>% 
      dplyr::select(char_class) %>% 
      purrr::map2(.y = char_class, 
                  .f = checkClass, class = "character") -> ret
    
    # check event id (column1)
    dtData %>% 
      dplyr::select(id) %>% 
      dplyr::n_distinct() -> n
    m <- nrow(dtData)
    testthat::expect_true(object = n == m, 
                          label  = "Unique IDs", 
                          info   = "Please ensure that all event IDs are unique.")
    
    # check start event window: 
    # - start event window <= 0
    # - start event window <= end event window
    # purrr version on CRAN do not contain by_row
    # dtData %>% 
    #   purrr::by_row(..f = function(x) {
    #     msg <- paste("Event ID:", x[1], "The event window start should be <= 0 and smaller equals event window end.")
    #     testthat::expect(x[6] <= 0 && x[6] <= x[7], msg)
    #   })
    
    dtData %>% 
      tidyr::nest(-id, -firm) %>% 
      dplyr::mutate(event_window = purrr::map(data, function(x) {
        if (x$startEventWindow <= 0 & x$startEventWindow <= x$endEventWindow) {
          msg <- "All fine"
        } else {
          msg <- paste("Event ID:", x$id, "The event window start should be <= 0 and smaller equals event window end.")
          print(msg)
        }
        return(msg)
      }))
    
    
    # check date format
    checkDateFormat(dtData$eventDate)
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
    checkDateFormat(dtData[[2]])
  } 
  
  return(dtData)
}


#' @title Check EventStudy input files
#' 
#' @description Check each input file plus inter file relations, whether market
#' index and firm identifier in request file match market index in market_data and firm identifier in 
#' in firm_data file.
#' 
#' @param dataFiles A named character vector. The names must be request_file, 
#' firm_data, and market_data
#' @param returnData returns the data as list of data.frames
#' 
#' @examples
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
  # For R checks
  X1 <- NULL
  
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
  
  # parse dates
  eventDate <- requestData[[4]]
  eventDate <- as.Date(eventDate, format = "%d.%m.%Y")
  firmData[[2]] <- as.Date(firmData[[2]], format = "%d.%m.%Y")
  marketData[[2]] <- as.Date(marketData[[2]], format = "%d.%m.%Y")
  
  # check event window range
  maxEventDate <- eventDate + requestData[[7]]
  firmIndex %>% 
    purrr::map2(.y = maxEventDate, .f = function(x, y, firmData, marketData) {
      firmData %>% 
        dplyr::filter(X1 == x) -> subFirmData
      
      testthat::expect(y <= max(subFirmData[[2]]), paste0("Event window end is after max firm data for firm: ", x))
      testthat::expect(y <= max(marketData[[2]]), paste0("Event window end is after max market data"))
    }, firmData = firmData, marketData = marketData)

  # check estimation window range
  minEstimationDate <- eventDate - requestData[[9]]
  firmIndex %>% 
    purrr::map2(.y = minEstimationDate, .f = function(x, y, firmData, marketData) {
      firmData %>% 
        dplyr::filter(X1 == x) -> subFirmData
      
      testthat::expect(y >= min(subFirmData[[2]]), paste0("Estimation window start is before min firm data for firm: ", x))
      testthat::expect(y >= min(marketData[[2]]), paste0("Estimation window start is before min market data"))
    }, firmData = firmData, marketData = marketData)
  
  # check date 01.01.1970
  testthat::expect(all(eventDate >= as.Date("1970-01-01")), "Dates must be after 1970-01-01")
  testthat::expect(all(firmData[[2]] >= as.Date("1970-01-01")), "Dates must be after 1970-01-01")
  testthat::expect(all(marketData[[2]] >= as.Date("1970-01-01")), "Dates must be after 1970-01-01")
  
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
checkDateFormat <- function(x) {
  x <- as.Date(x, format = "%d.%m.%Y")
  testthat::expect(!any(is.na(x)), "Incorrect date format. Correct format is: %d.%m.%Y, e.g. 28.10.2011")
}


