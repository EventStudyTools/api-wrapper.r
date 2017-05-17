#' This function copies the 3 csv files to the actual working directory. This 
#' example data is used as motivation for using Event Studies in M&A research.
#' 
#' For more details see the help vignette:
#' \code{vignette("introduction_eventstudy", package = "EventStudy")}
#' 
#' or on our website:
#' \url{https://www.eventstudytools.com/mergers-acquisitions}
#' 
#' @param targetDir directory to save example files
#' 
#' @examples 
#' \dontrun{
#' getSP500ExampleFiles("data")
#' }
#'
#' @export
getSP500ExampleFiles <- function(targetDir = getwd()) {
  c("01_RequestFile.csv", "02_FirmData.csv", "03_MarketData.csv") %>% 
    purrr::map(.f = function(x, targetDir) {
      requestFile <- system.file("extdata/SP500Example", x, package = "EventStudy")
      file.copy(from = requestFile, to = targetDir, overwrite = T)
    }, targetDir = targetDir) %>% 
    unlist() -> ret
  if (sum(ret) == 3)
    message("Files are generated.")
}
