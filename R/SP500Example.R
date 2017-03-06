#' This function copies the 3 csv files to the actual working directory
#'
#' @export
getSP500ExampleFiles <- function(targetDir = getwd()) {
  c("01_RequestFile.csv", "02_FirmData.csv", "03_MarketData.csv") %>% 
    purrr::map(.f = function(x, targetDir) {
      requestFile <- system.file("extdata/SP500Example", x, package = "EventStudy")
      file.copy(from = requestFile, to = targetDir, overwrite = T)
    }, targetDir = targetDir)
}
