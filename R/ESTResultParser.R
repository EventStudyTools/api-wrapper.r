#' A parser for Event Study result files
#' 
#' This result file parser works at the moment just for csv files.
#' 
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to }
#'   \item{\code{new(dir)}}{This method is used to create object of this class with \code{dir} as the directory of result files.}
#'
#'   \item{\code{parseReport()}}{This method parses the analysis report file (analysis_report.csv).}
#'   \item{\code{parseAR()}}{This method parses the abnormal return file (ar_results.csv). Furthermore, it triggers \code{parseReport} and join firm and index name.}
#'   \item{\code{plotAR(id = NULL)}}{This method abnormal returns time series with \code{id} as the firm id.}}
#' @export
ESTResultParser <- R6::R6Class(classname = "ESTResultParser",
                               public = list(
                                 destDir        = NULL,
                                 analysisReport = NULL,
                                 arResults      = NULL,
                                 initialize = function(destDir = getwd()) {
                                   self$destDir <- destDir
                                 },
                                 parseReport = function() {
                                   self$analysisReport <- data.table::fread(paste0(self$destDir, "/analysis_report.csv"))
                                 },
                                 parseAR = function() {
                                   if (is.null(self$analysisReport))
                                     self$parseReport()
                                   
                                   abnormalReturns <- data.table::fread(paste0(self$destDir, "/ar_results.csv"))
                                   stringr::str_detect(names(abnormalReturns), "AR") %>%
                                     which() -> id
                                   
                                   abnormalReturns %>%
                                     dplyr::select(c(1, id)) %>%
                                     reshape2::melt(id.vars = 1) %>%
                                     dplyr::rename(eventTime = variable, ar = value) -> self$arResults
                                   
                                   self$arResults %>%
                                     dplyr::mutate(eventTime = as.numeric(stringr::str_replace_all(as.character(eventTime), "[a-zA-Z()]", ""))) -> self$arResults
                                   
                                   self$analysisReport %>%
                                     dplyr::select(`Event ID`, Firm, `Reference Market`) -> arReport
                                   
                                   self$arResults %>%
                                     dplyr::left_join(arReport, by = "Event ID") -> self$arResults
                                 },
                                 plotAR = function(id = NULL) {
                                   
                                 }
                               ))
