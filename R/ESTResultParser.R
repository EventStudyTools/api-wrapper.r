#' A parser for Event Study result files
#'
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

                                   arResults %>%
                                     dplyr::mutate(eventTime = as.numeric(stringr::str_replace_all(as.character(eventTime), "[a-zA-Z()]", ""))) -> self$arResults

                                 }
                               ))
