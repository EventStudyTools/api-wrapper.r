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
                                 aarResults     = NULL,
                                 aar            = NULL,
                                 groups         = NULL,
                                 initialize = function(destDir = getwd()) {
                                   self$destDir <- destDir
                                 },
                                 parseReport = function(fileName = "analysis_report.csv") {
                                   self$analysisReport <- data.table::fread(paste0(self$destDir, "/", fileName))
                                 },
                                 parseAR = function(fileName = "ar_results.csv") {
                                   if (is.null(self$analysisReport))
                                     self$parseReport()
                                   
                                   abnormalReturns <- data.table::fread(paste0(self$destDir, "/", fileName))
                                   
                                   # Abnormal Returns
                                   stringr::str_detect(names(abnormalReturns), "AR") %>%
                                     which() -> id
                                   
                                   abnormalReturns %>%
                                     dplyr::select(c(1, id)) %>%
                                     reshape2::melt(id.vars = 1) %>%
                                     dplyr::rename(eventTime = variable, ar = value) -> self$arResults
                                   
                                   self$arResults %>%
                                     dplyr::mutate(eventTime = as.numeric(stringr::str_replace_all(as.character(eventTime), "[a-zA-Z()]", ""))) -> self$arResults
                                   
                                  # t-Values 
                                  stringr::str_detect(names(abnormalReturns), "t-value") %>%
                                     which() -> id
                                   
                                   abnormalReturns %>%
                                     dplyr::select(c(1, id)) %>%
                                     reshape2::melt(id.vars = 1) %>%
                                     dplyr::rename(eventTime = variable, tValue = value) -> tValues
                                   
                                   tValues %>%
                                     dplyr::mutate(eventTime = stringr::str_trim(stringr::str_replace_all(as.character(eventTime), "t-value", ""))) %>% 
                                     dplyr::mutate(eventTime = as.numeric(stringr::str_replace_all(as.character(eventTime), "[()]", ""))) -> tValues
                                   
                                   # Join t-Values
                                   self$arResults %>% 
                                     dplyr::left_join(tValues, by = c("Event ID", "eventTime")) -> self$arResults
                                   
                                   # Add additional Information
                                   self$analysisReport %>%
                                     dplyr::select(`Event ID`, Firm, `Reference Market`) -> arReport
                                   
                                   self$arResults %>%
                                     dplyr::left_join(arReport, by = "Event ID") %>% 
                                     dplyr::mutate(arLevel = "ar") -> self$arResults
                                 },
                                 parseAAR = function(fileName = "aar_results.csv", groups = NULL) {
                                   self$groups <- groups
                                   if (is.null(self$analysisReport))
                                     self$parseReport()
                                   
                                   aar <- data.table::fread(paste0(self$destDir, "/", fileName))
                                   stringr::str_detect(names(aar), "AAR") %>%
                                     which() -> id
                                   
                                   aar %>% 
                                     reshape2::melt(id.vars = 1) %>% 
                                     dplyr::rename(level     = `Grouping Variable/N`,
                                                   eventTime = variable) -> aar
                                   self$aarResults <- aar
                                   
                                   self$aarResults %>% 
                                     dplyr::mutate(eventTime = as.numeric(stringr::str_replace_all(as.character(eventTime), "[a-zA-Z()]", ""))) -> self$aarResults
                                   
                                   if (!is.null(groups) && !is.null(self$arResults)) {
                                     self$aarResults %>% 
                                       dplyr::filter(level %in% groups) -> self$aar
                                     }
                                   
                                   
                                 },
                                 plotAR = function(id = NULL) {
                                   if (is.null(id)) {
                                     hc <- highchart(type = "chart")
                                     nCols <- dplyr::n_distinct(self$arResults$Firm)
                                     mCols <- min(nCols, 9)
                                     pal <- brewer.pal(mCols, "Blues")
                                     
                                     if (nCols > mCols)
                                       pal <- grDevices::colorRampPalette(pal)(nCols)
                                     
                                     for (i in 1:nCols) {
                                       firm <- self$arResults$Firm[i]
                                       self$arResults %>% 
                                         dplyr::filter(Firm  == firm ) %>% 
                                         dplyr::mutate(ar = 100 * ar) %>% 
                                         dplyr::rename(x = eventTime, y = ar) -> tmp
                                       hc %>% 
                                         hc_add_series(tmp %>% dplyr::select(x, y), 
                                                       type        = "area", 
                                                       fillOpacity = .25, 
                                                       lineWidth   = 1, 
                                                       color       = pal[i],
                                                       marker      = list( enabled = F),
                                                       name        = unique(tmp$Firm)) -> hc
                                     }
                                     hc %>%   
                                       hc_tooltip(headerFormat  = '<b><span style="font-size: 12px">Event Day: {point.x}</span></b><br>',
                                                  pointFormat   = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y}</b><br/>',
                                                  sort          = F,
                                                  valueDecimals = 2,
                                                  valueSuffix   = "%",
                                                  table         = T) %>% 
                                       hc_yAxis(title = list(text = "Abnormal Returns"),
                                                labels = list(
                                                  format = "{value}%"
                                                )) %>% 
                                       hc_xAxis(title = list(text = "Event Day")) %>% 
                                       hc_legend(align = "right",
                                                 title = list(text = "Firms"),
                                                 verticalAlign = "top",
                                                 layout = "vertical") %>% 
                                       hc_title(text = "Abnormal Returns")
                                   }
                                   
                                 }
                               ))
