#' A parser for Event Study result files
#' 
#' This result file parser works at the moment just for csv files.
#' 
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to }
#'   \item{\code{new(dir)}}{This method is used to create object of this class with \code{dir} as the directory of result files.}
#'   \item{\code{parseReport(fileName = "analysis_report.csv")}}{This method parses the analysis report file (analysis_report.csv).}
#'   \item{\code{parseAR(fileName = "ar_results.csv")}}{This method parses the abnormal return file (ar_results.csv). Furthermore, it triggers \code{parseReport} and join firm and index name.}
#'   \item{\code{plotAR(id = NULL)}}{This method abnormal returns time series with \code{id} as the firm id.}}
#'   
#' @export
ESTResultParser <- R6::R6Class(classname = "ESTResultParser",
                               public = list(
                                 destDir        = NULL,
                                 requestFile    = NULL,
                                 analysisReport = NULL,
                                 arResults      = NULL,
                                 aarResults     = NULL,
                                 aar            = NULL,
                                 groups         = NULL,
                                 initialize = function() {
                                 },
                                 parseRequestFile = function(path = "01_RequestFile.csv") {
                                   if (file.exists(path)) {
                                     self$requestFile <- data.table::fread(path, header = F)
                                   } else {
                                     message(paste0("File ", fileName, " not found!"))
                                   }
                                 },
                                 parseReport = function(fileName = "analysis_report.csv") {
                                   if (file.exists(fileName)) {
                                     self$analysisReport <- data.table::fread(fileName)
                                   } else {
                                     message(paste0("File ", fileName, " not found!"))
                                   }
                                 },
                                 parseAR = function(fileName = "ar_results.csv", analysisType = "AR") {
                                   if (is.null(self$analysisReport))
                                     self$parseReport()
                                   
                                   if (file.exists(fileName)) {
                                     abnormalReturns <- data.table::fread(fileName)
                                   } else {
                                     message(paste0("File ", fileName, " not found!"))
                                     return(NULL)
                                   }
                                   
                                   # parse Abnormal Returns
                                   stringr::str_detect(names(abnormalReturns), analysisType) %>%
                                     which() -> id
                                   
                                   abnormalReturns %>%
                                     dplyr::select(c(1, id)) %>%
                                     reshape2::melt(id.vars = 1) %>%
                                     dplyr::rename(eventTime = variable, ar = value) -> self$arResults
                                   
                                   self$arResults %>%
                                     dplyr::mutate(eventTime = as.numeric(stringr::str_replace_all(as.character(eventTime), "[a-zA-Z()]", ""))) -> self$arResults
                                   
                                   # parse t-Values 
                                   stringr::str_detect(names(abnormalReturns), "t-value") %>%
                                     which() -> id
                                   
                                   if (length(id)) {
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
                                     
                                   }
                                   
                                   # Add additional Information
                                   id <- which(names(self$analysisReport) %in% c("Event ID", "Firm", "Reference Market", "Estimation Window Length"))
                                   arReport <- self$analysisReport[, id]
                                   
                                   self$arResults %>%
                                     dplyr::left_join(arReport, by = "Event ID") %>% 
                                     dplyr::mutate(arLevel = "ar") -> self$arResults
                                   
                                   # if available add grouping
                                   self$requestFile %>% 
                                     dplyr::select(1, 5) -> requestFile
                                   names(requestFile) <- c("Event ID", "Group")
                                   
                                   self$arResults %>% 
                                     dplyr::left_join(requestFile, by = "Event ID") -> self$arResults
                                 },
                                 parseAAR = function(fileName = "aar_results.csv", groups = NULL) {
                                   self$groups <- groups
                                   if (is.null(self$analysisReport))
                                     self$parseReport()
                                   
                                   # parse AAR values
                                   aar <- data.table::fread(fileName)
                                   stringr::str_detect(names(aar), "AAR") %>%
                                     which() -> id
                                   
                                   aar %>% 
                                     reshape2::melt(id.vars    = 1, 
                                                    value.name = "aar") %>% 
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
                                 plotAbnormalReturns = function(id = NULL, windows = NULL) {
                                   if (is.null(id)) {
                                     hc <- highchart(type = "chart")
                                     nCols <- dplyr::n_distinct(self$arResults$Firm)
                                     mCols <- min(nCols, 9)
                                     pal <- brewer.pal(mCols, "Blues")
                                     
                                     if (nCols > mCols)
                                       pal <- grDevices::colorRampPalette(pal)(nCols)
                                     
                                     if (is.null(windows))
                                       window <- range(self$arResults$eventTime)
                                     selectedWindow <- seq(from = window[1], to = window[2], by = 1)
                                     
                                     for (i in 1:nCols) {
                                       firm <- self$arResults$Firm[i]
                                       self$arResults %>% 
                                         dplyr::filter(eventTime %in% selectedWindow) %>% 
                                         dplyr::filter(Firm  == firm ) %>% 
                                         dplyr::mutate(ar = 100 * ar) %>% 
                                         dplyr::rename(x = eventTime, y = ar) -> tmp
                                       hc %>% 
                                         hc_add_series(tmp %>% dplyr::select(x, y), 
                                                       type        = "area", 
                                                       fillOpacity = .15, 
                                                       lineWidth   = 1, 
                                                       color       = pal[i],
                                                       marker      = list(enabled = F),
                                                       name        = firm) -> hc
                                     }
                                     hc %>%   
                                       hc_tooltip(headerFormat  = '<b><span style="font-size: 12px">Event Day: {point.x}</span></b><br>',
                                                  pointFormat   = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y}</b><br/>',
                                                  sort          = F,
                                                  valueDecimals = 2,
                                                  valueSuffix   = "%",
                                                  table         = T) %>% 
                                       hc_yAxis(title = list(text = ""),
                                                labels = list(
                                                  format = "{value}%"
                                                )) %>% 
                                       hc_xAxis(title = list(text = ""),
                                                plotLines = list(
                                                  list(
                                                    label = list(text          = "Event Day",
                                                                 style         = list(color = "gray"),
                                                                 rotation      = 0,
                                                                 verticalAlign = "top",
                                                                 y             = 10),
                                                    dashStyle = "Dash",
                                                    color     = "gray",
                                                    width     = 1,
                                                    value     = 0
                                                  )
                                                )) %>% 
                                       hc_legend(align         = "right",
                                                 title         = list(text = "Firms"),
                                                 verticalAlign = "top",
                                                 layout        = "vertical")
                                   } else {
                                     self$arResults %>% 
                                       dplyr::filter(eventTime %in% selectedWindow) %>% 
                                       dplyr::filter(`Event ID` == id) -> tmp
                                     
                                     if (nrow(tmp) == 0)
                                       return(NULL)
                                     
                                     self$analysisReport %>% 
                                       dplyr::filter(`Event ID` == id) -> tmpReport
                                     n <- tmpReport$`Estimation Window Length`
                                     tmp$pValue <- 100 * abs(qt(0.975, df = n - 1) * tmp$tValue / sqrt(n))
                                     
                                     firmName <- tmp$Firm[1]
                                     tmp %>% 
                                       dplyr::mutate(ar = ar * 100) %>% 
                                       dplyr::rename(x = eventTime, y = ar) -> tmp
                                     
                                     hc <- highchart(type = "chart")
                                     tmp %>% 
                                       dplyr::mutate(low = y - pValue,
                                                     high = y + pValue) -> tmp
                                     
                                     pal <- brewer.pal(3, "Blues")
                                     hc %>% 
                                       hc_add_series(tmp %>% dplyr::select(x, y),
                                                     type        = "area", 
                                                     fillOpacity = .15, 
                                                     lineWidth   = 1, 
                                                     color       = pal[3],
                                                     marker = list(enabled = F),
                                                     name        = firmName) -> hc
                                     
                                     hc %>%   
                                       hc_tooltip(headerFormat = '<b><span style="font-size: 12px">Event Day: {point.x}</span></b><br>',
                                                  pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y}</b><br/>',
                                                  sort = F,
                                                  valueDecimals = 2,
                                                  valueSuffix = "%",
                                                  table = T) %>% 
                                       hc_yAxis(title = list(text = ""),
                                                labels = list(
                                                  format = "{value}%"
                                                )) %>% 
                                       hc_xAxis(title = list(text = ""),
                                                plotLines = list(
                                                  list(
                                                    label = list(text = "Event Day",
                                                                 style = list(color = "gray"),
                                                                 rotation = 0,
                                                                 verticalAlign = "top",
                                                                 y = 10),
                                                    dashStyle = "Dash",
                                                    color = "gray",
                                                    width = 1,
                                                    value = 0
                                                  )
                                                )) %>% 
                                       hc_legend(enabled = F,
                                                 align = "right",
                                                 verticalAlign = "top",
                                                 layout = "vertical") 
                                   }
                                 }
                               ))
