# // Copyright (C) 2017 Simon Müller
# // This file is part of EventStudy
# //
# // EventStudy is free software: you can redistribute it and/or modify it
# // under the terms of the GNU General Public License as published by
# // the Free Software Foundation, either version 2 of the License, or
# // (at your option) any later version.
# //
# // EventStudy is distributed in the hope that it will be useful, but
# // WITHOUT ANY WARRANTY; without even the implied warranty of
# // MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# // GNU General Public License for more details.
# //
# // You should have received a copy of the GNU General Public License
# // along with EventStudy  If not, see <http://www.gnu.org/licenses/>.
#' @name ResultParser
#' 
#' @title Parses request and results files returned from our Event Study API
#' interface.
#' 
#' @description 
#' This result file parser works currently only with csv files. Please read
#' the vignette for further details (coming soon). We will restructure our 
#' result reports soon. So, this function may change dramatically. This object 
#' can be used for plotting your results.
#' 
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{new(dir)}}{This method is used to create object of this class 
#'   with \code{dir} as the directory of result files.}
#'   \item{\code{parseReport(path = "analysis_report.csv")}}{This method 
#'   parses the analysis report file (analysis_report.csv).}
#'   \item{\code{parseAR(path = "ar_results.csv")}}{This method parses the 
#'   abnormal return file (ar_results.csv). Furthermore, it triggers 
#'   \code{parseReport} and join firm and index name.}
#'   \item{\code{parseCAR(path = "car_results.csv")}}{This method parses the 
#'   cumulative abnormal return file (ar_results.csv). Furthermore, it triggers 
#'   \code{parseReport} and join firm and index name.}
#' }
#'   
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#' \dontrun{
#' # Assume you already performed an Event Study and result files are saved in 
#' # the actual working directory.
#' estParser <- ResultParser$new()
#' 
#' # parse request file
#' estParser$parseRequestFile("01_RequestFile.csv")
#' 
#' # parse result files
#' estParser$parseReport("Analysis report.csv")
#' estParser$parseAR("AR results.csv")
#' estParser$parseAAR("AAR results.csv")
#' }
#' 
#' @export
ResultParser <- R6::R6Class(classname = "ResultParser",
                            public = list(
                              destDir        = NULL,
                              requestData    = NULL,
                              analysisReport = NULL,
                              arResults      = NULL,
                              carResults     = NULL,
                              aarResults     = NULL,
                              aarStatistics  = NULL,
                              groups         = NULL,
                              initialize = function() {
                              },
                              parseRequestFile = function(path = "01_RequestFile.csv") {
                                parseReturn <- private$parseFile(path, "requestData", header = F)
                                if (parseReturn) {
                                  # add groups
                                  groups <- unique(self$requestData$V5)
                                }
                                parseReturn
                              },
                              parseReport = function(path = "analysis_report.csv") {
                                private$parseFile(path, "analysisReport", T)
                                self$analysisReport <- self$analysisReport[-1, ]
                              },
                              parseAR = function(path = "ar_results.csv", analysisType = "AR") {
                                if (is.null(self$analysisReport)) {
                                  self$parseReport()
                                }
                                
                                parseReturn <- private$parseFile(path, "arResults", T)
                                if (nrow(aarResults) == 0) {
                                  message("Analysis performed, but no AR Results. Please look at comments in Analysis report.")
                                  return(NULL)
                                }
                                
                                if (!parseReturn) {
                                  return(NULL)
                                } else {
                                  abnormalReturns <- data.table::copy(self[["arResults"]])
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
                                idP <- which(names(self$analysisReport) == "p-value")
                                names(self$analysisReport)[idP] <- paste0("p-value", 1:length(idP))
                                
                                id <- which(names(self$analysisReport) %in% c("Event ID", "Firm", "Reference Market", "Estimation Window Length"))
                                self$analysisReport %>% 
                                  dplyr::select(id) -> arReport
                                
                                self$arResults %>%
                                  dplyr::left_join(arReport, by = "Event ID") -> self$arResults
                                
                                # if available add grouping
                                if (!is.null(self$requestData)) {
                                  requestData <- self$requestData[, c(1, 5)]
                                  names(requestData) <- c("Event ID", "Group")
                                  
                                  self$arResults %>% 
                                    dplyr::left_join(requestData, by = "Event ID") -> self$arResults
                                }
                              },
                              parseCAR = function(path = "car_results.csv", analysisType = "CAR") {
                                if (is.null(self$analysisReport))
                                  self$parseReport()
                                
                                # parse CAR values & check file
                                carResults <- data.table::fread(path)
                                if (nrow(carResults) == 0) {
                                  message("Analysis performed, but no CAR Results. Please look at comments in Analysis report.")
                                  return(NULL)
                                }
                                
                                self$analysisReport %>% 
                                  dplyr::select(`Event ID`, Firm) %>% 
                                  dplyr::right_join(carResults) -> carResults
                                
                                self$carResults <- carResults
                              },
                              parseAAR = function(path = "aar_results.csv", groups = NULL, analysisType = "AAR") {
                                if (is.null(self$analysisReport))
                                  self$parseReport()
                                
                                if (!is.null(self$groups))
                                  self$groups <- groups
                                
                                # parse AAR values & check file
                                aarResults <- data.table::fread(path)
                                if (nrow(aarResults) < 2) {
                                  message("Analysis performed, but no AAR Results. Please look at comments in Analysis report.")
                                  return(NULL)
                                }
                                
                                stringr::str_detect(names(aarResults), analysisType) %>%
                                  which() -> id
                                
                                aarResults %>% 
                                  reshape2::melt(id.vars    = 1, 
                                                 value.name = tolower(analysisType)) %>% 
                                  dplyr::rename(level     = `Grouping Variable/N`,
                                                eventTime = variable) -> aarResults
                                self$aarResults <- aarResults
                                
                                aarResults %>% 
                                  dplyr::mutate(eventTime = as.numeric(stringr::str_replace_all(as.character(eventTime), "[a-zA-Z()]", ""))) -> aarResults
                                
                                # get AAR, N, positive N; this information is 
                                # always in the result file
                                aarResults$level %>% 
                                  stringr::str_detect("Pos:Neg") %>% 
                                  which() -> idPos
                                idN <- idPos - 1
                                idAAR <- idN - 1
                                # AAR
                                aarFinal <- aarResults[idAAR, ]
                                aarFinal$aar <- as.numeric(aarFinal$aar)
                                # get N
                                aarFinal$N <- as.numeric(aarResults[idN, ]$aar)
                                # Parse positive
                                aarResults[idPos, ]$aar %>% 
                                  stringr::str_split(pattern = ":") %>% 
                                  purrr::map(.f = function(x) as.numeric(x[[1]])) %>% 
                                  unlist() -> aarFinal$Pos
                                
                                # get statistics
                                nStat <- idPos[2] - (idPos[1] + 1) - 2
                                statistics <- c()
                                if (nStat > 0) {
                                  for (i in 1:nStat) {
                                    idStat <- idPos + i
                                    dfStat <- aarResults[idStat, ]
                                    statistics <- c(statistics, dfStat$level[1])
                                    aarFinal[[paste0("stat", i)]] <- as.numeric(dfStat$aar)
                                  }
                                  names(statistics) <- paste0("stat", 1:nStat)
                                  self$aarStatistics <- statistics
                                }
                                self$aarResults <- aarFinal
                              },
                              calcAARCI = function(statistic = "Patell Z", 
                                                   p         = 0.95, 
                                                   twosided  = T, 
                                                   type      = "zStatistic") {
                                type <- match.arg(type, c("tStatistic", "zStatistic"))
                                if (twosided) {
                                  p <- 0.5 + p / 2
                                  if (type == "zStatistic") {
                                    zStar <- qnorm(p)
                                  } else {
                                    # TODO
                                  }
                                }
                                idStat <- which(self$aarStatistics == statistic)
                                lower <- NULL
                                upper <- NULL
                                if (length(idStat)) {
                                  statCol <- names(self$aarStatistics)[idStat]
                                  statValue <- self$aarResults[[statCol]]
                                  aar <- self$aarResults[["aar"]]
                                  lower <- aar - abs(aar) * zStar / abs(statValue)
                                  upper <- aar + abs(aar) * zStar / abs(statValue)
                                }
                                return(list(lower = lower,
                                            upper = upper))
                              },
                              cumSum = function(df, var = "aar", timeVar = NULL, cumVar = NULL, fun = cumsum) {
                                # calculate cumulative sum
                                df <- data.table::as.data.table(df)
                                data.table::setkeyv(df, c(cumVar, timeVar))
                                setnames(df, var, "car")
                                df[, car := fun(car), by = cumVar]
                                df[[var]] <- NULL
                                setnames(df, "car", var)
                                df
                              },
                              createReport = function(file = "EventStudy.xlsx") {
                                # the report file must have Excel filename extension
                                if (!stringr::str_detect(file, ".xlsx")) {
                                  file <- paste0(file, ".xlsx")
                                }
                                
                                wb <- openxlsx::createWorkbook()
                                
                                # Styles 
                                hs1 <- openxlsx::createStyle(fgFill = "#4F81BD", halign = "CENTER", textDecoration = "Bold",
                                                             border = "Bottom", fontColour = "white")
                                numStyle <- openxlsx::createStyle(numFmt = "0.00")
                                centreStyle <- openxlsx::createStyle(halign = "center", valign = "center")
                                intNumStyle <- openxlsx::createStyle(numFmt = "0")
                                options("openxlsx.numFmt" = "#,#0.00")
                                
                                # Analysis Report
                                openxlsx::addWorksheet(wb, sheetName = "Analysis Report")
                                openxlsx::writeData(wb, sheet = "Analysis Report", x = self$analysisReport, headerStyle = hs1)
                                openxlsx::setColWidths(wb, sheet = "Analysis Report", cols = 1:ncol(self$analysisReport), widths = 15)
                                
                                # Abnormal Return Report 
                                if (!is.null(self$arResults)) {
                                  self$arResults %>% 
                                    dplyr::select(-`Estimation Window Length`) -> dtData
                                  class(dtData$ar) <- "percentage"
                                  names(dtData)[2:4] <- c("Event Time", "AR", "t-Value")
                                  
                                  openxlsx::addWorksheet(wb, sheetName = "AR Report")
                                  openxlsx::writeData(wb, sheet = "AR Report", x =dtData, headerStyle = hs1)
                                  openxlsx::setColWidths(wb, sheet = 1, cols = ncol(dtData), widths = 15)
                                  
                                  wb <- private$setCenterStyle(wb    = wb, 
                                                               sheet = "AR Report",
                                                               rows  =  1:(nrow(dtData) + 1), 
                                                               cols  = 3:ncol(dtData))
                                  
                                  openxlsx::addStyle(wb,  "AR Report", 
                                                     style      = intNumStyle, 
                                                     rows       = 2:(nrow(dtData) + 1), 
                                                     cols       = 2, 
                                                     stack      = T, 
                                                     gridExpand = TRUE)
                                }
                                
                                
                                # CAR Report ----
                                if (!is.null(self$carResults)) {
                                  dtData <- self$carResults
                                  names(dtData)[4] <- "CAR"
                                  
                                  class(dtData$CAR) <- "percentage"
                                  
                                  openxlsx::addWorksheet(wb, sheetName = "CAR Report")
                                  openxlsx::writeData(wb, sheet = "CAR Report", x = dtData, headerStyle = hs1)
                                  
                                  wb <- private$setCenterStyle(wb    = wb, 
                                                               sheet = "CAR Report",
                                                               rows  =  1:(nrow(dtData) + 1), 
                                                               cols  = 3:ncol(dtData))
                                  
                                  openxlsx::addStyle(wb,  "CAR Report", 
                                                     style      = centreStyle, 
                                                     rows       = 1:(nrow(dtData) + 1), 
                                                     cols       = 3:ncol(dtData), 
                                                     stack      = T, 
                                                     gridExpand = TRUE)
                                }
                                
                                # Averaged Abnormal Return Report ----
                                if (!is.null(self$aarResults)) {
                                  dtData <- self$aarResults
                                  
                                  # Adjust column names
                                  statNames <- as.character(self$aarStatistics[names(self$aarResults)])
                                  statId <- which(!is.na(statNames))
                                  names(dtData)[statId] <- statNames[statId]
                                  names(dtData)[1:5] <- c("Group", "Event Time", "AAR", "N Firms", "N positive AR")
                                  
                                  # AAR as percentage
                                  class(dtData$AAR) <- "percentage"
                                  
                                  openxlsx::addWorksheet(wb, sheetName = "AAR Report")
                                  openxlsx::setColWidths(wb, sheet = "AAR Report", cols = 1:ncol(dtData), widths = 15)
                                  openxlsx::writeData(wb, sheet = "AAR Report", x = dtData, headerStyle = hs1)
                                  
                                  wb <- private$setCenterStyle(wb    = wb, 
                                                               sheet = "AAR Report",
                                                               rows  =  1:(nrow(dtData) + 1), 
                                                               cols  = 3:ncol(dtData))
                                  openxlsx::addStyle(wb,  "AAR Report", 
                                                     style      = intNumStyle, 
                                                     rows       = 2:(nrow(dtData) + 1), 
                                                     cols       = c(2, 4:5), 
                                                     stack      = T, 
                                                     gridExpand = TRUE)
                                }
                                
                                # Write Data ----
                                openxlsx::saveWorkbook(wb, file, overwrite = T)
                              }
                            ),
                            private = list(
                              setCenterStyle = function(wb, sheet, rows, cols) {
                                centreStyle <- openxlsx::createStyle(halign = "center", valign = "center")
                                openxlsx::addStyle(wb,  sheet, 
                                                   style = centreStyle, 
                                                   rows = rows, 
                                                   cols = cols, 
                                                   stack = T, 
                                                   gridExpand = TRUE)
                                wb
                              },
                              parseFile = function(path, dataName, header = F) {
                                # check local and url file
                                if (file.exists(path) || !httr::http_error(path)) {
                                  self[[dataName]] <- data.table::fread(path, header = header)
                                  TRUE
                                } else {
                                  message(paste0("File ", path, " not found!"))
                                  FALSE
                                }
                              }
                            ))
