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
#' @title A parser for Event Study result files
#' 
#' @description 
#' This result file parser works at the moment just for csv files. Please read
#' the vignette for further details.
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
#'   \item{\code{plotAR(id = NULL)}}{This method abnormal returns time series 
#'   with \code{id} as the firm id.}}
#'   
#' @section Arguments:
#' 
#' 
#' @section Class Members:
#' 
#'   
#' @format \code{\link{R6Class}} object.
#' 
#' @export
ResultParser <- R6::R6Class(classname = "ResultParser",
                            public = list(
                              destDir        = NULL,
                              requestData    = NULL,
                              analysisReport = NULL,
                              arResults      = NULL,
                              aarResults     = NULL,
                              aarStatistics  = NULL,
                              groups         = NULL,
                              initialize = function() {
                              },
                              parseRequestFile = function(path = "01_RequestFile.csv") {
                                parseReturn <- private$parseFile(path, "requestData")
                                if (parseReturn) {
                                  # add groups
                                  groups <- unique(self$requestData$V5)
                                }
                                parseReturn
                              },
                              parseReport = function(path = "analysis_report.csv") {
                                private$parseFile(path, "analysisReport")
                              },
                              parseAR = function(path = "ar_results.csv", analysisType = "AR") {
                                if (is.null(self$analysisReport)) {
                                  self$parseReport()
                                }
                                
                                parseReturn <- private$parseFile(path, "arResults")
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
                                id <- which(names(self$analysisReport) %in% c("Event ID", "Firm", "Reference Market", "Estimation Window Length"))
                                arReport <- self$analysisReport[, id]
                                
                                self$arResults %>%
                                  dplyr::left_join(arReport, by = "Event ID") -> self$arResults
                                
                                # if available add grouping
                                if (!is.null(self$requestData)) {
                                  self$requestData %>% 
                                    dplyr::select(1, 5) -> requestData
                                  names(requestData) <- c("Event ID", "Group")
                                  
                                  self$arResults %>% 
                                    dplyr::left_join(requestData, by = "Event ID") -> self$arResults
                                }
                              },
                              parseAAR = function(path = "aar_results.csv", groups = NULL) {
                                if (is.null(self$analysisReport))
                                  self$parseReport()
                                
                                if (!is.null(self$groups))
                                  self$groups <- groups
                                
                                # parse AAR values
                                aarResults <- data.table::fread(path)
                                stringr::str_detect(names(aarResults), "AAR") %>%
                                  which() -> id
                                
                                aarResults %>% 
                                  reshape2::melt(id.vars    = 1, 
                                                 value.name = "aar") %>% 
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
                              }
                            ),
                            private = list(
                              parseFile = function(path, dataName) {
                                if (file.exists(path)) {
                                  self[[dataName]] <- data.table::fread(path, header = F)
                                  TRUE
                                } else {
                                  message(paste0("File ", path, " not found!"))
                                  FALSE
                                }
                              }
                            ))
