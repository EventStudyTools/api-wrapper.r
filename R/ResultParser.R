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
                              #' @field destDir Result dir.
                              destDir        = NULL,
                              #' @description Parse request file
                              #' 
                              #' @param path path to request file.
                              get_request_file = function(path = "01_RequestFile.csv") {
                                request_data = private$load_file(path, header = F)
                                colnames(request_data) = c("Event ID", "Firm", "Reference Market", "Event Date", "Group", "", "", "", "Estimation Window Length")
                                request_data
                              },
                              #' @description Parse request file
                              #' 
                              #' @param path path to request file.
                              get_analysis_report = function(path = "analysis_report.csv") {
                                analysis_report_tbl = private$load_file(path, T)
                                analysis_report_tbl %>% 
                                  dplyr::filter(!is.na(`Event ID`))
                              },
                              #' @description Parse request file
                              #' 
                              #' @param path path to request file.
                              #' @param analysis_report_tbl PArsed analysis report
                              #' @param request_tbl parsed request file
                              get_ar = function(path = "ar_results.csv", analysis_report_tbl=NULL, request_tbl=NULL) {
                                ar_result_tbl <- private$load_file(path, T)
                                if (nrow(ar_result_tbl) == 0) {
                                  stop("Analysis performed, but no AR Results. Please look at comments in Analysis report.")
                                }
                                # parse Abnormal Returns
                                ar_result_tbl %>% 
                                  dplyr::select("Event ID", dplyr::contains("AR")) %>% 
                                  tidyr::pivot_longer(cols = -"Event ID", 
                                                      names_to = "Day Relative to Event",
                                                      values_to = "AR") %>% 
                                  dplyr::mutate(`Day Relative to Event` = as.integer(stringr::str_replace_all(as.character(`Day Relative to Event`), "[a-zA-Z()]", ""))) -> ar_tbl
                                
                                # parse t-Values 
                                ar_result_tbl %>% 
                                  dplyr::select("Event ID", dplyr::contains("t-value")) %>% 
                                  tidyr::pivot_longer(cols      = -"Event ID", 
                                                      names_to  = "Day Relative to Event",
                                                      values_to = "t-value") %>% 
                                  dplyr::mutate(`Day Relative to Event` = stringr::str_replace_all(as.character(`Day Relative to Event`), "t-value", "")) %>% 
                                  dplyr::mutate(`Day Relative to Event` = as.integer(stringr::str_replace_all(as.character(`Day Relative to Event`), "[()]", ""))) -> t_val_tbl
                                
                                ar_tbl %>% 
                                  dplyr::left_join(t_val_tbl, by=c("Event ID", "Day Relative to Event")) -> ar_tbl
                                
                                # Add analysis report information
                                if (!is.null(analysis_report_tbl)) {
                                  analysis_report_tbl %>% 
                                    dplyr::select(c("Event ID", "Firm", "Reference Market", "Estimation Window Length")) -> report_tbl
                                  ar_tbl %>%
                                    dplyr::left_join(report_tbl, by = "Event ID") -> ar_tbl
                                }
                                
                                
                                # if available add grouping
                                if (!is.null(request_tbl)) {
                                  ar_tbl %>% 
                                    dplyr::left_join(request_tbl %>% dplyr::select("Event ID", "Group"), by = "Event ID") -> ar_tbl
                                }
                                ARResults$new(ar_tbl)
                              },
                              #' @description Parse Cumulative Abnormal Return
                              #'
                              #' @param path The path to the CAR result CSV file.
                              #' @param analysis_report_tbl The analyis report table. It will be used for extracting the group.
                              #' 
                              #' @export
                              get_car = function(path = "car_results.csv", analysis_report_tbl=NULL) {
                                # parse CAR values & check file
                                car_result_tbl = private$load_file(path, T)
                                if (nrow(car_result_tbl) == 0) {
                                  stop("Analysis performed, but no CAR Results. Please look at comments in Analysis report.")
                                }
                                
                                if (!is.null(analysis_report_tbl)) {
                                  analysis_report_tbl %>% 
                                    dplyr::select(c("Event ID", "Firm")) -> report_tbl
                                  car_result_tbl %>%
                                    dplyr::left_join(report_tbl, by = "Event ID") -> car_result_tbl
                                  CAResults$new(car_result_tbl)
                                } else {
                                  warning("Please add analysis report!")
                                }
                              },
                              #' @description Parse AAR results
                              #' 
                              #' @param path path to aar result file.
                              #' @param analysis_report Extracted analysis report
                              get_aar = function(path = "aar_results.csv", analysis_report=NULL) {
                                # parse AAR values & check file
                                aar_tbl = private$load_file(path, F)
                                if (nrow(aar_tbl) < 2) {
                                  stop("Analysis performed, but no AAR Results. Please look at comments in Analysis report.")
                                }
                                test_statistics <- getOption("EventStudy.test_statistics")
                                
                                aar_tbl %>% 
                                  dplyr::filter(V2 == "Day Relative to Event") %>% 
                                  as.character() -> col_names
                                
                                
                                # Parse Statistics
                                aar_tbl %>% 
                                  dplyr::filter(V2 %in% test_statistics) -> test_statistics_tbl
                                colnames(test_statistics_tbl) = c("Group", "Test Statistic", col_names[3:length(col_names)])
                                
                                test_statistics_tbl %>% 
                                  tidyr::pivot_longer(col_names[3:length(col_names)],
                                                      names_to = c("Day Relative to Event"),
                                                      values_to = "Statistics") -> test_statistics_tbl
                                
                                # Parse P Values
                                aar_tbl %>% 
                                  dplyr::filter(V2 %in% paste(test_statistics, "P-Values")) -> p_values_tbl
                                colnames(p_values_tbl) = c("Group", "Test Statistic", col_names[3:length(col_names)])
                                
                                p_values_tbl %>% 
                                  tidyr::pivot_longer(col_names[3:length(col_names)],
                                                      names_to = c("Day Relative to Event"),
                                                      values_to = "P Values") -> p_values_tbl
                                p_values_tbl %>% 
                                  dplyr::mutate(`Test Statistic` = stringr::str_replace(`Test Statistic`, " P-Values", "")) -> p_values_tbl
                                
                                # Merge Statistics & P Values
                                test_statistics_tbl %>% 
                                  dplyr::left_join(p_values_tbl, by = c("Group", "Test Statistic", "Day Relative to Event")) -> test_statistics_tbl
                                
                                # Parse AAR values
                                aar_tbl %>% 
                                  dplyr::filter(V2 %in% c("Average Abnormal Return (AAR)")) -> aar_values_tbl
                                colnames(aar_values_tbl) = c("Group", "Test Statistic", col_names[3:length(col_names)])
                                
                                aar_values_tbl %>% 
                                  tidyr::pivot_longer(c(col_names[3:length(col_names)]),
                                                      names_to = c("Day Relative to Event"),
                                                      values_to = "AAR") %>% 
                                  dplyr::select(-"Test Statistic") -> aar_values_tbl
                                
                                # Parse number stocks
                                aar_tbl %>% 
                                  dplyr::filter(V2 %in% "N") -> n_values_tbl
                                colnames(n_values_tbl) = c("Group", "Test Statistic", col_names[3:length(col_names)])
                                n_values_tbl %>% 
                                  tidyr::pivot_longer(c(col_names[3:length(col_names)]),
                                                      names_to = c("Day Relative to Event"),
                                                      values_to = "N") %>% 
                                  dplyr::select(-"Test Statistic") -> n_values_tbl
                                
                                # Parse Pos:Neg
                                aar_tbl %>% 
                                  dplyr::filter(V2 %in% "Pos:Neg") -> pos_neg_values_tbl
                                colnames(pos_neg_values_tbl) = c("Group", "Test Statistic", col_names[3:length(col_names)])
                                pos_neg_values_tbl %>% 
                                  tidyr::pivot_longer(c(col_names[3:length(col_names)]),
                                                      names_to = c("Day Relative to Event"),
                                                      values_to = "Pos:Neg") %>% 
                                  dplyr::select(-"Test Statistic") -> pos_neg_values_tbl
                                
                                aar_values_tbl %>% 
                                  dplyr::left_join(n_values_tbl, by = c("Group", "Day Relative to Event")) %>% 
                                  dplyr::left_join(pos_neg_values_tbl, by = c("Group", "Day Relative to Event")) %>% 
                                  tidyr::separate(`Pos:Neg`, c("Pos", "Neg"), sep=":") -> aar_values_tbl
                                
                                aar_values_tbl %>% 
                                  dplyr::mutate(
                                    `Day Relative to Event` = as.numeric(`Day Relative to Event`),
                                    AAR = as.numeric(AAR),
                                    N = as.numeric(N),
                                    Pos = as.numeric(Pos),
                                    Neg = as.numeric(Neg)
                                  ) -> aar_values_tbl
                                
                                test_statistics_tbl %>% 
                                  dplyr::mutate(
                                    `Day Relative to Event` = as.numeric(`Day Relative to Event`),
                                    Statistics = as.numeric(Statistics),
                                    `P Values` = as.numeric(`P Values`)
                                  ) -> test_statistics_tbl
                                
                                # Return result object
                                AARResults$new(aar_tbl        = aar_values_tbl, 
                                               statistics_tbl = test_statistics_tbl)
                              },
                              #' @description Parse caar results
                              #' 
                              #' @param path path to caar result file.
                              get_caar = function(path = "caar_results.csv") {
                                # parse AAR values & check file
                                caar_result_tbl <- data.table::fread(path)
                                g_names <- c("Grouping Variable", "CAAR Type", "CAAR Value", "Precision Weighted CAAR Value", "ABHAR", "pos:neg CAR", "Number of CARs considered")
                                caar_result_tbl %>% 
                                  dplyr::select(g_names) -> caar_values_tbl
                                
                                var_names <- setdiff(names(caar_result_tbl), g_names)
                                s_names <- c("Grouping Variable", "CAAR Type", var_names)
                                
                                caar_result_tbl %>% 
                                  dplyr::select(s_names) %>% 
                                  tidyr::pivot_longer(cols=var_names) %>% 
                                  dplyr::mutate(type = ifelse(stringr::str_detect(name, "P-Value"), "P-Value", "Statistics")) %>% 
                                  dplyr::mutate(name = stringr::str_replace(name, " P-Value", "")) %>% 
                                  tidyr::pivot_wider(id_cols=c("Grouping Variable", "CAAR Type", "name"), 
                                                     names_from = "type", values_from = "value") -> caar_statistics_tbl
                                
                                CAAResults$new(caar_values_tbl, caar_statistics_tbl)
                              }
                            ),
                            private = list(
                              load_file = function(path, header = F) {
                                # check local and url file
                                if (file.exists(path) || !httr::http_error(path)) {
                                  data.table::fread(path, header = header)
                                } else {
                                  stop(paste0("File ", path, " not found!"))
                                }
                              }
                            ))
