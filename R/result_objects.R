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
#' @name aar_results
#' 
#' @title An R6 object that contains AAR results.
#' 
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{plot}}{This method plots aar results.}
#' }
#'   
#' @format \code{\link[R6]{R6Class}} object.
#' @export
AARResults <- R6::R6Class(classname="AARResults",
                          public = list(
                            #' @field aar_tbl AAR results.
                            aar_tbl = NULL,
                            #' @field statistics_tbl AAR test statistic results.
                            statistics_tbl = NULL,
                            #' @description Class initialization
                            #' 
                            #' @param aar_tbl AAR result table.
                            #' @param statistics_tbl Table with statistics.
                            initialize = function(aar_tbl, statistics_tbl) {
                              self$aar_tbl <- aar_tbl
                              self$statistics_tbl <- statistics_tbl
                            },
                            #' @description Print key characteristics.
                            print = function() {
                              cat("AAR Results\n")
                              cat("-----------\n")
                              cat(paste0("N Groups: ", length(unique(self$aar_tbl$Group)), "\n"))
                              cat(paste0("Groups: ", paste0(unique(self$aar_tbl$Group), collapse = ", "), "\n"))
                              cat(paste0("Length Event Window: ", length(unique(self$aar_tbl$`Day Relative to Event`)), "\n"))
                              cat(paste0("Event Window: ", paste0(unique(self$aar_tbl$`Day Relative to Event`), collapse = ", "), "\n"))
                              cat(paste0("Test Statistics: ", paste0(unique(self$statistics_tbl$`Test Statistic`), collapse = ", "), "\n"))
                            },
                            #' @description Plots AAR results for each analysis group.
                            #'
                            #' @param group Subset to your analysed groups, else all groups will be plotted.
                            #' @param ci_statistics Statistic used for confidence intervals
                            #' @param p The desired p-value
                            #' @param ci_type type of CI band for ggplot2, available are band or ribbon. 
                            #' @param xlab x-axis label
                            #' @param ylab y-axis label
                            #' @param facet should each group get its own plot (default = T)
                            #' @param ncol number of facet columns
                            plot = function(group         = NULL, 
                                            ci_statistics = NULL, 
                                            p             = .95, 
                                            ci_type       = "two-sided",
                                            xlab          = "Event Window", 
                                            ylab          = "Averaged Abnormal Returns", 
                                            facet         = T, 
                                            ncol          = 4) {
                              library(RColorBrewer)
                              
                              ci_type <- rlang::arg_match(ci_type, c("two-sided", "lower", "upper"))
                              if (is.null(ci_statistics)) {
                                ci_statistics = self$statistics_tbl['Test Statistic'][[1]][1]
                              }
                              ci_statistics <- rlang::arg_match(ci_statistics, unique(self$statistics_tbl['Test Statistic'] %>% .[[1]]))
                              testthat::expect_true(p < 1, "p must be smaller than 1.")
                              ci_stat_tbl = self$confidence_interval(ci_statistics, p, ci_type)
                              
                              # CRAN check
                              level <- eventTime <- lower <- upper <- NULL
                              pal <- RColorBrewer::brewer.pal(3, "Blues")
                              
                              aar_plt = self$aar_tbl %>% 
                                dplyr::mutate(`Day Relative to Event` = as.numeric(`Day Relative to Event`)) %>% 
                                ggplot() +
                                geom_hline(yintercept = 0, color = "black", alpha = .5) +
                                geom_vline(xintercept = 0, color = "black", linetype = 2, alpha = .5) +
                                geom_line(aes(x = `Day Relative to Event`, y = AAR), color = pal[3], size=1) + 
                                scale_y_continuous(labels = scales::percent) +
                                xlab(xlab) +
                                ylab(ylab)
                              
                              if (nrow(ci_stat_tbl)) {
                                aar_plt <- aar_plt +
                                  geom_ribbon(data=ci_stat_tbl, aes(x = `Day Relative to Event`, ymin = lower, ymax = upper), fill = pal[3], alpha = .25)
                              }
                              
                              # facet wrap
                              if (facet) {
                                aar_plt <- aar_plt +
                                  facet_wrap( ~ Group, ncol = ncol, scales = "free")
                              }
                              aar_plt
                            },
                            #' @description Plot Cumulative Abnormal Return. No test statistic is available.
                            #' 
                            #' @param xlab x axis lab
                            #' @param ylab y axis lab
                            #' @param facet Shall the plot faceted by Group
                            #' @param ncol Number of cols when faceting.
                            #' 
                            #' @export
                            plot_cumulative = function(xlab  = "Event Window", 
                                                       ylab  = "Cumulative Averaged Abnormal Returns", 
                                                       facet = T, 
                                                       ncol  = 4) {
                              pal <- RColorBrewer::brewer.pal(3, "Blues")
                              
                              caar_plt = self$aar_tbl %>%
                                dplyr::group_by(Group) %>% 
                                dplyr::mutate(CAAR = cumsum(AAR), .groups = "drop") %>% 
                                dplyr::mutate(`Day Relative to Event` = as.numeric(`Day Relative to Event`)) %>% 
                                ggplot() +
                                geom_hline(yintercept = 0, color = "black", alpha = .5) +
                                geom_vline(xintercept = 0, color = "black", linetype = 2, alpha = .5) +
                                geom_line(aes(x = `Day Relative to Event`, y = CAAR), color = pal[3], size=1) + 
                                scale_y_continuous(labels = scales::percent) +
                                xlab(xlab) +
                                ylab(ylab) 
                              
                              # facet wrap
                              if (facet) {
                                caar_plt <- caar_plt +
                                  facet_wrap( ~ Group, ncol = ncol, scales = "free")
                              }
                              caar_plt
                            },
                            #' @description Calculates Confidence band for given test statistic.
                            #' 
                            #' @param statistic Chosen test statistics for calculation.
                            #' @param p Chosen p value.
                            #' @param ci_type Type of confidence interval.
                            confidence_interval = function(statistic = "Patell Z", 
                                                           p         = 0.95, 
                                                           ci_type   = "two-sided") {
                              if (ci_type == 'two-sided') {
                                p <- 0.5 + p / 2
                                if (stringr::str_ends(statistic, "Z")) {
                                  zStar <- qnorm(p)
                                } else if(stringr::str_ends(statistic, "T")) {
                                  zStar <- qt(p, unique(aar_result$aar_tbl$N))
                                }
                              }
                              self$statistics_tbl %>% 
                                dplyr::filter(`Test Statistic` == statistic) -> statistics_tbl
                              if (length(statistics_tbl)) {
                                aar <- as.numeric(self$aar_tbl[["AAR"]])
                                statistics_tbl[['lower']] <- aar - abs(aar) * zStar / abs(as.numeric(statistics_tbl$Statistics))
                                statistics_tbl[['upper']] <- aar + abs(aar) * zStar / abs(as.numeric(statistics_tbl$Statistics))
                                
                                statistics_tbl[["is_significant"]] = "Not Significant"
                                statistics_tbl %>% 
                                  dplyr::mutate(
                                    is_significant = ifelse(lower > 0, "Significant", is_significant),
                                    is_significant = ifelse(upper < 0, "Significant", is_significant),
                                  ) -> statistics_tbl
                              }
                              statistics_tbl
                            },
                            #' @description Plots a heatmap with test statistics on y axis and Day Relative to Event on x axis. Colorization is done according to significance according to given p.
                            #' 
                            #' @param p Chosen p value.
                            #' @param ci_type CI type.
                            #' 
                            #' @export
                            plot_test_statistics = function(p       = 0.95, 
                                                            ci_type = "two-sided") {
                              self$statistics_tbl$`Test Statistic` %>% 
                                unique() %>% 
                                purrr::map(.f=function(x) {
                                  self$confidence_interval(statistic = x, p = p, ci_type = ci_type)
                                }) %>% 
                                purrr::reduce(rbind) %>% 
                                dplyr::select(Group, `Test Statistic`, `Day Relative to Event`, `P Values`, is_significant) %>% 
                                ggplot() +
                                geom_tile(aes(x=`Day Relative to Event`, y=`Test Statistic`, fill=is_significant), color="white", size=1) +
                                facet_wrap( ~ Group, ncol = 4, scales = "free") +
                                scale_fill_manual(name=NULL, values = c("steelblue", "darkred")) +
                                theme(legend.position = "top")
                            }
                          )
)

#' @name aar_results
#' 
#' @title An R6 object that contains AAR results.
#' 
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{plot}}{This method plots aar results.}
#' }
#'   
#' @format \code{\link[R6]{R6Class}} object.
#' @export
ARResults <- R6::R6Class(classname="ARResults",
                         public = list(
                           #' @field ar_tbl AR result table.
                           ar_tbl = NULL,
                           #' Class initialization
                           #' 
                           #' @param ar_tbl AR result table.
                           initialize = function(ar_tbl) {
                             self$ar_tbl <- ar_tbl
                           },
                           #' @description  Print key characteristics.
                           print = function() {
                             cat("AR Results\n")
                             cat("-----------\n")
                             cat(paste0("N Groups: ", length(unique(self$ar_tbl$Group)), "\n"))
                             cat(paste0("Groups: ", paste0(unique(self$ar_tbl$Group), collapse = ", "), "\n"))
                             cat(paste0("Length Event Window: ", length(unique(self$ar_tbl$`Day Relative to Event`)), "\n"))
                             cat(paste0("Event Window: ", paste0(unique(self$ar_tbl$`Day Relative to Event`), collapse = ", "), "\n"))
                           },
                           #' @description Plot abnormal returns in the event window of single or multiple
                           #' firms. 
                           #'
                           #' @param firm set this parameter if just a subset of firms should be plotted
                           #' @param xlab x-axis label of the plot
                           #' @param ylab y-axis label
                           #' @param addAAR add aar line
                           #' 
                           #' @return a ggplot2 object
                           plot = function(firm = NULL, 
                                           xlab = "", 
                                           ylab = "Abnormal Returns", 
                                           addAAR = F) {
                             # CRAN check
                             Firm <- eventTime <- y <- NULL
                             
                             ar_tbl <- self$ar_tbl
                             if (!is.null(firm)) {
                               ar_tbl %>% 
                                 dplyr::filter(Firm == firm) -> ar_tbl
                             }
                             
                             pal <- RColorBrewer::brewer.pal(3, "Blues")
                             
                             ar_plt = ar_tbl %>% 
                               ggplot() +
                               geom_hline(yintercept = 0, color = "black", alpha = .5) +
                               geom_vline(xintercept = 0, color = "black", linetype = 2, alpha = .5) +
                               geom_line(aes(x = `Day Relative to Event`, 
                                             y = AR, 
                                             group = Firm), 
                                         color = pal[3], alpha = 0.5) + 
                               scale_y_continuous(labels = scales::percent) +
                               xlab(xlab) +
                               ylab(ylab)
                             
                             if (addAAR) {
                               data.table::setnames(ar_tbl, "AR", "y")
                               ar_tbl %>% 
                                 dplyr::group_by(`Day Relative to Event`) %>% 
                                 dplyr::summarise(y = mean(y, na.rm = T)) -> aar_tbl
                               data.table::setnames(ar_tbl, "y", "AR")
                               ar_plt <- ar_plt +
                                 geom_line(data = aar_tbl, aes(x = `Day Relative to Event`, y = y), color = "black")
                             }
                             ar_plt
                           }
                         )
)

#' @name aar_results
#' 
#' @title An R6 object that contains AAR results.
#' 
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{plot}}{This method plots aar results.}
#' }
#'   
#' @format \code{\link[R6]{R6Class}} object.
#' @export
CAResults <- R6::R6Class(classname="CAResults",
                         public = list(
                           #' @field car_tbl Car result table
                           car_tbl = NULL,
                           #' Class initialization
                           #' 
                           #' @param car_tbl CAR result table.
                           initialize = function(car_tbl) {
                             self$car_tbl <- car_tbl
                           },
                           #' @description  Print key characteristics.
                           print = function() {
                             cat("CAR Results\n")
                             cat("-----------\n")
                             cat(paste0("N Groups: ", length(unique(self$aar_tbl$Group)), "\n"))
                             cat(paste0("Groups: ", paste0(unique(self$aar_tbl$Group), collapse = ", "), "\n"))
                             cat(paste0("Length Event Window: ", length(unique(self$aar_tbl$`Day Relative to Event`)), "\n"))
                             cat(paste0("Event Window: ", paste0(unique(self$aar_tbl$`Day Relative to Event`), collapse = ", "), "\n"))
                           }
                         )
)

#' @name CAAR Results.
#' 
#' @title An R6 object that contains CAAR results.
#' 
#' @format \code{\link[R6]{R6Class}} object.
#' @export
CAAResults <- R6::R6Class(classname="CAAResults",
                         public = list(
                           #' @field caar_tbl CAAR results.
                           caar_tbl = NULL,
                           #' @field statistics_tbl CAAR test statistic results.
                           statistics_tbl = NULL,
                           #' Class initialization
                           #' 
                           #' @param caar_tbl CAAR result table.
                           #' @param statistics_tbl Table with statistics.
                           initialize = function(caar_tbl, statistics_tbl) {
                             self$caar_tbl <- caar_tbl
                             self$statistics_tbl <- statistics_tbl
                           },
                           #' @description  Print key characteristics.
                           print = function() {
                             cat("CAAR Results\n")
                             cat("-----------\n")
                             cat(paste0("N Groups: ", length(unique(self$caar_tbl$`Grouping Variable`)), "\n"))
                             cat(paste0("Groups: ", paste0(unique(self$caar_tbl$`Grouping Variable`), collapse = ", "), "\n"))
                           }
                         )
)