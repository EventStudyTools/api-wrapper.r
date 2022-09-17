#' EventStudy
#'
#' This package provides functionality for doing Event Studies from R by using EventStudyTools.com API interface, parsing results, and visualize them.
#'
#' Start with the vignettes:
#' \code{browseVignettes(package = "EventStudy")}
#'
#' @name EventStudy
#' @docType package
NULL

#' @importFrom magrittr %>%
#' @importFrom stats ar
#' @importFrom stats as.formula
#' @importFrom stats na.omit
#' @import shiny
#' @import miniUI
#' @import rstudioapi
#' @import ggplot2
#' @importFrom data.table :=
#' @importFrom utils data
#' @importFrom rlang .data

#' @export
magrittr::`%>%`

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))