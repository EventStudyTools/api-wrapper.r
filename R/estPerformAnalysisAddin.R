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
#' @name estPerformAnalysisAddin
#' 
#' @title RStudio Addin for performing an Event Study
#'
#' @description 
#' Call this as an addin to replace occurrences of a particular sequence of
#' characters in a document with a new sequence of characters.
#'
#' @export
estPerformAnalysisAddin <- function() {
  
  ui <- miniPage(
    gadgetTitleBar("Return Event Study", right = miniTitleBarButton("performAnalysis", "Perform Analysis", primary = TRUE)),
    miniTabstripPanel(
      miniTabPanel("Parameters", icon = icon("sliders"),
                   miniContentPanel(
                     uiOutput("parameters_ui")
                   )
      ),
      miniTabPanel("Statistics", icon = icon("linode"),
                   miniContentPanel(
                     uiOutput("statistics_ui") 
                   )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    context <- rstudioapi::getActiveDocumentContext()
    original <- context$contents
    
    output$parameters_ui <- renderUI({
      fillCol(flex = c(NA, NA, 1),
              tagList(
                fluidRow(style = "margin: 5px 5px 5px 5px",
                         h4("API Parameters"),
                         column(4,
                                textInput("apiUrl", "API URL", value = "www.api.eventstudytools.com", width = "100%")
                         ),
                         column(4,
                                textInput("apiKey", "API Key", value = "", placeholder = "Please add your API key", width = "100%")
                         ),
                         column(2, style = "margin-top: 23px;",
                                actionButton(inputId = "connect", label = "Connect")       
                         ),
                         column(2,
                                htmlOutput("connectResponse")
                         )
                ),
                hr()
              ),
              tagList(
                fluidRow(style = "margin: 5px 5px 5px 5px",
                         column(2, h5("Result File Format"),
                                radioButtons(inputId = "resultFileFormat", 
                                             label   = NA, 
                                             choices = c("CSV"  = "csv", 
                                                         "XLS"  = "xls", 
                                                         "XLSX" = "xlsx", 
                                                         "ODS"  = "ods"))),
                         column(4, h5("Benchmark Model"),
                                radioButtons(inputId = "benchmarkModel", 
                                             label   = NA, 
                                             choices = c("Market Model"                        = "mm", 
                                                         "Scholes/Williams Model"              = "XLS", 
                                                         "Market Adjusted"                     = "XLSX", 
                                                         "Comparison Period Mean Adjusted"     = "ods",
                                                         "Fama-French 3 Factor Model"          = "d",
                                                         "Fama-French-Momentum 4 Factor Model" = "dfd",
                                                         "GARCH"                               = "garch", 
                                                         "EGARCH"                              = "egarch"))),
                         column(2, h5("Return Type"),
                                radioButtons(inputId = "returnType", 
                                             label   = NA, 
                                             choices = c("Simple" = "simple", 
                                                         "Log"    = "log"))),
                         column(4, h5("Adjustment Rule for Non-Trading Days"),
                                radioButtons(inputId = "adjustmentNonTradingDays", 
                                             label   = NA, 
                                             choices = c("Take earlier trading day"     = "csv", 
                                                         "Take later trading day"       = "XLS", 
                                                         "Keep non-trading day"         = "XLSX", 
                                                         "Skip respective observations" = "ods")))
                ), hr()),
              fluidRow(style = "margin: 5px 5px 5px 5px",
                       h4("Files"),
                       column(4, 
                              fileInput("requestFile", "Request File", width = "100%")
                       ),
                       column(4, 
                              fileInput("dataFile", "Firm Data", width = "100%")
                       ),
                       column(4,
                              fileInput("marketFile", "Market Data", width = "100%")
                       )
              )
      )
    })

    
    output$statistics_ui <- renderUI({
      fillCol(flex = c(NA, NA, 1),
              tagList(
                fluidRow(style = "margin: 5px 5px 5px 5px",
                         column(2, h5("Result File Format"),
                                radioButtons(inputId = "resultFileFormat", 
                                             label   = NA, 
                                             choices = c("CSV" = "csv", "XLS" = "XLS", "XLSX" = "XLSX", "ODS" = "ods"))),
                         column(4, h5("Benchmark Model"),
                                radioButtons(inputId = "benchmarkModel", 
                                             label   = NA, 
                                             choices = c("Market Model" = "mm", 
                                                         "Scholes/Williams Model" = "XLS", 
                                                         "Market Adjusted" = "XLSX", 
                                                         "Comparison Period Mean Adjusted" = "ods",
                                                         "Fama-French 3 Factor Model" = "d",
                                                         "Fama-French-Momentum 4 Factor Model" = "dfd",
                                                         "GARCH" = "garch", 
                                                         "EGARCH" = "egarch"))),
                         column(2, h5("Return Type"),
                                radioButtons(inputId = "returnType", 
                                             label   = NA, 
                                             choices = c("Simple" = "simple", "Log" = "log"))),
                         column(4, h5("Adjustment Rule for Non-Trading Days"),
                                radioButtons(inputId = "adjustmentNonTradingDays", 
                                             label   = NA, 
                                             choices = c("Take earlier trading day" = "csv", 
                                                         "Take later trading day" = "XLS", 
                                                         "Keep non-trading day" = "XLSX", 
                                                         "Skip respective observations" = "ods")))
                ))
      )
    })
        
    estAPI <- NULL
    observeEvent(input$connect, {
      apiKey <- input$apiKey
      apiUrl <- input$apiUrl
      
      if(!is.null(apiKey) || apiKey != "") {
        output$connectResponse <- renderUI({
          HTML("Please add valid API key")
        })
      }
      
      if(!is.null(apiUrl) || apiUrl != "") {
        output$connectResponse <- renderUI({
          HTML("Please add valid API url")
        })
      }
      browser()
      
      estAPI <<- EventStudyAPI$new(apiUrl)
      estAPI$authentication(apiKey)
    })
  }
  
  viewer <- dialogViewer("Event Study", width = 1200, height = 800)
  runGadget(ui, server, viewer = viewer)
}

performEventStudy <- function(contents, from, to, useWordBoundaries = TRUE) {
  
  reFrom <- if (useWordBoundaries)
    paste("\\b", from, "\\b", sep = "")
  else
    from
  
  reTo <- to
  matches <- gregexpr(reFrom, contents, perl = TRUE)
  changes <- sum(unlist(lapply(matches, function(x) {
    if (x[[1]] == -1) 0 else length(x)
  })))
  
  refactored <- unlist(lapply(contents, function(x) {
    gsub(reFrom, reTo, x, perl = TRUE)
  }))
  
  list(
    refactored = refactored,
    changes = changes
  )
}