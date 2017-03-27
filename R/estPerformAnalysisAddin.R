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
    gadgetTitleBar(title = "Return Event Study", 
                   right = miniTitleBarButton("performAnalysis", "Perform Analysis", primary = TRUE)),
    miniTabstripPanel(
      miniTabPanel("Parameters", icon = icon("sliders"),
                   miniContentPanel(
                     uiOutput("parameters_ui")
                   )
      ),
      miniTabPanel("Statistics", icon = icon("linode"),
                   miniContentPanel(
                     fluidRow(style = "margin: 5px 5px 5px 5px",
                              uiOutput("statistics_ui")
                     )
                   )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    context <- rstudioapi::getActiveDocumentContext()
    original <- context$contents
    
    
    # Statistics
    StatisticsNames <- c("Csect T", "Patell Z", "Adjusted Patell Z", 
                         "StdCSect Z", "Adjusted StdCSect Z", 
                         "Skewness Corrected T", "Rank Z", "Generalized Rank T", 
                         "Generalized Rank Z", "Generalized Sign Z")
    
    statisticID <- list(
      c("art", "cart", "aart", "caart", "abhart"),
      c(NA, NA, "aarptlz", "caarptlz", NA),
      c(NA, NA, "aaraptlz", "caaraptlz", NA),
      c(NA, NA, "aarbmpz", "caarbmpz", NA),
      c(NA, NA, "aarabmpz", "caarabmpz", NA),
      c(NA, NA, "aarskewadjt", "caarskewadjt", "abharskewadjt"),
      c(NA, NA, "aarrankz", "caarrankz", NA),
      c(NA, NA, "aargrankt", "caargrankt", NA),
      c(NA, NA, "aargsignz", "caargsignz", NA),
      c(NA, NA, "aargsignz", "caargsignz", NA)
    )
    
    output$parameters_ui <- renderUI({
      fillCol(flex = c(NA, NA, 1),
              tagList(
                fluidRow(style = "margin: 5px 5px 5px 5px",
                         h4("API Parameters"),
                         column(4,
                                textInput("apiUrl", "API URL", value = "http://api.dev.eventstudytools.com", width = "100%")
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
                tags$hr(style = "margin-top: 0px; margin-bottom: 0px")
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
                                                         "Scholes/Williams Model"              = "mm-sw", 
                                                         "Market Adjusted"                     = "mam", 
                                                         "Comparison Period Mean Adjusted"     = "cpmam",
                                                         "Fama-French 3 Factor Model"          = "ff3fm",
                                                         "Fama-French-Momentum 4 Factor Model" = "ffm4fm",
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
                                                         "Take later trading day"       = "later", 
                                                         "Keep non-trading day"         = "XLSX", 
                                                         "Skip respective observations" = "ods")))
                ), tags$hr(style = "margin-top: 0px; margin-bottom: 0px")),
              fluidRow(style = "margin: 5px 5px 5px 5px",
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
      ui <- tagList(generateStatisticColumn(T))
      n <- length(StatisticsNames)
      for (i in 1:n) {
        ui <- shiny::tagAppendChild(ui,
                                    generateStatisticColumn(header        = F, 
                                                            statisticName = StatisticsNames[i],
                                                            statisticsID  = statisticID[[i]])
        )
      }
      
      ui
    })
    
    observe({
      print(input$art)
    })
    
    estAPI <- NULL
    observeEvent(input$connect, {
      apiKey <- input$apiKey
      apiUrl <- input$apiUrl
      
      validKey <- ifelse(is.null(apiKey) || apiKey == "", F, T)
      validUrl <- ifelse(is.null(apiUrl) || apiUrl == "", F, T)
      
      if(!validUrl) {
        output$connectResponse <- renderUI({
          HTML("Please add valid API url")
        })
      } else {
        estAPI <<- EventStudy::EventStudyAPI$new(apiUrl)
        if(!validKey) {
          output$connectResponse <- renderUI({
            return(HTML("Please add valid API key"))
          })
        } else {
          authSuccess <- estAPI$authentication(apiKey)
          output$connectResponse <- renderUI({
            if (authSuccess) {
              HTML("Authentication was successful")
            } else {
              HTML("Authentication was not successful")
            }
            
          })
        }
      }
    })
  }
  
  
  viewer <- dialogViewer("Event Study", width = 1200, height = 800)
  runGadget(ui, server, viewer = viewer)
}

generateStatisticColumn <- function(header = T, statisticName = "", statisticsID = rep(NA, 5)) {
  if (header) {
    ret <- tagList(fluidRow(style = "margin: 0px 0px 0px 0px",
                            column(2, NULL),
                            column(2, h6("AR")),
                            column(2, h6("CAR")),
                            column(2, h6("AAR")),
                            column(2, h6("CAAR")),
                            column(2, h6("ABHAR"))
    ),
    tags$hr(style = "margin-top: 0px; margin-bottom: 0px")
    )
  } else {
    ret <- tagList(
      fluidRow(style = "margin: 0px 0px 0px 0px;",
               div(class = "col-sm-2", style = "margin-top: 0px; margin-bottom: 0px; max-height: 35px !important;", h6(statisticName)),
               div(class = "col-sm-2", style = "margin-top: 0px; margin-bottom: 0px", ifelse(is.na(statisticsID[1]), 
                                                                                             "", 
                                                                                             shiny::tagList(shiny::checkboxInput(inputId = statisticsID[1], 
                                                                                                                                 label   = NULL,
                                                                                                                                 value   = T, 
                                                                                                                                 width   = "100%")))),
               div(class = "col-sm-2", style = "margin-top: 0px; margin-bottom: 0px; max-height: 35px !important;", ifelse(is.na(statisticsID[2]), 
                                                                                             "", 
                                                                                             shiny::tagList(shiny::checkboxInput(inputId = statisticsID[2], 
                                                                                                                                 label   = NULL,
                                                                                                                                 value   = T, 
                                                                                                                                 width   = "100%")))),
               div(class = "col-sm-2", style = "margin-top: 0px; margin-bottom: 0px; max-height: 35px !important;", ifelse(is.na(statisticsID[3]), 
                                                                                             "", 
                                                                                             shiny::tagList(shiny::checkboxInput(inputId = statisticsID[3], 
                                                                                                                                 label   = NULL,
                                                                                                                                 value   = T, 
                                                                                                                                 width   = "100%")))),
               div(class = "col-sm-2", style = "margin-top: 0px; margin-bottom: 0px; max-height: 35px !important;", ifelse(is.na(statisticsID[4]), 
                                                                                             "", 
                                                                                             shiny::tagList(shiny::checkboxInput(inputId = statisticsID[4], 
                                                                                                                                 label   = NULL,
                                                                                                                                 value   = T, 
                                                                                                                                 width   = "100%")))),
               div(class = "col-sm-2", style = "margin-top: 0px; margin-bottom: 0px; max-height: 35px !important;", ifelse(is.na(statisticsID[5]), 
                                                                                             "", 
                                                                                             shiny::tagList(shiny::checkboxInput(inputId = statisticsID[5], 
                                                                                                                                 label   = NULL,
                                                                                                                                 value   = T, 
                                                                                                                                 width   = "100%"))))
      ),
      tags$hr(style = "margin-top: 0px; margin-bottom: 0px")
    )
  }
  ret
}
