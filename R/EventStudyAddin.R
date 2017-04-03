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
#' @name EventStudyAddin
#' 
#' @title RStudio Addin for performing an Event Study
#'
#' @description 
#' Call this as an addin to replace occurrences of a particular sequence of
#' characters in a document with a new sequence of characters. Te interface is 
#' similar to our Event Study web interface \url{www.eventstudytools.com}.
#'
#' @export
EventStudyAddin <- function() {
  
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
    
    statisticID %>% 
      unlist() %>% 
      na.omit() %>% 
      as.character() -> statisticsIDVector
    
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
                              fileInput("firmDataFile", "Firm Data", width = "100%")
                       ),
                       column(4,
                              fileInput("marketDataFile", "Market Data", width = "100%")
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
          userMsg$output <- "Please add valid API key"
        } else {
          authSuccess <- estAPI$authentication(apiKey)
          if (authSuccess) {
            userMsg$output <- "Authentication was successful."
          } else {
            userMsg$output <- "Authentication was not successful."
          }
        }
      }
    })
    
    
    userMsg <- reactiveValues(output = "")
    output$connectResponse <- renderUI({
      return(HTML(userMsg$output))
    })
    
    observeEvent(input$performAnalysis, {
      if (is.null(estAPI)) {
        userMsg$output <- "API is not initialized. Please connect to API."
      } else {
        # 1. Set Parameters 
        returnEstParams <- ARCApplicationInput$new()
        
        ## get result file format
        resultFile <- input$resultFileFormat
        returnEstParams$setResultFileType(resultFile)
        
        ## get and set benchmark model
        benchmarkModel <- input$benchmarkModel
        returnEstParams$setBenchmarkModel(benchmarkModel)
        
        ## get and set returnType
        returnType <- input$returnType
        returnEstParams$setReturnType(returnType)
        
        ## get und set non-trading days
        adjustmentNonTradingDays <- input$adjustmentNonTradingDays
        returnEstParams$setNonTradingDays(adjustmentNonTradingDays)
        
        # get and set statistics
        .getStatistics <- function(x, y) {
          y[[x]]
        }
        statisticsIDVector %>% 
          purrr::map2(.y = input, .f = .getStatistics) %>% 
          unlist() %>% 
          which() -> selectedStatistics
        if (length(selectedStatistics)) {
          selectedStatistics <- statisticsIDVector[selectedStatistics]
          returnEstParams$setTestStatistics(selectedStatistics)
        }
        
        # initialize API object with parameter object
        # get data files
        requestFile <- input$requestFile$dataPath
        firmDataFile <- input$firmDataFile$dataPath
        marketDataFile <- input$marketDataFile$dataPath
        shiny::validate(shiny::need(!is.null(requestFile) && !is.null(firmDataFile) && !is.null(marketDataFile), "Please upload your data!"))
        dataFiles <- c("request_file" = requestFile, 
                       "firm_data"    = firmDataFile, 
                       "market_data"  = marketDataFile)
        
        # result path
        resultPath <- input$resultPath
        if (is.null(resultPath) || resultPath == "") {
          resultPath <- getwd()
        }
        
        ret <- estAPI$performEventStudy(estParams  = returnEstParams,
                                        dataFiles  = dataFiles,
                                        resultPath = resultPath)
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
