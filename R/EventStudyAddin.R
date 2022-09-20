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
#' Call this as an addin to perform an Event Study on an interface in R. The 
#' interface is similar to our Event Study web interface 
#' \url{https://www.eventstudytools.com}.
#'
#' @export
EventStudyAddin <- function() {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(title = "Event Study", 
                           right = miniTitleBarButton("performAnalysis", "Perform Analysis", primary = TRUE)),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel("Parameters", icon = icon("sliders"),
                           miniUI::miniContentPanel(
                             uiOutput("parameters_ui")
                           )
      ),
      miniUI::miniTabPanel("Statistics", icon = icon("linode"),
                           miniUI::miniContentPanel(
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
    
    # -------------------------------------------------------------------------.
    # -------------------------------------------------------------------------.
    # Parameters ----
    # -------------------------------------------------------------------------.
    # -------------------------------------------------------------------------.
    # > Models ----
    returnModels <- c("Market Model"                        = "mm", 
                      "Scholes/Williams Model"              = "mm-sw", 
                      "Market Adjusted"                     = "mam", 
                      "Comparison Period Mean Adjusted"     = "cpmam",
                      "Fama-French 3 Factor Model"          = "ff3fm",
                      "Fama-French-Momentum 4 Factor Model" = "ffm4fm",
                      "GARCH"                               = "garch", 
                      "EGARCH"                              = "egarch")
    
    volumeModels <- c("Market Model"                        = "mm", 
                      "Scholes/Williams Model"              = "mm-sw", 
                      "Market Adjusted"                     = "mam", 
                      "Comparison Period Mean Adjusted"     = "cpmam")
    
    volatilityModels <- c("GARCH" = "garch")
    
    modelList <- list("return"     = returnModels,
                      "volume"     = volumeModels,
                      "volatility" = volatilityModels)
    
    # > Statistics ----
    StatisticsNamesReturn <- c("Csect T", "Patell Z", "Adjusted Patell Z", 
                               "StdCSect Z", "Adjusted StdCSect Z", 
                               "Skewness Corrected T", "Rank Z", "Generalized Rank T", 
                               "Generalized Rank Z", "Generalized Sign Z")
    
    StatisticsNamesVolatility <- c("Cross-Sectional-Vy-t-Test",
                                   "Cross-Sectional-Corrected-Vy-t-Test",
                                   "Cross-Sectional-AR-t-Test", 
                                   "Cross-Sectional-Corrected-AR-t-Test")
    
    StatisticsNamesList <- list("return"     = StatisticsNamesReturn,
                                "volume"     = StatisticsNamesReturn,
                                "volatility" = StatisticsNamesVolatility)
    
    
    # > Statistics Table ----
    statisticIDReturn <- list(
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
    
    statisticIDVolatility <- list(
      c(NA, NA, "aarcsvyt", NA, NA),
      c(NA, NA, "aarcscvyt", NA, NA),
      c(NA, NA, "aarcsart", NA, NA),
      c(NA, NA, "aarcscart", NA, NA)
    )
    
    statisticIDList <- list("return"     = statisticIDReturn,
                            "volume"     = statisticIDReturn,
                            "volatility" = statisticIDVolatility)
    
    
    # > Statistics Table as Vector ----
    statisticIDReturn %>% 
      unlist() %>% 
      na.omit() %>% 
      as.character() -> statisticIDReturnVector
    
    statisticIDVolatility %>% 
      unlist() %>% 
      na.omit() %>% 
      as.character() -> statisticIDVolatilityVector
    
    statisticsIDVectorList <- list("return"     = statisticIDReturnVector,
                                   "volume"     = statisticIDReturnVector,
                                   "volatility" = statisticIDVolatilityVector)
    
    
    # Styles ----
    hrStyle <- "margin-top: 0px; margin-bottom: 0px"
    fluidRowStyle <- "margin: 5px 5px 5px 5px"
    
    # -------------------------------------------------------------------------.
    # -------------------------------------------------------------------------.
    # UX ----
    # -------------------------------------------------------------------------.
    # -------------------------------------------------------------------------.
    output$parameters_ui <- renderUI({
      fillCol(flex = c(NA, NA, 1),
              tagList(
                fluidRow(style = fluidRowStyle,
                         h4("API Parameters"),
                         column(12, 
                                shiny::radioButtons(inputId = "eventStudyType", 
                                                    label   = "Type of Event Study", 
                                                    inline  = T,
                                                    choices = c("Return Event Study"     = "return",
                                                                "Volume Event Study"     = "volume",
                                                                "Volatility Event Study" = "volatility")),
                                hr(style = hrStyle)
                         ),
                         column(4,
                                textInput(inputId = "apiUrl", 
                                          label   = "API URL", 
                                          value   = getOption("EventStudy.URL"), 
                                          width   = "100%")
                         ),
                         column(4,
                                textInput(inputId     = "apiKey", 
                                          label       = "API Key", 
                                          value       = getOption("EventStudy.KEY"), 
                                          placeholder = "Please add your API key", 
                                          width       = "100%")
                         ),
                         column(2, style = "margin-top: 23px;",
                                actionButton(inputId = "connect", 
                                             label   = "Connect")       
                         ),
                         column(2,
                                htmlOutput("connectResponse")
                         )
                ),
                tags$hr(style = hrStyle)
              ),
              tagList(
                fluidRow(style = fluidRowStyle,
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
                                             choices = returnModels)),
                         column(2, h5("Return Type"),
                                radioButtons(inputId = "returnType", 
                                             label   = NA, 
                                             choices = c("Simple" = "simple", 
                                                         "Log"    = "log"))),
                         column(4, h5("Adjustment Rule for Non-Trading Days"),
                                radioButtons(inputId = "adjustmentNonTradingDays", 
                                             label   = NA, 
                                             choices = c("Take earlier trading day"     = "earlier", 
                                                         "Take later trading day"       = "later", 
                                                         "Keep non-trading day"         = "keep", 
                                                         "Skip respective observations" = "skip")))
                ), tags$hr(style = hrStyle)),
              fluidRow(style = fluidRowStyle,
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
    
    
    # > User Messages UI ----
    userMsg <- reactiveValues(output = "")
    output$connectResponse <- renderUI({
      return(HTML(userMsg$output))
    })
    # -------------------------------------------------------------------------.
    # -------------------------------------------------------------------------.
    # Statistics UI ----
    # -------------------------------------------------------------------------.
    # -------------------------------------------------------------------------.
    output$statistics_ui <- renderUI({
      # generate header
      ui <- tagList(generateStatisticColumn(T))
      
      # Type of Event Study
      selectedType <- input$eventStudyType
      if (is.null(selectedType))
        selectedType <- "return"
      
      StatisticsNames <- StatisticsNamesList[[selectedType]]
      n <- length(StatisticsNames)
      statisticID <- statisticIDList[[selectedType]]
      
      # Generate Table
      for (i in 1:n) {
        ui <- shiny::tagAppendChild(ui,
                                    generateStatisticColumn(header        = F, 
                                                            statisticName = StatisticsNames[i],
                                                            statisticsID  = statisticID[[i]])
        )
      }
      ui
    })
    
    
    # -------------------------------------------------------------------------.
    # -------------------------------------------------------------------------.
    # Observer ----
    # -------------------------------------------------------------------------.
    # -------------------------------------------------------------------------.
    # > Switch Model ----
    observeEvent(input$eventStudyType, {
      selectedType <- input$eventStudyType
      shiny::req(selectedType)
      updateRadioButtons(session = session,
                         inputId = "benchmarkModel", 
                         label   = NA, 
                         choices = modelList[[selectedType]])
    })
    
    
    # > Connect to API ----
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
    
    
    # > Perform Analysis -----
    observeEvent(input$performAnalysis, {
      if (is.null(estAPI)) {
        userMsg$output <- "API is not initialized. Please connect to API."
      } else {
        # Type of Event Study
        selectedType <- input$eventStudyType
        if (is.null(selectedType))
          selectedType <- "return"
        
        # 1. Set Parameters 
        if (selectedType == "return") {
          returnEstParams <- ARCApplicationInput$new()
        } else if (selectedType == "volume") {
          returnEstParams <- AVCApplicationInput$new()
        } else if (selectedType == "volatility") {
          returnEstParams <- AVyCApplicationInput$new()
        }
        
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
        
        statisticsIDVector <- statisticsIDVectorList[[selectedType]]
        statisticsIDVector %>% 
          purrr::map(.f = .getStatistics, y = input) %>% 
          unlist() -> selectedStatistics
        if (!is.null(selectedStatistics)) {
          selectedStatistics <- which(selectedStatistics)
          selectedStatistics <- statisticsIDVector[selectedStatistics]
          returnEstParams$setTestStatistics(selectedStatistics)
        }
        
        # initialize API object with parameter object
        # get data files
        requestFile <- input$requestFile$datapath
        firmDataFile <- input$firmDataFile$datapath
        marketDataFile <- input$marketDataFile$datapath
        print("Validate input files.")
        shiny::validate(shiny::need(!is.null(requestFile) && !is.null(firmDataFile) && !is.null(marketDataFile), "Please upload your data!"))
        dataFiles <- c("request_file" = requestFile, 
                       "firm_data"    = firmDataFile, 
                       "market_data"  = marketDataFile)
        
        # result path
        resultPath <- input$resultPath
        if (is.null(resultPath) || resultPath == "") {
          resultPath <- getwd()
        }
        
        print("Perform Event Study")
        estResult <- estAPI$performEventStudy(estParams = returnEstParams,
                                              dataFiles = dataFiles,
                                              destDir   = resultPath)
        
        save(estResult, file = "estResultAddin.RData")
        print("Event Study finished")
        rstudioapi::sendToConsole(code = "load('estResultAddin.RData'); estResult", execute = T)
        invisible(stopApp())
      }
    })
  }
  
  
  viewer <- dialogViewer(dialogName = "Event Study", width = 1200, height = 800)
  runGadget(app = ui, server = server, viewer = viewer)
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
               div(class = "col-sm-2", 
                   style = "margin-top: 0px; margin-bottom: 0px; max-height: 35px !important;", h6(statisticName)),
               div(class = "col-sm-2", 
                   style = "margin-top: 0px; margin-bottom: 0px; max-height: 35px !important;", ifelse(is.na(statisticsID[1]), 
                                                                                                       "", 
                                                                                                       shiny::tagList(shiny::checkboxInput(inputId = statisticsID[1], 
                                                                                                                                           label   = NULL,
                                                                                                                                           value   = T, 
                                                                                                                                           width   = "100%")))),
               div(class = "col-sm-2", 
                   style = "margin-top: 0px; margin-bottom: 0px; max-height: 35px !important;", ifelse(is.na(statisticsID[2]), 
                                                                                                       "", 
                                                                                                       shiny::tagList(shiny::checkboxInput(inputId = statisticsID[2], 
                                                                                                                                           label   = NULL,
                                                                                                                                           value   = T, 
                                                                                                                                           width   = "100%")))),
               div(class = "col-sm-2", 
                   style = "margin-top: 0px; margin-bottom: 0px; max-height: 35px !important;", ifelse(is.na(statisticsID[3]), 
                                                                                                       "", 
                                                                                                       shiny::tagList(shiny::checkboxInput(inputId = statisticsID[3], 
                                                                                                                                           label   = NULL,
                                                                                                                                           value   = T, 
                                                                                                                                           width   = "100%")))),
               div(class = "col-sm-2", 
                   style = "margin-top: 0px; margin-bottom: 0px; max-height: 35px !important;", ifelse(is.na(statisticsID[4]), 
                                                                                                       "", 
                                                                                                       shiny::tagList(shiny::checkboxInput(inputId = statisticsID[4], 
                                                                                                                                           label   = NULL,
                                                                                                                                           value   = T, 
                                                                                                                                           width   = "100%")))),
               div(class = "col-sm-2", 
                   style = "margin-top: 0px; margin-bottom: 0px; max-height: 35px !important;", ifelse(is.na(statisticsID[5]), 
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
