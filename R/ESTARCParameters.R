#' Abnormal Return Calculation Parameters
#' 
#' This R6 class defines the parameters for the Event Study. 
#'
#' @export
ESTARCParameters <- R6::R6Class(classname = "ESTParameters",
                                inherit = ApplicationInputInterface,
                                public = list(
                                  task = list(locale = 'en'),
                                  result_file_type = list(result_file_type = "xls"),
                                  benchmark_model  = list(benchmark_model = "mm"),
                                  return_type      = list(return_type = "log"),
                                  non_trading_days = list(non_trading_days = "later"),
                                  test_statistics  = list("art", "cart", "aart", "caart", "abhart",
                                    "aarptlz", "caarptlz", "aaraptlz", "caaraptlz", "aarbmpz",
                                    "caarbmpz", "aarabmpz", "caarabmpz", "aarskewadjt", "caarskewadjt",
                                    "abharskewadjt", "aarrankz", "caarrankz", "aargrankt", "caargrankt",
                                    "aargrankz", "caargrankz", "aargsignz", "caargsignz",
                                    "aarcdat", "aarjackknivet"),
                                  request_file = list(
                                    key  = "request_file",
                                    type = "csv"
                                  ),
                                  firm_data    = list(
                                    key  = "firm_data",
                                    type = "csv"
                                  ),
                                  market_data  = list(
                                    key  = "market_data",
                                    type = "csv"
                                  ),
                                  # get parameters
                                  getParameters = function() {
                                    self$getMember()
                                  },
                                  # set email
                                  setEMail = function(eMail) {
                                    self$task[["email"]] <- eMail
                                  },
                                  # set benchmark model
                                  setBenchmarkModel = function(model) {
                                    model <- match.arg(model, c("mm"))
                                    self$benchmark_model <- model
                                  },
                                  # set return type
                                  setReturnType = function(returnType) {
                                    returnType <- match.arg(returnType, c("log"))
                                    self$return_type <- returnType
                                  },
                                  # set test statistics
                                  setTestStatistics = function(testStatistic, active=T) {
                                    testStatistic <- match.arg(testStatistic, private$test_statistics)
                                    if (active) {
                                      self[["test_statistics"]][[testStatistic]] <- "1"
                                    } else {
                                      self[["test_statistics"]][[testStatistic]] <- NULL
                                    }
                                  },
                                  # returns available test statistics
                                  getTestStatistics = function() {
                                    private$allowed_test_statistics
                                  }
                                ),
                                private = list(
                                  allowed_test_statistics = c(
                                    "art",
                                    "cart",
                                    "aart",
                                    "caart",
                                    "abhart",
                                    "aarptlz",
                                    "caarptlz",
                                    "aaraptlz",
                                    "caaraptlz",
                                    "aarbmpz",
                                    "caarbmpz",
                                    "aarabmpz",
                                    "caarabmpz",
                                    "aarskewadjt",
                                    "caarskewadjt",
                                    "abharskewadjt",
                                    "aarrankz",
                                    "caarrankz",
                                    "aargrankt",
                                    "caargrankt",
                                    "aargrankz",
                                    "caargrankz",
                                    "aargsignz",
                                    "caargsignz",
                                    "aarcdat",
                                    "aarjackknivet"
                                  )
                                )
)
