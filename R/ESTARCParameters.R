#' Get parameters for EventStudyTools API
#'
#' @export
ESTARCParameters <- R6::R6Class(classname = "ESTParameters",
                                inherit = ApplicationInputInterface,
                                public = list(
                                  task = list(locale = 'en'),
                                  result_file_type = "xls",
                                  benchmark_model  = "mm",
                                  return_type      = "log",
                                  non_trading_days = "later",
                                  test_statistics  = list(
                                    art           = "1",
                                    cart          = "1",
                                    aart          = "1",
                                    caart         = "1",
                                    abhart        = "1",
                                    aarptlz       = "1",
                                    caarptlz      = "1",
                                    aaraptlz      = "1",
                                    caaraptlz     = "1",
                                    aarbmpz       = "1",
                                    caarbmpz      = "1",
                                    aarabmpz      = "1",
                                    caarabmpz     = "1",
                                    aarskewadjt   = "1",
                                    caarskewadjt  = "1",
                                    abharskewadjt = "1",
                                    aarrankz      = "1",
                                    caarrankz     = "1",
                                    aargrankt     = "1",
                                    caargrankt    = "1",
                                    aargrankz     = "1",
                                    caargrankz    = "1",
                                    aargsignz     = "1",
                                    caargsignz    = "1",
                                    aarcdat       = "1",
                                    aarjackknivet = "1"
                                  ),
                                  request_file = list(
                                    key = "request_file",
                                    type = "csv"
                                  ),
                                  firm_data    = list(
                                    key = "firm_data",
                                    type = "csv"
                                  ),
                                  market_data  = list(
                                    key = "market_data",
                                    type = "csv"
                                  ),
                                  # initialize object
                                  initialize = function() {
                                    private$getTestStatisticNames()
                                  },
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
                                  allowed_test_statistics = NULL,
                                  getTestStatisticNames = function() {
                                    private$allowed_test_statistics = names(self[["test_statistics"]])
                                  }
                                )
)
