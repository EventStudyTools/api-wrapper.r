#' Get parameters for EventStudyTools API
#'
#' @export
ESTCataParameters <- R6::R6Class(classname = "ESTCataParameters",
                                 inherit = ApplicationInputInterface,
                                 public = list(
                                   datasources = list(
                                     text_data = "csv_zip",
                                     keywords_data = "csv_zip"
                                   ),
                                   # get parameters
                                   getParameters = function() {
                                     self$getMember()
                                   }
                                 )
)
