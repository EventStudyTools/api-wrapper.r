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
#' @name ESTCataParameters
#' 
#' @title CATA Parameters
#' 
#' @export
ESTCataParameters <- R6::R6Class(classname = "ESTCataParameters",
                                 inherit = ApplicationInputInterface,
                                 public = list(
                                   text_data = "csv_zip",
                                   keywords_data = "csv_zip"
                                 )
)
