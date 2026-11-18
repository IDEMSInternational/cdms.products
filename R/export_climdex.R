# Copyright 2022 IDEMS International Community Interest Company
#
# This file is part of cdms.products.
#
# cdms.products is free software: you can redistribute it and/or modify it under the
# terms of the GNU Lesser General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later version.
#
# cdms.products is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License along with
# cdms.products. If not, see <https://www.gnu.org/licenses/>.

#' Export data in the format for RClimDex
#' 
#' @description `export_climdex` rearranges a data frame using `prepare_climdex` to a
#' format suitable for use in RClimDex. The data frame is then written to a
#' file or connection.
#' 
#' @inheritParams prepare_climdex
#' @param file_type \code{character(1)} A character specifying the file type to export as, either \code{"csv"} or `\code{"txt"}.
#' @param file_path \code{character(1)} A character specifying the file path and file name to export
#' @param ... Other parameters passed to \code{write.table()}
#'
#' @return Invisibly returns the file path of the saved data
#' @export
#'
#' @examples # Export data to climdex
#' # NOT RUN:
#' # export_climdex(data = daily_niger, date = "date", prcp = "rain", tmax = "tmax", tmin = "tmin")
export_climdex <- function(data, prcp, tmax, tmin, date = NULL, year = NULL, 
                           month = NULL, day = NULL, 
                           file_type = c("csv", "txt"),
                           file_path = paste0("climdex-", Sys.Date()),
                           ...) {
  checkmate::check_string(file)
  file_type <- match.arg(file_type)
  
  climdex_data <- prepare_climdex(data = data, prcp = prcp, tmax = tmax, 
                                  tmin = tmin, date = date, year = year, 
                                  month = month, day = day)
  
  file_path <- paste(file_path, file_type, sep = ".")

  if (file_type == "csv"){
    file_params <- utils::modifyList(list(x = climdex_data,
                                          file = file_path,
                                          row.names = FALSE,
                                          col.names = FALSE,
                                          sep=","),
                                     list(...))
  } else {
    file_params <- utils::modifyList(list(x = climdex_data,
                                          file = file_path,
                                          row.names = FALSE,
                                          col.names = FALSE),
                                     list(...))
  }
  do.call("write.table", file_params)
  message("ClimDex data saved at: '", file_path, "'")
  invisible(file_path)
}
