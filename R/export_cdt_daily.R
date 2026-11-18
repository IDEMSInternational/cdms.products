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

#' Export daily data in the format for CDT
#' 
#' @description `export_cdt_daily` takes a data frame with daily data.
#' This data frame is then rearranged using `prepare_cdt_daily` to a
#' format suitable for use in CDT, and then written to a file or connection.
#' 
#' @inheritParams prepare_cdt_daily
#' @inheritParams export_cdt
#' @param ... Other parameters passed to \code{write.csv()}
#'
#' @return Invisibly returns the file path of the saved data
#' @export
#'
#' @examples # To write daily_niger data to CDT format
#' # NOT RUN:
#' #export_cdt_daily(data = daily_niger, station = "station_name", element = "rain", type = "daily",
#' #           date_time = "date", latitude = "lat", longitude = "long", altitude = "alt",
#' #           metadata = stations_niger)
export_cdt_daily <- function(data, station, element, date_time, latitude, longitude,
                             altitude, metadata = NULL,
                             file_path = paste0("CDT-", element, ".csv"),
                             ...) {
  export_cdt(data = data, station = station, element = element, 
             latitude = latitude, longitude = longitude, altitude = altitude, 
             type = "daily", date_time = date_time, metadata = metadata, 
             file_path = file_path)
}