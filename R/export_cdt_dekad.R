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

#' Export dekad data in CDT format
#' 
#' @description `export_cdt_dekad` takes a data frame that has elements summarised
#' by dekad. This data frame is then rearranged using `prepare_cdt_dekad` to a
#' format suitable for use in CDT, and then written to a file or connection.
#' 
#' @inheritParams prepare_cdt_dekad
#' @inheritParams export_cdt
#' @param ... Other parameters passed to \code{write.csv()}
#'
#' @return Invisibly returns the file path of the saved data
#' @export
#'
#' @examples # To write daily_niger data to CDT format
#' # first put daily_niger data into a dekad format
#' data(daily_niger)
#'summary_data <- daily_niger %>%
#'                    dplyr::mutate(dekad_date = dekad(daily_niger$date)) %>%
#'                    dplyr::group_by(station_name, year, dekad_date) %>%
#'                    dplyr::summarise(date = dplyr::first(date), sum_tmax = sum(tmax))
#' # NOT RUN:
#' # export_cdt_dekad(data = summary_data, station = "station_name", element = "sum_tmax",
#' #           date_time = "date", latitude = "lat", longitude = "long", altitude = "alt",
#' #           dekad = "dekad_date", metadata = stations_niger)
export_cdt_dekad <- function(data, station, element, date_time, latitude, 
                             longitude, altitude, year = NULL, month = NULL, 
                             dekad = NULL, metadata = NULL,
                             file_path = paste0("CDT-", element, ".csv"),
                             ...) {
  export_cdt(data = data, station = station, element = element, 
             latitude = latitude, longitude = longitude, altitude = altitude, 
             type = "dekad", date_time = date_time, year = year, month = month, 
             dekad = dekad, metadata = metadata, file_path = file_path)
}
