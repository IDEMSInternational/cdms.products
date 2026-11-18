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

#' Export monthly data in GeoCLIM format
#' 
#' @description `export_geoclim_month` takes a data frame that is given by
#' month. This data frame is then rearranged using `prepare_geoclim_month` to a
#' format suitable for use in GeoCLIM, and then written to a file or connection.
#' 
#' @inheritParams prepare_geoclim_month
#' @param file_path \code{character(1)} A character specifying the file path and file name to export.
#' @param ... Other parameters passed to \code{write.csv()}.
#'
#' @return Invisibly returns the file path of the saved data.
#' @export
#'
#' @examples
#' # Calculate monthly summaries for the rainfall column
#' summary_data <- daily_niger %>% dplyr::group_by(year, month, station_name) %>%
#'      dplyr::summarise(mean_rain = mean(rain))
#' export_geoclim_month(data = summary_data, year = "year", month = "month",
#'                      station_id = "station_name",
#'                      element = "mean_rain", metadata = stations_niger,
#'                      join_by = "station_name",
#'                      latitude = "lat", longitude = "long")
export_geoclim_month <- function(data, year, month, element,  station_id,
                                 latitude, longitude, metadata = NULL,
                                 join_by = NULL, add_cols = NULL, 
                                 file_path = paste0("GEOCLIM-", element, ".csv"),
                                 ...) {
  checkmate::check_string(file)
  geomclim_data <- 
    prepare_geoclim_month(data = data, year = year, month = month,
                          element = element, station_id = station_id,
                          latitude = latitude, longitude = longitude,
                          metadata = metadata, join_by = join_by, 
                          add_cols = add_cols)
  csv_params <- utils::modifyList(list(x = geomclim_data,
                                file = file_path,
                                row.names = FALSE), 
                           list(...))
  do.call("write.csv", csv_params)
  message("GEOCLIM data saved at: '", file_path, "'")
  invisible(file_path)
}