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

#' Export dekad data in GeoCLIM format
#' 
#' @description `export_geoclim_dekad` takes a data frame that is given by
#' by dekad. This data frame is then rearranged using `prepare_geoclim_dekad` to a
#' format suitable for use in GeoCLIM, and then written to a file or connection.
#' 
#' @inheritParams export_geoclim
#' @inheritParams prepare_geoclim_dekad
#'
#' @return Invisibly returns the file path of the saved data.
#' @export
#'
#' @examples
#' # Calculate dekadal summaries for the rainfall column
#' dekad_data <- daily_niger %>% dplyr::mutate(dekad = dekad(date))
#' # Summarise the data
#' summary_data <- dekad_data %>% dplyr::group_by(station_name, year, dekad) %>%
#'       dplyr::summarise(mean_rain = mean(rain, na.rm = TRUE))
#' # NOT RUN: Export the data to CSV format
#' #export_geoclim_dekad(data = summary_data, year = "year",
#' #                     station_id = "station_name",
#' #                     dekad = "dekad",
#' #                     element = "mean_rain", metadata = stations_niger, 
#' #                     join_by = "station_name",
#' #                     latitude = "lat", longitude = "long")

export_geoclim_dekad <- function(data, year, dekad, element, metadata = NULL,
                                 join_by = NULL, station_id,
                                 latitude, longitude, add_cols = NULL,
                                 file_path = paste0("GEOCLIM-", 
                                                    element, 
                                                    ".csv"),
                                 ...) {
  export_geoclim(data = data, year = year, type = "dekad", type_col = dekad, 
                 element = element, metadata = metadata, join_by = join_by,
                 station_id = station_id, latitude = latitude, 
                 longitude = longitude, add_cols = add_cols)
}