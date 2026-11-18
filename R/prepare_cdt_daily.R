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

#' Prepare CDT for Daily data
#' 
#' @description `prepare_cdt_daily` takes daily data. This data is then
#' rearranged to a format suitable for use in CDT. This data frame can be
#' rearranged and exported to a file or connection with `export_cdt_daily`.
#'
#' @inheritParams prepare_cdt
#' @param data \code{data.frame} data.frame of daily climatic data in tidy format i.e. one row
#'   per day (per station) and one column per element.
#'   
#' @return A data.frame formatted for use in CDT
#' @export
#'
#' @examples # Prepare daily_niger data for CDT export
#' prepare_cdt_daily(data = daily_niger, date_time = "date", station = "station_name",
#'                   element = "tmax", metadata = stations_niger, 
#'                   latitude = "lat", longitude = "long", altitude = "alt")
prepare_cdt_daily <- function(data, station, element, date_time, metadata = NULL,
                              latitude, longitude, altitude) {
  prepare_cdt(data = data, station = station, element = element, type = "daily",
              date_time = date_time, metadata = metadata, latitude = latitude,
              longitude = longitude, altitude = altitude)
}