#' Prepare CDT for Daily data
#' 
#' @description `prepare_cdt_daily` takes a daily data. This data is then
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