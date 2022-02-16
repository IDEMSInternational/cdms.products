#' Prepare CDT for Daily data
#'
#' @inheritParams prepare_cdt
#' @param data data.frame of daily climatic data in tidy format i.e. one row
#'   per day (per station) and one column per element.
#'   
#' @return A data.frame formatted for use in CDT
#' @export
#'
#' @examples
#' prepare_cdt_daily(data = daily_niger, date = "date", station = "station_name",
#'                   element = "tmax", metadata = stations_niger, 
#'                   latitude = "lat", longitude = "long", altitude = "alt")
prepare_cdt_daily <- function(data, station, element, date, metadata = NULL,
                              latitude, longitude, altitude) {
  prepare_cdt(data = data, station = station, element = element, type = "daily",
              date = date, metadata = metadata, latitude = latitude,
              longitude = longitude, altitude = altitude)
}