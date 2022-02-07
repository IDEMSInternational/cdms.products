#' Prepare CDT for Daily data
#'
#' @inheritParams prepare_cdt
#'
#' @return
#' @export
#'
#' @examples
#' # With daily_niger data:
#' data("daily_niger"); data("stations_niger")
#' prepare_cdt_daily(data = daily_niger, date = "date", station = "station_name",
#'                   element = "tmax", metadata = stations_niger, 
#'                   latitude = "lat", longitude = "long", altitude = "alt")
prepare_cdt_daily <- function(data, station, element, date, metadata = NULL,
                              latitude, longitude, altitude) {
  prepare_cdt(data = data, station = station, element = element, type = "daily",
              date = date, metadata = metadata, latitude = latitude,
              longitude = longitude, altitude = altitude)
}