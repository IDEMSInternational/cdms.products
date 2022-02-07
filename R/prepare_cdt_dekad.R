#' Prepare CDT for dekad data
#'
#' @inheritParams prepare_cdt
#'
#' @return
#' @export
#'
#' @examples
prepare_cdt_dekad <- function(data, station, element, date = NULL, year = NULL, month = NULL,
                              dekad = NULL, metadata = NULL, latitude, longitude, altitude) {
  prepare_cdt(data = data, station = station, element = element, type = "dekad",
              date = date, year = year, month = month, dekad = dekad, metadata = metadata,
              latitude = latitude, longitude = longitude, altitude = altitude)
}