#' Prepare data in format for CDT
#'
#' @inheritParams prepare_cdt
#' @param data data.frame of dekadal climatic data in tidy format i.e. one row
#'   per dekad (per station) and one column per element.
#'
#' @return A data.frame formatted for use in CDT
#' @export
#'
#' @examples # TODO
prepare_cdt_dekad <- function(data, station, element, date = NULL, year = NULL, month = NULL,
                              dekad = NULL, metadata = NULL, latitude, longitude, altitude) {
  prepare_cdt(data = data, station = station, element = element, type = "dekad",
              date = date, year = year, month = month, dekad = dekad, metadata = metadata,
              latitude = latitude, longitude = longitude, altitude = altitude)
}