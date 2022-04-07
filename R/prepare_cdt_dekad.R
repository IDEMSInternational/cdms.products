#' Prepare data in format for CDT
#' 
#' @description `prepare_cdt_dekad` takes a dekad data. This data is then
#' rearranged to a format suitable for use in CDT. This data frame can be
#' rearranged and exported to a file or connection with `export_CDT_dekad`.
#'
#' @inheritParams prepare_cdt
#' @param data \code{data.frame} data.frame of dekadal climatic data in tidy format i.e. one row
#'   per dekad (per station) and one column per element.
#'
#' @return A data.frame formatted for use in CDT.
#' @export
#'
#' @examples # Create prepare summary dekad data for CDT export
#' summary_data <- daily_niger %>%
#'                    dplyr::mutate(dekad_date = dekad(daily_niger$date)) %>%
#'                    dplyr::group_by(station_name, year, dekad_date) %>%
#'                    dplyr::summarise(date = dplyr::first(date), sum = sum(tmax))
#' 
#' prepare_cdt_dekad(data = summary_data, date_time = "date", year = "year",
#'                   station = "station_name",
#'                   element = "sum", metadata = stations_niger, 
#'                   latitude = "lat", longitude = "long", altitude = "alt")
prepare_cdt_dekad <- function(data, station, element, date_time = NULL, year = NULL, month = NULL,
                              dekad = NULL, metadata = NULL, latitude, longitude, altitude) {
  prepare_cdt(data = data, station = station, element = element, type = "dekad",
              date_time = date_time, year = year, month = month, dekad = dekad, metadata = metadata,
              latitude = latitude, longitude = longitude, altitude = altitude)
}