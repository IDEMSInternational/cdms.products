#' Prepare dekad data in GeoCLIM format
#' @inheritParams prepare_geoclim
#' @param dekad \code{character(1)} The name of the dekad column in \code{data}.
#'
#' @return A data.frame formatted for use in GeoCLIM
#' @export
#'
#' @examples
#' # Calculate dekadal summaries for the rainfall column
#' dekad_data <- daily_niger %>% dplyr::mutate(dekad = dekad(date))
#' summary_data <- dekad_data %>% dplyr::group_by(station_name, year, dekad) %>%
#'     dplyr::summarise(mean_rain = mean(rain, na.rm = TRUE))
#' prepare_geoclim_dekad(data = summary_data, year = "year",
#'                       station_id = "station_name",
#'                       dekad = "dekad",
#'                       element = "mean_rain", metadata = stations_niger, 
#'                       join_by = "station_name",
#'                       latitude = "lat", longitude = "long")
prepare_geoclim_dekad <- function(data, year, dekad, element, station_id,
                                  latitude, longitude, metadata = NULL,
                                  join_by = NULL, add_cols = NULL) {
  prepare_geoclim(data = data, year = year, type_col = dekad, 
                  element = element, station_id = station_id, 
                  latitude = latitude, longitude = longitude, type = "dekad", 
                  metadata = metadata, join_by = join_by, add_cols = add_cols)
}