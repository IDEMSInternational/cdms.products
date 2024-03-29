#' Prepare pentad data in GeoCLIM format
#' 
#' @description `prepare_geoclim_pentad` takes a data frame that is in pentad
#' format. This data is then rearranged to a format suitable for use in GeoCLIM.
#' This data frame can be rearranged and exported to a file or connection with
#' `export_geoclim_pentad`.
#' 
#' @inheritParams prepare_geoclim
#' @param pentad \code{character(1)} The name of the pentad column in \code{data}.
#'
#' @return A data.frame formatted for use in GeoCLIM
#' @export
#'
#' @examples
#' # Calculate pentad summaries for the rainfall column
#' pentad_data <- daily_niger %>% dplyr::mutate(pentad = pentad(date))
#' summary_data <- pentad_data %>% dplyr::group_by(station_name, year, pentad) %>%
#'       dplyr::summarise(mean_rain = mean(rain, na.rm = TRUE))
#' prepare_geoclim_pentad(data = summary_data, year = "year",
#'                 station_id = "station_name",
#'                 pentad = "pentad",
#'                 element = "mean_rain", metadata = stations_niger, 
#'                 join_by = "station_name",
#'                 latitude = "lat", longitude = "long")
prepare_geoclim_pentad <- function(data, year, pentad, element, station_id, 
                                   latitude, longitude, metadata = NULL, 
                                   join_by = NULL, add_cols = NULL) {
  prepare_geoclim(data = data, year = year, type_col = pentad, 
                  element = element, station_id = station_id, 
                  latitude = latitude, longitude = longitude, type = "pentad",
                  metadata = metadata, join_by = join_by, add_cols = add_cols)
}
