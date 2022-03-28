#' Export pentad data in GeoCLIM format
#' @inheritParams export_geoclim
#' @inheritParams prepare_geoclim_pentad
#'
#' @return Invisibly returns the file path of the saved data.
#' @export
#'
#' @examples
#' # Calculate pentad summaries for the rainfall column
#' pentad_data <- daily_niger %>% dplyr::mutate(pentad = pentad(date))
#' # Summarise the data
#' summary_data <- pentad_data %>% dplyr::group_by(station_name, year, pentad) %>%
#'       dplyr::summarise(mean_rain = mean(rain, na.rm = TRUE))
#' # NOT RUN: Export the data to CSV format
#' #export_geoclim_pentad(data = summary_data, year = "year",
#' #                     station_id = "station_name",
#' #                     pentad = "pentad",
#' #                     element = "mean_rain", metadata = stations_niger, 
#' #                     join_by = "station_name",
#' #                     latitude = "lat", longitude = "long")
export_geoclim_pentad <- function(data, year, pentad, element, metadata = NULL,
                                  join_by = NULL, station_id,
                                  latitude, longitude, add_cols = NULL,
                                  file_path = paste0("GEOCLIM-",
                                                     element,
                                                     ".csv"),
                                 ...) {
  export_geoclim(data = data, year = year, type = "pentad", type_col = pentad,
                 element = element, metadata = metadata, join_by = join_by,
                 station_id = station_id, latitude = latitude, 
                 longitude = longitude, add_cols = add_cols)
}