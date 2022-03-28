#' Export dekad data in GeoCLIM format
#' @inheritParams export_geoclim
#' @inheritParams prepare_geoclim_dekad
#'
#' @return Invisibly returns the file path of the saved data.
#' @export
#'
#' @examples
#' # Calculate dekadal summaries for the rainfall column
#' dekad_data <- daily_niger %>% dplyr::mutate(dekad = dekad(date))
#' # Summarise the data
#' summary_data <- dekad_data %>% dplyr::group_by(station_name, year, dekad) %>%
#'       dplyr::summarise(mean_rain = mean(rain, na.rm = TRUE))
#' # NOT RUN: Export the data to CSV format
#' #export_geoclim_dekad(data = summary_data, year = "year",
#' #                     station_id = "station_name",
#' #                     dekad = "dekad",
#' #                     element = "mean_rain", metadata = stations_niger, 
#' #                     join_by = "station_name",
#' #                     latitude = "lat", longitude = "long")

export_geoclim_dekad <- function(data, year, dekad, element, metadata = NULL,
                                 join_by = NULL, station_id,
                                 latitude, longitude, add_cols = NULL,
                                 file_path = paste0("GEOCLIM-", 
                                                    element, 
                                                    ".csv"),
                                 ...) {
  export_geoclim(data = data, year = year, type = "dekad", type_col = dekad, 
                 element = element, metadata = metadata, join_by = join_by,
                 station_id = station_id, latitude = latitude, 
                 longitude = longitude, add_cols = add_cols)
}