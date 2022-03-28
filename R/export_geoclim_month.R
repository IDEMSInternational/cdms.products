#' Export monthly data in GeoCLIM format
#' @inheritParams prepare_geoclim_month
#' @param file_path \code{character(1)} A character specifying the file path and file name to export.
#' @param ... Other parameters passed to \code{write.csv()}.
#'
#' @return Invisibly returns the file path of the saved data.
#' @export
#'
#' @examples
#' # Calculate monthly summaries for the rainfall column
#' summary_data <- daily_niger %>% dplyr::group_by(year, month, station_name) %>%
#'      dplyr::summarise(mean_rain = mean(rain))
#' export_geoclim_month(data = summary_data, year = "year", month = "month",
#'                      station_id = "station_name",
#'                      element = "mean_rain", metadata = stations_niger,
#'                      join_by = "station_name",
#'                      latitude = "lat", longitude = "long")
export_geoclim_month <- function(data, year, month, element,  station_id,
                                 latitude, longitude, metadata = NULL,
                                 join_by = NULL, add_cols = NULL, 
                                 file_path = paste0("GEOCLIM-", element, ".csv"),
                                 ...) {
  checkmate::check_string(file)
  geomclim_data <- 
    prepare_geoclim_month(data = data, year = year, month = month,
                          element = element, station_id = station_id,
                          latitude = latitude, longitude = longitude,
                          metadata = metadata, join_by = join_by, 
                          add_cols = add_cols)
  csv_params <- utils::modifyList(list(x = geomclim_data,
                                file = file_path,
                                row.names = FALSE), 
                           list(...))
  do.call("write.csv", csv_params)
  message("GEOCLIM data saved at: '", file_path, "'")
  invisible(file_path)
}