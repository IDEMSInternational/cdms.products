#' Export dekad or pentad data in GeoCLIM format
#' 
#' @description `export_geoclim` rearranges a data frame using `prepare_geoclim` to a
#' format suitable for use in GeoCLIM. The data frame is then written to a
#' file or connection.
#' 
#' @inheritParams prepare_geoclim
#' @param file_path \code{character(1)} A character specifying the file path and file name to export.
#' @param ... Other parameters passed to \code{write.csv()}
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
#' #export_geoclim(data = summary_data, year = "year",
#' #                station_id = "station_name",
#' #                type_col = "dekad",
#' #                element = "mean_rain", metadata = stations_niger, 
#' #                join_by = "station_name",
#' #                latitude = "lat", longitude = "long")

export_geoclim <- function(data, year, type_col, element, station_id, 
                           latitude, longitude, type = c("dekad", "pentad"),
                           metadata = NULL, join_by = NULL, add_cols = NULL,
                           file_path = paste0("GEOCLIM-", element, ".csv"),
                           ...) {
  checkmate::check_string(file)
  geomclim_data <- 
    prepare_geoclim(data = data, year = year, type_col = type_col, 
                    element = element, station_id = station_id, 
                    latitude = latitude, longitude = longitude, type = type, 
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