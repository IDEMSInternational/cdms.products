#' Export dekad or pentad data in GeoCLIM format
#' @inheritParams prepare_geoclim
#' @param file_path 
#' @param ... Other parameters passed to \code{write.csv()}
#'
#' @return Invisibly returns the file path of the saved data
#' @export
#'
#' @examples
export_geoclim <- function(data, year, type = c("dekad", "pentad"),
                           type_col, element, metadata = NULL,
                           join_by = NULL, station_id,
                           latitude, longitude, add_cols = NULL,
                           file_path = paste0("GEOCLIM-", element, ".csv"),
                           ...) {
  checkmate::check_string(file)
  geomclim_data <- 
    prepare_geoclim(data = data, year = year, type = type, 
                    type_col = type_col, element = element, 
                    metadata = metadata, join_by = join_by, 
                    station_id = station_id, latitude = latitude, 
                    longitude = longitude, add_cols = add_cols)
  csv_params <- utils::modifyList(list(x = geomclim_data,
                                       file = file_path,
                                       row.names = FALSE),
                                  list(...))
  do.call("write.csv", csv_params)
  message("GEOCLIM data saved at: '", file_path, "'")
  invisible(file_path)
}