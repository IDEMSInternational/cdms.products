#' Export daily or dekadal data in the format for CDT
#' 
#' @description `export_cdt` rearranges a data frame using `prepare_cdt` to a
#' format suitable for use in CDT. The data frame is then written to a
#' file or connection.
#' 
#' @inheritParams prepare_cdt
#' @param file_path \code{character(1)} A character specifying the file path and file name to export.
#' @param ... Other parameters passed to \code{write.csv()}.
#'
#' @return Invisibly returns the file path of the saved data.
#' @export
#'
#' @examples # To write daily_niger data to CDT format
#' # NOT RUN:
#' #export_cdt_daily(data = daily_niger, station = "station_name", element = "rain", type = "daily",
#' #           date_time = "date", latitude = "lat", longitude = "long", altitude = "alt",
#' #           metadata = stations_niger)
#'
#' # To write daily_niger data to CDT format for dekad data
#' # first put daily_niger data into a dekad format
#' summary_data <- daily_niger %>%
#'                     dplyr::mutate(dekad_date = dekad(daily_niger$date)) %>%
#'                     dplyr::group_by(station_name, year, dekad_date) %>%
#'                     dplyr::summarise(date = dplyr::first(date), sum_tmax = sum(tmax))
#' # NOT RUN:
#' # export_cdt(data = summary_data, station = "station_name", element = "sum_tmax", type = "dekad",
#' #            date_time = "date", latitude = "lat", longitude = "long", altitude = "alt",
#' #            dekad = "dekad_date", metadata = stations_niger)
export_cdt <- function(data, station, element, latitude, longitude, altitude,
                       type = c("dekad", "daily"), date_time = NULL, year = NULL, 
                       month = NULL, dekad = NULL, metadata = NULL,
                       file_path = paste0("CDT-", element, ".csv"),
                       ...) {
  checkmate::check_string(file_path)
  cdt_data <- prepare_cdt(data = data, station = station, element = element, 
                          latitude = latitude, longitude = longitude,
                          altitude = altitude, type = type, date_time = date_time, 
                          year = year, month = month, dekad = dekad,
                          metadata = metadata)
  csv_params <- utils::modifyList(list(x = cdt_data,
                                       file = file_path,
                                       row.names = FALSE),
                                  list(...))
  do.call("write.csv", csv_params)
  message("CDT data saved at: '", file_path, "'")
  invisible(file_path)
}
