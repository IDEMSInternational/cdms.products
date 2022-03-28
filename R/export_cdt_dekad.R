#' Export dekad data in CDT format
#' 
#' @inheritParams prepare_cdt_dekad
#' @inheritParams export_cdt
#' @param ... Other parameters passed to \code{write.csv()}
#'
#' @return Invisibly returns the file path of the saved data
#' @export
#'
#' @examples # To write daily_niger data to CDT format
#' # first put daily_niger data into a dekad format
#' data(daily_niger)
#' summary_data <- daily_niger %>%
#'                    dplyr::mutate(dekad_date = dekad(daily_niger$date)) %>%
#'                    dplyr::group_by(station_name, year, dekad_date) %>%
#'                    dplyr::summarise(date = dplyr::first(date), sum = sum(tmax))
#' # NOT RUN:
#' #export_cdt_dekad(data = daily_niger, station = "station_name", element = "rain",
#' #           date_time = "date", latitude = "lat", longitude = "long", altitude = "alt",
#' #           metadata = stations_niger)
export_cdt_dekad <- function(data, station, element, date_time, latitude, 
                             longitude, altitude, year = NULL, month = NULL, 
                             dekad = NULL, metadata = NULL,
                             file_path = paste0("CDT-", element, ".csv"),
                             ...) {
  export_cdt(data = data, station = station, element = element, 
             latitude = latitude, longitude = longitude, altitude = altitude, 
             type = "dekad", date_time = date_time, year = year, month = month, 
             dekad = dekad, metadata = metadata, file_path = file_path)
}