#' Export daily data in the format for CDT
#' 
#' @inheritParams prepare_cdt_daily
#' @inheritParams export_cdt
#' @param ... Other parameters passed to \code{write.csv()}
#'
#' @return Invisibly returns the file path of the saved data
#' @export
#'
#' @examples # To write daily_niger data to CDT format
#' ## NOT RUN ##
#' #export_cdt_daily(data = daily_niger, station = "station_name", element = "rain", type = "daily",
#' #           date_time = "date", latitude = "lat", longitude = "long", altitude = "alt",
#' #           metadata = stations_niger)
export_cdt_daily <- function(data, station, element, date_time, latitude, longitude,
                             altitude, metadata = NULL,
                             file_path = paste0("CDT-", element, ".csv"),
                             ...) {
  export_cdt(data = data, station = station, element = element, 
             latitude = latitude, longitude = longitude, altitude = altitude, 
             type = "daily", date_time = date_time, metadata = metadata, 
             file_path = file_path)
}