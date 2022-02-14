#' Export data in CDT format for daily data
#' 
#' @inheritParams export_cdt
#' @param ... Other parameters passed to \code{write.csv()}
#'
#' @return Invisibly returns the file path of the saved data
#' @export
#'
#' @examples # TODO
#' 
#' 
#' 
export_cdt_daily <- function(data, station, element, date, latitude, longitude,
                             altitude, metadata = NULL,
                             file_path = paste0("CDT-", element, ".csv"),
                             ...) {
  export_cdt(data = data, station = station, element = element, 
             latitude = latitude, longitude = longitude, altitude = altitude, 
             type = "daily", date = date, metadata = metadata, 
             file_path = file_path)
}