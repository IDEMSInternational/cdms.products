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
export_cdt_daily <- function(data, station, element, date, metadata = NULL,
                             latitude, longitude, altitude, file_path = paste0("CDT-", element, ".csv"),
                             ...) {
  export_cdt(data = data, station = station, element = element, type = "daily",
             date = date, metadata = metadata, latitude = latitude, longitude = longitude,
             altitude = altitude, file_path = file_path)
}