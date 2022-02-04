#' Export data in CDT format
#' @inheritParams prepare_CDT
#' @param file_path TODO
#' @param ... Other parameters passed to \code{write.csv()}
#'
#' @return Invisibly returns the file path of the saved data
#' @export
#'
#' @examples # TODO
#' 
#' 
#' 
export_CDT <- function(data, date, year, station, element, metadata = NULL,
                           join_by = NULL, latitude, longitude, altitude,
                           file_path = paste0("GEOCLIM-", element, ".csv"),
                           ...) {
  checkmate::check_string(file)
  CDT_data <- 
     prepare_cdt(data = data, date = date, year = year, station = station,
                 element = element, metadata = metadata, join_by = join_by,
                 latitude = latitude, longitude = longitude, altitude = altitude)
  
  csv_params <- utils::modifyList(list(x = CDT_data,
                                       file = file_path,
                                       row.names = FALSE),
                                  list(...))
  do.call("write.csv", csv_params)
  message("CDT data saved at: '", file_path, "'")
  invisible(file_path)
}
