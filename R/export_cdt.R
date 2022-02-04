#' Export data in CDT format
#' @inheritParams prepare_cdt
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
export_cdt <- function(data, date, year, station, element, metadata = NULL,
                           join_by = NULL, latitude, longitude, altitude,
                           file_path = paste0("CDT-", element, ".csv"),
                           ...) {
  checkmate::check_string(file)
  cdt_data <- 
     prepare_cdt(data = data, date = date, year = year, station = station,
                 element = element, metadata = metadata, join_by = join_by,
                 latitude = latitude, longitude = longitude, altitude = altitude)
  
  csv_params <- utils::modifyList(list(x = cdt_data,
                                       file = file_path,
                                       row.names = FALSE),
                                  list(...))
  do.call("write.csv", csv_params)
  message("CDT data saved at: '", file_path, "'")
  invisible(file_path)
}
