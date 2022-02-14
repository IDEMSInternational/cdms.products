#' Export ClimDex data
#' 
#' @inheritParams prepare_climdex
#' @param file_type Whether to convert to \code{csv} or `\code{txt} format.
#' @param file_path Default name of the file
#' @param ... Other parameters passed to \code{write.csv()}
#'
#' @return Invisibly returns the file path of the saved data
#' @export
#'
#' @examples # TODO
#' 
export_climdex <- function(data, prcp, tmax, tmin, date = NULL, year = NULL, 
                           month = NULL, day = NULL, 
                           file_type = c("csv", "txt"),
                           file_path = paste0("climdex-", Sys.Date()),
                           ...) {
  checkmate::check_string(file)
  file_type <- match.arg(file_type)
  
  climdex_data <- prepare_climdex(data = data, prcp = prcp, tmax = tmax, 
                                  tmin = tmin, date = date, year = year, 
                                  month = month, day = day)
  
  file_path <- paste(file_path, file_type, sep = ".")

  if (file_type == "csv"){
    file_params <- utils::modifyList(list(x = climdex_data,
                                          file = file_path,
                                          row.names = FALSE,
                                          col.names = FALSE,
                                          sep=","),
                                     list(...))
  } else {
    file_params <- utils::modifyList(list(x = climdex_data,
                                          file = file_path,
                                          row.names = FALSE,
                                          col.names = FALSE),
                                     list(...))
  }
  do.call("write.table", file_params)
  message("ClimDex data saved at: '", file_path, "'")
  invisible(file_path)
}
