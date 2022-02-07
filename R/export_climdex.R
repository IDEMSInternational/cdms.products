#' Export ClimDex data
#' 
#' @param data The data.frame to calculate from
#' @param date The name of the date column in \code{data}. This is only needed if \code{year}, \code{month}, and \code{day} are not specified.
#' @param year The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date]])}.
#' @param month The name of the month column in \code{data}. If \code{NULL} it will be created using \code{lubridate::month(data[[date]])}.
#' @param day The name of the day column in \code{data}. If \code{NULL} it will be created using \code{lubridate::day(data[[date]])}.
#' @param prcp The name of the rainfall column in \code{data}.
#' @param tmax The name of the maximum temperature column in \code{data}.
#' @param tmin The name of the minimum temperature column in \code{data}.
#' @param file_type Whether to convert to \code{csv} or `\code{txt} format.
#' @param file_path Default name of the file
#' @param ... Other parameters passed to \code{write.csv()}
#'
#' @return Invisibly returns the file path of the saved data
#' @export
#'
#' @examples # TODO
#' 
export_climdex <- function(data, date = NULL, year = NULL, month = NULL, day = NULL, prcp, tmax, tmin,
                           file_type = c("csv", "txt"),
                           file_path = paste0("climdex-", Sys.Date()),
                           ...) {
  checkmate::assert_data_frame(data)
  checkmate::assert_character(prcp)
  assert_column_names(data, prcp)
  checkmate::assert_character(tmax)
  assert_column_names(data, tmax)
  checkmate::assert_character(tmin)
  assert_column_names(data, tmin)
  if (!is.null(date)) assert_column_names(data, date)
  if (!is.null(year)) assert_column_names(data, year)
  if (!is.null(month)) assert_column_names(data, month)
  if (!is.null(day)) assert_column_names(data, day)
  checkmate::check_string(file)
  file_type <- match.arg(file_type)
  
  if(is.null(year)) {
    year <- "year"
    data[[year]] <- lubridate::year(data[[date]])
  }
  if(is.null(month)) {
    month <- ".month"
    data[[month]] <- lubridate::month(data[[date]])
  }
  if(is.null(day)) {
    day <- ".day"
    data[[day]] <- lubridate::day(data[[date]])
  }
  
  file_path <- paste(file_path, file_type, sep = ".")
  climdex_data <- data %>%
    dplyr::select(c(.data[[year]], .data[[month]], .data[[day]], .data[[prcp]], .data[[tmax]], .data[[tmin]]))
  
  file_params <- utils::modifyList(list(x = climdex_data,
                                        file = file_path,
                                        row.names = FALSE),
                                   list(...))
  
  if (file_type == "csv"){
    do.call("write.csv", file_params)
  } else {
    do.call("write.table", file_params)
  }
  message("ClimDex data saved at: '", file_path, "'")
  invisible(file_path)
}
