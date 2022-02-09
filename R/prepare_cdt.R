#' Prepare CDT
#'
#' @param data The data.frame to calculate from.
#' @param station The name of the station column in \code{data}.
#' @param element The name of the element column in \code{data}.
#' @param type Whether \code{dekad} or \code{daily} data is used.
#' @param date The name of the date column in \code{data}. Required if \code{type = "daily"}. If \code{type = "dekad"} this is only needed if \code{year}, \code{month}, and \code{dekad} are not specified.
#' @param year The name of the year column in \code{data}. Only needed if \code{type = "dekad"}. If \code{NULL} it will be created using \code{lubridate::year(data[[date]])}.
#' @param month The name of the month column in \code{data}. Only needed if \code{type = "dekad"}. If \code{NULL} it will be created using \code{lubridate::month(data[[date]])}.
#' @param dekad The name of the dekad column in \code{data}. Only needed if \code{type = "dekad"}. If \code{NULL} it will be created using \code{dekad} function.
#' @param metadata The metadata data.frame.
#' @param latitude The name of the latitude column in \code{metadata}.
#' @param longitude The name of the longitude column in \code{metadata}.
#' @param altitude The name of the altitude column in \code{metadata}.
#'
#' @return
#' @export
#'
#' @examples
#' # create summary data
#' data("daily_niger"); data("stations_niger")
#' summary_data <- daily_niger %>%
#' dplyr::mutate(dekad_date = dekad(daily_niger$date)) %>%
#' dplyr::group_by(station_name, year, dekad_date) %>%
#' dplyr::summarise(date = dplyr::first(date), sum = sum(tmax))
#' 
#' prepare_cdt(data = summary_data, date = "date", year = "year",
#'             station = "station_name",
#'             element = "sum", metadata = stations_niger, 
#'             latitude = "lat", longitude = "long", altitude = "alt")


prepare_cdt <- function(data, station, element, type = c("dekad", "daily"),
                        date = NULL, year = NULL, month = NULL, dekad = NULL, 
                        metadata = NULL, latitude, longitude, altitude) {
  
  checkmate::assert_data_frame(data)
  checkmate::assert_string(element)
  assert_column_names(data, element)
  checkmate::assert_data_frame(metadata, null.ok = TRUE)
  checkmate::assert_string(station)
  checkmate::assert_string(latitude)
  checkmate::assert_string(longitude)
  checkmate::assert_string(altitude)
  type <- match.arg(type)
  # data_with_meta is whichever data.frame has the metadata columns in
  if (is.null(metadata)) {
    data_with_meta <- data
  } else {
    data_with_meta <- metadata
  }
  assert_column_names(data_with_meta, station)
  assert_column_names(data_with_meta, latitude)
  assert_column_names(data_with_meta, longitude)
  assert_column_names(data_with_meta, altitude)
  if (!is.null(year)) assert_column_names(data, year)
  if (!is.null(month)) assert_column_names(data, month)
  if (!is.null(dekad)) assert_column_names(data, dekad)
  if (!is.null(date)) assert_column_names(data, date)
  if (type == "daily") checkmate::assert_date(data[[date]])
  if (type == "dekad") {
    if (is.null(date)){
      checkmate::assert_string(year)
      checkmate::assert_string(month)
      checkmate::assert_string(dekad)
    } else {
      checkmate::assert_date(data[[date]])
    }
  }
  
  if (type == "dekad") {
    date <- "date"
    if (is.null(date)){
      data[[date]] <- paste0(.data[[year]], .data[[month]], .data[[dekad]])
    } else {
      year <- lubridate::year(data[[date]])
      month <- lubridate::month(data[[date]])
      dekad <- dekad(data[[date]])
      data[[date]] <- paste0(year, month, dekad)
    }
  }
  
  data[[date]] <- as.character(data[[date]])
  data_date <- data %>%
    dplyr::ungroup() %>%
    dplyr::select(c(.data[[station]], .data[[date]], .data[[element]])) %>%
    tidyr::pivot_longer(cols = c(.data[[date]]),
                        names_to = ".x", values_to = "names") %>%
    dplyr::select(-c(.data$`.x`)) %>%
    dplyr::rename(values = .data[[element]])
  
  data_meta <- data_with_meta %>%
    dplyr::select(c(.data[[station]], .data[[latitude]], .data[[longitude]], .data[[altitude]])) %>%
    tidyr::pivot_longer(cols = c(.data[[latitude]], .data[[longitude]], .data[[altitude]]),
                        names_to = "names", values_to = "values")

  cdt_data <- dplyr::bind_rows(data_meta, data_date)
  
  cdt_data <- cdt_data %>%
    tidyr::pivot_wider(names_from = .data[[station]], values_from = .data$values)
  return(cdt_data)
}