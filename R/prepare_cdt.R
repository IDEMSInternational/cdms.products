#' Prepare data in format for CDT
#' 
#' @description `prepare_cdt` rearranges a data frame to a
#' format suitable for use in CDT. This data frame can be rearranged and
#' exported to a file or connection with `export_CDT`.
#'
#' @param data \code{data.frame} data.frame of daily or dekadal climatic data in tidy format i.e.
#'   one row per time point (per station) and one column per element.
#' @param station \code{character(1)} Name of the station identifying column in \code{data}.
#' @param element \code{character(1)} Name of the element column in \code{data}.
#' @param latitude \code{character(1)} Name of the latitude column in \code{metadata}.
#' @param longitude \code{character(1)} Name of the longitude column in \code{metadata}.
#' @param altitude \code{character(1)} Name of the altitude column in \code{metadata}.
#' @param type \code{character(1)} Character indicating the type of data, either \code{"dekad"} or
#'   \code{"daily"}.
#' @param date_time \code{character(1)} Name of the date column in \code{data}. Required if \code{type =
#'   "daily"}. If \code{type = "dekad"} this is only needed if \code{year},
#'   \code{month}, and \code{dekad} are not specified.
#' @param year \code{character(1)} Name of the year column in \code{data}. Only needed if \code{type
#'   = "dekad"}. If \code{NULL} it will be created using
#'   \code{lubridate::year(data[[date_time]])}.
#' @param month \code{character(1)} Name of the month column in \code{data}. Only needed if
#'   \code{type = "dekad"}. If \code{NULL} it will be created using
#'   \code{lubridate::month(data[[date_time]])}.
#' @param dekad \code{character(1)} Name of the dekad column in \code{data}. Only needed if
#'   \code{type = "dekad"}. If \code{NULL} it will be created using \code{dekad}
#'   function.
#' @param metadata \code{data.frame} data.frame of station metadata. Use this is the station
#'   details are in a separate data.frame with one row per station. If
#'   specified, \code{latitude}, \code{longitude} and \code{altitude} are
#'   assumed to be in \code{metadata} and \code{station} must be in both
#'   \code{data} and \code{metadata} to facilitate joining.
#'
#' @return A data.frame formatted for use in CDT
#' @export
#'
#' @examples # Create summary daily data
#' data("daily_niger"); data("stations_niger")
#' cdt_data <- prepare_cdt(data = daily_niger, station = "station_name", element = "rain",
#'                         type = "daily", date_time = "date", latitude = "lat",
#'                         longitude = "long", altitude = "alt", metadata = stations_niger)
prepare_cdt <- function(data, station, element, latitude, longitude, 
                        altitude, type = c("dekad", "daily"), date_time = NULL, 
                        year = NULL, month = NULL, dekad = NULL, 
                        metadata = NULL) {
  
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
  if (!is.null(date_time)) assert_column_names(data, date_time)
  if (type == "daily") checkmate::assert_date(data[[date_time]])
  if (type == "dekad") {
    if (is.null(date_time)){
      checkmate::assert_string(year)
      checkmate::assert_string(month)
      checkmate::assert_string(dekad)
    } else {
      checkmate::assert_date(data[[date_time]])
    }
  }
  
  if (type == "dekad") {
    date_time <- "date"
    if (is.null(date_time)){
      data[[date_time]] <- paste0(.data[[year]], .data[[month]], .data[[dekad]])
    } else {
      year <- lubridate::year(data[[date_time]])
      month <- lubridate::month(data[[date_time]])
      dekad <- dekad(data[[date_time]])
      data[[date_time]] <- paste0(year, month, dekad)
    }
  }
  
  data[[date_time]] <- as.character(data[[date_time]])
  data_date <- data %>%
    dplyr::ungroup() %>%
    dplyr::select(c(.data[[station]], .data[[date_time]], .data[[element]])) %>%
    tidyr::pivot_longer(cols = c(.data[[date_time]]),
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
