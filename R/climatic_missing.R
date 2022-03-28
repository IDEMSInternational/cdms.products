#' Summarise missing data in a data frame
#'
#' @param data The data.frame to calculate from.
#' @param date_time The name of the date column in \code{data}.
#' @param elements The name of the column in \code{data} to apply the function to.
#' @param station_id The name of the station column in \code{data}, if the data are for multiple station. 
#' The calculations are performed separately for each station.
#' @param start \code{logical(1)} If \code{TRUE} start date as ...
#' @param end \code{logical(1)} If \code{TRUE} set end date as ...
#'
#' @return Data frame summarising the missing data
#' @export
#'
#' @examples 
#' # Summarising missing data in the rainfall, temperature, and sun columns
#' climatic_missing(data = daily_niger, date_time = "date", 
#'                  elements = c("rain", "tmax", "tmin", "sunh"),
#'                  station_id = "station_name")
#' 
climatic_missing <- function(data, date_time, elements, station_id = NULL,
                             start = TRUE, end = FALSE) {
  checkmate::assert_data_frame(data)
  assert_column_names(data, date_time)
  checkmate::assert_string(date_time)
  checkmate::assert(checkmate::check_date(data[[date_time]]), 
                    checkmate::check_posixct(data[[date_time]]))
  checkmate::assert_character(elements)
  assert_column_names(data, elements)
  if (!is.null(station_id)) assert_column_names(data, station_id)
  if (!is.null(station_id)) checkmate::assert_character(station_id)
  checkmate::assert_logical(start)
  checkmate::assert_logical(end)
  
  if(is.null(station_id)) {
    station_id <- "station_id"
    data[[station_id]] <- 1
    created_station <- TRUE
  } else {
    created_station <- FALSE
  }
  
  data_longer <- data %>% 
    tidyr::pivot_longer(cols = tidyselect::all_of(elements), 
                        names_to = "element",
                        values_to = "value")
  if (start) {
    data_longer <- data_longer %>%
      dplyr::group_by(.data[[station_id]], .data$element) %>%
      dplyr::mutate(start = (data[[date_time]])[which.min(is.na(.data$value))])
    
  } else {
    data_longer <- data_longer %>%
      dplyr::group_by(.data[[station_id]]) %>%
      dplyr::mutate(start = dplyr::first(data[[date_time]]))
  }
  
  if (end) {
    data_longer <- data_longer %>%
      dplyr::group_by(.data[[station_id]], .data$element) %>%
      dplyr::mutate(end = (data[[date_time]])[dplyr::last(which(!is.na(.data$value)))])
  } else {
    data_longer <- data_longer %>%
      dplyr::group_by(.data[[station_id]]) %>%
      dplyr::mutate(end = dplyr::last(data[[date_time]]))
  }
  
  # number and percentage missing
  summary.data <- data_longer %>%
    dplyr::group_by(.data[[station_id]], .data$element) %>%
    dplyr::filter((.data[[date_time]]) >= start & (.data[[date_time]]) <= end) %>%
    dplyr::summarise(from = dplyr::first(start),
                     to = dplyr::last(end),
                     missing = sum(is.na(.data$value)),
                     `%` = round(sum(is.na(.data$value))/dplyr::n()*100, 1))
  # complete years
  complete.years <- data_longer %>%
    dplyr::group_by(.data[[station_id]]) %>%
    dplyr::filter((.data[[date_time]]) >= start & (.data[[date_time]]) <= end) %>%
    dplyr::group_by(lubridate::year(.data[[date_time]]), .data[[station_id]], .data$element) %>%
    dplyr::summarise(count = sum(is.na(.data$value)))
  complete.years <- complete.years %>%
    dplyr::group_by(.data[[station_id]], .data$element) %>%
    dplyr::summarise(full_years = sum(.data$count == 0))

  # bind together
  summary.data <- merge(summary.data, complete.years)
  
  if(created_station) {
    summary.data <- summary.data %>% dplyr::select(-.data[[station_id]])
  }
  
  return(summary.data)
}