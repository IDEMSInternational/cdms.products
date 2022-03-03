#' Title
#'
#' @param data The data.frame to calculate from.
#' @param date_time The name of the date column in \code{data}.
#' @param elements The name of the column in \code{data} to apply the function to.
#' @param stations The name of the station column in \code{data}, if the data are for multiple station. 
#' The calculations are performed separately for each station.
#' @param start \code{logical(1)} If \code{TRUE} start date as ...
#' @param end  \code{logical(1)} If \code{TRUE} set end date as ...
#'
#' @return
#' @export
#'
#' @examples # TODO
climatic_missing <- function(data, date_time, elements, stations,
                             start = TRUE, end = FALSE) {
  
  if (missing(date_time)) {
    stop('argument "date_time" is missing, with no default')
  }
  
  if (missing(elements)) {
    stop('argument "elements" is missing, with no default')
  }
  
  # stack data
  data.stack <- data %>%
    tidyr::pivot_longer(cols = c({{ elements }}),
                        names_to = "Element",
                        values_to = "value")
  
  # sort start/end times
  
  # set start date_time
  if (start) {
    data.stack <- data.stack %>%
      dplyr::group_by({{ stations }}, .data$Element) %>%
      dplyr::mutate(start = ({{ date_time }})[which.min(is.na( .data$value ))])
    
  } else {
    data.stack <- data.stack %>%
      dplyr::group_by({{ stations }}) %>%
      dplyr::mutate(start = dplyr::first( {{ date_time }} ))
  }
  
  # set end date_time
  if (end) {
    data.stack <- data.stack %>%
      dplyr::group_by({{ stations }}, .data$Element ) %>%
      dplyr::mutate(end = ({{ date_time }} )[dplyr::last(which(!is.na( .data$value )))])
  } else {
    data.stack <- data.stack %>%
      dplyr::group_by({{ stations }} ) %>%
      dplyr::mutate(end = dplyr::last({{ date_time }}))
  }
  
  # number and percentage missing
  summary.data <- data.stack %>%
    dplyr::group_by({{ stations }}, .data$Element) %>%
    dplyr::filter(({{ date_time }}) >= start & ({{ date_time }}) <= end) %>%
    dplyr::summarise(From = dplyr::first(start),
                     To = dplyr::last(end),
                     Missing = sum(is.na(.data$value)),
                     `%` = round(sum(is.na(.data$value))/dplyr::n()*100, 1))
  
  # complete years
  complete.years <- data.stack %>%
    dplyr::group_by({{ stations }}) %>%
    dplyr::filter(({{ date_time }}) >= start & ({{ date_time }}) <= end) %>%
    dplyr::group_by(lubridate::year({{ date_time }}), {{ stations }}, .data$Element) %>%
    dplyr::summarise(count = sum(is.na(.data$value)))
  complete.years <- complete.years %>%
    dplyr::group_by({{ stations }}, .data$Element) %>%
    dplyr::summarise(Full_Years = sum(.data$count == 0))
  
  
  # bind together
  summary.data <- merge(summary.data, complete.years)
  
  if (missing(stations)) {
    summary.data$stations <- NULL
  }
  
  return(summary.data)
}  