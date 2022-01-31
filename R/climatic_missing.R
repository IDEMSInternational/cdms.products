#' Title
#'
#' @param data The data.frame to calculate from.
#' @param date The name of the date column in \code{data}.
#' @param elements The name of the column in \code{data} to apply the function to.
#' @param stations The name of the station column in \code{data}, if the data are for multiple station. 
#' The calculations are performed separately for each station.
#' @param start A logical value. If \code{TRUE} start date as ...
#' @param end  A logical value. If \code{TRUE} set end date as ...
#'
#' @return
#' @export
#'
#' @examples
climatic_missing <- function(data, date, elements, stations,
                             start = TRUE, end = FALSE){
  
  
  if (missing(date)){
    stop('argument "date" is missing, with no default')
  }
  
  if (missing(elements)){
    stop('argument "elements" is missing, with no default')
  }
  
  # stack data
  data.stack <- data %>%
    tidyr::pivot_longer(cols = c({{ elements }}),
                        names_to = "Element",
                        values_to = "value")
  
  # sort start/end times
  
  # set start date
  if (start){
    data.stack <- data.stack %>%
      dplyr::group_by({{ stations }}, .data$Element) %>%
      dplyr::mutate(start = ({{ date }})[which.min(is.na( .data$value ))])
    
  }else{
    data.stack <- data.stack %>%
      dplyr::group_by({{ stations }}) %>%
      dplyr::mutate(start = dplyr::first( {{ date }} ))
  }
  
  # set end date
  if (end){
    data.stack <- data.stack %>%
      dplyr::group_by({{ stations }}, .data$Element ) %>%
      dplyr::mutate(end = ({{ date }} )[dplyr::last(which(!is.na( .data$value )))])
  }else{
    data.stack <- data.stack %>%
      dplyr::group_by({{ stations }} ) %>%
      dplyr::mutate(end = dplyr::last({{ date }}))
  }
  
  # number and percentage missing
  summary.data <- data.stack %>%
    dplyr::group_by({{ stations }}, .data$Element) %>%
    dplyr::filter(({{ date }}) >= start & ({{ date }}) <= end) %>%
    dplyr::summarise(From = dplyr::first(start),
                     To = dplyr::last(end),
                     Missing = sum(is.na(.data$value)),
                     `%` = round(sum(is.na(.data$value))/dplyr::n()*100, 1))
  
  # complete years
  complete.years <- data.stack %>%
    dplyr::group_by({{ stations }}) %>%
    dplyr::filter(({{ date }}) >= start & ({{ date }}) <= end) %>%
    dplyr::group_by(lubridate::year({{ date }}), {{ stations }}, .data$Element) %>%
    dplyr::summarise(count = sum(is.na(.data$value)))
  complete.years <- complete.years %>%
    dplyr::group_by({{ stations }}, .data$Element) %>%
    dplyr::summarise(Full_Years = sum(.data$count == 0))
  
  
  # bind together
  summary.data <- merge(summary.data, complete.years)
  
  if (missing(stations)){
    summary.data$stations <- NULL
  }
  
  return(summary.data)
}  