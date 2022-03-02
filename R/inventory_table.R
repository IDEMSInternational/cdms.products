#' Inventory Table
#'
#' @param data \code{data.frame} The data.frame to calculate from.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param elements \code{character} The name of the elements column in \code{data} to apply the function to.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' The inventory table is calculated separately for each station.
#' @param year \code{character(1)} The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param month \code{character(1)} The name of the month column in \code{data}. If \code{NULL} it will be created using \code{lubridate::month(data[[date_time]])}.
#' @param day \code{character(1)} The name of the day column in \code{data}. If \code{NULL} it will be created using \code{lubridate::day(data[[date_time]])}.
#' @param missing_indicator \code{character(1)} Indicator to give if the data is missing. Default \code{"M"}.
#' @param observed_indicator \code{character(1)} Indicator to give if the data is observed. Default \code{"X"}.
#'
#' @return
#' @export
#'
#' @examples # TODO
inventory_table <- function(data, date_time, elements, station = NULL, year = NULL, month = NULL,  
                            day = NULL, missing_indicator = "M", observed_indicator = "X") {
  
  checkmate::assert_data_frame(data)
  checkmate::assert_string(date_time)
  checkmate::assert_date(data[[date_time]])
  checkmate::assert_character(elements)
  assert_column_names(data, elements)
  if (!is.null(station)) assert_column_names(data, station)
  if (!is.null(date_time)) assert_column_names(data, date_time)
  if (!is.null(year)) assert_column_names(data, year)
  if (!is.null(month)) assert_column_names(data, month)
  if (!is.null(day)) assert_column_names(data, day)
  
  if(is.null(year)) {
    year <- "year"
    data[[year]] <- lubridate::year(data[[date_time]])
  }
  if(is.null(month)) {
    month <- "month"
    data[[month]] <- lubridate::month(data[[date_time]])
  }
  if(is.null(day)) {
    day <- "day"
    data[[day]] <- lubridate::day(data[[date_time]])
  }
  
  if (is.null(station)){
    selected_cols <- c(year, month)
  } else {
    selected_cols <- c(station, year, month)
  }
  
    inventory_data <- data %>%
        tidyr::pivot_longer(cols = tidyselect::all_of(elements), names_to = "element") %>%
        dplyr::select(c(tidyselect::all_of(selected_cols), .data[[day]], .data$element, .data$value)) %>%
      dplyr::mutate(value = ifelse(is.na(.data$value), missing_indicator, observed_indicator))
    
    if (is.null(station)){
      summary_data <- inventory_data %>%
        dplyr::group_by(.data$element, .data[[year]], .data[[month]])
    } else {
      summary_data <- inventory_data %>%
        dplyr::group_by(.data[[station]], .data$element, .data[[year]], .data[[month]])
    }
    summary_data <- summary_data %>%
      dplyr::summarise(Available = sum(.data$value == observed_indicator),
                       Missing = sum(.data$value == missing_indicator))
    
    inventory_data_wider <- inventory_data %>%
      tidyr::pivot_wider(id_cols = c(tidyselect::all_of(selected_cols), .data$element),
                         names_from = .data[[day]],
                         values_from = .data$value)
  return(dplyr::full_join(inventory_data_wider, summary_data))
}
