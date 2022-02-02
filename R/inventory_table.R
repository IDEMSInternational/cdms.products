#' Inventory Table
#'
#' @param data The data.frame to calculate from
#' @param date The name of the date column in \code{data}. This is only needed if \code{year}, \code{month}, and \code{day} are not specified.
#' @param elements The name of the columns in \code{data} to apply the inventory calculation to.
#' @param station The name of the station column in \code{data}, if the data are for multiple station. 
#' The inventory table is calculated separately for each station.
#' @param year The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date]])}.
#' @param month The name of the month column in \code{data}. If \code{NULL} it will be created using \code{lubridate::month(data[[date]])}.
#' @param day The name of the day column in \code{data}. If \code{NULL} it will be created using \code{lubridate::day(data[[date]])}.
#' @param missing_indicator Indicator to give if the data is missing. Default \code{"M"}.
#' @param observed_indicator Indicator to give if the data is observed. Default \code{"X"}.
#'
#' @return
#' @export
#'
#' @examples # TODO
inventory_table <- function(data, date, elements, station = NULL, year = NULL, month = NULL,  
                            day = NULL, missing_indicator = "M", observed_indicator = "X") {
  
  #checkmate::assert_data_frame(data)
  #assert_column_names(data, date)
  #checkmate::assert_string(date)
  #checkmate::assert_date(data[[date]])
  #checkmate::assert_character(elements)
  #assert_column_names(data, elements)
  #if (!is.null(station)) assert_column_names(data, station)
  #if (!is.null(year)) assert_column_names(data, year)
  #if (!is.null(month)) assert_column_names(data, month)
  #if (!is.null(day)) assert_column_names(data, day)
  
  if(is.null(year)) {
    year <- ".year"
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
        group_by(.data$element, .data[[year]], .data[[month]])
    } else {
      summary_data <- inventory_data %>%
        group_by(.data[[station]], .data$element, .data[[year]], .data[[month]])
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
