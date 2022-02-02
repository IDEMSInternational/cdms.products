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
#'
#' @return
#' @export
#'
#' @examples # TODO
inventory_table <- function(data, date, elements, station = NULL, year = NULL, month = NULL,  
                            day = NULL) {
  
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
  
  if(is.null(station)){
    inventory_data <- data %>%
      tidyr::pivot_longer(cols = all_of(elements), names_to = "element") %>%
      dplyr::select(c(.data[[year]], .data[[month]], .data[[day]], .data$element, .data$value)) %>%
      dplyr::mutate(value = ifelse(is.na(.data$value), "M", "X")) %>% 
      tidyr::pivot_wider(id_cols = c(.data$element, .data[[year]], .data[[month]]),
                         names_from = .data[[day]],
                         values_from = .data$value) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Available = sum(c(`1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, `11`, `12`, `13`, `14`, `15`,
                                      `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`, `26`, `27`, `28`,
                                      `29`, `30`, `31`) == "X", na.rm = TRUE)) %>%
      dplyr::mutate(Missing = sum(c(`1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, `11`, `12`, `13`, `14`, `15`,
                                    `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`, `26`, `27`, `28`,
                                    `29`, `30`, `31`) == "M", na.rm = TRUE)) 
  } else {
    inventory_data <- data %>%
        tidyr::pivot_longer(cols = all_of(elements), names_to = "element") %>%
        dplyr::select(c(.data[[year]], .data[[month]], .data[[day]], .data[[station]], .data$element, .data$value)) %>%
      dplyr::mutate(value = ifelse(is.na(.data$value), "M", "X")) %>% 
      tidyr::pivot_wider(id_cols = c(.data[[station]], .data$element, .data[[year]], .data[[month]]),
                         names_from = .data[[day]],
                         values_from = .data$value) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Available = sum(c(`1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, `11`, `12`, `13`, `14`, `15`,
                                      `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`, `26`, `27`, `28`,
                                      `29`, `30`, `31`) == "X", na.rm = TRUE)) %>%
      dplyr::mutate(Missing = sum(c(`1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, `11`, `12`, `13`, `14`, `15`,
                                    `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`, `26`, `27`, `28`,
                                    `29`, `30`, `31`) == "M", na.rm = TRUE)) 
  }
  return(inventory_data)
}
