#' Title
#'
#' @param data The data.frame to calculate from
#' @param station The name of the station column in \code{data}, if the data are for multiple station. 
#' The summary table is calculated separately for each station.
#' @param elements The name of the columns in \code{data} to apply the summary calculation to.
#'
#' @return
#' @export
#'
#' @examples
#' data(daily_niger)
#' summarise_inventory_data(data = daily_niger, station = "station_name", elements = c("tmax", "tmin"))
summarise_inventory_data <- function(data, station, elements){
  checkmate::assert_data_frame(data)
  checkmate::assert_character(elements)
  checkmate::assert_character(station, null.ok = TRUE)

  data_longer <- data %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(elements), names_to = "elements_list")
  
  if (is.null(station)){
    data_summary <- data_longer %>%
      dplyr::group_by(.data$elements_list)
    } else {
    data_summary <- data_longer %>%
      dplyr::group_by(.data[[station]], .data$elements_list)
    }

  data_summary <- data_summary %>%
    dplyr::summarise(total_missing = sum(is.na(.data$value)),
                     percentage = .data$total_missing/dplyr::n() * 100)
  
  return(data_summary)
}
