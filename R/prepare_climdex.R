#' Prepare data in the format for RClimDex
#' 
#' @param data data.frame of daily climatic data in tidy format i.e. one row per
#'   day and one column per element.
#' @param prcp Name of the precipitation/rainfall column in \code{data}.
#' @param tmax Name of the maximum temperature column in \code{data}.
#' @param tmin Name of the minimum temperature column in \code{data}.
#' @param date Name of the date column in \code{data}. This is only needed if
#'   \code{year}, \code{month}, and \code{day} are not specified.
#' @param year Name of the year column in \code{data}. If \code{NULL} it will be
#'   created using \code{lubridate::year(data[[date]])}.
#' @param month Name of the month column in \code{data}. If \code{NULL} it will
#'   be created using \code{lubridate::month(data[[date]])}.
#' @param day Name of the day of the month column in \code{data}. If \code{NULL}
#'   it will be created using \code{lubridate::day(data[[date]])}.
#'
#' @return A data.frame formatted for use in RClimDex
#' @export
#'
#' @examples # Preparing the daily_niger data for export to ClimDex
#' prepare_climdex(data = daily_niger, date = "date", prcp = "rain", tmax = "tmax", tmin = "tmin")
#' 
prepare_climdex <- function(data, prcp, tmax, tmin, date = NULL, year = NULL,
                            month = NULL, day = NULL, na = -99.9) {
  
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
  
  if(is.null(year)) {
    year <- "year"
    data[[year]] <- lubridate::year(data[[date]])
  }
  if(is.null(month)) {
    month <- "month"
    data[[month]] <- lubridate::month(data[[date]])
  }
  if(is.null(day)) {
    day <- "day"
    data[[day]] <- lubridate::day(data[[date]])
  }
  
  #TODO Add check for missing dates and add rows if needed.
  
  climdex_data <- data %>%
    dplyr::select(c(.data[[year]], .data[[month]], .data[[day]], .data[[prcp]], .data[[tmax]], .data[[tmin]])) %>%
    dplyr::arrange(.data[[year]], .data[[month]], .data[[day]])
  
  replace <- as.list(rep(-99.9, 3))
  names(replace) <- c(prcp, tmax, tmin)
  climdex_data <- climdex_data %>%
    replace_na(replace)

  return(climdex_data)
}
