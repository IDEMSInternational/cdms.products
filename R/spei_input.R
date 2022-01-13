#' Title
#'
#' @param data The data.frame to calculate from.
#' @param station The name of the station column in \code{data}, if the data are for multiple station.
#' @param year The name of the year column in \code{data}.
#' @param month The name of the month column in \code{data}.
#' @param element The name of the column(s) in \code{data} to apply the condition to.
#'
#' @return
#' @export
#'
#' @examples
spei_input <- function(data, station, year, month, element) {
  if (missing(station)) id_cols <- c(year, month) else id_cols <- c(station, year, month)
  # SPEI package assumes data is ordered so must be sorted
  data_sort <- data %>% dplyr::arrange(!!! rlang::syms(id_cols))
  data <- data_sort
  # There should be a better way to check this.
  if (!all(data == data_sort, na.rm = TRUE)) stop("data must be sorted by (", paste(id_cols, collapse = ", "), ") for SPEI/SPI to be calculated correctly.")
  # Monthly data i.e. one value per month (per station) is required
  if (anyDuplicated(data %>% dplyr::select(!!! rlang::syms(id_cols)))) stop("Multiple values per month detected. SPEI/SPI requires monthly data.")
  if (!missing(station)) {
    for (s in unique(data[[station]])) {
      df <- data %>% dplyr::filter(.data[[station]] == s)
      dates_seq <- seq.Date(from = as.Date(paste(df[[year]][1], as.numeric(df[[month]][1]), 1), format = "%Y %m %d"),
                            to = as.Date(paste(tail(df[[year]], 1), tail(as.numeric(df[[month]]), 1), 1), format = "%Y %m %d"),
                            by = "1 month")
      if (length(dates_seq) != nrow(df)) stop("Less rows than expected. data has gaps for missing months in '", s, "'. SPEI/SPI requires no date gaps.")
    }
  } else {
    dates_seq <- seq.Date(from = as.Date(paste(data[[year]][1], as.numeric(data[[month]][1]), 1), format = "%Y %m %d"),
                          to = as.Date(paste(tail(data[[year]], 1), tail(as.numeric(data[[month]]), 1), 1), format = "%Y %m %d"),
                          by = "1 month")
    if (length(dates_seq) != nrow(data)) stop("Less rows than expected. data has gaps for missing months. SPEI/SPI requires no date gaps.")
  }
  cols <- c(id_cols, element)
  start <- c(data[[year]][1], data[[month]][1])
  # If multiple stations, needs to be in "wide" format for SPEI
  if (!missing(station)) {
    ts_data <- tidyr::pivot_wider(data, id_cols = tidyselect::all_of(c(year, month)), 
                                  names_from = tidyselect::all_of(station), values_from = tidyselect::all_of(element),
                                  values_fill = NA)
    ts_data <- ts_data %>% dplyr::arrange(!!! rlang::syms(c(year, month)))
    # Not sure how to do this using dplyr::select
    ts_data[id_cols] <- NULL
    ts_data <- ts(as.matrix(ts_data), frequency = 12, start = start)
  } else {
    ts_data <- ts(as.matrix(data[[element]]), frequency = 12, start = start)
  }
  ts_data
}
