#' Calculate extremes from climatic data
#' 
#' @inheritParams climatic_summary
#' 
#' @param max_val Logical, if \code{TRUE} the extreme maximum is calculated
#' @param min_val Logical, if \code{TRUE} the extreme minimum is calculated
#'
#' @return
#' @export
#'
#' @examples # TODO
climatic_extremes <- function(data, date_time, elements, station = NULL,
                              year = NULL, month = NULL, dekad = NULL, 
                              pentad = NULL,
                              to = c("hourly", "daily", "pentad", "dekadal", 
                                     "monthly", "annual-within-year", 
                                     "annual", "longterm-monthly", 
                                     "longterm-within-year", "station",
                                     "overall"),
                              by = NULL,
                              doy = NULL, doy_first = 1, doy_last = 366, 
                              max_val = TRUE, min_val = FALSE,
                              first_date = FALSE, n_dates = FALSE, last_date = FALSE,
                              na_rm = FALSE,
                              na_prop = NULL, na_n = NULL, na_consec = NULL, 
                              na_n_non = NULL, 
                              names = "{.fn}_{.col}") {
  summaries <- c()
  if (max_val) summaries <- c(max = "max")
  if (min_val) summaries <- c(min = "min")
  climatic_summary(data = data, date_time = date_time, station = station, 
                   elements = elements, year = year, month = month, 
                   dekad = dekad, pentad = pentad,
                   to = to, by = by, doy = doy, 
                   doy_first = doy_first, doy_last = doy_last, 
                   summaries = summaries, na_rm = na_rm,
                   na_prop = na_prop, na_n = na_n, na_consec = na_consec, 
                   na_n_non = na_n_non,
                   first_date = first_date, n_dates = n_dates, 
                   last_date = last_date, names = names)
}