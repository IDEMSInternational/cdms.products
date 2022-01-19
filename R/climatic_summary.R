#' Title
#'
#' @param data 
#' @param date_time 
#' @param station 
#' @param elements 
#' @param year 
#' @param month 
#' @param dekad 
#' @param pentad 
#' @param to 
#' @param by 
#' @param doy 
#' @param doy_first 
#' @param doy_last 
#' @param summaries 
#' @param na.rm 
#' @param na_prop 
#' @param na_n 
#' @param na_consec 
#' @param na_n_non 
#' @param summaries.params 
#' @param .names 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom rlang .data
climatic_summary <- function(data, date_time, station = NULL, elements, year = NULL, month = NULL, dekad = NULL, pentad = NULL,
                             to = c("hourly", "daily", "pentad", "dekadal", "monthly", "annual-within-year", 
                                    "annual", "longterm-monthly", "longterm-within-year", "station"),
                             by = NULL,
                             doy = NULL, doy_first = 1, doy_last = 366, 
                             summaries, na.rm = FALSE,
                             na_prop = NULL, na_n = NULL, na_consec = NULL, na_n_non = NULL, 
                             summaries.params = list(), names = "{.fn}_{.col}") {
  
  checkmate::assert_data_frame(data)
  assert_column_names(data, date_time)
  checkmate::assert(checkmate::check_date(data[[date_time]]), checkmate::check_posixct(data[[date_time]]))
  checkmate::assert_string(station, null.ok = TRUE)
  if (!is.null(station)) assert_column_names(data, station)
  checkmate::assert_string(year, null.ok = TRUE)
  if (!is.null(year)) assert_column_names(data, year)
  checkmate::assert_string(dekad, null.ok = TRUE)
  if (!is.null(dekad)) assert_column_names(data, dekad)
  checkmate::assert_string(pentad, null.ok = TRUE)
  if (!is.null(dekad)) assert_column_names(data, pentad)
  if (!is.null(by)) assert_column_names(data, by)

  to <- match.arg(to)
  checkmate::assert_character(summaries)

  checkmate::assert_logical(na.rm)
  checkmate::assert_number(na_prop, lower = 0, upper = 1, null.ok = TRUE)
  checkmate::assert_int(na_n, lower = 0, null.ok = TRUE)
  checkmate::assert_int(na_consec, lower = 0, null.ok = TRUE)
  checkmate::assert_int(na_n_non, lower = 0, null.ok = TRUE)
  
  checkmate::assert_list(summaries.params, types = "list", names = "unique")
  
  checkmate::assert_int(doy_first, lower = 1, upper = 366)
  checkmate::assert_int(doy_last, lower = 1, upper = 366)
  if (doy_first > doy_last) stop("doy_first must be less than or equal to doy_last")
  if (is.null(doy)) {
    doy <- ".doy"
    data[[doy]] <- yday_366(data[[date_time]])
  }
  
  if (to == "hourly") {
    hour <- ".hour"
    data[[hour]] <- lubridate::hour(data[[date_time]])
    date <- ".date"
    data[[date]] <- as.Date(data[[date_time]])
    grp_data <- data %>%
      dplyr::group_by(.data[[date]], .data[[hour]])
  } else if (to == "daily") {
    date <- ".date"
    data[[date]] <- as.Date(data[[date_time]])
    grp_data <- data %>%
      dplyr::group_by(.data[[date]])
  } else if (to == "pentad") {
    if (is.null(year)) year <- ".year"
    data[[year]] <- lubridate::year(data[[date_time]])
    if (lubridate::is.Date(data[[date_time]])) date <- date_time
    else {
      date <- ".date"
      data[[date]] <- as.Date(data[[date_time]])
    }
    if (is.null(pentad)) {
      pentad <- ".pentad"
      data[[pentad]] <- RInstatClimatic::pentad(data[[date]])
    }
    grp_data <- data %>%
      dplyr::group_by(.data[[year]], .data[[pentad]])
  } else if (to == "monthly") {
    if (is.null(year)) year <- ".year"
    data[[year]] <- lubridate::year(data[[date_time]])
    if (lubridate::is.Date(data[[date_time]])) date <- date_time
    else {
      date <- ".date"
      data[[date]] <- as.Date(data[[date_time]])
    }
    if (is.null(month)) {
      month <- "month"
      data[[month]] <- lubridate::month(data[[date]])
    }
    grp_data <- data %>%
      dplyr::group_by(.data[[year]], .data[[month]])
  }
  
  if (is.null(na_prop) && is.null(na_n) && is.null(na_consec) && is.null(na_n_non)) .x_call <- ".x"
  else {
    na_params <- c(null_to_string(na_prop), null_to_string(na_n),
                   null_to_string(na_consec), null_to_string(na_n_non))
    .x_call <- paste0("naflex::na_omit_if(.x, prop = ", na_params[1], ", ", 
                      "n = ", na_params[2], ", ", "consec = ", na_params[3], ", ",
                      "n_non = ", na_params[4], ")")
  }
  lambda_summaries <- vector("list", length(summaries))
  for (i in seq_along(summaries)) {
    fn_exp <- summaries[[i]]
    fn_exp <- paste0(fn_exp, "(", .x_call)
    add_params <- summaries.params[[names(summaries)[i]]]
    # TODO summarise() will give error if "na.rm" is not a parameter of the function
    if (is.null(add_params)) fn_exp <- paste0(fn_exp, ", ", "na.rm = ", na.rm)
    else fn_exp <- add_params(fn_exp, add_params)
    fn_exp <- paste0(fn_exp, ")")
    lambda_summaries[[i]] <- fn_exp
  }
  lambda_summaries <- paste0("~", lambda_summaries)
  lambda_summaries <- sapply(lambda_summaries, formula)
  names(lambda_summaries) <- names(summaries)
  grp_data %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(elements), lambda_summaries, .names = names))
}