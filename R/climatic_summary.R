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
#' @param summaries A named character vector of summary functions. The names are
#'   the used as the column names in the results. The values can be any function
#'   name as a string. e.g. c(mean = "mean", st_dv = "sd", n_na =
#'   "naflex::na_n")
#' @param na.rm If \code{TRUE} all \code{na_} parameters are ignored and missing
#'   values are removed. If \code{FALSE} missing values are not removed unless
#'   any \code{na_} parameters are specified.
#' @param na_prop Max proportion of missing values allowed
#' @param na_n Max number of missing values allowed
#' @param na_consec Max number of consecutive missing values allowed
#' @param na_n_non Min number of non-missing values required
#' @param names Format of column names. Passed to \code{.names} in
#'   \code{dplyr::across}
#' @param summaries.params Additional parameters to pass to \code{summaries}.
#'   Must be a list of lists with the same names as \code{summaries}.
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom rlang .data
climatic_summary <- function(data, date_time, station = NULL, elements, 
                             year = NULL, month = NULL, dekad = NULL, 
                             pentad = NULL,
                             to = c("hourly", "daily", "pentad", "dekadal", 
                                    "monthly", "annual-within-year", 
                                    "annual", "longterm-monthly", 
                                    "longterm-within-year", "station",
                                    "overall"),
                             by = NULL,
                             doy = NULL, doy_first = 1, doy_last = 366, 
                             summaries, na.rm = FALSE,
                             na_prop = NULL, na_n = NULL, na_consec = NULL, 
                             na_n_non = NULL, 
                             summaries.params = list(), names = "{.fn}_{.col}") {
  
  checkmate::assert_data_frame(data)
  assert_column_names(data, date_time)
  checkmate::assert(checkmate::check_date(data[[date_time]]), 
                    checkmate::check_posixct(data[[date_time]]))
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
  
  any_na_params <- !is.null(na_prop) || !is.null(na_n) || 
    !is.null(na_consec) || !is.null(na_n_non)
  if (na.rm) {
    if (any_na_params) {
      warning("'na.rm = TRUE' will override na_prop, 
            na_n, na_consec and na_n_non")
    }
    na_prop <- 1
    na_n <- NULL
    na_consec <- NULL
    na_n_non <- NULL
    any_na_params <- TRUE
  }
  checkmate::assert_list(summaries.params, types = "list", names = "unique")
  
  checkmate::assert_int(doy_first, lower = 1, upper = 366)
  checkmate::assert_int(doy_last, lower = 1, upper = 366)
  if (doy_first > doy_last) 
    stop("doy_first must be less than or equal to doy_last")
  if (is.null(doy)) {
    doy <- ".doy"
    data[[doy]] <- yday_366(data[[date_time]])
  }
  
  if (to == "station" && is.null(station)) {
    stop("station is required when to = 'station'")
  }
  
  # Check year column exists or create
  if (to %in% c("pentad", "dekadal", "monthly", "annual", 
                "annual-within-year")) {
    if (is.null(year)) {
      year <- ".year"
      data[[year]] <- lubridate::year(data[[date_time]])
    }
  }
  
  if (to %in% c("monthly", "longterm-monthly")) {
    if (is.null(month)) {
      month <- ".month"
      data[[month]] <- lubridate::month(data[[date_time]])
    }
    }
  
  # to = "pentad" "dekadal" "monthly" or "longterm-monthly"
  # are special cases of "within" cases and can be converted
  # to simplify case handling internally.
  if (to == "pentad") {
    if (is.null(pentad)) {
      pentad <- ".pentad"
      data[[pentad]] <- RInstatClimatic::pentad(data[[date_time]])
    }
    to <- "annual-within-year"
    by <- pentad
  } else if (to == "dekadal") {
    if (is.null(pentad)) {
      dekad <- ".dekad"
      data[[dekad]] <- RInstatClimatic::dekade(data[[date_time]])
    }
    to <- "annual-within-year"
    by <- dekad
  } else if (to == "monthly") {
    to <- "annual-within-year"
    by <- month
  } else if (to == "longterm-monthly") {
    to <- "longterm-within-year"
    by <- month
  } 
  
  if (!is.null(station) && to != "overall") {
    grp_data <- data %>%
      dplyr::group_by(.data[[station]])
  } else grp_data <- data
  if (to == "hourly") {
    hour <- ".hour"
    grp_data[[hour]] <- lubridate::hour(grp_data[[date_time]])
    date <- ".date"
    grp_data[[date]] <- as.Date(grp_data[[date_time]])
    grp_data <- grp_data %>%
      dplyr::group_by(.data[[date]], .data[[hour]], .add = TRUE)
  } else if (to == "daily") {
    date <- ".date"
    grp_data[[date]] <- as.Date(grp_data[[date_time]])
    grp_data <- grp_data %>%
      dplyr::group_by(.data[[date]])
  } else if (to == "annual-within-year") {
    if (is.null(by)) stop("by is required when to = 'annual-within-year'")
    grp_data <- grp_data %>%
      dplyr::group_by(.data[[year]], .add = TRUE)
    for (i in seq_along(by)) {
      grp_data <- grp_data %>%
        dplyr::group_by(.data[[by[i]]], .add = TRUE)
    }
  } else if (to == "annual") {
    grp_data <- grp_data %>%
      dplyr::group_by(.data[[year]], .add = TRUE)
  } else if (to == "longterm-within-year") {
    if (is.null(by)) stop("'by' is required when to = 'annual-within-year'")
    for (i in seq_along(by)) {
      grp_data <- grp_data %>%
        dplyr::group_by(.data[[by[i]]], .add = TRUE)
    }
  }
  
  if (!any_na_params) .x_call <- ".x"
  else {
    na_params <- c(null_to_string(na_prop), null_to_string(na_n),
                   null_to_string(na_consec), null_to_string(na_n_non))
    .x_call <- paste0("naflex::na_omit_if(.x, ",
                      "prop = ", na_params[1], ", ", 
                      "n = ", na_params[2], ", ", 
                      "consec = ", na_params[3], ", ",
                      "n_non = ", na_params[4], ")")
  }
  lambda_summaries <- vector("list", length(summaries))
  for (i in seq_along(summaries)) {
    fn_exp <- summaries[[i]]
    fn_exp <- paste0(fn_exp, "(", .x_call)
    add_params <- summaries.params[[names(summaries)[i]]]
    fn_exp <- add_params(fn_exp, add_params)
    fn_exp <- paste0(fn_exp, ")")
    lambda_summaries[[i]] <- fn_exp
  }
  lambda_summaries <- paste0("~", lambda_summaries)
  lambda_summaries <- sapply(lambda_summaries, formula)
  names(lambda_summaries) <- names(summaries)
  grp_data %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(elements), 
                                   lambda_summaries, .names = names))
}