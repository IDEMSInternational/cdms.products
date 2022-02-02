#' Title
#'
#' @param data The data.frame to calculate from
#' @param station The name of the station column in \code{data}, if the data are for multiple station. 
#' @param date The name of the date column in \code{data}.
#' @param year The name of the year column in \code{data}.
#' @param month The name of the month column in \code{data}.
#' @param prec The name of the precipitation column in \code{data}.
#' @param tmax The name of the maximum temperature column in \code{data}.
#' @param tmin The name of the minimum temperature column in \code{data}.
#' @param indices TODO
#' @param freq TODO
#' @param base.range TODO
#' @param n TODO
#' @param northern.hemisphere TODO
#' @param quantiles TODO
#' @param temp.qtiles TODO
#' @param prec.qtiles TODO
#' @param max.missing.days TODO
#' @param min.base.data.fraction.present TODO
#' @param spells.can.span.years TODO
#' @param gsl.mode TODO
#' @param threshold TODO
#'
#' @return
#' @export
#'
#' @examples # TODO
climdex <- function(data, station, date, year, month, prec, tmax, tmin, indices, freq = "annual",
                    base.range = c(1961, 1990), n = 5, northern.hemisphere = TRUE,
                    quantiles = NULL, temp.qtiles = c(0.1, 0.9), 
                    prec.qtiles = c(0.95, 0.99), max.missing.days = c(annual = 15, monthly = 3), 
                    min.base.data.fraction.present = 0.1, spells.can.span.years = FALSE,
                    gsl.mode = "GSL", threshold = 1) {
  stopifnot(freq %in% c("annual", "monthly"))
  if (freq == "monthly" && missing(month)) stop("month is required for freq = 'monthly'.")
  
  # All indices can be calculated annually. Only some have monthly versions as well.
  year_only_indices <- c("fd", "su", "id", "tr", "wsdi", "csdi", "gsl", "sdii", "r10mm", 
                         "r20mm", "rnnmm", "cdd", "cwd", "r95ptot", "r99ptot", "prcptot")
  if (freq == "monthly" && any(indices %in% year_only_indices)) stop("Some indices selected are not available on a monthly frequency.")
  
  # climdex.pcic only calculates for a single station at a time so need to do individually then dplyr::bind_rows() together.
  if (!missing(station)) {
    stations <- unique(data[[station]])
    # If data[[station]] is a factor this ensure stations are in factor order (and drops levels that don't appear in the data).
    if (is.factor(data[[station]])) stations <- intersect(levels(data[[station]]), stations)
    df_list <- vector(mode = "list", length = length(stations))
    for (s in seq_along(stations)) {
      df_station <- data %>% dplyr::filter(.data[[station]] == stations[s])
      ci <- climdex.pcic::climdexInput.raw(prec = df_station[[prec]], tmax = df_station[[tmax]], tmin = df_station[[tmin]], 
                                           base.range = base.range, northern.hemisphere = northern.hemisphere, 
                                           temp.qtiles = temp.qtiles, prec.qtiles = prec.qtiles, 
                                           max.missing.days = max.missing.days,
                                           min.base.data.fraction.present = min.base.data.fraction.present,
                                           tmax.dates = PCICt::as.PCICt(x = as.character(df_station[[date]]), cal="gregorian"),
                                           tmin.dates = PCICt::as.PCICt(x = as.character(df_station[[date]]), cal="gregorian"),
                                           prec.dates = PCICt::as.PCICt(x = as.character(df_station[[date]]), cal="gregorian"))
      df_list[[stations[s]]] <- climdex_single_station(ci = ci, freq = freq, indices = indices, year = year, month = month,
                                                       spells.can.span.years = spells.can.span.years, gsl.mode = gsl.mode,
                                                       threshold = threshold)
    }
    df_out <- dplyr::bind_rows(df_list, .id = station)
  }
  else {
    ci <- climdex.pcic::climdexInput.raw(prec = data[[prec]], tmax = data[[tmax]], tmin = data[[tmin]], 
                                         base.range = c(1961, 1990), northern.hemisphere = TRUE, 
                                         temp.qtiles = c(0.1, 0.9), prec.qtiles = c(0.95, 0.99), 
                                         max.missing.days = c(annual = 15, monthly = 3), 
                                         min.base.data.fraction.present=0.1, 
                                         tmax.dates = PCICt::as.PCICt(x = as.character(data[[date]]), cal="gregorian"), 
                                         tmin.dates = PCICt::as.PCICt(x = as.character(data[[date]]), cal="gregorian"), 
                                         prec.dates = PCICt::as.PCICt(x = as.character(data[[date]]), cal="gregorian"))
    df_out <- climdex_single_station(ci = ci, freq = freq, indices = indices, year = year, month = month, gsl.mode = gsl.mode)
  }
  # Make the type of the year/month column(s) the same in the output as in data.
  if (!missing(station)) {
    # TODO This is done in several places and should be extracted as a function.
    if (is.numeric(data[[station]])) df_out[[station]] <- as.numeric(df_out[[station]])
    else if (is.factor(data[[station]])) df_out[[station]] <- make_factor(df_out[[station]])
    else if (is.character(data[[station]])) df_out[[station]] <- as.character(df_out[[station]])
    else warning("Cannot recognise the class of station column. Link between data frames may be unstable.")
  }
  if (is.numeric(data[[year]])) df_out[[year]] <- as.numeric(df_out[[year]])
  else if (is.factor(data[[year]])) df_out[[year]] <- make_factor(df_out[[year]])
  else if (is.character(data[[year]])) df_out[[year]] <- as.character(df_out[[year]])
  if (freq == "monthly") {
    if (is.numeric(data[[month]])) df_out[[month]] <- as.numeric(df_out[[month]])
    else if (is.factor(data[[month]])) {
      lvs <- levels(data[[month]])
      if (length(lvs) == 12) df_out[[month]] <- factor(df_out[[month]], labels = lvs, ordered = is.ordered(data[[month]]))
      else {
        warning("month is a factor but does not have 12 levels. Output may not link correctly to data.")
        df_out[[month]] <- make_factor(df_out[[month]])
      }
    }
    else if (is.character(data[[month]])) {
      mns <- unique(data[[month]])
      # Also check English names as month.abb and month.name are locale dependent.
      if (length(mns) == 12) {
        if (setequal(mns, month.abb)) df_out[[month]] <- month.abb[df_out[[month]]]
        else if (setequal(mns, month.name)) df_out[[month]] <- month.name[df_out[[month]]]
        else if (setequal(mns, month_name_english)) df_out[[month]] <- month_abb_english[df_out[[month]]]
        else if (setequal(mns, month_name_english)) df_out[[month]] <- month_name_english[df_out[[month]]]
        else if (setequal(mns, tolower(month_name_english))) df_out[[month]] <- tolower(month_abb_english)[df_out[[month]]]
        else if (setequal(mns, tolower(month_name_english))) df_out[[month]] <- tolower(month_name_english)[df_out[[month]]]
        else if (setequal(mns, toupper(month_name_english))) df_out[[month]] <- toupper(month_abb_english)[df_out[[month]]]
        else if (setequal(mns, toupper(month_name_english))) df_out[[month]] <- toupper(month_name_english)[df_out[[month]]]
        else warning("Cannot determine format of month column in data. Output may not link correctly to data.")
      } else {
        warning("month does not have 12 unique values. Output may not link correctly to data.")
        df_out[[month]] <- as.character(df_out[[month]])
      }
    }
  }
  return(df_out)
}
