#' Annual Longest Spell
#' 
#' Calculates the length of the longest period each year for which the spell condition is valid 
#'
#' @param data The data.frame to calculate from
#' @param element The name of the column in \code{data} to apply the spell condition to
#' @param station The name of the station column in \code{data}, if the data are for multiple station. 
#' Spells are calculated separately for each station.
#' @param date The name of the date column in \code{data}. This is only needed if \code{year} is not specified as it will be created from \code{date}.
#' @param year The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date]])}.
#' @param spell_type The spell condition type. One of "between", "lte" (less than or equal to), 
#' "gte" (greater than or equal to) or "outside". See details for explanation.
#' @param lower The lower value of the spell condition (does not apply to spell type = "gte").
#' @param upper The lower value of the spell condition (does not apply to spell type = "lte").
#' @param result_name The name of column of the length of the longest spell period in the resulting summary dataset.
#' @param na.rm A logical value. Should \code{NA} values be removed from the spell lengths before calculating the longest spell length? 
#' Note that \code{NA} values in the element column are never removed to prevent spell runs continuing over non consecutive rows.
#' @param doy The name of the day of the year (1-366) column in \code{data} if filtering to a part of the year. 
#' Note that this filter is applied after calculating spell lengths, hence the longest spell could have started outside of this filter.
#' To exclude a part of the year from the calculations completely, filter the data before using \code{annual_longest_spell}.
#' If \code{doy} is \code{NULL} then it can be calculated as \code{yday_366(data[[date]])} if \code{date} is provided.
#' @param doy_first The first day of the year to consider the longest spell within.
#' @param doy_last The last day of the year to consider the longest spell within.
#'
#' @return A summary data.frame of the annual longest spell (for each station).
#' @export
#'
#' @importFrom rlang .data :=
annual_longest_spell <- function(data, element, station = NULL, date = NULL, year = NULL, 
                                 spell_type = c("between", "lte", "gte", "excluding between"),
                                 lower = 0, upper = 0.85, doy = NULL, doy_first = 1, doy_last = 366, result_name = "max_spell", 
                                 na.rm = FALSE) {
  spell_type <- match.arg(spell_type)
  
  if (is.null(year)) {
    if (is.null(date)) stop("year or date columns must be specified so that year can be calculated.")
    year <- "year"
    data[[year]] <- lubridate::year(data[[date]])
  }
  if (is.null(doy)) {
    if (doy_first != 1 || doy_last != 366) {
      if (is.null(date)) stop("doy or date columns must be specified if a day of year filter is required.")
      doy <- "doy"
      data[[doy]] <- yday_366(data[[date]])
    }
  }
  
  summary_data <- 
    switch(spell_type,
           between = data %>% dplyr::mutate(spell_day = .data[[element]] >= lower & .data[[element]] <= upper),
           lte = data %>% dplyr::mutate(spell_day = .data[[element]] <= upper),
           gte = data %>% dplyr::mutate(spell_day = .data[[element]] >= lower),
           outside = data %>% dplyr::mutate(spell_day = .data[[element]] < lower & .data[[element]] > upper))
  
  if (!is.null(station)) {
    summary_data <- data %>%
      dplyr::group_by(.data[[station]])
  }
  summary_data <- summary_data %>% 
    dplyr::mutate(spell_length = spells(.data$spell_day)) %>%
    dplyr::group_by(.data[[year]])
  if (!is.null(doy)) {
    summary_data <- summary_data %>% 
      dplyr::filter(.data[[doy]] >= doy_first & .data[[doy]] <= doy_last, .preserve = TRUE)
  }
  summary_data <- summary_data %>%
    dplyr::summarise({{result_name}} := max(.data$spell_length))
  return(summary_data)
}