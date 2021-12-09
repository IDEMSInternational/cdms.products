#' Annual Longest Spell
#' 
#' Calculates the length of the longest period each year for which the spell condition is valid 
#'
#' @param data The data.frame to calculate from
#' @param element The name of the column in \code{data} to apply the spell condition to
#' @param station The name of the station column in \code{data}, if the data are for multiple station. 
#' Spells are calculated separately for each station.
#' @param year The name of the year column in \code{data}.
#' @param spell_type The spell condition type. One of "between", "lte" (less than or equal to), 
#' "gte" (greater than or equal to) or "outside". See details for explanation.
#' @param lower The lower value of the spell condition (does not apply to spell type = "gte").
#' @param upper The lower value of the spell condition (does not apply to spell type = "lte").
#' @param result_name The name of column of the length of the longest spell period in the resulting summary dataset.
#' @param na.rm A logical value. Should \code{NA} values be removed from the spell lengths before calculating the longest spell length? 
#' Note that \code{NA} values in the element column are never removed to prevent spell runs continuing over non consecutive rows.
#'
#' @return A summary data.frame of the annual longest spell (for each station).
#' @export
#'
annual_longest_spell <- function(data, element, station = NULL, year, type = c("between", "lte", "gte", "excluding between"),
                                 lower = 0, upper = 0.85, result_name = "max_spell", na.rm = FALSE) {
  type <- match.arg(type)
  
  summary_data <- 
    switch(type,
           between = data %>% dplyr::mutate(spell_day = .data[[element]] >= lower & .data[[element]] <= upper),
           lte = data %>% dplyr::mutate(spell_day = .data[[element]] <= upper),
           gte = data %>% dplyr::mutate(spell_day = .data[[element]] >= lower),
           outside = data %>% dplyr::mutate(spell_day = .data[[element]] < lower & .data[[element]] > upper))
  if (!is.null(station)) {
    summary_data <- data %>%
      dplyr::group_by(.data[[station]])
  }
  summary_data <- summary_data %>% 
    dplyr::mutate(spell_length = spells(spell_day)) %>%
    dplyr::group_by(.data[[year]]) %>%
    dplyr::summarise({{result_name}} := max(spell_length))
  return(summary_data)
}