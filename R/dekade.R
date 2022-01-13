#' Title
#'
#' @param date A date variable.
#'
#' @return
#' @export
#'
#' @examples
dekade <- function(date) {
  temp_dekade <- 3 * (lubridate::month(date)) - 2 + (lubridate::mday(date) > 10) + (lubridate::mday(date) > 20)
  return(temp_dekade)
}
