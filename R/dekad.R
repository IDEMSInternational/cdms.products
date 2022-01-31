#' Title
#'
#' @param date A date variable.
#'
#' @return
#' @export
#'
#' @examples
dekad <- function(date) {
  d <- 3 * (lubridate::month(date)) - 2 + (lubridate::mday(date) > 10) + (lubridate::mday(date) > 20)
  return(d)
}
