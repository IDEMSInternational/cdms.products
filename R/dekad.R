#' Get the dekad component of a date-time
#'
#' @param date A date-time object
#'
#' @return
#' @export
#'
#' @examples 
#' dekad(as.Date("2020/12/25"))
#' dekad(as.Date("1999/01/01"))
dekad <- function(date) {
  d <- 3 * (lubridate::month(date)) - 2 + (lubridate::mday(date) > 10) + (lubridate::mday(date) > 20)
  return(d)
}
