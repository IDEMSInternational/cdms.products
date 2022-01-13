#' Title
#'
#' @param date A date variable.
#'
#' @return
#' @export
#'
#' @examples
pentad <- function(date){
  temp_pentad <- 6*(lubridate::month(date)) - 5 + (lubridate::mday(date) > 5) + (lubridate::mday(date) > 10) + (lubridate::mday(date) > 15) + (lubridate::mday(date) > 20) + (lubridate::mday(date) > 25)
  return(temp_pentad)	
}	 