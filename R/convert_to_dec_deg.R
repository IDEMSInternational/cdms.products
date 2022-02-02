#' Title
#'
#' @param dd TODO
#' @param mm TODO
#' @param ss TODO
#' @param dir Direction to --- to. Takes values \code{"N"}, \code{"E"}, \code{"S"}, or \code{"W"}
#'
#' @return
#' @export
#'
#' @examples # TODO
convert_to_dec_deg <- function (dd, mm = 0 , ss = 0, dir) {
  if(missing(dd))  stop("dd must be supplied")
  if(!missing(dir)) {
    dir <- toupper(dir)
    if(!all(stats::na.omit(dir) %in% c("E", "W", "N", "S"))) stop("dir must only contain direction letters E, W, N or S")
    if(any(stats::na.omit(dd) < 0)) stop("dd must be positive if dir is supplied") 
  }
  if(!all(mm >= 0 & mm <= 60, na.rm = TRUE)) stop("mm must be between 0 and 60")
  if(!all(ss >= 0 & ss <= 60, na.rm = TRUE)) stop("ss must be between 0 and 60")
  sgn <- ifelse(is.na(dir), NA, ifelse(dir %in% c("S", "W"), -1, 1))
  decdeg <- (dd + ((mm * 60) + ss)/3600) * sgn
  return(decdeg)
}
