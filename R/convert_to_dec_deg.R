#' Title
#'
#' @param dd TODO
#' @param mm TODO
#' @param ss TODO
#' @param dir Direction to --- to. Takes values \code{"n"}, \code{"e"}, \code{"s"}, or \code{"w"}
#'
#' @return
#' @export
#'
#' @examples # TODO
convert_to_dec_deg <- function (dd, mm = 0 , ss = 0, dir = c("n", "e", "s", "w")) {
  checkmate::assert_number(dd, lower = 0)
  dir <- match.arg(dir)
  checkmate::assert_number(mm, lower = 0, upper = 60)
  checkmate::assert_number(ss, lower = 0, upper = 60)
  sgn <- ifelse(dir %in% c("s", "w"), -1, 1)
  decdeg <- (dd + ((mm * 60) + ss)/3600) * sgn
  return(decdeg)
}
