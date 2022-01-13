#' Title
#'
#' @param x 
#' @param lat 
#'
#' @return
#' @export
#'
#' @examples
dd_to_dms <- function(x, lat) {
  if (lat) dir <- ifelse(x >= 0, "N", "S")
  else dir <- ifelse(x >= 0, "E", "W")
  x <- abs(x)
  d <- trunc(x)
  m <- trunc((x - d) * 60)
  s <- round((x - d - m/60) * 3600)
  return(paste(sprintf(ifelse(lat, "%02d", "%03d"), d), sprintf("%02d", m), sprintf("%02d", s), dir))
}