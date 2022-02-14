#' Get the longitude coordinate from the data
#'
#' @param datafile TODO
#'
#' @return
#' @export
#'
#' @examples # TODO
get_lon_from_data <- function(datafile) {
  return(stats::na.omit(as.numeric(unique(t(datafile[5,2:ncol(datafile)])))))
}