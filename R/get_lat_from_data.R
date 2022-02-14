#' Get the latitude coordinate from the data
#'
#' @param datafile TODO
#'
#' @return
#' @export
#'
#' @examples # TODO
get_lat_from_data <- function(datafile) {
  return(unique(stats::na.omit(as.numeric(as.character(datafile[5:nrow(datafile),1])))))
}