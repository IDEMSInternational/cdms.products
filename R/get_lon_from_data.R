#' Get the longitude coordinate from the data
#'
#' @param datafile 
#'
#' @return
#' @export
#'
#' @examples
get_lon_from_data <- function(datafile){
  return(na.omit(as.numeric(unique(t(datafile[5,2:ncol(datafile)])))))
}