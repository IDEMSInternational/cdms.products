#' Prepare pentad data in GeoCLIM format
#' @inheritParams prepare_geoclim
#' @param pentad \code{character(1)} The name of the pentad column in \code{data}.
#'
#' @return A data.frame formatted for use in geoclim.
#' @export
#'
#' @examples # TODO
prepare_geoclim_pentad <- function(data, year, pentad, element, station_id, 
                                   latitude, longitude, metadata = NULL, 
                                   join_by = NULL, add_cols = NULL) {
  prepare_geoclim(data = data, year = year, type_col = pentad, 
                  element = element, station_id = station_id, 
                  latitude = latitude, longitude = longitude, type = "pentad",
                  metadata = metadata, join_by = join_by, add_cols = add_cols)
}