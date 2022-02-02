#' Prepare pentad data in GeoCLIM format
#' @inheritParams prepare_geoclim
#' @param pentad TODO
#'
#' @return
#' @export
#'
#' @examples # TODO
prepare_geoclim_pentad <- function(data, year, pentad, element, 
                                   metadata = NULL, join_by = NULL, 
                                   station_id, latitude, longitude, 
                                   add_cols = NULL) {
  prepare_geoclim(data = data, year = year, type = "pentad", type_col = pentad, 
                  element = element, metadata = metadata, join_by = join_by,
                  station_id = station_id, latitude = latitude, 
                  longitude = longitude, add_cols = add_cols)
}