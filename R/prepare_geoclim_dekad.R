#' Prepare dekad data in GeoCLIM format
#' @inheritParams prepare_geoclim
#' @param dekad TODO
#'
#' @return
#' @export
#'
#' @examples # TODO
prepare_geoclim_dekad <- function(data, year, dekad, element, station_id,
                                  latitude, longitude, metadata = NULL,
                                  join_by = NULL, add_cols = NULL) {
  prepare_geoclim(data = data, year = year, type_col = dekad, 
                  element = element, station_id = station_id, 
                  latitude = latitude, longitude = longitude, type = "dekad", 
                  metadata = metadata, join_by = join_by, add_cols = add_cols)
}