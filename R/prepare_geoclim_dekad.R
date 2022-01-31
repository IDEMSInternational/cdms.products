#' Prepare dekad data in GeoCLIM format
#' @inheritParams prepare_geoclim
#' @param dekad
#'
#' @return
#' @export
#'
#' @examples
prepare_geoclim_dekad <- function(data, year, dekad, element, metadata = NULL,
                                  join_by = NULL, station_id,
                                  latitude, longitude, add_cols = NULL) {
  prepare_geoclim(data = data, year = year, type = "dekad", type_col = dekad, 
                  element = element, metadata = metadata, join_by = join_by,
                  station_id = station_id, latitude = latitude, 
                  longitude = longitude, add_cols = add_cols)
}