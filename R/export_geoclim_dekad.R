#' Export dekad data in GeoCLIM format
#' @inheritParams export_geoclim
#' @inheritParams prepare_geoclim_dekad
#'
#' @return Invisibly returns the file path of the saved data.
#' @export
#'
#' @examples # TODO
export_geoclim_dekad <- function(data, year, dekad, element, metadata = NULL,
                                 join_by = NULL, station_id,
                                 latitude, longitude, add_cols = NULL,
                                 file_path = paste0("GEOCLIM-", 
                                                    element, 
                                                    ".csv"),
                                 ...) {
  export_geoclim(data = data, year = year, type = "dekad", type_col = dekad, 
                 element = element, metadata = metadata, join_by = join_by,
                 station_id = station_id, latitude = latitude, 
                 longitude = longitude, add_cols = add_cols)
}