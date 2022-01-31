#' Export pentad data in GeoCLIM format
#' @inheritParams export_geoclim
#' @inheritParams prepare_geoclim_pentad
#'
#' @return Invisibly returns the file path of the saved data
#' @export
#'
#' @examples
export_geoclim_pentad <- function(data, year, pentad, element, metadata = NULL,
                                  join_by = NULL, station_id,
                                  latitude, longitude, add_cols = NULL,
                                  file_path = paste0("GEOCLIM-",
                                                     element,
                                                     ".csv"),
                                 ...) {
  export_geoclim(data = data, year = year, type = "pentad", type_col = pentad,
                 element = element, metadata = metadata, join_by = join_by,
                 station_id = station_id, latitude = latitude, 
                 longitude = longitude, add_cols = add_cols)
}