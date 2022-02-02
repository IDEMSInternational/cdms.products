#' Prepare dekad or pentad data in GeoCLIM format
#'
#' @param data 
#' @param year 
#' @param element 
#' @param metadata 
#' @param latitude 
#' @param longitude 
#' @param station_id 
#' @param join_by 
#' @param add_cols 
#' @param type 
#' @param type_col 
#'
#' @return
#' @export
#'
#' @examples
prepare_geoclim <- function(data, year, type = c("dekad", "pentad"),
                            type_col, element, metadata = NULL,
                            join_by = NULL, station_id,
                            latitude, longitude, add_cols = NULL) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(year)
  assert_column_names(data, year)
  type <- match.arg(type)
  checkmate::assert_string(type_col)
  assert_column_names(data, type_col)
  checkmate::assert_string(element)
  assert_column_names(data, element)
  checkmate::assert_data_frame(metadata, null.ok = TRUE)
  checkmate::assert_string(join_by, null.ok = TRUE)
  if (is.null(names(join_by))) names(join_by) <- join_by
  checkmate::assert_string(station_id)
  checkmate::assert_string(latitude)
  checkmate::assert_string(longitude)
  # data_with_meta is whichever data.frame has the metadata columns in
  if (is.null(metadata)) {
    data_with_meta <- data
  } else {
    data_with_meta <- metadata
    if (is.null(join_by)) stop("join_by must be specified when metadata is supplied.")
  }
  assert_column_names(data_with_meta, station_id)
  assert_column_names(data_with_meta, latitude)
  assert_column_names(data_with_meta, longitude)
  
  unique_types <- unique(data[[type_col]])
  if (type == "dekad") ntypes <- 36
  else if (type == "pentad") ntypes <- 72
  if (setequal(as.character(unique_types), as.character(1:ntypes))) {
    data[[type_col]] <- factor(data[[type_col]], 
                            levels = 1:ntypes, 
                            labels = 1:ntypes)
  } else {
    if (is.factor(data[[type_col]]) && nlevels(data[[type_col]]) == ntypes) {
      levels(data[[type_col]]) <- 1:ntypes
    } else {
      stop("Values in type column are not recognised. ",
           "Values must be numbers 1 to ", ntypes, 
           " or a factor with ", ntypes, " levels.")
    }
  }
  type_levels <- levels(data[[type_col]])
  
  if (!is.null(metadata)) {
    data_by <- unique(data[[names(join_by)]])
    metadata_by <- unique(metadata[[as.vector(join_by)]])
    if (!all(data_by %in% metadata_by)) {
      stop("metadata is missing some values of '", join_by, "' found in data.")
    }
    names_data <- setdiff(names(data), names(join_by))
    names_metadata <- setdiff(names(metadata), as.vector(join_by))
    same_names <- intersect(names_data, names_metadata)
    for (col in c(station_id, latitude, longitude)) {
      if (col %in% same_names) {
        names(data)[names(data) == col] <- paste0(col, "_x")
      }
    }
    data_with_meta <- dplyr::left_join(data, metadata, by = join_by)
    id_cols <- c(names(metadata), year)
  } else {
    data_with_meta <- data
    id_cols <- c(station_id, latitude, longitude, add_cols, year)
  }
  data_with_meta[[element]] <- tidyr::replace_na(data_with_meta[[element]], -999)
  geoclim_data <- 
    tidyr::pivot_wider(data_with_meta,
                       id_cols = tidyselect::all_of(id_cols),
                       names_from = tidyselect::all_of(type_col),
                       values_from = tidyselect::all_of(element), 
                       values_fill = -999)
  names(geoclim_data)[names(geoclim_data) == station_id] <- "id"
  names(geoclim_data)[names(geoclim_data) == latitude] <- "lat"
  names(geoclim_data)[names(geoclim_data) == longitude] <- "lon"
  names(geoclim_data)[names(geoclim_data) == year] <- "year"
  geoclim_data
}