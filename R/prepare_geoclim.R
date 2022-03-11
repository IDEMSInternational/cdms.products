#' Prepare dekad or pentad data in GeoCLIM format
#'
#' @param data \code{data.frame} The data.frame to calculate from.
#' @param year \code{character(1)} The name of the year column in \code{data}.
#' @param type_col \code{character(1)} The name of the dekad or pentad column in \code{data}.
#' @param element \code{character(1)} The name of the element column in \code{data} to apply the function to.
#' @param station_id \code{character(1)} The name of the station column in \code{metadata}, or \code{data} if \code{metadata = NULL}.
#' @param latitude \code{character(1)} The name of the latitude column in \code{metadata}, or \code{data} if \code{metadata = NULL}.
#' @param longitude \code{character(1)} The name of the longitude column in \code{metadata}, or \code{data} if \code{metadata = NULL}.
#' @param type \code{character(1)} Whether the data is in `dekad` or `pentad` format.
#' @param metadata \code{data.frame} The metadata data.frame to calculate from.
#' @param join_by \code{character} The variable(s) to merge the \code{data} and \code{metadata} data frames.
#' @param add_cols Names of additional metadata columns that should be included in the output
#'
#' @return A data.frame formatted for use in geoclim.
#' @export
#'
#' @examples # TODO
prepare_geoclim <- function(data, year, type_col, element, station_id, 
                            latitude, longitude, type = c("dekad", "pentad"), 
                            metadata = NULL, join_by = NULL, add_cols = NULL) {
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
    # Check that metadata has all stations in data
    data_by <- unique(data[[names(join_by)]])
    metadata_by <- unique(metadata[[as.vector(join_by)]])
    if (!all(data_by %in% metadata_by)) {
      stop("metadata is missing some values of '", join_by, "' found in data.")
    }
    # column names without the joining columns
    names_data <- setdiff(names(data), names(join_by))
    names_metadata <- setdiff(names(metadata), as.vector(join_by))
    # names that are the same other than joining columns
    # this will cause renaming issues when merging
    same_names <- intersect(names_data, names_metadata)
    # rename columns that are the same before joining 
    # to prevent renaming issues
    for (col in c(station_id, latitude, longitude)) {
      if (col %in% same_names) {
        names(data)[names(data) == col] <- paste0(col, "_x")
      }
    }
    data_with_meta <- dplyr::left_join(data, metadata, by = join_by)
  } else {
    data_with_meta <- data
  }
  id_cols <- c(station_id, latitude, longitude, add_cols, year)
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
