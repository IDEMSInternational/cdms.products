#' Prepare CDT
#'
#' @param data The data.frame to calculate from.
#' @param date The name of the date column in \code{data}.
#' @param station The name of the station column in \code{data}.
#' @param year The name of the year column in \code{data}.
#' @param element The name of the element column in \code{data}.
#' @param metadata The metadata data.frame.
#' @param join_by The variable to join the two data frames. 
#' @param latitude The name of the latitude column in \code{metadata}.
#' @param longitude The name of the longitude column in \code{metadata}.
#' @param altitude The name of the altitude column in \code{metadata}.
#'
#' @return
#' @export
#'
#' @examples
#' # create summary data
#' data("daily_niger"); data("stations_niger")
#' summary_data <- daily_niger %>%
#' dplyr::mutate(dekad_date = dekad(daily_niger$date)) %>%
#' dplyr::group_by(station_name, year, dekad_date) %>%
#' dplyr::summarise(date = dplyr::first(date), sum = sum(tmax))
#' 
#' prepare_cdt(data = summary_data, date = "date", year = "year",
#'             station = "station_name",
#'             element = "sum", metadata = stations_niger, join_by = "station_name",
#'             latitude = "lat", longitude = "long", altitude = "alt")


prepare_cdt <- function(data, date, year, element, metadata = NULL, join_by = NULL,
                        station, latitude, longitude, altitude) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(year)
  assert_column_names(data, year)
  assert_column_names(data, date)
  checkmate::assert_string(element)
  assert_column_names(data, element)
  checkmate::assert_data_frame(metadata, null.ok = TRUE)
  checkmate::assert_string(join_by, null.ok = TRUE)
  if (is.null(names(join_by))) names(join_by) <- join_by
  checkmate::assert_string(station)
  checkmate::assert_string(latitude)
  checkmate::assert_string(longitude)
  checkmate::assert_string(altitude)
  # data_with_meta is whichever data.frame has the metadata columns in
  if (is.null(metadata)) {
    data_with_meta <- data
  } else {
    data_with_meta <- metadata
    if (is.null(join_by)) stop("join_by must be specified when metadata is supplied.")
  }
  assert_column_names(data_with_meta, station)
  assert_column_names(data_with_meta, latitude)
  assert_column_names(data_with_meta, longitude)
  assert_column_names(data_with_meta, altitude)
  
  if (!is.null(metadata)) {
    data_by <- unique(data[[names(join_by)]])
    metadata_by <- unique(metadata[[as.vector(join_by)]])
    if (!all(data_by %in% metadata_by)) {
      stop("metadata is missing some values of '", join_by, "' found in data.")
    }
    names_data <- setdiff(names(data), names(join_by))
    names_metadata <- setdiff(names(metadata), as.vector(join_by))
    same_names <- intersect(names_data, names_metadata)
    for (col in c(station, latitude, longitude)) {
      if (col %in% same_names) {
        names(data)[names(data) == col] <- paste0(col, "_x")
      }
    }
    data_with_meta <- dplyr::left_join(data, metadata, by = join_by)
    id_cols <- c(names(metadata), year)
  } else {
    data_with_meta <- data
    id_cols <- c(station, latitude, longitude, year)
  }
  data_with_meta[[element]] <- tidyr::replace_na(data_with_meta[[element]], -999)
  
  data_with_meta[[date]] <- as.character(data_with_meta[[date]])
  data_with_meta_date <- data_with_meta %>%
    dplyr::ungroup() %>%
    dplyr::select(c(.data[[station]], .data[[date]], .data[[element]])) %>%
    tidyr::pivot_longer(cols = c(.data[[date]]),
                        names_to = ".x", values_to = "names") %>%
    dplyr::select(-c(.data$`.x`)) %>%
    dplyr::rename(values = .data[[element]])
  
  data_with_meta_meta <- data_with_meta %>%
    dplyr::ungroup() %>%
    dplyr::select(c(.data[[station]], .data[[latitude]], .data[[longitude]], .data[[altitude]])) %>%
    tidyr::pivot_longer(cols = c(.data[[latitude]], .data[[longitude]], .data[[altitude]]),
                        names_to = "names", values_to = "values") %>%
    dplyr::distinct()
  
  cdt_data <- rbind(data_with_meta_meta, data_with_meta_date)
  
  cdt_data <- cdt_data %>%
    tidyr::pivot_wider(names_from = .data[[station]], values_from = .data$values)
  return(cdt_data)
}