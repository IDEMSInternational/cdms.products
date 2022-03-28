#' Prepare monthly data in GeoCLIM format
#'
#' @param data \code{data.frame} The data.frame to calculate from.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param year \code{character(1)} The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param month \code{character(1)} The name of the month column in \code{data}. If \code{NULL} it will be created using \code{lubridate::month(data[[date_time]])}.
#' @param element \code{character(1)} The name of the element column in \code{data} to apply the function to.
#' @param station_id \code{character(1)} The name of the station column in \code{metadata}, or \code{data} if \code{metadata = NULL}.
#' @param latitude \code{character(1)} The name of the latitude column in \code{metadata}, or \code{data} if \code{metadata = NULL}.
#' @param longitude \code{character(1)} The name of the longitude column in \code{metadata}, or \code{data} if \code{metadata = NULL}.
#' @param metadata \code{data.frame} The metadata data.frame to calculate from.
#' @param join_by \code{character} The variable(s) to merge the \code{data} and \code{metadata} data frames.
#' @param add_cols \code{character} Names of additional metadata columns that should be included in the output
#'
#' @return A data.frame formatted for use in GeoCLIM
#' @export
#'
#' @examples
#' # Calculate monthly summaries for the rainfall column
#' summary_data <- daily_niger %>% dplyr::group_by(year, month, station_name) %>%
#'      dplyr::summarise(mean_rain = mean(rain))
#' prepare_geoclim_month(data = summary_data, year = "year", month = "month",
#'                    station_id = "station_name",
#'                    element = "rain", metadata = stations_niger,
#'                    join_by = "station_name",
#'                    latitude = "lat", longitude = "long")

prepare_geoclim_month <- function(data, date_time = NULL, year = NULL, month = NULL, element, station_id, 
                                  latitude, longitude, metadata = NULL,
                                  join_by = NULL, add_cols = NULL) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(year)
  checkmate::assert_string(month)
  checkmate::assert_string(element)
  assert_column_names(data, element)
  checkmate::assert_data_frame(metadata, null.ok = TRUE)
  checkmate::assert_string(join_by, null.ok = TRUE)
  if (!is.null(date_time)) assert_column_names(data, date_time)
  if (!is.null(year)) assert_column_names(data, year)
  if (!is.null(month)) assert_column_names(data, month)
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
  
  if(is.null(year)) {
    year <- "year"
    data[[year]] <- lubridate::year(data[[date_time]])
  }
  if(is.null(month)) {
    month <- "month"
    data[[month]] <- lubridate::month(data[[date_time]])
  }
  
  unique_months <- unique(data[[month]])
  if (setequal(as.character(unique_months), as.character(1:12))) {
    data[[month]] <- factor(data[[month]], 
                            levels = 1:12, 
                            labels = month_name_english)
  } else if (setequal(unique_months, month_abb_english)) {
    data[[month]] <- factor(data[[month]], levels = month_abb_english)
  } else if (setequal(unique_months, month_name_english)) {
    data[[month]] <- factor(data[[month]], levels = month_name_english)
  } else if (setequal(unique_months, month.abb)) {
    data[[month]] <- factor(data[[month]], levels = month.abb)
  } else if (setequal(unique_months, month.name)) {
    data[[month]] <- factor(data[[month]], levels = month.name)
  } else {
    if (!is.factor(data[[month]]) || nlevels(data[[month]]) != 12) {
      stop("Values in month column are not recognised. ",
           "Values must be full or abbreviated month names ", 
           "or numbers 1 to 12 or a factor with 12 levels.")
    }
  }
  month_levels <- levels(data[[month]])
  
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
                       names_from = tidyselect::all_of(month),
                       values_from = tidyselect::all_of(element), 
                       values_fill = -999)
  names(geoclim_data)[names(geoclim_data) == station_id] <- "id"
  names(geoclim_data)[names(geoclim_data) == latitude] <- "lat"
  names(geoclim_data)[names(geoclim_data) == longitude] <- "lon"
  names(geoclim_data)[names(geoclim_data) == year] <- "year"
  geoclim_data
}
