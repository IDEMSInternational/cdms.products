#' Title
#'
#' @param path 
#' @param vars 
#' @param keep_raw_time 
#' @param include_metadata 
#' @param boundary 
#' @param lon_points 
#' @param lat_points 
#' @param id_points 
#' @param show_requested_points 
#' @param great_circle_dist 
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
multiple_nc_as_data_frame <- function(path, vars, keep_raw_time = TRUE, include_metadata = TRUE, boundary = NULL, lon_points = NULL, lat_points = NULL, id_points = NULL, show_requested_points = TRUE, great_circle_dist = TRUE, id = "id") {
  filepaths <- list.files(path = path, pattern="*\\.nc", full.names = TRUE)
  filenames <- basename(filepaths)
  nc_list <- list()
  
  n_files <- length(filepaths)
  pb <- utils::winProgressBar(title = "Reading files", min = 0, max = n_files)
  for(i in seq_along(filepaths)) {
    nc <- ncdf4::nc_open(filename = filepaths[i])
    dat <- nc_as_data_frame(nc = nc, vars = vars, keep_raw_time = keep_raw_time, include_metadata = include_metadata, boundary = boundary, lon_points = lon_points, lat_points = lat_points, id_points = id_points, show_requested_points = show_requested_points, great_circle_dist = great_circle_dist)
    nc_list[[length(nc_list) + 1]] <- dat
    ncdf4::nc_close(nc)
    info <- paste0("Reading file ", i, " of ", n_files, " - ", round(100*i/n_files), "%")
    utils::setWinProgressBar(pb, value = i, title = info, label = info)
  }
  close(pb)
  names(nc_list) <- tools::file_path_sans_ext(filenames)
  merged_data <- dplyr::bind_rows(nc_list, .id = id)
  return(merged_data)
}