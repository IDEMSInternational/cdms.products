#' Outputs data in the format for the CPT software
#' 
#' @description `output_CPT` returns a data frame to a format suitable for
#' use in the CPT software.
#' 
#' @param data \code{data.frame} The data.frame to calculate from.
#' @param lat_lon_data \code{data.frame} The name of the metadata to calculate from.
#' @param station_latlondata \code{character(1)} The name of the station column in \code{lat_lon_data}, or \code{data} if \code{long_data = FALSE}.
#' @param latitude \code{character(1)} The name of the latitude column in \code{lat_lon_data}, or \code{data} if \code{long_data = FALSE}.
#' @param longitude \code{character(1)} The name of the longitude column in \code{lat_lon_data}, or \code{data} if \code{long_data = FALSE}.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param year \code{character(1)} The name of the year column in \code{data}.
#' @param element \code{character(1)} The name of the element column in \code{data} to apply the function to.
#' @param long_data \code{logical(1)} Whether all columns are in \code{data}. If all data is in one data frame then must have \code{long_data = TRUE}.
#' @param na_code \code{numeric(1)} Indicator for NA values in data.
#'
#' @return A data.frame formatted for use in CPT.
#' @export
#'
#' @examples
#' # Create summary data
#' yearly_niger <- daily_niger %>% dplyr::group_by(station_name, year) %>%
#'     dplyr::summarise(mean_rain = mean(rain))
#' output_CPT(data = yearly_niger, lat_lon_data = stations_niger,
#'            station_latlondata = "station_name", latitude = "lat", longitude = "long",
#'            station = "station_name", year = "year", element = "mean_rain")

output_CPT <- function(data, lat_lon_data, station_latlondata, latitude, longitude, station, year,
                       element, long_data = TRUE, na_code = -999) {
  
  if(missing(data)) stop("data should be provided")
  if(missing(station)) stop("station must be provided")
  if(missing(year) ||  missing(latitude) || missing(longitude)) stop("year, latitude and longiude must be provided")
  
  station_label <- "STN"
  lat_lon_labels <- c("LAT", "LON")
  
  if(missing(lat_lon_data)) {
    if(long_data) {
      data <- data %>% dplyr::select(!!! rlang::quos(station, year, element, latitude, longitude))
      names(data)[1] <- "station"
      names(data)[2] <- "year"
      names(data)[3] <- "element"
      names(data)[4] <- "latitude"
      names(data)[5] <- "longitude"
      
      data <- data %>% dplyr::filter(!is.na(station))
    }
    else stop("If all data is in one data frame then must have long_data = TRUE")
  }
  else {
    if(missing(station_latlondata)) stop("station must be provided for lat_lon_data")
    
    if(long_data) {
      yearly_data <- data %>% dplyr::select(!!! rlang::quos(station, year, element))
      names(yearly_data)[1] <-  "station"
      names(yearly_data)[2] <-  "year"
      names(yearly_data)[3] <-  "element"
      
      lat_lon_data <- lat_lon_data %>% dplyr::select(!!! rlang::quos(station_latlondata, latitude, longitude))
      names(lat_lon_data)[1] <- "station"
      names(lat_lon_data)[2] <- "latitude"
      names(lat_lon_data)[3] <- "longitude"
      
      data <- merge(yearly_data, lat_lon_data, by =  "station")
    }
    else {
      stations <- data.frame(data[station])
      # TODO Check this. It was previously data_unstacked but not defined.
      year <- data %>% dplyr::select(!!! rlang::quos(year))
      data <- data.frame(year, stations)
      stacked_data <- reshape2::melt(data, id.vars=c("year"))
      names(stacked_data)[2] <-  "station"
      names(stacked_data)[3] <- "element"
      
      lat_lon_data <- lat_lon_data %>% dplyr::select(!!! rlang::quos(station_latlondata, latitude, longitude))
      names(lat_lon_data)[1] <- "station"
      names(lat_lon_data)[2] <- "latitude"
      names(lat_lon_data)[3] <- "longitude"
      
      data <- merge(stacked_data, lat_lon_data, by = "station")
      
    }
  }
  
  unstacked_data <- data %>% dplyr::select(station, year, element) %>% tidyr::spread(key = station, value = element)
  names(unstacked_data)[1] <- station_label
  unstacked_data <- unstacked_data %>% dplyr::mutate_all(list(~replace(.x, is.na(.x), na_code)))
  
  lat_lon_data <- data %>% dplyr::group_by(station) %>% dplyr::summarise(latitude = min(latitude, na.rm = TRUE), longitude = min(longitude, na.rm = TRUE))
  if(anyNA(data$latitude) || anyNA(data$longitude)) warning("Missing values in latitude or longitude.")
  t_lat_lon_data <- t(lat_lon_data)
  station.names <- as.vector(t_lat_lon_data[1, ])
  t_lat_lon_data <- data.frame(t_lat_lon_data, stringsAsFactors = FALSE)
  t_lat_lon_data <- t_lat_lon_data %>% dplyr::slice(-1)
  names(t_lat_lon_data) <- station.names
  row.names(t_lat_lon_data) <- lat_lon_labels
  t_lat_lon_data <- tibble::rownames_to_column(t_lat_lon_data, station_label)
  
  cpt_data <- rbind(t_lat_lon_data, unstacked_data)
  return(cpt_data)
}
