#' Get the latitude and longitude coordinates from a data frame
#'
#' @param datafile 
#'
#' @return
#' @export
#'
#' @examples
lat_lon_dataframe <- function(datafile){
  latitude  <- get_lat_from_data(datafile)
  longitude <- get_lon_from_data(datafile)
  lat <- rep(latitude, each = length(longitude))
  lon <- rep(longitude, length(latitude))
  lat_lon <- as.data.frame(cbind(lat, lon))
  station <- c()
  for (j in 1:nrow(lat_lon)){
    if(lat_lon[j,1]>=0){
      station = append(station, paste(paste("latN", lat_lon[j,1], sep = ""), paste("lon", lat_lon[j,2], sep = ""), sep = "_"))
    }
    else{
      station = append(station, paste(paste("latS", abs(lat_lon[j,1]), sep = ""), paste("lon", lat_lon[j,2], sep = ""), sep = "_"))
    }
  }
  return(cbind(lat_lon,station))
}

output_CPT <- function(data, lat_lon_data, station_latlondata, latitude, longitude, station, year, element, long.data = TRUE, na_code = -999) {
  
  if(missing(data)) stop("data should be provided")
  if(missing(station)) stop("station must be provided")
  if(missing(year) ||  missing(latitude) || missing(longitude)) stop("year, latitude and longiude must be provided")
  
  station_label <- "STN"
  lat_lon_labels <- c("LAT", "LON")
  
  if(missing(lat_lon_data)) {
    if(long.data) {
      data <- data %>% dplyr::select(!!! quos(station, year, element, latitude, longitude))
      names(data)[1] <- "station"
      names(data)[2] <- "year"
      names(data)[3] <- "element"
      names(data)[4] <- "latitude"
      names(data)[5] <- "longitude"
      
      data <- data %>% dplyr::filter(!is.na(station))
    }
    else stop("If all data is in one data frame then must have long.data = TRUE")
  }
  else {
    if(missing(station_latlondata)) stop("station must be provided for lat_lon_data")
    
    if(long.data) {
      yearly_data <- data %>% dplyr::select(!!! quos(station, year, element))
      names(yearly_data)[1] <-  "station"
      names(yearly_data)[2] <-  "year"
      names(yearly_data)[3] <-  "element"
      
      lat_lon_data <- lat_lon_data %>% dplyr::select(!!! quos(station_latlondata, latitude, longitude))
      names(lat_lon_data)[1] <- "station"
      names(lat_lon_data)[2] <- "latitude"
      names(lat_lon_data)[3] <- "longitude"
      
      data <- merge(yearly_data, lat_lon_data, by =  "station")
    }
    else {
      stations <- data.frame(data[station])
      year <- data_unstacked %>% dplyr::select(!!! quos(year))
      data <- data.frame(year, stations)
      stacked_data <- reshape2::melt(data, id.vars=c("year"))
      names(stacked_data)[2] <-  "station"
      names(stacked_data)[3] <- "element"
      
      lat_lon_data <- lat_lon_data %>% dplyr::select(!!! quos(station_latlondata, latitude, longitude))
      names(lat_lon_data)[1] <- "station"
      names(lat_lon_data)[2] <- "latitude"
      names(lat_lon_data)[3] <- "longitude"
      
      data <- merge(stacked_data, lat_lon_data, by = "station")
      
    }
  }
  
  unstacked_data <- data %>% dplyr::select(station, year, element) %>% tidyr::spread(key = station, value = element)
  names(unstacked_data)[1] <- station_label
  unstacked_data <- unstacked_data %>% mutate_all(funs(replace(., is.na(.), na_code)))
  
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