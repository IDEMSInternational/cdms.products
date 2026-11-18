# Copyright 2022 IDEMS International Community Interest Company
#
# This file is part of cdms.products.
#
# cdms.products is free software: you can redistribute it and/or modify it under the
# terms of the GNU Lesser General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later version.
#
# cdms.products is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License along with
# cdms.products. If not, see <https://www.gnu.org/licenses/>.

#' Get the latitude and longitude coordinates from a data frame
#'
#' @param datafile TODO
#'
#' @return
#' @export
#'
#' @examples # TODO
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