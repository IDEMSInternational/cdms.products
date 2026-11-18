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

library(here)

stations_niger <- read.csv(here("data-raw", "stations_niger_raw.csv"))

names(stations_niger) <- c("id", "station_name", "lat_d", "lat_m", 
                          "long_d", "long_m", "alt")
stations_niger$lat_d <- stations_niger$lat_d + stations_niger$lat_m/60
stations_niger$long_d <- stations_niger$long_d + stations_niger$long_m/60
names(stations_niger)[3] <- "lat"
stations_niger$lat_m <- NULL
names(stations_niger)[4] <- "long"
stations_niger$long_m <- NULL
stations_niger$daily <- stations_niger$station_name %in%
  c("Agades", "Niamey_Aero", "Birni N'Konni", "Zinder")
usethis::use_data(stations_niger, overwrite = TRUE)
