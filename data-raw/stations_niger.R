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
