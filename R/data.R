#' Daily weather measurements from Niger 
#'
#' A dataset containing daily measurements of eight climatic elements from four
#' weather stations in Niger from 1940 to 1980.
#'
#' @format A data frame with 54423 rows and 14 variables:
#' \describe{
#'   \item{station_name}{the name of the weather station}
#'   \item{date}{date of the measurements}
#'   \item{year}{year, numeric}
#'   \item{month}{month, factor}
#'   \item{doy}{day of the year, a number from 1 to 366}
#'   \item{day}{day of the month, a number from 1 to 31}
#'   \item{tmax}{daily maximum temperature, in degrees Celsius}
#'   \item{tmin}{daily maximum temperature, in degrees Celsius}
#'   \item{rain}{daily total rainfall, in mm}
#'   \item{hmax}{daily maximum relative humidity, percentage}
#'   \item{hmin}{daily minimum relative humidity, percentage}
#'   \item{sunh}{daily number of sunshine hours}
#'   \item{ws}{daily mean wind speed, m/s}
#'   \item{wd}{daily mean wind direction, degrees}
#' }
#' @source Meteo-France \url{https://meteofrance.com/}
"daily_niger"

#' Details of eleven weather stations in Niger 
#'
#' A dataset containing details of eleven weather stations in Niger.
#' Measurements from four of these stations are available in the
#' \code{daily_niger} dataset.
#'
#' @format A data frame with 11 rows and 6 variables:
#' \describe{
#'   \item{id}{the id of the station}
#'   \item{station_name}{the name of the station}
#'   \item{lat}{the latitude of the station, in degrees}
#'   \item{long}{the longitude of the station, in degrees}
#'   \item{alt}{the altitude of the station, in m}
#'   \item{daily}{logical, if \code{TRUE} daily data is available for this
#'   station in the \code{daily_niger} dataset}
#' }
#' @source Meteo-France \url{https://meteofrance.com/}
"stations_niger"
