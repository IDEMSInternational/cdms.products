# Copyright 2021 IDEMS International Community Interest Company
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

#' Get the 366-based day of the year of a date
#' 
#' \code{yday_366} returns an integer between 1 and 366 representing the day of the year number of \code{x}.
#' 
#' In contrast to \code{lubridate::yday}, \code{yday_366} considers the length
#' of all years to be 366 days. In non leap years, there is no day 60 (29
#' February) and 1 March is always day 61.
#'
#' This convention is often used for weather data and other applications. It's
#' advantage is that all days have the same day number regardless of the year
#' e.g. 31 December is always day 366, even in non leap years. This may be
#' desirable, for example, when comparing day of year numbers across years that
#' contain leap years and non leap years.
#'
#' @param x A Date object
#' 
#' @return An integer between 1 and 366 representing the day of the year number of \code{x}.
#' @examples
#' yday_366(as.Date("1999-03-01"))
#' yday_366(as.Date("2000-12-31"))
#' yday_366(as.Date("2005-12-31"))
#' 
#' @export

yday_366 <- function(x) {
  doy <- lubridate::yday(x)
  leap <- lubridate::leap_year(x)
  doy[!is.na(doy) & doy > 59 & !leap] <- 1 + doy[!is.na(doy) & doy > 59 & !leap]
  return(doy)
}