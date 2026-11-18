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

#' Get the pentad component of a date-time object
#'
#' @description Convert a date or date-time object to a yearly pentad (5-day period)
#' 
#' @param date A date-time object
#'
#' @return a numerical vector of pentad objects corresponding to date variable.
#' @export
#'
#' @examples
#' pentad(as.Date("2020/12/25"))
#' pentad(as.Date("1999/01/01"))
pentad <- function(date) {
  temp_pentad <- 6 * (lubridate::month(date)) - 5 + (lubridate::mday(date) > 5) + (lubridate::mday(date) > 10) + (lubridate::mday(date) > 15) + (lubridate::mday(date) > 20) + (lubridate::mday(date) > 25)
  return(temp_pentad)	
}	
