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

#' Get the dekad component of a date-time object
#'
#' @description Convert a date or date-time object to a yearly dekad (10-day period)
#'
#' @param date A date-time object
#'
#' @return a numerical vector of dekad objects corresponding to date variable.
#' @export
#'
#' @examples 
#' dekad(as.Date("2020/12/25"))
#' dekad(as.Date("1999/01/01"))
dekad <- function(date) {
  d <- 3 * (lubridate::month(date)) - 2 + (lubridate::mday(date) > 10) + (lubridate::mday(date) > 20)
  return(d)
}
