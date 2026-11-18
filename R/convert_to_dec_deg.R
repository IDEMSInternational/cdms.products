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

#' Title
#'
#' @param dd TODO
#' @param mm TODO
#' @param ss TODO
#' @param dir Direction to --- to. Takes values \code{"n"}, \code{"e"}, \code{"s"}, or \code{"w"}
#'
#' @return
#' @export
#'
#' @examples # TODO
convert_to_dec_deg <- function (dd, mm = 0 , ss = 0, dir = c("n", "e", "s", "w")) {
  checkmate::assert_number(dd, lower = 0)
  dir <- match.arg(dir)
  checkmate::assert_number(mm, lower = 0, upper = 60)
  checkmate::assert_number(ss, lower = 0, upper = 60)
  sgn <- ifelse(dir %in% c("s", "w"), -1, 1)
  decdeg <- (dd + ((mm * 60) + ss)/3600) * sgn
  return(decdeg)
}
