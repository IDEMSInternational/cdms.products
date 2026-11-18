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
#' @param x TODO
#' @param lat TODO
#'
#' @return
#' @export
#'
#' @examples # TODO
dd_to_dms <- function(x, lat) {
  if (lat) dir <- ifelse(x >= 0, "N", "S")
  else dir <- ifelse(x >= 0, "E", "W")
  x <- abs(x)
  d <- trunc(x)
  m <- trunc((x - d) * 60)
  s <- round((x - d - m/60) * 3600)
  return(paste(sprintf(ifelse(lat, "%02d", "%03d"), d), sprintf("%02d", m), sprintf("%02d", s), dir))
}