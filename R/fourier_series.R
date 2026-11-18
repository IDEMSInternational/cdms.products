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
#' @param n TODO
#' @param period TODO
#'
#' @return
#' @export
#'
#' @examples # TODO
fourier_series <- function(x, n, period) {
  p2 <- "2 * pi"
  h <-  seq_len(n)
  paste0("sin(", x, " * ", h, " * ", p2, " / ", period, ")", " + ", 
         "cos(", x, " * ", h, " * ", p2, " / ", period, ")", 
         collapse = " + ")
}
