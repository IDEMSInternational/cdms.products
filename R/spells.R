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

#' Running spell length
#' 
#' Calculates the running number of consecutive non-zero values of a vector
#'
#' @param x A numeric vector
#' @param initial_value The initial value of the spell length
#'
#' @return a vector of length \code{x} of the running number of consecutive non-zero values of \code{x}
#' @export
spells <- function(x, initial_value = NA_real_) {
  y <- mat.or.vec(length(x), 1)
  if(length(x) > 0) {
    y[1] <- dplyr::if_else(x[1] == 0, 0, initial_value + 1)
    if(length(x) > 1) {
      for(i in 2:length(x)) {
        y[i] <- dplyr::if_else(x[i] == 0, 0, y[i - 1] + 1)
      }
    }
  }
  return(y)
}