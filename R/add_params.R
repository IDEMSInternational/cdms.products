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

add_params <- function(fn_exp, add_params = list()) {
  for (i in seq_along(add_params)) {
    fn_exp <- paste0(fn_exp, ", ", names(add_params)[i], " = ", add_params[[i]])
  }
  fn_exp
}

null_to_string <- function(x) ifelse(is.null(x), "NULL", x)