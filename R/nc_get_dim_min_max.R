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
#' @param nc TODO
#' @param dimension TODO 
#' @param time_as_date TODO
#'
#' @return
#' @export
#'
#' @examples # TODO
nc_get_dim_min_max <- function(nc, dimension, time_as_date = TRUE) {
  if(!dimension %in% names(nc$dim)) stop(dimension, " not found in file.")
  vals <- nc$dim[[dimension]]$vals
  dim_axes <- ncdf4.helpers::nc.get.dim.axes(nc)
  time_dims <- names(dim_axes[which(dim_axes == "T")])
  if(dimension %in% time_dims && time_as_date) {
    time_vals <- c()
    try({
      units <- ncdf4::ncatt_get(nc, dimension, "units")
      if(units$hasatt && units$value == "julian_day") {
        # RDotNet interprets Date class as numeric so character needed to preserve date
        time_vals <- as.character(as.Date(vals, origin = structure(-2440588, class = "Date")))
      }
      else {
        pcict_time <- ncdf4.helpers::nc.get.time.series(nc, time.dim.name = dimension)
        posixct_time <- PCICt::as.POSIXct.PCICt(pcict_time)
        # RDotNet interprets Date class as numeric so character needed to preserve date
        time_vals <- as.character(as.Date(posixct_time))
      }
    })
    if(length(time_vals) > 0 && !anyNA(time_vals)) vals <- time_vals
  }
  bounds <- c(min(vals, na.rm = TRUE), max(vals, na.rm = TRUE))
  return(bounds)
}