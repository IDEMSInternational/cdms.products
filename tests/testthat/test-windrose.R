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

library(cdms.products)
data("daily_niger")

test_that("windrose returns an error when conditions are not met", {
  mandatory_inputs <- list(data = daily_niger, speed = "ws", direction = "wd")
  
  expect_error(windrose(),)
  expect_error(do.call(windrose, mandatory_inputs[-1]))
  expect_error(do.call(windrose, mandatory_inputs[-2]))
  expect_error(do.call(windrose, mandatory_inputs[-3]))
})

test_that("windrose graphs with facets and without facets are correct", {
  r1 <- clifro::windrose(speed = daily_niger$ws, direction = daily_niger$wd, facet = daily_niger$station_name,
                         legend_title = "Wind Speed", speed_cuts = NA, variable_wind = 990, n_col = NULL)
  
  t1 <- windrose(daily_niger, speed = "ws", direction = "wd", facet_by = "station_name")
  
  r2 <- clifro::windrose(speed = daily_niger$ws, direction = daily_niger$wd, legend_title = "Wind Speed", 
                         speed_cuts = NA, variable_wind = 990, n_col = NULL)
  t2 <- windrose(daily_niger, speed = "ws", direction = "wd")
  
  expect_equal(gg_data(r1),gg_data(t1))
  expect_equal(gg_data(r2),gg_data(t2))
})
