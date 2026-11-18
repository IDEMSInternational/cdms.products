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

x <- as.Date("1999-03-01")
x_yday <- lubridate::yday(x) + 1
y <- c(as.Date("1999-01-01"), as.Date("1999-01-02"), as.Date("1999-09-09"))
y_yday <- c(1, 2, lubridate::yday(y[3]) + 1)
z <- c(y, NA, x)
z_yday <- c(y_yday, NA, x_yday)
# 2004 is a leap year, 2003 is not. But they should give the same day
w <- c(as.Date("2004-10-10"), as.Date("2003-10-10"))

test_that("yday_366 is giving the correct day", {
  expect_equal(yday_366(x), x_yday)
  expect_equal(yday_366(y), y_yday)
  expect_equal(yday_366(z), z_yday)
  expect_equal(yday_366(w[1]), yday_366(w[2]))
})
