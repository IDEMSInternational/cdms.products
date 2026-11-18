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
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

zinder51 <- daily_niger %>%
  dplyr::filter(year == 1951 & station_name == "Zinder")

test_that("prepre_climdex correctly formats data when given year, month, day columns", {
  
  t1 <- prepare_climdex(data = zinder51, prcp = "rain", tmax = "tmax", tmin = "tmin", 
                        year = "year", month = "month", day = "day")
  r1 <- zinder51 %>%
    select(year, month, day, rain, tmax, tmin) %>%
    mutate(across(c(rain, tmax, tmin), replace_na, -99.9))
  expect_equal(t1, r1)
})

test_that("prepre_climdex correctly formats data when given date column only", {
  
  t1 <- prepare_climdex(data = zinder51, prcp = "rain", tmax = "tmax", tmin = "tmin", 
                        date = "date")
  r1 <- zinder51 %>%
    mutate(year = lubridate::year(date),
           month = lubridate::month(date),
           day = lubridate::day(date)) %>%
    select(year, month, day, rain, tmax, tmin) %>%
    mutate(across(c(rain, tmax, tmin), replace_na, -99.9))
  expect_equal(t1, r1)
})
