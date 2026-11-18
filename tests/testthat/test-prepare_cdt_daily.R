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
library(tidyr)

data("daily_niger"); data("stations_niger")
# create metadata
metadata <- stations_niger %>% 
  select(-id, -daily) %>% 
  tidyr::pivot_longer(cols = c(lat, long, alt), names_to = "names", values_to = "values")

# create daily summary data
daily_summary_data <- daily_niger %>%
  dplyr::group_by(station_name, year, date) %>%
  dplyr::summarise(date = date, sum = sum(tmax))

# create expected daily output
x_daily_data <- daily_summary_data %>%
  dplyr::ungroup() %>%
  dplyr::select(c(station_name, date, sum)) %>%
  tidyr::pivot_longer(cols = c(date), names_to = ".x", values_to = "names") %>%
  dplyr::select(-c(`.x`)) %>%
  mutate(names = as.character(names)) %>%
  dplyr::rename(values = sum)
x_daily <- dplyr::bind_rows(metadata, x_daily_data)
x_daily <- x_daily %>%
  tidyr::pivot_wider(names_from = station_name, values_from = values)

# create functions daily output
y_daily <- prepare_cdt_daily(data = daily_summary_data, date_time = "date",
                             station = "station_name", element = "sum", metadata = stations_niger,
                             latitude = "lat", longitude = "long", altitude = "alt")

test_that("prepare_cdt_daily gives correct values", {
  expect_equal(x_daily,y_daily)
})

test_that("prepare_cdt_daily returns an error when conditions are not met", {
  mandatory_inputs <- list(data = daily_summary_data, date_time = "date",
                           station = "station_name", element = "sum", metadata = stations_niger,
                           latitude = "lat", longitude = "long", altitude = "alt")
  
  expect_error(prepare_cdt_daily(),)
  expect_error(do.call(prepare_cdt_daily, mandatory_inputs[-1]))
  expect_error(do.call(prepare_cdt_daily, mandatory_inputs[-2]))
  expect_error(do.call(prepare_cdt_daily, mandatory_inputs[-3]))
  expect_error(do.call(prepare_cdt_daily, mandatory_inputs[-4]))
  expect_error(do.call(prepare_cdt_daily, mandatory_inputs[-5]))
  expect_error(do.call(prepare_cdt_daily, mandatory_inputs[-6]))
  expect_error(do.call(prepare_cdt_daily, mandatory_inputs[-7]))
  expect_error(do.call(prepare_cdt_daily, mandatory_inputs[-8]))
})