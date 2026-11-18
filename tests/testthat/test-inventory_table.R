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
all_elements <- c("tmax","tmin","rain","hmax","hmin","sunh","ws","wd")
list_of_days <- c("day","doy")

#Functions output for one element (rain) for day
x_day_rain <- inventory_table(data = daily_niger, date = "date", elements = c("rain"), 
                              station = "station_name", year = "year", month = "month", 
                              day = "day")
#Functions output for all elements for day
x_day_all_elements <- inventory_table(data = daily_niger, date = "date", 
                                      elements = all_elements, 
                                      station = "station_name", year = "year", month = "month", 
                                      day = "day")
#Functions output for one element (rain) for doy
x_doy_rain <- inventory_table(data = daily_niger, date = "date", elements = c("rain"), 
                              station = "station_name", year = "year", month = "month", 
                              day = "doy")
#Functions output for all elements for doy
x_doy_all_elements <- inventory_table(data = daily_niger, date = "date", 
                                      elements = all_elements, 
                                      station = "station_name", year = "year", month = "month", 
                                      day = "doy")

#Expected output for one element (rain) for day
for (i in list_of_days) {
  inventory_data <- daily_niger %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(c("rain")), names_to = "element") %>%
    dplyr::select(c(tidyselect::all_of(c("station_name", "year", "month")), i, element, value)) %>%
    dplyr::mutate(value = ifelse(is.na(value), "M", "X"))
  summary_data <- inventory_data %>%
    dplyr::group_by(station_name, element, year, month)  %>%
    dplyr::summarise(Available = sum(value == "X"), Missing = sum(value == "M"))
  inventory_data_wider <- inventory_data %>%
    tidyr::pivot_wider(id_cols = c(tidyselect::all_of(c("station_name", "year", "month")), element),
                       names_from = i, values_from = value)
  inventory <- dplyr::full_join(inventory_data_wider, summary_data)
  assign(paste0("y_", i ,"_rain"),inventory)
}

#Expected output for one element (rain) for day
for (i in list_of_days) {
  inventory_data <- daily_niger %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(c("tmax","tmin","rain","hmax","hmin","sunh","ws","wd")), names_to = "element") %>%
    dplyr::select(c(tidyselect::all_of(c("station_name", "year", "month")), i, element, value)) %>%
    dplyr::mutate(value = ifelse(is.na(value), "M", "X"))
  summary_data <- inventory_data %>%
    dplyr::group_by(station_name, element, year, month)  %>%
    dplyr::summarise(Available = sum(value == "X"), Missing = sum(value == "M"))
  inventory_data_wider <- inventory_data %>%
    tidyr::pivot_wider(id_cols = c(tidyselect::all_of(c("station_name", "year", "month")), element),
                       names_from = i, values_from = value)
  inventory <- dplyr::full_join(inventory_data_wider, summary_data)
  assign(paste0("y_",i,"_all_elements"),inventory)
}

test_that("inventory_table returns correct length", {
  expect_length(x_day_rain, 37)
  expect_length(x_doy_rain, 372)
  expect_length(x_day_all_elements, 37)
  expect_length(x_doy_all_elements, 372)
})

test_that("inventory_table returns correct result", {
  expect_equal(x_day_rain,y_day_rain)
  expect_equal(x_doy_rain,y_doy_rain)
  expect_equal(x_day_all_elements,y_day_all_elements)
  expect_equal(x_doy_all_elements,y_doy_all_elements)
})

inputs <- list(data = daily_niger, date = "date", elements = c("rain"), station = "station_name", 
               year = "year", month = "month", day = "day")

test_that("inventory returns an error whn conditions are not met",{
  expect_error(inventory_table(),)
  expect_error(do.call(inventory_table, inputs[-1]))
  expect_error(do.call(inventory_table, inputs[-2]))
  expect_error(do.call(inventory_table, inputs[-3]))
})
