library(RInstatClimatic)
data("daily_niger")
all_elements <- c("tmax","tmin","rain","hmax","hmin","sunh","ws","wd")

#Functions output for one element (rain) for day
x1_day_inventory <- inventory_table(data = daily_niger, date = "date", elements = c("rain"), 
                                    station = "station_name", year = "year", month = "month", 
                                    day = "day")
#Functions output for all elements for day
x2_day_inventory <- inventory_table(data = daily_niger, date = "date", 
                                    elements = all_elements, 
                                    station = "station_name", year = "year", month = "month", 
                                    day = "day")
#Functions output for one element (rain) for doy
x1_doy_inventory <- inventory_table(data = daily_niger, date = "date", elements = c("rain"), 
                                    station = "station_name", year = "year", month = "month", 
                                    day = "doy")
#Functions output for all elements for doy
x2_doy_inventory <- inventory_table(data = daily_niger, date = "date", 
                                    elements = all_elements, 
                                    station = "station_name", year = "year", month = "month", 
                                    day = "doy")

#Expected output for one element (rain) for day
y1_day_inventory_data <- daily_niger %>%
  tidyr::pivot_longer(cols = tidyselect::all_of(c("rain")), names_to = "element") %>%
  dplyr::select(c(tidyselect::all_of(c("station_name", "year", "month")), day, element, value)) %>%
  dplyr::mutate(value = ifelse(is.na(value), "M", "X"))
y1_day_summary_data <- y1_day_inventory_data %>%
  dplyr::group_by(station_name, element, year, month)  %>%
  dplyr::summarise(Available = sum(value == "X"), Missing = sum(value == "M"))
y1_day_inventory_data_wider <- y1_day_inventory_data %>%
  tidyr::pivot_wider(id_cols = c(tidyselect::all_of(c("station_name", "year", "month")), element),
                     names_from = day, values_from = value)
y1_day_inventory <- dplyr::full_join(y1_day_inventory_data_wider, y1_day_summary_data)

#Expected output for all elements for doy
y2_day_inventory_data <- daily_niger %>%
  tidyr::pivot_longer(cols = tidyselect::all_of(all_elements), names_to = "element") %>%
  dplyr::select(c(tidyselect::all_of(c("station_name", "year", "month")), day, element, value)) %>%
  dplyr::mutate(value = ifelse(is.na(value), "M", "X"))
y2_day_summary_data <- y2_day_inventory_data %>%
  dplyr::group_by(station_name, element, year, month)  %>%
  dplyr::summarise(Available = sum(value == "X"), Missing = sum(value == "M"))
y2_day_inventory_data_wider <- y2_day_inventory_data %>%
  tidyr::pivot_wider(id_cols = c(tidyselect::all_of(c("station_name", "year", "month")), element),
                     names_from = day, values_from = value)
y2_day_inventory <- dplyr::full_join(y2_day_inventory_data_wider, y2_day_summary_data)


#Expected output for one element (rain) for doy
y1_doy_inventory_data <- daily_niger %>%
  tidyr::pivot_longer(cols = tidyselect::all_of(c("rain")), names_to = "element") %>%
  dplyr::select(c(tidyselect::all_of(c("station_name", "year", "month")), day, element, value)) %>%
  dplyr::mutate(value = ifelse(is.na(value), "M", "X"))
y1_doy_summary_data <- y1_doy_inventory_data %>%
  dplyr::group_by(station_name, element, year, month)  %>%
  dplyr::summarise(Available = sum(value == "X"), Missing = sum(value == "M"))
y1_doy_inventory_data_wider <- y1_doy_inventory_data %>%
  tidyr::pivot_wider(id_cols = c(tidyselect::all_of(c("station_name", "year", "month")), element),
                     names_from = day, values_from = value)
y1_doy_inventory <- dplyr::full_join(y1_doy_inventory_data_wider, y1_doy_summary_data)

#Expected output for all elements for doy
y2_doy_inventory_data <- daily_niger %>%
  tidyr::pivot_longer(cols = tidyselect::all_of(all_elements), names_to = "element") %>%
  dplyr::select(c(tidyselect::all_of(c("station_name", "year", "month")), day, element, value)) %>%
  dplyr::mutate(value = ifelse(is.na(value), "M", "X"))
y2_doy_summary_data <- y2_doy_inventory_data %>%
  dplyr::group_by(station_name, element, year, month)  %>%
  dplyr::summarise(Available = sum(value == "X"), Missing = sum(value == "M"))
y2_doy_inventory_data_wider <- y2_doy_inventory_data %>%
  tidyr::pivot_wider(id_cols = c(tidyselect::all_of(c("station_name", "year", "month")), element),
                     names_from = day, values_from = value)
y2_doy_inventory <- dplyr::full_join(y2_doy_inventory_data_wider, y2_doy_summary_data)

test_that("inventory_table returns correct length", {
  expect_length(x1_day_inventory, 37)
  expect_length(x2_day_inventory, 37)
  expect_length(x1_doy_inventory, 372)
  expect_length(x2_doy_inventory, 372)
})
test_that("inventory_table returns correct result", {
  expect_equal(x1_day_inventory,y1_day_inventory)
  expect_equal(x2_day_inventory,y2_day_inventory)
  expect_equal(x1_doy_inventory,y1_doy_inventory)
  expect_equal(x2_doy_inventory,y2_doy_inventory)
})
