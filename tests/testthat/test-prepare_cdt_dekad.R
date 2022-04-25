library(cdms.products)
library(dplyr)
library(tidyr)

data("daily_niger"); data("stations_niger")
# create metadata
metadata <- stations_niger %>% 
  select(-id, -daily) %>% 
  tidyr::pivot_longer(cols = c(lat, long, alt), names_to = "names", values_to = "values")

# create dekad summary data
dekad_summary_data <- daily_niger %>%
  dplyr::mutate(dekad_date = dekad(daily_niger$date)) %>%
  dplyr::group_by(station_name, year, dekad_date) %>%
  dplyr::summarise(date = dplyr::first(date), sum = sum(tmax))

# create expected dekad output
x_dekad_data <- dekad_summary_data %>%
  dplyr::ungroup() %>%
  dplyr::select(c(station_name, dekad_date, date, sum)) %>%
  mutate(year = lubridate::year(date), month = lubridate::month(date),
         dekad = dekad(date), date = paste0(year, month, dekad)) %>%
  dplyr::select(c(station_name, date, sum)) %>%
  tidyr::pivot_longer(cols = c(date), names_to = ".x", values_to = "names") %>%
  dplyr::select(-c(`.x`)) %>%
  mutate(names = as.character(names)) %>%
  dplyr::rename(values = sum)
x_dekad <- dplyr::bind_rows(metadata, x_dekad_data)
x_dekad <- x_dekad %>%
  tidyr::pivot_wider(names_from = station_name, values_from = values)

#  create functions dekad output
y_dekad <- prepare_cdt_dekad(data = dekad_summary_data, date_time = "date",
                       station = "station_name", element = "sum", metadata = stations_niger, 
                       latitude = "lat", longitude = "long", altitude = "alt")

test_that("prepare_cdt gives correct values", {
  expect_equal(x_dekad,y_dekad)
})

test_that("prepare_cdt_dekad returns an error when conditions are not met", {
  mandatory_inputs <- list(data = dekad_summary_data, date_time = "date",
                           station = "station_name", element = "sum", metadata = stations_niger,
                           latitude = "lat", longitude = "long", altitude = "alt")
  
  expect_error(prepare_cdt_dekad(),)
  expect_error(do.call(prepare_cdt_dekad, mandatory_inputs[-1]))
  expect_error(do.call(prepare_cdt_dekad, mandatory_inputs[-2]))
  expect_error(do.call(prepare_cdt_dekad, mandatory_inputs[-3]))
  expect_error(do.call(prepare_cdt_dekad, mandatory_inputs[-4]))
  expect_error(do.call(prepare_cdt_dekad, mandatory_inputs[-5]))
  expect_error(do.call(prepare_cdt_dekad, mandatory_inputs[-6]))
  expect_error(do.call(prepare_cdt_dekad, mandatory_inputs[-7]))
  expect_error(do.call(prepare_cdt_dekad, mandatory_inputs[-8]))
})
