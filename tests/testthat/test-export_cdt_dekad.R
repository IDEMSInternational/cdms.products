library(cdms.products)
library(dplyr)
library(readr)

data("daily_niger"); data("stations_niger")

# create dekad summary data
dekad_summary_data <- daily_niger %>%
  dplyr::mutate(dekad_date = dekad(daily_niger$date)) %>%
  dplyr::group_by(station_name, year, dekad_date) %>%
  dplyr::summarise(date = dplyr::first(date), sum = sum(tmax))
# create expected dekad output
x_dekad <- prepare_cdt(data = dekad_summary_data, date_time = "date", year = "year",
                       station = "station_name", element = "sum", metadata = stations_niger, 
                       latitude = "lat", longitude = "long", altitude = "alt", type = "dekad")

# create functions dekad output
y_dekad <- export_cdt_dekad(data = dekad_summary_data, station = "station_name", 
                      element = "sum", latitude = "lat", longitude = "long", 
                      altitude = "alt", type =  "dekad", date_time = "date", 
                      metadata = stations_niger, 
                      file_path = paste0("CDT-", element, ".csv"))
y_dekad <- read_csv("CDT-sum.csv")

test_that("export_cdt gives correct values", {
  expect_equal(y_dekad, x_dekad)
})

test_that("export_cdt_dekad returns an error when conditions are not met", {
  mandatory_inputs <- list(data = daily_summary_data, station = "station_name", 
                           element = "sum", latitude = "lat", longitude = "long", 
                           altitude = "alt", date_time = "date", 
                           metadata = stations_niger)
  
  expect_error(export_cdt_dekad(),)
  expect_error(do.call(export_cdt_dekad, mandatory_inputs[-1]))
  expect_error(do.call(export_cdt_dekad, mandatory_inputs[-2]))
  expect_error(do.call(export_cdt_dekad, mandatory_inputs[-3]))
  expect_error(do.call(export_cdt_dekad, mandatory_inputs[-4]))
  expect_error(do.call(export_cdt_dekad, mandatory_inputs[-5]))
  expect_error(do.call(export_cdt_dekad, mandatory_inputs[-6]))
  expect_error(do.call(export_cdt_dekad, mandatory_inputs[-7]))
  expect_error(do.call(export_cdt_dekad, mandatory_inputs[-8]))
})