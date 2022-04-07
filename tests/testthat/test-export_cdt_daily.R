library(cdms.products)
library(dplyr)
library(readr)

data("daily_niger"); data("stations_niger")

# create daily summary data
daily_summary_data <- daily_niger %>%
  dplyr::group_by(station_name, year, date) %>%
  dplyr::summarise(date = date, sum = sum(tmax))

# create expected daily output
x_daily <- prepare_cdt(data = daily_summary_data, date_time = "date", year = "year",
                       station = "station_name", element = "sum", metadata = stations_niger, 
                       latitude = "lat", longitude = "long", altitude = "alt",type = "daily")

# create functions daily output
y_daily <- export_cdt_daily(data = daily_summary_data, station = "station_name", 
                      element = "sum", latitude = "lat", longitude = "long", 
                      altitude = "alt", type =  "daily", date_time = "date", 
                      metadata = stations_niger, 
                      file_path = paste0("CDT-", element, ".csv"))
y_daily <- read_csv(paste0("CDT-", element, ".csv"))

test_that("export_cdt_daily gives correct values", {
  expect_equal(y_daily, x_daily)
})

test_that("export_cdt_daily returns an error when conditions are not met", {
  mandatory_inputs <- list(data = daily_summary_data, station = "station_name", 
                           element = "sum", latitude = "lat", longitude = "long", 
                           altitude = "alt", date_time = "date", 
                           metadata = stations_niger)
  
  expect_error(export_cdt_daily(),)
  expect_error(do.call(export_cdt_daily, mandatory_inputs[-1]))
  expect_error(do.call(export_cdt_daily, mandatory_inputs[-2]))
  expect_error(do.call(export_cdt_daily, mandatory_inputs[-3]))
  expect_error(do.call(export_cdt_daily, mandatory_inputs[-4]))
  expect_error(do.call(export_cdt_daily, mandatory_inputs[-5]))
  expect_error(do.call(export_cdt_daily, mandatory_inputs[-6]))
  expect_error(do.call(export_cdt_daily, mandatory_inputs[-7]))
  expect_error(do.call(export_cdt_daily, mandatory_inputs[-8]))
})