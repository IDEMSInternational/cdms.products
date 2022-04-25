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
y_dekad <- readr::read_csv(paste0("CDT-", element, ".csv"))

test_that("export_cdt gives correct values", {
  expect_equal(y_dekad, x_dekad)
})

test_that("export_cdt_dekad returns an error when conditions are not met", {
  mandatory_inputs <- list(data = dekad_summary_data, station = "station_name", 
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

test_that("export_cdt_dekad returns an error when numbers outside valid ranges are supplied", {
  entries_outside_valid_ranges <- data.frame(year = character(), station_name = integer(), 
                                dekad_date = character(), date =  numeric(), sum = character())
  expect_error(export_cdt_dekad(data = entries_outside_valid_ranges, station = "station_name", 
                                element = "sum", latitude = "lat", longitude = "long", 
                                altitude = "alt", type =  "dekad", date_time = "date", 
                                metadata = stations_niger, 
                                file_path = paste0("CDT-", element, ".csv")),)
})

test_that("export_cdt_dekad  does not return an error when an empty dataframe is supplied", {
  empty_metadata <- data.frame(station_name = character(), id = integer(), 
                              lat = numeric(), long = numeric(), alt = integer(), daily = logical())
  empty_dataframe <- data.frame(station_name = character(), year = integer(), 
                               dekad_date = numeric(), date =  as.Date(integer(0)), sum = numeric())
  expect_error(export_cdt_dekad(data = empty_dataframe, station = "station_name", 
                               element = "sum", latitude = "lat", longitude = "long", 
                               altitude = "alt", type =  "dekad", date_time = "date", 
                               metadata = empty_metadata, 
                               file_path = paste0("CDT-", element, ".csv")),NA)
  
})

test_that("export_cdt_dekad returns an error when an data frame with incorrectly formatted data  is supplied", {
  Poorly_firmatted_data <- lapply(dekad_summary_data, as.character)
  expect_error(export_cdt_dekad(data = Poorly_firmatted_data, station = "station_name", 
                                element = "sum", latitude = "lat", longitude = "long", 
                                altitude = "alt", type =  "dekad", date_time = "date", 
                                metadata = stations_niger, 
                                file_path = paste0("CDT-", element, ".csv")),)
})