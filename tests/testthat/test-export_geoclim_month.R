library(cdms.products)
library(dplyr)
library(readr)

#' # Calculate monthly summaries for the rainfall column
summary_data <- daily_niger %>% dplyr::group_by(year, month, station_name) %>%
  dplyr::summarise(mean_rain = mean(rain))

element <- "mean_rain"
y_dekad <- export_geoclim_month(data = summary_data, year = "year", month = "month",
                          station_id = "station_name",
                          element = "mean_rain", metadata = stations_niger,
                          join_by = "station_name",
                          latitude = "lat", longitude = "long")
y_dekad <- as.data.frame(readr::read_csv(paste0("GEOCLIM-", element, ".csv")))
colnames(y_dekad)[1:2] <- c("id", "id.1")

x_dekad <- prepare_geoclim_month(data = summary_data, year = "year", month = "month",
                    station_id = "station_name",
                    element = "mean_rain", metadata = stations_niger,
                    join_by = "station_name",
                    latitude = "lat", longitude = "long")
columns <- names(x_dekad)[-c(2, 6)]
x_dekad[, columns] <- lapply(columns, function(x) as.numeric(x_dekad[[x]]))
x_dekad <- data.frame(x_dekad)

test_that("export_geoclim_month gives correct values", {
  expect_equal(x_dekad, y_dekad)
})

test_that("export_geoclim_month returns an error when numbers outside valid ranges are supplied", {
  entries_outside_valid_ranges <- data.frame(year = character(), station_name = integer(), 
                                             month = character(), mean_rain = character())
  
  expect_error(export_geoclim_month(data = entries_outside_valid_ranges, year = "year", month = "month",
                                    station_id = "station_name",
                                    element = "mean_rain", metadata = stations_niger,
                                    join_by = "station_name",
                                    latitude = "lat", longitude = "long"), )
})

test_that("export_geoclim_month  does not return an error when an empty dataframe is supplied", {
  empty_metadata <- data.frame(station_name = character(), id = integer(),
                               lat = numeric(), long = numeric(), alt = integer(), daily = logical())
  empty_dataframe <- data.frame(station_name = character(), year = integer(),
                                month = factor(), mean_rain  = numeric())
  expect_error(export_geoclim_month(data = empty_dataframe, year = "year", month = "month",
                                    station_id = "station_name",
                                    element = "mean_rain", metadata = empty_metadata,
                                    join_by = "station_name",
                                    latitude = "lat", longitude = "long"), NA)
})

test_that("export_geoclim_month returns an error when an data frame with incorrectly formatted data  is supplied", {
  poorly_formatted_data <- lapply(summary_data, as.character)
  expect_error(export_geoclim_month(data = poorly_formatted_data, year = "year", month = "month",
                                    station_id = "station_name",
                                    element = "mean_rain", metadata = empty_metadata,
                                    join_by = "station_name",
                                    latitude = "lat", longitude = "long"), )
})
