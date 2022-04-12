library(cdms.products)
library(dplyr)
library(readr)

niger51 <- daily_niger %>%
  dplyr::filter(year == 1951)

niger_dekad <- niger51 %>%
  mutate(dekad = dekad(date)) %>%
  group_by(station_name, year, dekad) %>%
  summarise(rain = sum(rain))

df_dekad <- left_join(niger_dekad, stations_niger, by = "station_name")

x_dekad <- prepare_geoclim(data = df_dekad, year = "year", type_col = "dekad",
                           element = "rain", station_id = "id", latitude = "lat",
                           longitude = "long", type = "dekad")

y_dekad <- export_geoclim_dekad(data = df_dekad, dekad = "dekad", year = "year",
                          element = "rain", station_id = "id", latitude = "lat",
                          longitude = "long", metadata = NULL, 
                          join_by = NULL, add_cols = NULL,
                          file_path = paste0("GEOCLIM-",  "rain", ".csv"))
y_dekad <- readr::read_csv(paste0("GEOCLIM-",  "rain", ".csv")) 

test_that("export_geoclim_dekad gives correct values", {
  expect_equal(x_dekad, y_dekad)
})

x_dekad <- prepare_geoclim(data = df_dekad, year = "year", type_col = "dekad",
                           element = "rain", station_id = "id", latitude = "lat",
                           longitude = "long", type = "dekad", 
                           metadata = stations_niger, join_by = "station_name")

y_dekad <- export_geoclim_dekad(data = df_dekad, year = "year", dekad = "dekad",
                          element = "rain", station_id = "id", latitude = "lat",
                          longitude = "long", type = "dekad", 
                          metadata = stations_niger, join_by = "station_name",
                          add_cols = NULL,
                          file_path = paste0("GEOCLIM-",  "rain", ".csv"))
y_dekad <- readr::read_csv(paste0("GEOCLIM-",  "rain", ".csv")) 

test_that("export_geoclim_dekad gives correct values", {
  expect_equal(x_dekad, y_dekad)
})
