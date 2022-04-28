library(cdms.products)
library(dplyr)
library(readr)

niger51 <- daily_niger %>%
  dplyr::filter(year == 1951)

niger_dekad <- niger51 %>%
  mutate(dekad = dekad(date)) %>%
  group_by(station_name, year, dekad) %>%
  summarise(rain = sum(rain))

niger_pentad <- niger51 %>%
  mutate(pentad = pentad(date)) %>%
  group_by(station_name, year, pentad) %>%
  summarise(rain = sum(rain))

df_dekad <- left_join(niger_dekad, stations_niger, by = "station_name")

df_pentad <- left_join(niger_pentad, stations_niger, by = "station_name")

x_dekad <- data.frame(lapply(prepare_geoclim(data = df_dekad, year = "year", type_col = "dekad",
                            element = "rain", station_id = "id", latitude = "lat",
                            longitude = "long", type = "dekad"), as.numeric))
y_dekad <- export_geoclim(data = df_dekad, year = "year", type_col = "dekad",
                           element = "rain", station_id = "id", latitude = "lat",
                           longitude = "long", type = "dekad", metadata = NULL, 
                           join_by = NULL, add_cols = NULL,
                           file_path = paste0("GEOCLIM-", "rain", ".csv"))
y_dekad <- as.data.frame(readr::read_csv(paste0("GEOCLIM-", "rain", ".csv"))) 
colnames(y_dekad)[5:40] <- paste0("X", colnames(y_dekad)[5:40])

test_that("export_geoclim gives correct values", {
  expect_equal(x_dekad, y_dekad)
})

x_dekad <- data.frame(lapply(prepare_geoclim(data = df_dekad, year = "year", type_col = "dekad",
                            element = "rain", station_id = "id", latitude = "lat",
                            longitude = "long", type = "dekad", 
                            metadata = stations_niger, join_by = "station_name"), as.numeric))
y_dekad <- export_geoclim(data = df_dekad, year = "year", type_col = "dekad",
                           element = "rain", station_id = "id", latitude = "lat",
                           longitude = "long", type = "dekad", 
                           metadata = stations_niger, join_by = "station_name",
                           add_cols = NULL,
                           file_path = paste0("GEOCLIM-",  "rain", ".csv"))
y_dekad <- as.data.frame(readr::read_csv(paste0("GEOCLIM-",  "rain", ".csv"))) 
colnames(y_dekad)[5:40] <- paste0("X", colnames(y_dekad)[5:40])

test_that("export_cdt gives correct values", {
  expect_equal(x_dekad, y_dekad)
})


x_pentad <- data.frame(lapply(prepare_geoclim(data = df_pentad, year = "year", type_col = "pentad",
                            element = "rain", station_id = "id", latitude = "lat",
                            longitude = "long", type = "pentad"), as.numeric))
y_pentad <- export_geoclim(data = df_pentad, year = "year", type_col = "pentad",
                           element = "rain", station_id = "id", latitude = "lat",
                           longitude = "long", type = "pentad", metadata = NULL, 
                           join_by = NULL, add_cols = NULL,
                           file_path = paste0("GEOCLIM-",  "rain", ".csv"))
y_pentad <- as.data.frame(readr::read_csv(paste0("GEOCLIM-",  "rain", ".csv"))) 
colnames(y_pentad)[5:76] <- paste0("X", colnames(y_pentad)[5:76])

test_that("export_geoclim gives correct values", {
  expect_equal(x_pentad, y_pentad)
})

x_pentad <- data.frame(lapply(prepare_geoclim(data = df_pentad, year = "year", type_col = "pentad",
                            element = "rain", station_id = "id", latitude = "lat",
                            longitude = "long", type = "pentad", 
                            metadata = stations_niger, join_by = "station_name"), as.numeric))
y_pentad <- export_geoclim(data = df_pentad, year = "year", type_col = "pentad",
                           element = "rain", station_id = "id", latitude = "lat",
                           longitude = "long", type = "pentad", 
                           metadata = stations_niger, join_by = "station_name",
                           add_cols = NULL,
                           file_path = paste0("GEOCLIM-",  "rain", ".csv"))
y_pentad <- as.data.frame(readr::read_csv(paste0("GEOCLIM-",  "rain", ".csv"))) 
colnames(y_pentad)[5:76] <- paste0("X", colnames(y_pentad)[5:76])

test_that("export_cdt gives correct values", {
  expect_equal(x_pentad, y_pentad)
})

