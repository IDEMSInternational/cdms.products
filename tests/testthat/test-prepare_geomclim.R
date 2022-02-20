library(cdms.products)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

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
r1 <- df_dekad %>% 
  rename(lon = long) %>%
  pivot_wider(id_cols = c(id, lat, lon, year), names_from = dekad,
              values_from = rain)

df_pentad <- left_join(niger_pentad, stations_niger, by = "station_name")
r2 <- df_pentad %>% 
  rename(lon = long) %>%
  pivot_wider(id_cols = c(id, lat, lon, year), names_from = pentad,
              values_from = rain)

test_that("prepre_climdex correctly formats dekad data with metadata included", {
  t1 <- prepare_geoclim(data = df_dekad, year = "year", type_col = "dekad",
                        element = "rain", station_id = "id", latitude = "lat",
                        longitude = "long", type = "dekad")
  expect_equal(t1, r1)
})

test_that("prepre_climdex correctly formats dekad data with metadata separate", {
  t1 <- prepare_geoclim(data = niger_dekad, year = "year", type_col = "dekad",
                        element = "rain", station_id = "id", latitude = "lat",
                        longitude = "long", type = "dekad", 
                        metadata = stations_niger, join_by = "station_name")
  expect_equal(t1, r1)
})

test_that("prepre_climdex correctly formats pentad data with metadata included", {
  t2 <- prepare_geoclim(data = df_pentad, year = "year", type_col = "pentad",
                        element = "rain", station_id = "id", latitude = "lat",
                        longitude = "long", type = "pentad")
  expect_equal(t2, r2)
})

test_that("prepre_climdex correctly formats pentad data with metadata separate", {
  t2 <- prepare_geoclim(data = df_pentad, year = "year", type_col = "pentad",
                        element = "rain", station_id = "id", latitude = "lat",
                        longitude = "long", type = "pentad", 
                        metadata = stations_niger, join_by = "station_name")
  expect_equal(t2, r2)
})
