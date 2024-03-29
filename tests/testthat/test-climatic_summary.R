library(cdms.products)
library(dplyr)
library(naflex)

niger <- daily_niger %>%
  filter(year %in% 1950:1951)

x_daily <- niger %>% 
  group_by(date) %>% 
  summarise(mean_rain = mean(rain), st_dv_rain = sd(rain), n_na_rain = naflex::na_n(rain))

x_monthly <- niger %>% 
  group_by(station_name, year, month) %>% 
  summarise(mean_rain = mean(rain), st_dv_rain = sd(rain), n_na_rain = naflex::na_n(rain))

x_annual <- niger %>% 
  group_by(station_name, year) %>% 
  summarise(mean_rain = mean(rain), st_dv_rain = sd(rain), n_na_rain = naflex::na_n(rain))

x_station <- niger %>% 
  group_by(station_name) %>% 
  summarise(mean_rain = mean(rain), st_dv_rain = sd(rain), n_na_rain = naflex::na_n(rain))

y_hourly <- climatic_summary(data = niger, date_time = "date", 
                             by = "station_name", station = "station_name", 
                             to = "hourly", elements ="rain", 
                             summaries = c(mean = "mean", st_dv = "sd", n_na = "naflex::na_n"))

y_daily <- climatic_summary(data = niger, date_time = "date", by = "station_name", 
                            station = "station_name", to = "daily", elements ="rain", 
                            summaries = c(mean = "mean", st_dv = "sd", n_na = "naflex::na_n"))

y_pentad <- climatic_summary(data = niger, date_time = "date", by = "station_name", 
                             station = "station_name", to = "pentad", elements = "rain", 
                             summaries = c(mean = "mean", st_dv = "sd", n_na = "naflex::na_n"))

y_dekadal <- climatic_summary(data = niger, date_time = "date", by = "station_name", 
                              station = "station_name", to = "dekadal", elements = "rain", 
                              summaries = c(mean = "mean", st_dv = "sd", n_na = "naflex::na_n"))

y_monthly <- climatic_summary(data = niger, date_time = "date", year = "year", month = "month", 
                              by = "station_name", station = "station_name", to = "monthly", 
                              elements = "rain", 
                              summaries = c(mean = "mean", st_dv = "sd", n_na = "naflex::na_n"))

y_annual_within_year <- climatic_summary(data = niger, date_time = "date", by = "station_name", 
                                         station = "station_name", to = "annual-within-year", 
                                         elements ="rain", 
                                         summaries = c(mean = "mean", st_dv = "sd", n_na = "naflex::na_n"))

y_annual <- climatic_summary(data = niger, date_time = "date", by = "station_name", 
                             station = "station_name", to = "annual", elements ="rain", 
                             summaries = c(mean = "mean", st_dv = "sd", n_na = "naflex::na_n"))

y_longterm_monthly <- climatic_summary(data = niger, date_time = "date", by = "station_name", 
                                       station = "station_name", to = "longterm-monthly", elements ="rain", 
                                       summaries = c(mean = "mean", st_dv = "sd", n_na = "naflex::na_n"))

y_longterm_within_year <- climatic_summary(data = niger, date_time = "date", by = "station_name", 
                                           station = "station_name", to = "longterm-within-year", 
                                           elements ="rain", summaries = c(mean = "mean", st_dv = "sd", n_na = "naflex::na_n"))

y_station <- climatic_summary(data = niger, date_time = "date", by = "station_name", station = "station_name", 
                              to = "station", elements = "rain", summaries = c(mean = "mean", st_dv = "sd", n_na = "naflex::na_n"))

y_overall <- climatic_summary(data = niger, date_time = "date", by = "station_name", 
                              station = "station_name", to = "overall", 
                              elements ="rain", 
                              summaries = c(mean = "mean", st_dv = "sd", n_na = "naflex::na_n"))


test_that("Returns correct station summaries", {
  expect_length(y_station, 4)
  expect_equal(x_station, y_station)
  expect_setequal(colnames(x_station), colnames(y_station))
})

test_that("Returns correct daily summaries", {
  expect_length(y_daily, 4)
  expect_equal(x_daily, y_daily)
  expect_setequal(colnames(x_daily), colnames(y_daily))
})
 
test_that("Returns correct monthly summaries", {
 expect_length(y_monthly, 6)
 expect_equal(x_monthly, y_monthly)
 expect_setequal(colnames(x_monthly), colnames(y_monthly))
})

test_that("Returns correct annual summaries", {
 expect_length(y_annual, 5)
 expect_equal(x_annual, y_annual)
 expect_setequal(colnames(x_annual), colnames(y_annual))
})

test_that("climatic_summary filters by day of year correctly", {
  t_annual_doy <- climatic_summary(data = niger, date_time = "date", by = "station_name", 
                               station = "station_name", to = "annual", elements ="rain", 
                               summaries = c(mean = "mean"), doy = "doy", doy_first = 30,
                               doy_last = 200)
  r_annual_doy <- niger %>% 
    group_by(station_name, year) %>% 
    filter(doy >= 30 & doy <= 200, .preserve = TRUE) %>%
    summarise(mean_rain = mean(rain))
  expect_equal(t_annual_doy, r_annual_doy)
})
