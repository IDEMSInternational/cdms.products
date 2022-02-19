library(cdms.products)
library(dplyr)
library(naflex)

niger <- daily_niger %>%
  filter(year %in% 1950:1951)

test_that("Max and min are correcty calculated", {
  x_monthly <- niger %>% 
    group_by(station_name, year, month) %>% 
    summarise(max_tmax = max(tmax), min_tmax = min(tmax))
  
  y_monthly <- climatic_extremes(data = niger, date_time = "date", year = "year", month = "month",
                                 to = "monthly", station = "station_name",
                                 elements = "tmax", max_val = TRUE, min_val = TRUE)
  expect_equal(x_station, y_station)
})

test_that("Date cols are correctly calculated", {
  t_first <- climatic_extremes(data = niger, date_time = "date", year = "year", month = "month",
                               to = "monthly", station = "station_name",
                               elements = "tmax", max_val = TRUE, first = TRUE, n_dates = TRUE,
                               last_date = TRUE)
  r_first <- niger %>%
    dplyr::group_by(station_name, year, month) %>% 
    dplyr::mutate(max_tmax = max(tmax)) %>%
    dplyr::filter(tmax == max_tmax, .preserve = TRUE) %>%
    summarise(max_tmax = first(max_tmax),
              first_date = first(date),
              n_date = n(),
              last_date = last(date))
    
  expect_equal(t_first, r_first)
})
