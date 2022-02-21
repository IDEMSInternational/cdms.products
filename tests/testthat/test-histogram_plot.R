library(cdms.products)
library(dplyr)
library(ggplot2)

# niger50 <- daily_niger %>%
#   dplyr::filter(year %in% 1950:1952)
# 
# histogram_plot(data = niger50, date_time = "date", elements = "rain", station = "station_name", facets = "stations")
# 
# test_that("yday_366 is giving the correct day", {
#   expect_equal(yday_366(x), x_yday)
#   expect_equal(yday_366(y), y_yday)
#   expect_equal(yday_366(z), z_yday)
#   expect_equal(yday_366(w[1]), yday_366(w[2]))
# })