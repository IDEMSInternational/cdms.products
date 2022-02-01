library(RInstatClimatic)

x <- as.Date("1999-03-01")
x_yday <- lubridate::yday(x) + 1
y <- c(as.Date("1999-01-01"), as.Date("1999-01-02"), as.Date("1999-09-09"))
y_yday <- c(1, 2, lubridate::yday(y[3]) + 1)
z <- c(y, NA, x)
z_yday <- c(y_yday, NA, x_yday)
# 2004 is a leap year, 2003 is not. But they should give the same day
w <- c(as.Date("2004-10-10"), as.Date("2003-10-10"))

test_that("yday_366 is giving the correct day", {
  expect_equal(yday_366(x), x_yday)
  expect_equal(yday_366(y), y_yday)
  expect_equal(yday_366(z), z_yday)
  expect_equal(yday_366(w[1]), yday_366(w[2]))
})