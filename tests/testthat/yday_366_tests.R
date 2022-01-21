context("yday_366 output")

library(RInstatClimatic)

x <- as.Date("1999-03-01")
y <- c(as.Date("1999-01-01"), as.Date("1999-01-02"), as.Date("1999-09-09"))
z <- c(y, NA, x)
w <- c(as.Date("2004-10-10"), as.Date("2003-10-10")) # 2004 is a leap year, 2003 is not. But they should give the same day

test_that("yday_366 is giving the correct day", {
  expect_equal(yday_366(x), lubridate::yday(x) + 1)
  expect_equal(yday_366(y), c(1, 2, lubridate::yday(y[3]) + 1))
  expect_equal(yday_366(z), c(1, 2, lubridate::yday(as.Date("1999-09-09")) + 1, NA, lubridate::yday(as.Date("1999-03-01")) + 1))
  expect_equal(yday_366(w[1]), yday_366(w[2]))
})