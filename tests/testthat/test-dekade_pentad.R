context("dekade/pentad output")

library(RInstatClimatic)

x <- as.Date("1999-03-01")
y <- c(as.Date("1999-01-01"), as.Date("1999-01-02"), as.Date("1999-09-09"))
z <- c(y, NA, x)
w <- c(as.Date("2004-10-10"), as.Date("2003-10-10"))

test_that("dekade is giving the correct day", {
  expect_equal(dekade(x), 7)
  expect_equal(dekade(y), c(1, 1, 25))
  expect_equal(dekade(z), c(1, 1, 25, NA, 7))
  expect_equal(dekade(w[1]), dekade(w[2]))
})

test_that("pentad is giving the correct day", {
  expect_equal(pentad(x), 13)
  expect_equal(pentad(y), c(1, 1, 50))
  expect_equal(pentad(z), c(1, 1, 50, NA, 13))
  expect_equal(pentad(w[1]), pentad(w[2]))
})
