library(cdms.products)

x <- as.Date("1999-03-01")
x_dekad <- 7
x_pentad <- 13
y <- c(as.Date("1999-01-01"), as.Date("1999-01-02"), as.Date("1999-09-09"))
y_dekad <- c(1, 1, 25)
y_pentad <- c(1, 1, 50)
z <- c(y, NA, x)
z_dekad <- c(y_dekad, NA, x_dekad)
z_pentad <- c(y_pentad, NA, x_pentad)
w <- c(as.Date("2004-10-10"), as.Date("2003-10-10"))

test_that("dekad is giving the correct day", {
  expect_equal(dekad(x), x_dekad)
  expect_equal(dekad(y), y_dekad)
  expect_equal(dekad(z), z_dekad)
  expect_equal(dekad(w[1]), dekad(w[2]))
})

test_that("pentad is giving the correct day", {
  expect_equal(pentad(x), x_pentad)
  expect_equal(pentad(y), y_pentad)
  expect_equal(pentad(z), z_pentad)
  expect_equal(pentad(w[1]), pentad(w[2]))
})
