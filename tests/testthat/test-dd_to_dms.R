# Copyright 2022 IDEMS International Community Interest Company
#
# This file is part of cdms.products.
#
# cdms.products is free software: you can redistribute it and/or modify it under the
# terms of the GNU Lesser General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later version.
#
# cdms.products is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License along with
# cdms.products. If not, see <https://www.gnu.org/licenses/>.

library(cdms.products)

positive_int <- 3
negative_int <- -3
positive_dec1 <- 0.3
negative_dec1 <- -0.3
positive_dec2 <- 3.3
negative_dec2 <- -3.3
N_int <- "03 00 00 N"
S_int <- "03 00 00 S"
E_int <- "003 00 00 E"
W_int <- "003 00 00 W"
N_dec1 <- "00 18 00 N"
S_dec1 <- "00 18 00 S"
E_dec1 <- "000 18 00 E"
W_dec1 <- "000 18 00 W"
N_dec2 <- "03 17 60 N"
S_dec2 <- "03 17 60 S"
E_dec2 <- "003 17 60 E"
W_dec2 <- "003 17 60 W"

test_that("dd_to_dms returns a vector of length 1", {
  expect_length(dd_to_dms(30,TRUE),1)
})
test_that("dd_to_dms returns a corect output for integers", {
  expect_equal(dd_to_dms(positive_int, TRUE), N_int)
  expect_equal(dd_to_dms(negative_int, TRUE), S_int)
  expect_equal(dd_to_dms(positive_int, FALSE), E_int)
  expect_equal(dd_to_dms(negative_int, FALSE), W_int)
})
test_that("dd_to_dms returns a corect output for decimals < 1", {
  expect_equal(dd_to_dms(positive_dec1, TRUE), N_dec1)
  expect_equal(dd_to_dms(negative_dec1, TRUE), S_dec1)
  expect_equal(dd_to_dms(positive_dec1, FALSE), E_dec1)
  expect_equal(dd_to_dms(negative_dec1, FALSE), W_dec1)
})
test_that("dd_to_dms returns a corect output decimals >=1", {
  expect_equal(dd_to_dms(positive_dec2, TRUE), N_dec2)
  expect_equal(dd_to_dms(negative_dec2, TRUE), S_dec2)
  expect_equal(dd_to_dms(positive_dec2, FALSE), E_dec2)
  expect_equal(dd_to_dms(negative_dec2, FALSE), W_dec2)
})
