library(RInstatClimatic)

x <- tibble::as_tibble(data.frame(station_name = c("Agades","Birni N'Konni","Niamey_Aero","Zinder"), mean_rain = as.numeric(unlist(c(NA,NA,NA,NA))), st_dv_rain = as.numeric(unlist(c(NA,NA,NA,NA))), n_na_rain = as.integer(unlist(c(101, 88,  122, 112)))))
y1 <- climatic_summary(data=daily_niger,date_time ="date", by = c("station_name"), station = "station_name", to = "station", elements ="rain", summaries = c(mean = "mean", st_dv = "sd", n_na = "naflex::na_n"))
y2 <- climatic_summary(data=daily_niger,date_time ="date", by = c("station_name","year","month"), station = "station_name", to = "monthly", elements ="rain", summaries = c(mean = "mean", st_dv = "sd", n_na = "naflex::na_n"))

test_that("returns a vector of expected length", {
  expect_length(y1,4)
  expect_length(y2,6)
})

test_that("Returns correct summaries", {
  expect_equal(x,y1)
  #expect_equal(colnames(x))
  #expect_setequal()
})

#test_that("Gives the expected warning / Error message", {
#  expect_warning()
#  expect_message()
#})
#
#test_that("Returns a output object", {
#  expect_
#})