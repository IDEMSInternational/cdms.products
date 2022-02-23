library(cdms.products)
library(dplyr)
library(ggplot2)
library(tidyr)

niger50 <- daily_niger %>%
  dplyr::filter(year == 1950)

agades <- niger50 %>%
  dplyr::filter(station_name == "Agades")

test_that("single element facet by station graphs are correct", {
  t1 <- timeseries_plot(data = niger50, date_time = "date", elements = "tmax", 
                        station = "station_name", facet_by = "stations")
  r1 <- ggplot(niger50, aes(x = date, y = tmax)) +
    geom_line() +
    facet_wrap(vars(station_name))
  expect_equal(gg_data(t1), gg_data(r1))
})

test_that("single element single station graphs are correct", {
  t1 <- timeseries_plot(data = agades, date_time = "date", elements = "tmax", 
                        facet_by = "none")
  r1 <- ggplot(agades, aes(x = date, y = tmax)) +
    geom_line()
  expect_equal(gg_data(t1), gg_data(r1))
})

test_that("single element single station bar graphs are correct", {
  t1 <- timeseries_plot(data = agades, date_time = "date", elements = "tmax", 
                        facet_by = "none", type = "bar")
  r1 <- ggplot(agades, aes(x = date, y = tmax)) +
    geom_col()
  expect_equal(gg_data(t1), gg_data(r1))
})

test_that("single element colour by station no facet graphs are correct", {
  t1 <- timeseries_plot(data = niger50, date_time = "date", elements = "tmax", 
                        station = "station_name", facet_by = "none")
  r1 <- ggplot(niger50, aes(x = date, y = tmax, colour = station_name)) +
    geom_line()
  expect_equal(gg_data(t1), gg_data(r1))
})

test_that("multiple element single station graphs are correct", {
  t1 <- timeseries_plot(data = agades, date_time = "date", 
                        elements = c("tmin", "tmax"), 
                        facet_by = "none")
  d1 <- agades %>%
    pivot_longer(c(tmin, tmax), names_to = "element")
  r1 <- ggplot(d1, aes(x = date, y = value, colour = element)) +
    geom_line()
  expect_equal(gg_data(t1), gg_data(r1))
})

test_that("multiple element single station facet by elements graphs are correct", {
  t1 <- timeseries_plot(data = agades, date_time = "date", 
                        elements = c("tmin", "tmax"), 
                        facet_by = "elements")
  d1 <- agades %>%
    pivot_longer(c(tmin, tmax), names_to = "element")
  r1 <- ggplot(d1, aes(x = date, y = value)) +
    geom_line() +
    facet_wrap(vars(element))
  expect_equal(gg_data(t1), gg_data(r1))
})

test_that("multiple element & multiple stations with both as facet_by graphs are correct", {
  t1 <- timeseries_plot(data = niger50, date_time = "date",
                        station = "station_name",
                        elements = c("tmin", "tmax"), 
                        facet_by = "stations-elements")
  d1 <- niger50 %>%
    pivot_longer(c(tmin, tmax), names_to = "element")
  r1 <- ggplot(d1, aes(x = date, y = value)) +
    geom_line() +
    facet_grid(vars(station_name), vars(element))
  expect_equal(gg_data(t1), gg_data(r1))
})

test_that("multiple element & multiple stations facet by elements graphs are correct", {
  t1 <- timeseries_plot(data = niger50, date_time = "date",
                        station = "station_name",
                        elements = c("tmin", "tmax"), 
                        facet_by = "elements")
  d1 <- niger50 %>%
    pivot_longer(c(tmin, tmax), names_to = "element")
  r1 <- ggplot(d1, aes(x = date, y = value, colour = station_name)) +
    geom_line() +
    facet_wrap(vars(element))
  expect_equal(gg_data(t1), gg_data(r1))
})

test_that("multiple element & multiple stations facet by stations graphs are correct", {
  t1 <- timeseries_plot(data = niger50, date_time = "date",
                        station = "station_name",
                        elements = c("tmin", "tmax"), 
                        facet_by = "stations")
  d1 <- niger50 %>%
    pivot_longer(c(tmin, tmax), names_to = "element")
  r1 <- ggplot(d1, aes(x = date, y = value, colour = element)) +
    geom_line() +
    facet_wrap(vars(station_name))
  expect_equal(gg_data(t1), gg_data(r1))
})

test_that("multiple element & multiple stations no facet graphs are correct", {
  t1 <- timeseries_plot(data = niger50, date_time = "date",
                        station = "station_name",
                        elements = c("tmin", "tmax"), 
                        facet_by = "none")
  d1 <- niger50 %>%
    pivot_longer(c(tmin, tmax), names_to = "element") %>%
    mutate(station_elements = paste(station_name, element, sep = "_"))
  r1 <- ggplot(d1, aes(x = date, y = value, colour = station_elements)) +
    geom_line()
  expect_equal(gg_data(t1), gg_data(r1))
})

test_that("points, LOBF, path and step are correctly added", {
  t1_points <- timeseries_plot(data = agades, date_time = "date", 
                               facet_by = "none",
                               elements = "tmin", add_points = TRUE)
  t1_lobf <- timeseries_plot(data = agades, date_time = "date",
                             facet_by = "none",
                             elements = "tmin", add_line_of_best_fit = TRUE)
  t1_path <- timeseries_plot(data = agades, date_time = "date",
                             facet_by = "none",
                             elements = "tmin", add_path = TRUE)
  t1_step <- timeseries_plot(data = agades, date_time = "date",
                             facet_by = "none",
                             elements = "tmin", add_step = TRUE)
  r1 <- ggplot(agades, aes(x = date, y = tmin)) +
    geom_line()
  r1_points <- r1 + geom_point()
  r1_lobf <- r1 + geom_smooth(method = "lm", formula = y ~ x)
  r1_path <- r1 + geom_path()
  r1_step <- r1 + geom_step()
  
  expect_equal(gg_data(t1_points), gg_data(r1_points))
  expect_equal(gg_data(t1_lobf), gg_data(r1_lobf))
  expect_equal(gg_data(t1_path), gg_data(r1_path))
  expect_equal(gg_data(t1_step), gg_data(r1_step))
})

test_that("facet warning is displayed", {
  expect_warning(timeseries_plot(data = niger50, date_time = "date", elements = "tmax", 
                 facet_by = "stations"))
})

