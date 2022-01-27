context("timeseries output")

library(RInstatClimatic)

# Get the data --------------------------------------------------------------------------------------------------------
niger_data <- readRDS("C:/Users/lzc1n17/OneDrive - University of Southampton/PhD/IDEMS/RInstatClimatic/Niger4_day_month_year.RDS")
niger_data <- niger_data$get_data_frame()
niger_data_1 <- niger_data$Data
niger_monthly_data <- niger_data$Data_by_year_month
niger_yearly_data <- niger_data$Data_by_year
# Using month to make multiple stations
niger_monthly_data$station = ifelse(niger_monthly_data$month_abbr %in% c("Jan", "Feb", "Mar"), "Hi", "Ho")

# no station
ts_line_plot(niger_yearly_data, time = "year", element = c("max_rain"), facets = "none")
ts_line_plot(niger_yearly_data, time = "year", element = c("max_rain"), facets = "elements")

# station
ts_line_plot(niger_monthly_data, time = "year", element = c("max_rain"), station = "station", facets = "none")
ts_line_plot(niger_monthly_data, time = "year", element = c("max_rain"), station = "station", facets = "elements")
ts_line_plot(niger_monthly_data, time = "year", element = c("max_rain"), station = "station", facets = "stations")
ts_line_plot(niger_monthly_data, time = "year", element = c("max_rain"), station = "station", facets = "both")

# no station
ts_line_plot(niger_yearly_data, time = "year", element = c("max_rain", "max_tmax"), facets = "none")
ts_line_plot(niger_yearly_data, time = "year", element = c("max_rain", "max_tmax"), facets = "elements")

# station
ts_line_plot(niger_monthly_data, time = "year", element = c("max_rain", "max_tmax"), station = "station", facets = "none")
ts_line_plot(niger_monthly_data, time = "year", element = c("max_rain", "max_tmax"), station = "station", facets = "elements")
ts_line_plot(niger_monthly_data, time = "year", element = c("max_rain", "max_tmax"), station = "station", facets = "stations")
ts_line_plot(niger_monthly_data, time = "year", element = c("max_rain", "max_tmax"), station = "station", facets = "both")
