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
timeseries_plot(niger_yearly_data, time = "year", element = c("max_rain"), facets = "none")
timeseries_plot(niger_yearly_data, time = "year", element = c("max_rain"), facets = "elements")

# station
timeseries_plot(niger_monthly_data, time = "year", element = c("max_rain"), station = "station", facets = "none")
timeseries_plot(niger_monthly_data, time = "year", element = c("max_rain"), station = "station", facets = "elements")
timeseries_plot(niger_monthly_data, time = "year", element = c("max_rain"), station = "station", facets = "stations")
timeseries_plot(niger_monthly_data, time = "year", element = c("max_rain"), station = "station", facets = "both")

# no station
timeseries_plot(niger_yearly_data, time = "year", element = c("max_rain", "max_tmax"), facets = "none", show.legend = FALSE) +
  viridis::scale_color_viridis(discrete = TRUE, option = "C")
timeseries_plot(niger_yearly_data, time = "year", element = c("max_rain", "max_tmax"), facets = "elements")

# station
timeseries_plot(niger_monthly_data, time = "year", element = c("max_rain", "max_tmax"), station = "station", facets = "none")
timeseries_plot(niger_monthly_data, time = "year", element = c("max_rain", "max_tmax"), station = "station", facets = "elements")
timeseries_plot(niger_monthly_data, time = "year", element = c("max_rain", "max_tmax"), station = "station", facets = "stations")
timeseries_plot(niger_monthly_data, time = "year", element = c("max_rain", "max_tmax"), station = "station", facets = "both")



# Should throw error
timeseries_plot("niger_monthly_data", time = "year", element = c("max_rain", "max_tmax"), station = "station", facets = "none")
timeseries_plot(niger_monthly_data, time = year, element = c("max_rain", "max_tmax"), station = "station", facets = "none")
timeseries_plot(niger_monthly_data, time = "year", element = c("no"), station = "station", facets = "none")
timeseries_plot(niger_monthly_data, time = "year", element = c("max_rain"), facets = "none")
timeseries_plot(niger_monthly_data, time = "year", element = c("max_rain"), facets = "nonxe")
timeseries_plot(niger_monthly_data, time = "year", element = c("max_rain"))
