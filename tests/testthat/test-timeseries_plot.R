context("timeseries output")

library(RInstatClimatic)
library(openair)

# Get the data --------------------------------------------------------------------------------------------------------
data("mydata")
mydata$station_var <- ifelse(mydata$ws < 5, "A", "B")

# Test the data -------------------------------------------------------------------------------------------------------
# no station
timeseries_plot(mydata, date_time = "date", elements = c("nox"), facets = "none")
timeseries_plot(mydata, date_time = "date", elements = c("nox"), facets = "elements")
timeseries_plot(mydata, date_time = "date", elements = c("nox"), facets = "stations")
timeseries_plot(mydata, date_time = "date", elements = c("nox"), facets = "both")

# station
timeseries_plot(mydata, date_time = "date", elements = c("nox"), station = "station_var", facets = "none")
timeseries_plot(mydata, date_time = "date", elements = c("nox"), station = "station_var", facets = "elements")
timeseries_plot(mydata, date_time = "date", elements = c("nox"), station = "station_var")
timeseries_plot(mydata, date_time = "date", elements = c("nox"), station = "station_var", facets = "both")

# no station
timeseries_plot(mydata, date_time = "date", elements = c("nox", "no2"), facets = "none", show.legend = FALSE) +
  viridis::scale_color_viridis(discrete = TRUE, option = "C")
timeseries_plot(mydata, date_time = "date", elements = c("nox", "no2"), facets = "elements")

# station
timeseries_plot(mydata, date_time = "date", elements = c("nox", "no2"), station = "station_var", facets = "none")
timeseries_plot(mydata, date_time = "date", elements = c("nox", "no2"), station = "station_var", facets = "elements")
timeseries_plot(mydata, date_time = "date", elements = c("nox", "no2"), station = "station_var", facets = "stations")
timeseries_plot(mydata, date_time = "date", elements = c("nox", "no2"), station = "station_var", facets = "both")



# Should throw error
#timeseries_plot("mydata", date_time = "date", elements = c("nox", "max_tmax"), station = "station_var", facets = "none")
#timeseries_plot(mydata, date_time = year, elements = c("nox", "max_tmax"), station = "station_var", facets = "none")
#timeseries_plot(mydata, date_time = "date", elements = c("no"), station = "station_var", facets = "none")
#timeseries_plot(mydata, date_time = "date", elements = c("nox"), facets = "none")
#timeseries_plot(mydata, date_time = "date", elements = c("nox"), facets = "nonxe")
#timeseries_plot(mydata, date_time = "date", elements = c("nox"))
