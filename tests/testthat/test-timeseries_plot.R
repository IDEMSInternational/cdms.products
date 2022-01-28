context("timeseries output")

library(RInstatClimatic)
library(openair)

# Get the data --------------------------------------------------------------------------------------------------------
data("mydata")

mydata$station <- ifelse(mydata$ws < 5, "A", "B")

# no station
timeseries_plot(mydata, date_time = "date", elements = c("nox"), facets = "none")
timeseries_plot(mydata, date_time = "date", elements = c("nox"), facets = "elements")
timeseries_plot(mydata, date_time = "date", elements = c("nox"), facets = "stations")
timeseries_plot(mydata, date_time = "date", elements = c("nox"), facets = "both")

# station
timeseries_plot(mydata, date_time = "date", elements = c("nox"), station = "station", facets = "none")
timeseries_plot(mydata, date_time = "date", elements = c("nox"), station = "station", facets = "elements")
timeseries_plot(mydata, date_time = "date", elements = c("nox"), station = "station")
timeseries_plot(mydata, date_time = "date", elements = c("nox"), station = "station", facets = "both")

# no station
timeseries_plot(mydata, date_time = "date", elements = c("nox", "no2"), facets = "none", show.legend = FALSE) +
  viridis::scale_color_viridis(discrete = TRUE, option = "C")
timeseries_plot(mydata, date_time = "date", elements = c("nox", "no2"), facets = "elements")

# station
timeseries_plot(mydata, date_time = "date", elements = c("nox", "no2"), station = "station", facets = "none")
timeseries_plot(mydata, date_time = "date", elements = c("nox", "no2"), station = "station", facets = "elements")
timeseries_plot(mydata, date_time = "date", elements = c("nox", "no2"), station = "station", facets = "stations")
timeseries_plot(mydata, date_time = "date", elements = c("nox", "no2"), station = "station", facets = "both")



# Should throw error
#timeseries_plot("mydata", date_time = "date", elements = c("nox", "max_tmax"), station = "station", facets = "none")
#timeseries_plot(mydata, date_time = year, elements = c("nox", "max_tmax"), station = "station", facets = "none")
#timeseries_plot(mydata, date_time = "date", elements = c("no"), station = "station", facets = "none")
#timeseries_plot(mydata, date_time = "date", elements = c("nox"), facets = "none")
#timeseries_plot(mydata, date_time = "date", elements = c("nox"), facets = "nonxe")
#timeseries_plot(mydata, date_time = "date", elements = c("nox"))
