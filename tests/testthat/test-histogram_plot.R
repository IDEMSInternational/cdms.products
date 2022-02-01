# library(RInstatClimatic)
# library(openair)
# 
# # Get the data --------------------------------------------------------------------------------------------------------
# data("mydata")
# mydata$station_var <- ifelse(mydata$ws < 5, "A", "B")
# 
# ggplot(mydata, aes(x = date, fill = factor(station_var))) +
#   geom_histogram(data = mydata, aes(y = ..count..*10)) +
#   geom_histogram(data = mydata, )
# ggplot(mydata, aes(x = date, fill = factor(station_var))) + geom_histogram()
# 
# 
# ggplot(mydata,aes(date))+geom_histogram(aes(y=..count..*10))
# 
# 
# # Test the data -------------------------------------------------------------------------------------------------------
# # no station
# histogram_plot(mydata, date_time = "date", elements = c("nox"), facets = "none")
# histogram_plot(mydata, date_time = "date", elements = c("nox"), facets = "elements")
# histogram_plot(mydata, date_time = "date", elements = c("nox"), facets = "stations")
# histogram_plot(mydata, date_time = "date", elements = c("nox"), facets = "both")
# 
# # station
# histogram_plot(mydata, date_time = "date", elements = c("nox"), station = "station_var", facets = "none")
# histogram_plot(mydata, date_time = "date", elements = c("nox"), station = "station_var", facets = "elements")
# histogram_plot(mydata, date_time = "date", elements = c("nox"), station = "station_var")
# histogram_plot(mydata, date_time = "date", elements = c("nox"), station = "station_var", facets = "both")
# 
# # no station
# histogram_plot(mydata, date_time = "date", elements = c("nox", "no2"), facets = "none", show.legend = FALSE) +
#   viridis::scale_color_viridis(discrete = TRUE, option = "C")
# histogram_plot(mydata, date_time = "date", elements = c("nox", "no2"), facets = "elements")
# 
# # station
# histogram_plot(mydata, date_time = "date", elements = c("nox", "no2"), station = "station_var", facets = "none")
# histogram_plot(mydata, date_time = "date", elements = c("nox", "no2"), station = "station_var", facets = "elements")
# histogram_plot(mydata, date_time = "date", elements = c("nox", "no2"), station = "station_var", facets = "stations")
# histogram_plot(mydata, date_time = "date", elements = c("nox", "no2"), station = "station_var", facets = "both")



# Should throw error
#histogram_plot("mydata", date_time = "date", elements = c("nox", "no2"), station = "station_var", facets = "none")
#histogram_plot(mydata, date_time = year, elements = c("nox", "no2"), station = "station_var", facets = "none")
#histogram_plot(mydata, date_time = "date", elements = c("no"), station = "station_var", facets = "none")
#histogram_plot(mydata, date_time = "date", elements = c("nox"), facets = "none")
#histogram_plot(mydata, date_time = "date", elements = c("nox"), facets = "nonxe")
#histogram_plot(mydata, date_time = "date", elements = c("nox"))
