# multiple stations (month)
niger_data <- readRDS("C:/Users/lzc1n17/OneDrive - University of Southampton/PhD/IDEMS/RInstatClimatic/Niger4_day_month_year.RDS")
niger_data <- niger_data$get_data_frame()
niger_data_1 <- niger_data$Data
niger_monthly_data <- niger_data$Data_by_year_month
niger_yearly_data <- niger_data$Data_by_year
niger_monthly_data$station = ifelse(niger_monthly_data$month_abbr %in% c("Jan", "Feb", "Mar"), "A", "B")


# currently: ggplot, ggmiscp, viridis
  

# facets = none ----------------------

ts_line_plot <- function(data, time = year, element, station = NULL, facets = "none",
                         add_points = FALSE, add_line_of_best_fit = FALSE, add_peaks = FALSE, add_valleys = FALSE,
                         se = TRUE, add_path = FALSE, add_step = FALSE){
  
  # this is only applied when there is more than one element
  data_longer <- data %>% tidyr::pivot_longer(cols = c({{ element }}), names_to = "Element")
  
  if (facets == "none"){
    
    if (length(data %>% dplyr::select({{ element }})) == 1){ # what is the correct way to check the length!
      base_plot <- ggplot(data, mapping = aes(x = {{ time }}, y = {{ element }}, colour = {{ station }})) +
        geom_line()
    } else {
      if (missing(station)){
        base_plot <- ggplot(data_longer, mapping = aes(x = {{ time }}, y = value, colour = Element))
      } else {
        data_longer <- data_longer %>%
          mutate(station_element = paste(station, Element, sep = "_"))
        base_plot <- ggplot(data_longer, mapping = aes(x = {{ time }}, y = value, colour = station_element))
      }
    }
  }
  if (facets == "elements"){
    base_plot <- ggplot(data_longer, mapping = aes(x = {{ time }}, y = value, colour = {{ station }})) +
      facet_grid(cols = vars(Element))
  } else if (facets == "stations"){
  if (length(data %>% dplyr::select({{ element }})) == 1){
    base_plot <- ggplot(data, mapping = aes(x = {{ time }}, y = {{ element }}))
  } else {
    base_plot <- ggplot(data_longer, mapping = aes(x = {{ time }}, y = value, colour = Element))
  }
  
  base_plot <- base_plot +
    facet_grid(cols = vars({{ station }}))
} else if (facets == "both"){
  base_plot <- ggplot(data_longer, mapping = aes(x = {{ time }}, y = value)) +
    facet_grid({{ station }} ~ Element)
}
  
  base_plot <- base_plot + geom_line()
  
  # color by viridis
  base_plot <- base_plot + viridis::scale_colour_viridis(discrete = TRUE, option = "C") # colour blind friendly
  
  if (add_points){
    base_plot <- base_plot + geom_point()
  }
  
  if (add_line_of_best_fit){
    base_plot <- base_plot + geom_smooth(method = "lm", se = se)
  }
  
  if (add_peaks){
    base_plot <- base_plot + ggpmisc::stat_peaks(geom = "text", colour = "red")
  }
  
  if (add_valleys){
    base_plot <- base_plot + ggpmisc::stat_valleys(geom = "text", colour = "blue")
  }
  
  if (add_path){
    base_plot <- base_plot + geom_path()
  }
  
  if (add_step){
    base_plot <- base_plot + geom_step()
  }

  return(base_plot)
}


ts_line_plot(niger_monthly_data, element = c(max_rain, max_tmax), facets = "none", station = station)


# facets = elements with 1 station vs 1+ station
ts_line_plot(niger_yearly_data, element = c(max_rain, max_tmax), facets = "elements")
ts_line_plot(niger_monthly_data, element = max_rain, facets = "elements", station = station)

# facets = stations with 1 element vs 1+ element
ts_line_plot(niger_monthly_data, element = max_rain, facets = "stations", station = station)
ts_line_plot(niger_monthly_data, element = c(max_rain, max_tmax), facets = "stations", station = station, add_peaks = TRUE,, add_valleys = TRUE)

# facets = both with 1 element vs 1+ element
ts_line_plot(niger_monthly_data, element = c(max_rain, max_tmax), facets = "both", station = station)
ts_line_plot(niger_monthly_data, element = max_rain, facets = "both", station = station)
