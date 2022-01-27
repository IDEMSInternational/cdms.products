
ts_line_plot <- function(data, time, element, station, facets = "none",
                         add_points = FALSE, add_line_of_best_fit = FALSE,
                         se = TRUE, add_path = FALSE, add_step = FALSE){
  
  # this is only applied when there is more than one element
  data_longer <- data %>% tidyr::pivot_longer(cols = c({{ element }}), names_to = "element")

  if (facets == "none"){
    if (length(element) == 1){
      if (is.null(.data[[station]])){
        base_plot <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .data[[time]], y = .data[[element]]))
      } else {
        base_plot <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .data[[time]], y = .data[[element]], colour = .data[[station]]))          
      }
    } else {
      if (is.null(.data[[station]])){
        base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[time]], y = value, colour = element))
      } else {
        data_longer <- data_longer %>%
          dplyrl::mutate(station_element = paste(.data[[station]], element, sep = "_"))
        base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[time]], y = value, colour = station_element))
      }
    }
  } else if (facets == "elements"){
    if (is.null(.data[[station]])){
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[time]], y = value)) +
        ggplot2::facet_grid(cols = vars(element))
    } else {
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[time]], y = value, colour = .data[[station]])) +
        ggplot2::facet_grid(cols = vars(element))
    }
  } else if (facets == "stations"){
    if (length(element) == 1){
      base_plot <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .data[[time]], y = .data[[element]]))
    } else {
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[time]], y = value, colour = element))
    }
    base_plot <- base_plot + ggplot2::facet_grid(cols = vars(.data[[station]]))
  } else if (facets == "both"){
    base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[time]], y = value)) +
      ggplot2::facet_grid({{ station }} ~ element)
  }
  
  base_plot <- base_plot + ggplot2::geom_line()
  
  # color by viridis
  base_plot <- base_plot + viridis::scale_colour_viridis(discrete = TRUE, option = "C") # colour blind friendly
  
  if (add_points){
    base_plot <- base_plot + ggplot2::geom_point()
  }
  
  if (add_line_of_best_fit){
    base_plot <- base_plot + ggplot2::geom_smooth(method = "lm", se = se)
  }
  
  if (add_path){
    base_plot <- base_plot + ggplot2::geom_path()
  }
  
  if (add_step){
    base_plot <- base_plot + ggplot2::geom_step()
  }
  
  return(base_plot)
}

