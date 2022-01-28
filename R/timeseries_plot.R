#' Title
#'
#' @param data 
#' @param date_time 
#' @param elements 
#' @param station 
#' @param facets 
#' @param add_points 
#' @param add_line_of_best_fit 
#' @param se 
#' @param add_path 
#' @param add_step 
#' @param na.rm 
#' @param show.legend 
#'
#' @return
#' @export
#'
#' @examples
timeseries_plot <- function(data, date_time, elements, station = NULL, facets = c("stations", "elements", "both", "none"),
                            add_points = FALSE, add_line_of_best_fit = FALSE,
                            se = TRUE, add_path = FALSE, add_step = FALSE,
                            na.rm = FALSE, show.legend = NA){
  
  checkmate::assert_data_frame(data)
  # date_time can be a date, factor, or character.
  checkmate::assert_character(elements)
  checkmate::assert_character(station, null.ok = TRUE)
  facets <- match.arg(facets)
  checkmate::assert_logical(add_points)
  checkmate::assert_logical(add_line_of_best_fit)
  checkmate::assert_logical(se)
  checkmate::assert_logical(add_path)
  checkmate::assert_logical(add_step)
  checkmate::assert_logical(show.legend)
  
  if ((facets == "stations" | facets == "both") & is.null(station)){
    warning("facets set to none since no stations are given in data")
    facets = "none"
  }
  
  data_longer <- data %>% tidyr::pivot_longer(cols = all_of(elements), names_to = "elements_list")

  if (facets == "elements"){
    if (is.null(station)){
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value)) +
        ggplot2::facet_grid(cols = vars(elements_list))
    } else {
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data[[station]])) +
        ggplot2::facet_grid(cols = vars(elements_list))
    }
  } else if (facets == "stations"){
    if (length(elements) == 1){
      base_plot <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .data[[date_time]], y = .data[[elements]]))
    } else {
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data$elements_list))
    }
    base_plot <- base_plot + ggplot2::facet_grid(cols = vars(.data[[station]]))
  } else if (facets == "both"){
    base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value)) +
      ggplot2::facet_grid(.data$station ~ .data$elements_list)
  } else { # if "none", or NULL
    if (length(elements) == 1){
      if (is.null(station)) {
        base_plot <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .data[[date_time]], y = .data[[elements]]))
      } else {
        base_plot <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .data[[date_time]], y = .data[[elements]], colour = .data[[station]]))          
      }
    } else {
      if (is.null(station)){
        base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data$elements_list))
      } else {
        data_longer <- data_longer %>%
          dplyr::mutate(station_elements = paste(.data$station, .data$elements_list, sep = "_"))
        base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data$station_elements))
      }
    }
  }
  
  base_plot <- base_plot + ggplot2::geom_line(na.rm = na.rm, show.legend = show.legend)
  
  # color by viridis?
  # base_plot <- base_plot + viridis::scale_colour_viridis(discrete = TRUE, option = "C") # colour blind friendly
  
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

