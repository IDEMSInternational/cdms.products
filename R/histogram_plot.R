#' Title
#'
#' @param data 
#' @param date_time 
#' @param elements 
#' @param station 
#' @param facets 
#' @param na.rm 
#' @param show.legend 
#' @param position 
#' @param plot_type 
#' @param binwidth 
#' @param bins 
#' @param orientation 
#' @param breaks 
#'
#' @return
#' @export
#'
#' @examples
histogram_plot <- function(data, date_time, elements, station = NULL, facets = c("stations", "elements", "both", "none"),
                           position = c("identity", "dodge", "dodge2", "stack"), # others?
                           plot_type = c("histogram", "density", "frequency"), # ridges is in geom_density_ridges in ggridges pkg.
                           binwidth = NULL, bins = NULL, na.rm = FALSE, orientation = NA, show.legend = NA, breaks = NULL){
  
  checkmate::assert_data_frame(data)
  # date_time can be a date, factor, or character.
  checkmate::assert_character(elements)
  checkmate::assert_character(station, null.ok = TRUE)
  facets <- match.arg(facets)
  position <- match.arg(position)
  plot_type <- match.arg(plot_type)
  checkmate::assert_logical(na.rm)
  checkmate::assert_logical(show.legend)
  
  # todo: checks for binwidth, bins, orientation
  
  if ((facets == "stations" | facets == "both") & is.null(station)){
    warning("facets set to none since no stations are given in data")
    facets = "none"
  }
  data_longer <- data %>% tidyr::pivot_longer(cols = all_of(elements), names_to = "elements_list")
  data_longer$elements_list <- as.factor(data_longer$elements_list)
  
  if (facets == "elements"){
    if (is.null(station)){
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]])) +
        ggplot2::facet_grid(cols = vars(elements_list))
    } else {
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], fill = .data[[station]])) +
        ggplot2::facet_grid(cols = vars(elements_list))
    }
  } else if (facets == "stations"){
    if (length(elements) == 1){
      base_plot <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .data[[date_time]], ))
    } else {
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], fill = .data$elements_list))
    }
    base_plot <- base_plot + ggplot2::facet_grid(cols = vars(.data[[station]]))
  } else if (facets == "both"){
    base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]])) +
      ggplot2::facet_grid(rows = vars(.data[[station]]), cols = vars(.data$elements_list))
  } else { # if "none", or NULL
    if (length(elements) == 1){
      if (is.null(station)) {
        base_plot <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .data[[date_time]], ))
      } else {
        base_plot <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .data[[date_time]], , fill = .data[[station]]))          
      }
    } else {
      if (is.null(station)){
        base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], fill = .data$elements_list))
      } else {
        data_longer <- data_longer %>%
          dplyr::mutate(station_elements = paste(.data[[station]], .data$elements_list, sep = "_"))
        base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], fill = station_elements))
      }
    }
  }
  
  if (plot_type == "histogram"){
    base_plot <- base_plot + ggplot2::geom_histogram(position = position, binwidth = binwidth, bins = bins, na.rm = na.rm, orientation = orientation, show.legend = show.legend, breaks = breaks)
  } else if (plot_type == "frequency"){
    base_plot <- base_plot + ggplot2::geom_freqpoly(position = position, na.rm = na.rm, show.legend = show.legend)
  } else if (plot_type == "density"){
    base_plot <- base_plot + ggplot2::geom_density(position = position, na.rm = na.rm, orientation = orientation, show.legend = show.legend)
  }
  
  return(base_plot)
}