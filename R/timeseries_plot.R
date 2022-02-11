#' Timeseries Plot
#'
#' @param data The data.frame to calculate from.
#' @param date_time The name of the date column in \code{data}.
#' @param elements The name of the column in \code{data} to apply the function to.
#' @param station The name of the station column in \code{data}, if the data are for multiple station.
#' Timeseries plots are calculated separately for each station.
#' @param facets How to split the time series plots. Default \code{"station"}. Can be one of \code{"elements"}, \code{"both"}, or \code{"none"}.
#' @param add_points logical. If \code{TRUE}, points are added to the plot using  \code{"ggplot2::geom_point()"}.
#' @param add_line_of_best_fit logical. If \code{TRUE}, points are added to the plot using  \code{"ggplot2::geom_smooth(method = "lm")"}.
#' @param se logical. If \code{TRUE}, the standard error is are added to the line of best fit. Only works if \code{add_line_of_best_fit = TRUE}. 
#' @param add_path logical. If \code{TRUE}, paths are added to the plot using  \code{"ggplot2::geom_path()"}.
#' @param add_step logical. If \code{TRUE}, steps are added to the plot using  \code{"ggplot2::geom_step()"}.
#' @param na_rm If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing values are silently removed.
#' @param show_legend logical. Should this layer be included in the legends? \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE} never includes, and \code{TRUE} always includes.
#'
#' @return
#' @export
#'
#' @examples # TODO
timeseries_plot <- function(data, date_time, elements, station = NULL, facets = c("stations", "elements", "both", "none"),
                            add_points = FALSE, add_line_of_best_fit = FALSE,
                            se = TRUE, add_path = FALSE, add_step = FALSE,
                            na_rm = FALSE, show_legend = NA){
  
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
  checkmate::assert_logical(na_rm)
  checkmate::assert_logical(show_legend)
  
  if ((facets == "stations" || facets == "both") && is.null(station)) {
    warning("facets will be set to 'none' since station is missing")
    facets <- "none"
  }
  
  data_longer <- data %>% tidyr::pivot_longer(cols = tidyselect::all_of(elements), names_to = "elements_list")

  if (facets == "elements"){
    if (is.null(station)){
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value)) +
        ggplot2::facet_grid(cols = ggplot2::vars(.data$elements_list))
    } else {
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data[[station]])) +
        ggplot2::facet_grid(cols = ggplot2::vars(.data$elements_list))
    }
  } else if (facets == "stations"){
    if (length(elements) == 1){
      base_plot <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .data[[date_time]], y = .data[[elements]]))
    } else {
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data$elements_list))
    }
    base_plot <- base_plot + ggplot2::facet_grid(cols = ggplot2::vars(.data[[station]]))
  } else if (facets == "both"){
    base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value)) +
      ggplot2::facet_grid(rows = ggplot2::vars(.data[[station]]), cols = ggplot2::vars(.data$elements_list))
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
          dplyr::mutate(station_elements = paste(.data[[station]], .data$elements_list, sep = "_"))
        base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data$station_elements))
      }
    }
  }
  
  base_plot <- base_plot + ggplot2::geom_line(na.rm = na_rm, show.legend = show_legend)
  
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

