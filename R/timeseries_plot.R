#' Produce a timeseries graph
#' 
#' @description Creates a timeseries graph for each element and station given. Takes a data frame as an input and the relevant columns to create the plot.
#' Creates a graph using \code{ggplot2} and returns a timeseries plot.
#'
#' @param data `[data frame]` The data.frame to calculate from.
#' @param date_time [\code{\link[base]{Date}}] The name of the date column in \code{data}.
#' @param elements [\code{character}] The name of the column in \code{data} to apply the function to.
#' @param station [\code{character(1)}] The name of the station column in \code{data}, if the data are for multiple station.
#' Timeseries plots are calculated separately for each station.
#' @param facet_by [\code{character(1)}] Whether to facet by stations, elements, both, or neither. Options are \code{"stations"}, \code{"elements"}, \code{"station-elements"}, \code{"elements-stations"}, or \code{"none"}.
#' @param type [\code{character(1)}] The type of plot, either "line" or line graphs or "bar" for bar graphs.
#' @param add_points [\code{logical(1)}] If \code{TRUE}, points are added to the plot using  \code{"ggplot2::geom_point()"}.
#' @param add_line_of_best_fit [\code{logical(1)}] If \code{TRUE}, points are added to the plot using  \code{"ggplot2::geom_smooth(method = "lm")"}.
#' @param se [\code{logical(1)}] If \code{TRUE}, the standard error is are added to the line of best fit. Only works if \code{add_line_of_best_fit = TRUE}. 
#' @param add_path [\code{logical(1)}] If \code{TRUE}, paths are added to the plot using  \code{"ggplot2::geom_path()"}.
#' @param add_step [\code{logical(1)}] If \code{TRUE}, steps are added to the plot using  \code{"ggplot2::geom_step()"}.
#' @param na_rm [\code{logical(1)}] If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing values are silently removed.
#' @param show_legend [\code{logical(1)}] Should this layer be included in the legends? \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param title [\code{character(1)}] The text for the title.
#' @param x_title [\code{character(1)}] The text for the x-axis.
#' @param y_title [\code{character(1)}] The text for the y-axis.
#' 
#' @return A plot of type \code{ggplot} to the default plot device
#' @export
#'
#' @examples
#' # Create a time series plot with two elements and facet by station.
#' data(daily_niger)
#' timeseries_plot(data = daily_niger, date_time = "date", elements = c("tmax", "tmin"), 
#'                 station = "station_name", facet_by = "stations")
#' 
#' # Can make additional changes to the plot since the returned object is a \code{ggplot2} object
#' # for example, to add colour-blind friendly colours instead of the default colours
#' require(ggplot2)
#' t1 <- timeseries_plot(data = daily_niger, date_time = "date", elements = c("tmax", "tmin"), 
#'                       station = "station_name", facet_by = "stations")
#' t1 + ggplot2::scale_colour_discrete(type = c("#E69F00", "#56B4E9"))
timeseries_plot <- function(data, date_time, elements, station = NULL, 
                            facet_by = c("stations", "elements", "stations-elements", "elements-stations", "none"),
                            type = c("line", "bar"),
                            add_points = FALSE, add_line_of_best_fit = FALSE,
                            se = TRUE, add_path = FALSE, add_step = FALSE,
                            na_rm = FALSE, show_legend = NA,
                            title = "Timeseries Plot", x_title = NULL, y_title = NULL){
  
  checkmate::assert_data_frame(data)
  checkmate::assert_character(elements)
  checkmate::assert_character(station, null.ok = TRUE)
  facet_by <- match.arg(facet_by)
  type <- match.arg(type)
  checkmate::assert_logical(add_points)
  checkmate::assert_logical(add_line_of_best_fit)
  checkmate::assert_logical(se)
  checkmate::assert_logical(add_path)
  checkmate::assert_logical(add_step)
  checkmate::assert_logical(na_rm)
  checkmate::assert_logical(show_legend)
  
  if ((facet_by == "stations" | facet_by == "stations-elements" | facet_by == "elements-stations") & is.null(station)){
    warning("facet_by will be set to 'none' since station is missing")
    facet_by <- "none"
  }
  
  data_longer <- data %>% 
    tidyr::pivot_longer(cols = tidyselect::all_of(elements), 
                        names_to = "element",
                        values_to = "value")

  if (facet_by == "elements"){
    if (is.null(station)){
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value))
    } else {
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data[[station]]))
    }
    base_plot <- base_plot + ggplot2::facet_wrap(ggplot2::vars(.data$element))
  } else if (facet_by == "stations"){
    if (length(elements) == 1){
      base_plot <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .data[[date_time]], y = .data[[elements]]))
    } else {
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data$element))
    }
    base_plot <- base_plot + ggplot2::facet_wrap(ggplot2::vars(.data[[station]]))
  } else if (facet_by == "none") { # if "none", or NULL
    if (length(elements) == 1){
      if (is.null(station)) {
        base_plot <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .data[[date_time]], y = .data[[elements]]))
      } else {
        base_plot <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .data[[date_time]], y = .data[[elements]], colour = .data[[station]]))          
      }
    } else {
      if (is.null(station)){
        base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data$element))
      } else {
        data_longer <- data_longer %>%
          dplyr::mutate(station_elements = paste(.data[[station]], .data$element, sep = "_"))
        base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data$station_elements))
      }
    }
  } else {
    base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value))
    if (facet_by == "stations-elements"){
      base_plot <- base_plot +
      ggplot2::facet_grid(rows = ggplot2::vars(.data[[station]]), cols = ggplot2::vars(.data$element))
    } else {
      base_plot <- base_plot +
        ggplot2::facet_grid(rows = ggplot2::vars(.data$element), cols = ggplot2::vars(.data[[station]]))
    }
  }
    
  if (type == "line") {
    base_plot <- base_plot + 
      ggplot2::geom_line(na.rm = na_rm, show.legend = show_legend)
  } else {
    base_plot <- base_plot + 
      ggplot2::geom_col(na.rm = na_rm, show.legend = show_legend)
  }
 
  if (add_points){
    base_plot <- base_plot + ggplot2::geom_point()
  }
  
  if (add_line_of_best_fit){
    base_plot <- base_plot + ggplot2::geom_smooth(method = "lm", se = se, formula = y ~ x)
  }
  
  if (add_path){
    base_plot <- base_plot + ggplot2::geom_path()
  }
  
  if (add_step){
    base_plot <- base_plot + ggplot2::geom_step()
  }
  
  base_plot <- base_plot + 
    ggplot2::xlab(x_title) +
    ggplot2::ylab(y_title) +
    ggplot2::labs(title = title)
  
  return(base_plot)
}

