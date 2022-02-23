#' Produce a histogram of elements by station.
#'
#' @param data The data.frame to calculate from.
#' @param date_time The name of the date column in \code{data}.
#' @param elements The name of the elements column in \code{data}.
#' @param station The name of the station column in \code{data}, if the data are for multiple station. 
#' Histogram plots are calculated separately for each station.
#' @param facet_by Whether to facet by stations, elements, both, or neither. Options are \code{"stations"}, \code{"elements"}, \code{"station-elements"}, \code{"elements-stations"}, or \code{"none"}.
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing values are silently removed.
#' @param show_legend logical. Should this layer be included in the legends? \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param plot_type Type of plot to display. Can be one of \code{"histogram"}, \code{"density"}, or \code{"frequency"}.
#' @param binwidth The width of the bins. Can be specified as a numeric value or as a function that calculates width from unscaled x. Here, "unscaled x" refers to the original x values in the data, before application of any scale transformation. When specifying a function along with a grouping structure, the function will be called once per group. The default is to use the number of bins in \code{bins}, covering the range of the data. You should always override this value, exploring multiple widths to find the best to illustrate the stories in your data.
#' @param bins Number of bins. Overridden by \code{binwidth}. Defaults to 30.
#' @param orientation The orientation of the layer. The default (\code{NA}) automatically determines the orientation from the aesthetic mapping. In the rare event that this fails it can be given explicitly by setting \code{orientation} to either "x" or "y".
#' @param breaks A numeric vector giving the bin boundaries. Overrides \code{binwidth} and \code{bins}.
#' @param title The text for the title.
#' @param x_title The text for the x-axis.
#' @param y_title The text for the y-axis.
#'
#' @return A plot of type \code{ggplot} to the default plot device
#' @export
#'
#' @examples
#' data("daily_niger")
#' 
#' # Create a histogram plot with facets by both elements and stations
#' histogram_plot(data = daily_niger, date_time = "date",
#'                facet_by = "stations-elements",
#'                elements = c("tmax", "tmin"), station = "station_name")
#'                
#' # Can make additional changes to the plot since the returned object is a \code{ggplot2} object
#' # for example, to edit the colours in the plot:
#' require(ggplot2)
#' t1 <- histogram_plot(data = daily_niger, date_time = "date", elements = c("rain", "tmax"),
#'                plot_type = "frequency", position = "dodge", station = "station_name")
#' t1 + ggplot2::scale_colour_discrete(type = c("red", "black"))

histogram_plot <- function(data, date_time, elements, station = NULL,
                           facet_by = c("stations", "elements", "stations-elements", "elements-stations", "none"),
                           position = c("identity", "dodge", "dodge2", "stack"),
                           plot_type = c("histogram", "density", "frequency"),
                           binwidth = NULL, bins = NULL, na.rm = FALSE, orientation = NA, show_legend = NA, breaks = NULL,
                           title = "Histogram Plot", x_title = NULL, y_title = NULL){
  checkmate::assert_data_frame(data)
  checkmate::assert_character(elements)
  checkmate::assert_character(station, null.ok = TRUE)
  facet_by <- match.arg(facet_by)
  position <- match.arg(position)
  plot_type <- match.arg(plot_type)
  checkmate::assert_logical(na.rm)
  checkmate::assert_logical(show_legend)

  if ((facet_by == "stations" | facet_by == "stations-elements" | facet_by == "elements-stations") & is.null(station)){
    warning("facet_by set to none since no stations are given in data")
    facet_by = "none"
  }
  
  data_longer <- data %>% tidyr::pivot_longer(cols = tidyselect::all_of(elements), names_to = "elements_list")
  data_longer$elements_list <- as.factor(data_longer$elements_list)
    
  if (facet_by == "none"){
    data_longer$elements_stations <- paste(data_longer$station_name, data_longer$elements_list, sep = "_")
    base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data$elements_stations, fill = .data$elements_stations))
  } else if (facet_by == "elements"){
    if (is.null(station)){
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data$elements_list, fill = .data$elements_list))
    } else {
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data[[station]], fill = .data[[station]]))
    }
    base_plot <- base_plot +
      ggplot2::facet_wrap(ggplot2::vars(.data$elements_list))
  } else {
    base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data$elements_list, fill = .data$elements_list))
    if (facet_by == "stations"){
      base_plot <- base_plot + 
        ggplot2::facet_wrap(vars(.data[[station]]))
    } else if (facet_by == "stations-elements"){
      base_plot <- base_plot + 
        facet_grid(cols = vars(.data$elements_list), rows = vars(.data[[station]]))
    } else {
      base_plot <- base_plot + 
        facet_grid(cols = vars(.data[[station]]), rows = vars(.data$elements_list))
    }
  }
  
  base_plot <- base_plot + ggplot2::geom_bar(stat="identity")

  if(title == "Histogram Plot") {
    if (is.null(station)){
      title <- paste0(title, ": ", elements)
    } else {
      title <- paste0(title, ": ", elements, " by: ", station)
    }
  }
  base_plot <- base_plot + 
    ggplot2::xlab(x_title) +
    ggplot2::ylab(y_title) +
    ggplot2::labs(title = title)
  
  return(base_plot)
}