#' Histogram Plot
#'
#' @param data The data.frame to calculate from
#' @param date_time The name of the date column in \code{data}.
#' @param elements The name of the elements column in \code{data}.
#' @param station The name of the station column in \code{data}, if the data are for multiple station. 
#' Histogram plots are calculated separately for each station.
#' @param facets How to split the histograms. Default \code{"station"}. Can be one of \code{"elements"}, \code{"both"}, or \code{"none"}.
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param plot_type Type of plot to display. Can be one of \code{"histogram"}, \code{"density"}, or \code{"frequency"}.
#' @param binwidth The width of the bins. Can be specified as a numeric value or as a function that calculates width from unscaled x. Here, "unscaled x" refers to the original x values in the data, before application of any scale transformation. When specifying a function along with a grouping structure, the function will be called once per group. The default is to use the number of bins in \code{bins}, covering the range of the data. You should always override this value, exploring multiple widths to find the best to illustrate the stories in your data.
#' @param bins Number of bins. Overridden by \code{binwidth}. Defaults to 30.
#' @param orientation The orientation of the layer. The default (\code{NA}) automatically determines the orientation from the aesthetic mapping. In the rare event that this fails it can be given explicitly by setting \code{orientation} to either "x" or "y".
#' @param breaks A numeric vector giving the bin boundaries. Overrides \code{binwidth} and \code{bins}.
#'
#' @return
#' @export
#'
#' @examples # TODO
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