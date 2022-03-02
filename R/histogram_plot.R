#' Produce a histogram of elements by station.
#'
#' @param data The data.frame to calculate from.
#' @param date_time The name of the date column in \code{data}.
#' @param elements The name of the elements column in \code{data}.
#' @param station The name of the station column in \code{data}, if the data are for multiple station. 
#' Histogram plots are calculated separately for each station.
#' @param position Position adjustment. 
#' @param facet_by Whether to facet by stations, elements, both, or neither. Options are \code{"stations"}, \code{"elements"}, \code{"station-elements"}, \code{"elements-stations"}, or \code{"none"}.
#' @param facet_nrow Number of rows for the facets if `facet_by` is one of \code{"stations"} or \code{"elements"}. Only if \code{facet_ncol} is given.
#' @param facet_ncol Number of rows for the facets if `facet_by` is one of \code{"stations"} or \code{"elements"}. Only if \code{facet_nrow} is given.
#' @param orientation The orientation of the layer. The default (\code{NA}) automatically determines the orientation from the aesthetic mapping. In the rare event that this fails it can be given explicitly by setting \code{orientation} to either "x" or "y".
#' @param width Bar width. By default, set to 90% of the resolution of the data.
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing values are silently removed.
#' @param show_legend logical. Should this layer be included in the legends? \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param colour_bank A string denoting colour values if `position == "layer"`. By default, colours from `ggplot2::luv_colours` are used.
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
#'                      position = "dodge", station = "station_name")
#' t1 + ggplot2::scale_colour_discrete(type = c("red", "black"))
#' 
#' # Can additionally layer elements in a single plot
#' histogram_plot(data = daily_niger, date_time = "date", position = "layer",
#'                facet_by = "stations",
#'                elements = c("tmax", "tmin"), station = "station_name",
#'                colour_bank = c("purple", "orange"))

histogram_plot <- function(data, date_time, elements, station = NULL,
                           facet_by = c("stations", "elements", "stations-elements", "elements-stations", "none"),
                           position = c("identity", "dodge", "dodge2", "stack", "fill", "layer"), colour_bank = NULL,
                           #plot_type = c("histogram", "density", "frequency"),
                           na.rm = FALSE, orientation = NA, show_legend = NA, width = NULL,
                           facet_nrow = NULL, facet_ncol = NULL, 
                           title = "Histogram Plot", x_title = NULL, y_title = NULL){
  checkmate::assert_data_frame(data)
  checkmate::assert_character(elements)
  checkmate::assert_character(station, null.ok = TRUE)
  facet_by <- match.arg(facet_by)
  position <- match.arg(position)
  
  if ((facet_by == "stations" | facet_by == "stations-elements" | facet_by == "elements-stations") & is.null(station)){
    warning("facet_by set to none since no stations are given in data")
    facet_by = "none"
  }
  if (is.null(colour_bank)){ 
    colour_bank <- (ggplot2::luv_colours[-1,] %>% dplyr::slice(which(dplyr::row_number() %% 10 == 1)))$col
    if (length(elements) > length(colour_bank)) { colour_bank <- ggplot2::luv_colours$col }
  }
  if (position == "layer"){
    base_plot <- ggplot2::ggplot(data)
    for (i in 1:length(elements)){
      base_plot <- base_plot +
        ggplot2::geom_bar(data = data, stat = "identity",
                          mapping = ggplot2::aes(x = .data[[date_time]], y = .data[[elements[i]]]),
                          colour = colour_bank[i], width = width, na.rm = na.rm,
                          orientation = orientation, show.legend = show_legend)
    }
    if (facet_by == "elements"){
      base_plot <- base_plot + ggplot2::facet_wrap(ggplot2::vars(.data[[elements]]))
    } else if (facet_by == "stations") {
      base_plot <- base_plot + ggplot2::facet_wrap(ggplot2::vars(.data[[station]]))
    } else if (facet_by == "stations-elements"){
      base_plot <- base_plot + 
        ggplot2::facet_grid(cols = ggplot2::vars(.data[[elements]]), rows = ggplot2::vars(.data[[station]]))
    } else if (facet_by == "elements-stations") {
      base_plot <- base_plot + 
        ggplot2::facet_grid(cols = ggplot2::vars(.data[[station]]), rows = ggplot2::vars(.data[[elements]]))
    }
  } else {
    data_longer <- data %>% tidyr::pivot_longer(cols = tidyselect::all_of(elements), names_to = "elements_list")
    data_longer$elements_list <- as.factor(data_longer$elements_list)
    if (facet_by == "none"){
      data_longer$elements_stations <- paste(data_longer$station_name, data_longer$elements_list, sep = "_")
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data$elements_stations, fill = .data$elements_stations))
    } else if (facet_by == "stations"){
      if (length(elements) > 1){
        base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data$elements_list, fill = .data$elements_list))
      } else {
        base_plot <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .data[[date_time]], y = .data[[elements]]))
      }
      base_plot <- base_plot + 
        ggplot2::facet_wrap(ggplot2::vars(.data[[station]]), ncol = facet_ncol, nrow = facet_nrow)
    } else if (facet_by == "elements"){
        base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data[[station]], fill = .data[[station]]))
        base_plot <- base_plot + 
          ggplot2::facet_wrap(ggplot2::vars(.data$elements_list), ncol = facet_ncol, nrow = facet_nrow)
    } else {
      base_plot <- ggplot2::ggplot(data_longer, mapping = ggplot2::aes(x = .data[[date_time]], y = .data$value, colour = .data$elements_list, fill = .data$elements_list))
      if (facet_by == "stations-elements"){
        base_plot <- base_plot + 
          ggplot2::facet_grid(cols = ggplot2::vars(.data$elements_list), rows = ggplot2::vars(.data[[station]]))
      } else {
        base_plot <- base_plot + 
          ggplot2::facet_grid(cols = ggplot2::vars(.data[[station]]), rows = ggplot2::vars(.data$elements_list))
      }
    }
    base_plot <- base_plot + ggplot2::geom_bar(stat = "identity", position = position, width = width, na.rm = na.rm,
                                               orientation = orientation, show.legend = show_legend) #+

    #ggplot2::scale_color_discrete(type = colour_bank)
  }
  if(title == "Histogram Plot") {
    title <- paste0(title, ": ", elements)
  }
  base_plot <- base_plot + 
    ggplot2::xlab(x_title) +
    ggplot2::ylab(y_title) +
    ggplot2::labs(title = title)
  return(base_plot)
}
