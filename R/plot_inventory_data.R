#' Produce an bar chart of frequencies of missing data
#' 
#' @description 
#' Creates a bar chart displaying the frequency of missing data for each element and station given.
#' Takes a data frame as an input and the relevant columns to create the plot.
#' Creates a graph using \code{ggplot2} and returns a bar plot.
#' 
#' @param data The data.frame to calculate from
#' @param elements The name of the columns in \code{data} to apply the summary calculation to.
#' @param station The name of the station column in \code{data}, if the data are for multiple station. 
#' The summary table is calculated separately for each station.
#' @param y Whether to display counts or percentages.
#' @param columns Whether to display stations or elements on the columns.
#' @param fill Whether to fill by stations, elements, or neither.
#' @param facet_by Whether to facet by stations, elements, or neither
#' @param display_perc logical. Whether to display percentage values on the plot.
#' @param display_count logical. Whether to display the count values on the plot.
#' @param title The text for the title.
#' @param x_title The text for the x-axis.
#' @param y_title The text for the y-axis.
#' @param coord_flip logical. Whether to switch the x and y axes.
#'
#' @return A plot of type \code{ggplot} to the default plot device
#' @export
#'
#' @examples
#' data(daily_niger) 
#' plot_inventory_data(data = daily_niger, station = "station_name",
#'                     elements = c("tmin", "tmax"),
#'                     columns = "stations", facet_by = "elements")
#'                     
#' # can omit station argument
#' plot_inventory_data(data = daily_niger, 
#'                     elements = c("tmin", "tmax"))
#' 
#' # can view percentage instead
#' plot_inventory_data(data = daily_niger, y = "percentage",
#'                     elements = c("tmin", "tmax"), display_perc = TRUE,
#'                     display_count = TRUE)

plot_inventory_data <- function(data, elements, station = NULL, y = c("count", "percentage"),
                                columns = c("elements", "stations"),
                                fill = c("none", "stations", "elements"),
                                facet_by = c("stations", "elements", "none"),
                                display_perc = FALSE, display_count = FALSE,
                                title = "Inventory Plot", x_title = NULL, y_title = NULL,
                                coord_flip = FALSE) {
  checkmate::assert_logical(display_perc)
  checkmate::assert_logical(display_count)
  y <- match.arg(y)
  columns <- match.arg(columns)
  fill <- match.arg(fill)
  facet_by <- match.arg(facet_by)

  if ((facet_by == "stations") & is.null(station)){
    warning("facet_by set to none since no stations are given in data")
    facet_by <- "none"
  }
  if ((columns == "stations") & is.null(station)){
    warning("columns set to `elements` since no stations are given in data")
    columns <- "elements"
  }
  if ((fill == "stations") & is.null(station)){
    warning("fill set to `none` since no stations are given in data")
    fill <- "none"
  }
  
  summary_table <- summarise_inventory_data(data = data, station = station, elements = elements)
  
  if (y == "count"){
    if (columns == "stations"){
      inv_plot <- ggplot2::ggplot(summary_table, mapping = ggplot2::aes(x = .data[[station]], y = .data$total_missing))
    } else {
      inv_plot <- ggplot2::ggplot(summary_table, mapping = ggplot2::aes(x = .data$elements_list, y = .data$total_missing))
    }
  } else {
    if (columns == "stations"){
      inv_plot <- ggplot2::ggplot(summary_table, mapping = ggplot2::aes(x = .data[[station]], y = .data$percentage))
    } else {
      inv_plot <- ggplot2::ggplot(summary_table, mapping = ggplot2::aes(x = .data$elements_list, y = .data$percentage))
    }
  }
  if (fill == "stations"){
    inv_plot <- inv_plot + ggplot2::geom_col(summary_table, mapping = ggplot2::aes(fill = factor(.data[[station]])))
  } else if (fill == "elements"){
    inv_plot <- inv_plot + ggplot2::geom_col(summary_table, mapping = ggplot2::aes(fill = factor(.data$elements_list)))
  } else {
    inv_plot <- inv_plot + ggplot2::geom_col(geom = "text")
  }
  if (facet_by == "stations"){
    inv_plot <- inv_plot + ggplot2::facet_grid(rows = ggplot2::vars(.data[[station]]))
  } else if (facet_by == "elements"){
    inv_plot <- inv_plot + ggplot2::facet_grid(rows = ggplot2::vars(.data$elements_list))
  } else {
    inv_plot <- inv_plot
  }
  if (display_perc & !display_count){
  inv_plot <- inv_plot  + 
    ggplot2::geom_label(data = summary_table, mapping = ggplot2::aes(label = round(.data$percentage, 2)))  
  }
  if (display_count & !display_perc){
    inv_plot <- inv_plot  + 
      ggplot2::geom_label(data = summary_table, mapping = ggplot2::aes(label = .data$total_missing))  
  }
  if (display_perc & display_count){
    summary_table$lab <- paste0(summary_table$total_missing, " (", round(summary_table$percentage, 2), ")")
    inv_plot <- inv_plot  + 
      ggplot2::geom_label(data = summary_table, mapping = ggplot2::aes(label = .data$lab))  
  }
  
  if(title == "Inventory Plot") {
    if (is.null(station)){
      title <- paste0(title, ": ", elements)
    } else {
      title <- paste0(title, ": ", elements, " by: ", station)
    }
  }
  if (coord_flip) {
    inv_plot <- inv_plot + ggplot2::coord_flip()
  }
  inv_plot <- inv_plot + 
    ggplot2::xlab(x_title) +
    ggplot2::ylab(y_title) +
    ggplot2::labs(title = title)
  
  return(inv_plot)  
}
