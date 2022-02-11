#' Title
#'
#' @param data The data.frame to calculate from
#' @param station The name of the station column in \code{data}, if the data are for multiple station. 
#' The summary table is calculated separately for each station.
#' @param elements The name of the columns in \code{data} to apply the summary calculation to.
#' @param y Whether to display counts or percentages.
#' @param columns Whether to display stations or elements on the columns.
#' @param fill Whether to fill by stations, elements, or neither.
#' @param facets Whether to facet by stations, elements, or neither
#' @param display_perc logical. Whether to display percentage values on the plot.
#' @param display_count logical. Whether to display the count values on the plot.
#'
#' @return
#' @export
#'
#' @examples
#' data(daily_niger) 
#' plot_inventory_data(data = daily_niger, station = "station_name",
#'                     elements = c("tmin", "tmax"),
#'                     columns = "stations", facets = "elements")
#'                     
#' # can omit station argument
#' plot_inventory_data(data = daily_niger, 
#'                     elements = c("tmin", "tmax"))
#' 
#' # can view percentage instead
#' plot_inventory_data(data = daily_niger, y = "percentage",
#'                     elements = c("tmin", "tmax"), display_perc = TRUE,
#'                     display_count = TRUE)

plot_inventory_data <- function(data, station = NULL, elements, y = c("count", "percentage"),
                                columns = c("elements", "stations"),
                                fill = c("none", "stations", "elements"),
                                facets = c("stations", "elements", "none"),
                                display_perc = FALSE, display_count = FALSE){
  checkmate::assert_logical(display_perc)
  checkmate::assert_logical(display_count)
  y <- match.arg(y)
  columns <- match.arg(columns)
  fill <- match.arg(fill)
  facets <- match.arg(facets)

  if ((facets == "stations") & is.null(station)){
    warning("facets set to none since no stations are given in data")
    facets = "none"
  }
  if ((columns == "stations") & is.null(station)){
    warning("columns set to `elements` since no stations are given in data")
    columns = "elements"
  }
  if ((fill == "stations") & is.null(station)){
    warning("fill set to `none` since no stations are given in data")
    fill = "none"
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
  if (facets == "stations"){
    inv_plot <- inv_plot + ggplot2::facet_grid(rows = ggplot2::vars(.data[[station]]))
  } else if (facets == "elements"){
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
  
  return(inv_plot)  
}