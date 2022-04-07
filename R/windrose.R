#' Windrose from the clifro package
#' 
#' @description Returns a windrose plot using \code{ggplot2} of wind speed and
#' direction. This function is a wrapper of the `clifro::windrose()` function.
#'
#' @param data \code{data.frame} The data.frame to calculate from.
#' @param speed \code{numeric} A vector containing wind speeds.
#' @param direction \code{numeric} A vector containing wind direction.
#' @param facet_by \code{character(1)} Facets used to plot the various windroses.
#' @param n_directions \code{integer(1)} The number of direction bins to plot (petals on the rose).
#' The number of directions defaults to 12.
#' @param n_speeds \code{numeric(1)} The number of equally spaced wind speed bins to plot.
#' This is used if speed_cuts is NA (default 5).
#' @param speed_cuts \code{numeric} A vector containing the cut points for the wind speed intervals, or NA (default)
#' @param col_pal \code{character} String indicating the name of the \code{RColorBrewer} colour palette to be used for plotting.
#' @param ggtheme \code{character(1)} String (partially) matching the \code{\link[ggplot2]{ggtheme}} to be used for plotting.
#' @param legend_title \code{character(1)} Legend title.
#' @param calm_wind \code{numeric(1)} The upper limit for wind speed that is considered calm (default 0).
#' @param variable_wind \code{numeric(1)} Variable winds (if applicable).
#' @param n_col \code{integer(1)} The number of columns to plot (default 1).
#'
#' @seealso \code{\link[ggplot2]{theme}} for more possible arguments to pass to \code{windrose}.
#' 
#' @return a \code{ggplot} object.
#' @export
#'
#' @examples
#' # Generate a windrose plot for the daily_niger data
#' data(daily_niger)
#' windrose_plot <- windrose(data = daily_niger, speed = "ws", direction = "wd",
#'                           facet_by = "station_name")
windrose <- function(data, speed, direction, facet_by = NULL, n_directions = 12, n_speeds = 5, speed_cuts = NA, col_pal = "GnBu",
                     ggtheme = c("grey", "gray", "bw", "linedraw", "light", "minimal", "classic"),
                     legend_title = "Wind Speed", calm_wind = 0, variable_wind = 990, n_col = NULL) {
  
  checkmate::assert_data_frame(data)
  assert_column_names(data, speed)
  assert_column_names(data, direction)
  if (!is.null(facet_by)) {
    assert_column_names(data, facet_by)
    n_facets <- length(unique(data[[facet_by]]))
    if (is.null(n_col)) n_col <- ceiling(sqrt(n_facets))
    checkmate::assert_int(n_col)
  }
  checkmate::assert_int(n_directions)
  checkmate::assert_int(n_speeds)
  checkmate::assert_numeric(speed_cuts)
  checkmate::assert_string(col_pal)
  ggtheme <- match.arg(ggtheme)
  checkmate::assert_string(ggtheme)
  checkmate::assert_string(legend_title)
  checkmate::assert_numeric(calm_wind)
  checkmate::assert_numeric(variable_wind)
  
  if (!is.null(facet_by)) {
    clifro::windrose(speed = data[[speed]], direction = data[[direction]], facet = data[[facet_by]], n_directions = n_directions,
                     n_speeds = n_speeds, speed_cuts = speed_cuts, col_pal = col_pal, ggtheme = ggtheme, legend_title = legend_title,
                     calm_wind = calm_wind, variable_wind = variable_wind, n_col = n_col)
  } else {
    clifro::windrose(speed = data[[speed]], direction = data[[direction]], n_directions = n_directions,
                     n_speeds = n_speeds, speed_cuts = speed_cuts, col_pal = col_pal, ggtheme = ggtheme, legend_title = legend_title,
                     calm_wind = calm_wind, variable_wind = variable_wind, n_col = n_col)
  }
}
