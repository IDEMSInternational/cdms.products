#' Windrose from the clifro package
#' 
#' A wrapper for plotting a windrose of wind speed and direction using ggplot2
#'
#' @param data 
#' @param speed 
#' @param direction 
#' @param facet 
#' @param n_directions 
#' @param n_speeds 
#' @param speed_cuts 
#' @param col_pal 
#' @param ggtheme 
#' @param legend_title 
#' @param calm_wind 
#' @param variable_wind 
#' @param n_col 
#'
#' @return
#' @export
#'
#' @examples
windrose <- function(data, speed, direction, facet = NULL, n_directions = 12, n_speeds = 5, speed_cuts = NA, col_pal = "GnBu",
                     ggtheme = c("grey", "gray", "bw", "linedraw", "light", "minimal", "classic"),
                     legend_title = "Wind Speed", calm_wind = 0, variable_wind = 990, n_col = NULL) {
  
  checkmate::assert_data_frame(data)
  assert_column_name(data, speed)
  assert_column_name(data, direction)
  if (!is.null(facet)) {
    assert_column_name(data, facet)
    n_facets <- length(unique(data[[facet]]))
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
  
  if (!is.null(facet)) {
    clifro::windrose(speed = data[[speed]], direction = data[[direction]], facet = data[[facet]], n_directions = n_directions,
                     n_speeds = n_speeds, speed_cuts = speed_cuts, col_pal = col_pal, ggtheme = ggtheme, legend_title = legend_title,
                     calm_wind = calm_wind, variable_wind = variable_wind, n_col = n_col)
  } else {
    clifro::windrose(speed = data[[speed]], direction = data[[direction]], n_directions = n_directions,
                     n_speeds = n_speeds, speed_cuts = speed_cuts, col_pal = col_pal, ggtheme = ggtheme, legend_title = legend_title,
                     calm_wind = calm_wind, variable_wind = variable_wind, n_col = n_col)
  }
}
