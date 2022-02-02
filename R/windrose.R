#' Windrose from the clifro package
#' 
#' A wrapper for plotting a windrose of wind speed and direction using ggplot2
#'
#' @param data The data.frame to calculate from.
#' @param speed TODO
#' @param direction TODO
#' @param facet TODO
#' @param n_directions TODO
#' @param n_speeds TODO
#' @param speed_cuts TODO
#' @param col_pal TODO
#' @param ggtheme TODO
#' @param legend_title TODO
#' @param calm_wind TODO
#' @param variable_wind TODO
#' @param n_col TODO
#'
#' @return
#' @export
#'
#' @examples # TODO
windrose <- function(data, speed, direction, facet = NULL, n_directions = 12, n_speeds = 5, speed_cuts = NA, col_pal = "GnBu",
                     ggtheme = c("grey", "gray", "bw", "linedraw", "light", "minimal", "classic"),
                     legend_title = "Wind Speed", calm_wind = 0, variable_wind = 990, n_col = NULL) {
  
  checkmate::assert_data_frame(data)
  assert_column_names(data, speed)
  assert_column_names(data, direction)
  if (!is.null(facet)) {
    assert_column_names(data, facet)
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
