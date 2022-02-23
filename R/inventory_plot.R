#' Produce an inventory of available and missing data
#' 
#' @description 
#' Creates an inventory plot displaying whether a value is observed or missing for each element and station given.
#' Takes a data frame as an input and the relevant columns to create the plot.
#' Creates a graph using \code{ggplot2} and returns a inventory plot.
#'
#' @param data The data.frame to calculate from.
#' @param date The name of the date column in \code{data}.
#' @param elements The name of the column in \code{data} to apply the function to.
#' @param station The name of the station column in \code{data}, if the data are for multiple station. 
#' @param year The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date]])}.
#' @param doy The name of the day of the year (1-366) column in \code{data}. 
#' If \code{doy} is \code{NULL} then it can be calculated as \code{yday_366(data[[date]])} if \code{date} is provided.
#' @param year_doy_plot logical. Whether the day of year should be on the y-axis on the plot.
#' @param title The text for the title.
#' @param plot_title_size Text size for the title in pts.
#' @param plot_title_hjust Horizontal justification (in [0, 1]) for title.
#' @param x_title The text for the x-axis.
#' @param y_title The text for the y-axis.
#' @param x_scale_from The year to display the inventory plot from.
#' @param x_scale_to The year to display the inventory plot to.
#' @param x_scale_by The different, in years, to give the x tick marks between from and to.
#' @param y_date_format TODO
#' @param y_date_scale_by TODO
#' @param y_date_scale_step TODO
#' @param facet_by Whether to facet by stations, elements, or both. Options are \code{"stations"}, \code{"elements"}, \code{"station-elements"}, \code{"elements-stations"}.
#' In \code{"station-elements"}, stations are given as rows and elements as columns. In \code{"elements-stations"}, elements are given as rows and stations as columns.
#' @param facet_x_size Text size for the facets on the x-axis in pts.
#' @param facet_y_size Text size for the facets on the y-axis in pts.
#' @param facet_scales Are scales shared across all facets (the default, \code{"fixed"}),
#' or do they vary across rows (\code{"free_x"}), columns (\code{"free_y"}), or both rows and columns (\code{"free"})?
#' @param facet_dir TODO
#' @param facet_x_margin Margin width around the text for the x-facets. See \code{ggplot2::margin()} for more details.
#' @param facet_y_margin Margin width around the text for the y-facets. See \code{ggplot2::margin()} for more details.
#' @param facet_nrow Number of rows for the facets if `facet_by` is one of \code{"stations"} or \code{"elements"}. Only if `facet_ncol` is given.
#' @param facet_ncol Number of rows for the facets if `facet_by` is one of \code{"stations"} or \code{"elements"}. Only if `facet_nrow` is given.
#' @param missing_colour Colour to represent the missing values. Default \code{"red"}.
#' @param present_colour Colour to represent the observed values. Default \code{"grey"}.
#' @param missing_label Colour to give in legend for missing values. Default \code{"Missing"}.
#' @param present_label Colour to give in legend for observed values. Default \code{"Present"}.
#' @param display_rain_days logical. If \code{rain} parameter is not \code{NULL}, and \code{rain} is not an element in the \code{elements} parameter, whether to include dry and rainy days.
#' @param rain The name of the rain column in \code{data}. 
#' @param rain_cats TODO 
#' @param labels If \code{display_rain_days = TRUE}, the labels in the key for dry and rainy days. By default, \code{c("Dry", "Rain")}
#' @param key_colours If \code{display_rain_days = TRUE}, the colours for dry and rainy days. By default, \code{c("tan3", "blue"))}
#' @param coord_flip logical. Whether to switch the x and y axes.
#'
#' @return A plot of type \code{ggplot} to the default plot device
#' @export 
#'
#' @examples
#' # Create an inventory plot with two elements and by station.
#' data(daily_niger)
#' inventory_plot(data = daily_niger, station = "station_name", elements = c("tmax", "tmin"),
#'                date = "date")
#'
#' # Create an inventory plot by year and day of year
#' inventory_plot(data = daily_niger, station = "station_name", elements = c("tmax", "tmin"),
#'                date = "date", year_doy_plot = TRUE)
#'
#' # Can add in rainy/dry days into the plot
#' inventory_plot(data = daily_niger, station = "station_name", elements = c("tmax", "tmin"),
#'                date = "date", rain = "rain", display_rain_days = TRUE)
inventory_plot <- function(data, date, elements, station = NULL, year = NULL, doy = NULL,  
                           year_doy_plot = FALSE, facet_by = NULL, 
                           facet_x_size = 7, facet_y_size = 11,
                           title = "Inventory Plot", plot_title_size = NULL, plot_title_hjust = 0.5,
                           x_title = NULL, y_title = NULL, 
                           x_scale_from = NULL, x_scale_to = NULL, x_scale_by = NULL, 
                           y_date_format = NULL, y_date_scale_by = NULL, y_date_scale_step = 1,
                           facet_scales = "fixed", facet_dir = "h", 
                           facet_x_margin = ggplot2::margin(1, 0, 1, 0),
                           facet_y_margin = ggplot2::margin(1, 0, 1, 0),
                           facet_nrow = NULL, facet_ncol = NULL, 
                           missing_colour = "red",
                           present_colour = "grey",
                           missing_label = "Missing",
                           present_label = "Present",
                           display_rain_days = FALSE, rain = NULL, 
                           rain_cats = list(breaks = c(0, 0.85, Inf), labels = c("Dry", "Rain"), key_colours = c("tan3", "blue")),
                           coord_flip = FALSE) {
  
  checkmate::assert_data_frame(data)
  assert_column_names(data, date)
  checkmate::assert_string(date)
  checkmate::assert_date(data[[date]])
  checkmate::assert_character(elements)
  assert_column_names(data, elements)
  if (!is.null(station)) assert_column_names(data, station)
  
  if (display_rain_days && !is.null(rain) && !rain %in% elements) elements <- c(elements, rain)
  
  is_facet_wrap <- !is.null(facet_nrow) || !is.null(facet_ncol)
  scale_xdate <- !is.null(x_scale_from) || !is.null(x_scale_to) || !is.null(x_scale_by)
  
  # Add year and doy columns if doing year_doy plot
  if(year_doy_plot) {
    if(is.null(year)) {
      year <- "year"
      data[[year]] <- lubridate::year(data[[date]])
    }
    if(is.null(doy)) {
      doy <- "doy"
      data[[doy]] <- yday_366(data[[date]])
    }
  }
  
  theme_blank_y_axis <- ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), axis.line.y = ggplot2::element_blank())
  if (length(elements) > 1) {
    id.vars <- c(date, year, doy)
    if(!is.null(station)) id.vars <- c(station, id.vars)
    suppressWarnings(data <- reshape2::melt(data, measure.vars = elements, id.vars = id.vars, value.name = "value", variable.name = "variable"))
    elements <- "value"
    multi_elements <- TRUE
  } else multi_elements <- FALSE
  
  key_name <- "key"
  data[[key_name]] <- factor(ifelse(is.na(data[[elements]]), missing_label, present_label), levels = c(present_label, missing_label))
  
  key <- c(missing_colour, present_colour)
  names(key) <- c(missing_label, present_label)
  if(display_rain_days) {
    levels(data[[key_name]]) <- c(levels(data[[key_name]]), rain_cats$labels)
    if(!is.null(rain)) {
      if(multi_elements) {
        data[[key_name]][data[["variable"]] == rain & data[[key_name]] != missing_label] <- cut(data[["value"]][data[["variable"]] == rain & data[[key_name]] != missing_label], breaks = rain_cats$breaks, labels = rain_cats$labels, right = FALSE)
        key <- c(missing_colour, present_colour, rain_cats$key_colours)
        names(key) <- c(missing_label, present_label, rain_cats$labels)
      }
      else {
        data[[key_name]][data[[key_name]] != missing_label] <- cut(data[[rain]][data[[key_name]] != missing_label], breaks = rain_cats$breaks, labels = rain_cats$labels, right = FALSE)
        key <- c(missing_colour, rain_cats$key_colours)
        names(key) <- c(missing_label, rain_cats$labels)
      }
    }
  }
  if(year_doy_plot) {
    data[["common_date"]] <- as.Date(paste0("2000-", data[[doy]]), "%Y-%j")
    g <- ggplot2::ggplot(data = data, mapping = ggplot2::aes_(x = as.name(year), y = as.name("common_date"), colour = as.name(key_name))) + 
      ggplot2::geom_point(size = 5, shape = 15) + 
      ggplot2::scale_colour_manual(values = key) + 
      ggplot2::scale_y_date(date_breaks = "2 month", labels = function(x) format(x, "%e %b"))
    if(!is.null(station) && multi_elements) {
      if(is.null(facet_by)) {
        message("facet_by not specified. facets will be by stations-elements.")
        facet_by <- "stations-elements"
      }
      else if(facet_by == "stations" || facet_by == "elements") {
        warning("facet_by = stations. facet_by must be either stations-elements or elements-stations when there are multiple of both. Using stations-elements.")
        facet_by <- "stations-elements"
      }
      else {
        warning("Invalid facet_by. Using stations-elements.")
        facet_by <- "stations-elements"
      }
      if(facet_by == "stations-elements") {
        g <- g + 
          ggplot2::facet_grid(facets = stats::as.formula(paste(station, "~variable")), scales = facet_scales)
      }
      else if(facet_by == "elements-stations") {
        g <- g +
          ggplot2::facet_grid(facets = stats::as.formula(paste("variable~", station)), scales = facet_scales)
      } else stop("invalid facet_by value:", facet_by)
    } else if(!is.null(station)) {
      fm <- stats::as.formula(paste(station, "~."))
      if (is_facet_wrap) {
        g <- g + 
          ggplot2::facet_wrap(facets = fm, nrow = facet_nrow, ncol = facet_ncol, scales = facet_scales)
      } else {
        g <- g + 
          ggplot2::facet_grid(facets = fm, scales = facet_scales)
      }
      if(title == "Inventory Plot") {
        title <- paste0(title, ": ", elements)
      }
    } else if(multi_elements) {
      if (is_facet_wrap) {
        g <- g + 
          ggplot2::facet_wrap(facets = variable ~ ., nrow = facet_nrow, ncol = facet_ncol, scales = facet_scales)
      } else {
        g <- g + 
          ggplot2::facet_grid(facets = variable ~ ., scales = facet_scales)
      }
    }
    if (scale_xdate) {
      g <- g +
        ggplot2::scale_x_continuous(breaks = seq(x_scale_from, x_scale_to, x_scale_by))
    }
    if (!is.null(y_date_scale_by) && !is.null(y_date_format)) {
      g <- g +
        ggplot2::scale_y_date(breaks = seq(min(data[["common_date"]]), 
                                           max(data[["common_date"]]), 
                                           by = paste0(y_date_scale_step, " ", y_date_scale_by)),
                              date_labels = y_date_format)
    }
  } else {
    g <- ggplot2::ggplot(data = data, ggplot2::aes_(x = as.name(date), y = 1, fill = as.name(key_name))) + ggplot2::geom_raster() + ggplot2::scale_fill_manual(values = key) + ggplot2::scale_x_date(date_minor_breaks = "1 year")
    if (!is.null(station) && multi_elements) {
      if (is.null(facet_by) || facet_by == "stations") {
        if (is.null(facet_by)) message("facet_by not specified. facets will be by stations.")
        g <- g + 
          ggplot2::facet_grid(facets = stats::as.formula(paste(station, "+ variable~.")), scales = facet_scales) + 
          theme_blank_y_axis + 
          ggplot2::scale_y_continuous(breaks = NULL) + 
          ggplot2::labs(y = y_title)
      } else if (facet_by == "elements") {
        g <- g + 
          ggplot2::facet_grid(facets = stats::as.formula(paste("variable +", station, "~.")), scales = facet_scales) + 
          theme_blank_y_axis + 
          ggplot2::scale_y_continuous(breaks = NULL) + 
          ggplot2::labs(y = y_title)
      } else if (facet_by == "stations-elements") {
        g <- g + 
          ggplot2::facet_grid(facets = stats::as.formula(paste(station, "~variable")), scales = facet_scales) + 
          theme_blank_y_axis + 
          ggplot2::scale_y_continuous(breaks = NULL) + 
          ggplot2::labs(y = y_title)
      } else if (facet_by == "elements-stations") {
        g <- g + ggplot2::facet_grid(facets = stats::as.formula(paste("variable~", station)), scales = facet_scales) + 
          theme_blank_y_axis + 
          ggplot2::scale_y_continuous(breaks = NULL) + 
          ggplot2::labs(y = y_title)
      } else stop("invalid facet_by value:", facet_by)
    } else if (!is.null(station)) {
      if (!is.factor(data[[station]]))
        data[[station]] <- factor(data[[station]])
      g <-
        ggplot2::ggplot(data = data, ggplot2::aes_(
          x = as.name(date),
          y = as.name(station),
          fill = as.name(key_name)
        )) + 
        ggplot2::geom_raster() + 
        ggplot2::scale_fill_manual(values = key) + 
        ggplot2::scale_x_date(date_minor_breaks = "1 year") + 
        ggplot2::geom_hline(yintercept = seq(0.5, by = 1, length.out = nlevels(data[[station]]) + 1))
      if (title == "Inventory Plot") {
        title <- paste0(title, ": ", elements)
      }
    }
    else if (multi_elements) {
      g <- ggplot2::ggplot(data = data, ggplot2::aes_(x = as.name(date), y = as.name("variable"), fill = as.name(key_name))) + 
        ggplot2::geom_raster() + 
        ggplot2::scale_fill_manual(values = key) + 
        ggplot2::scale_x_date(date_minor_breaks = "1 year") + 
        ggplot2::geom_hline(yintercept = seq(0.5, by = 1, length.out = length(levels(data[["variable"]])) + 1)) + 
        ggplot2::labs(y = "Elements")
    }
    else {
      g <-
        ggplot2::ggplot(data = data, ggplot2::aes_(
          x = as.name(date),
          y = 1,
          fill = as.name(key_name)
        )) + 
        ggplot2::geom_raster() + 
        ggplot2::scale_fill_manual(values = key) + 
        ggplot2::scale_x_date(date_minor_breaks = "1 year") + 
        ggplot2::geom_hline(yintercept = seq(0.5, by = 1, length.out = nlevels(data[["variable"]]) + 1)) + 
        theme_blank_y_axis + 
        ggplot2::scale_y_continuous(breaks = NULL) + 
        ggplot2::labs(y = elements)
    }
    if (scale_xdate) {
      g <-
        g + ggplot2::scale_x_date(breaks = paste0(x_scale_by, " year"), 
                                  limits = c(from = as.Date(paste0(x_scale_from, "-01-01")), 
                                             to = as.Date(paste0(x_scale_to, "-12-31"))),
                                  date_labels = "%Y"
        )
    }
  }
  if (coord_flip) {
    g <- g + ggplot2::coord_flip()
  }
  g <- g + 
    ggplot2::xlab(x_title) +
    ggplot2::ylab(y_title) +
    ggplot2::labs(title = title) + 
    ggplot2::theme(strip.text.x = ggplot2::element_text(margin = facet_x_margin, size = facet_x_size), 
                   strip.text.y = ggplot2::element_text(margin = facet_y_margin, size = facet_y_size), 
                   plot.title = ggplot2::element_text(hjust = plot_title_hjust, size = plot_title_size))
  return(g)
}
