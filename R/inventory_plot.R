#' Inventory Plot
#' 
#' Produces an inventory of available and missing data
#'
#' @param data 
#' @param date 
#' @param elements 
#' @param station 
#' @param year 
#' @param doy 
#' @param year_doy_plot 
#' @param coord_flip 
#' @param facet_by 
#' @param facet_xsize 
#' @param facet_ysize 
#' @param title 
#' @param subtitle 
#' @param caption 
#' @param title_size 
#' @param subtitle_size 
#' @param caption_size 
#' @param xaxis_title 
#' @param xaxis_title_size 
#' @param yaxis_title 
#' @param yaxis_title_size 
#' @param xaxis_text_size 
#' @param xaxis_text_angle 
#' @param yaxis_text_size 
#' @param yaxis_text_angle 
#' @param xaxis_scale_from 
#' @param xaxis_scale_to 
#' @param xaxis_scale_by 
#' @param yaxis_date_format 
#' @param yaxis_date_scale_by 
#' @param yaxis_date_scale_step 
#' @param legend_position 
#' @param facet_scales 
#' @param facet_dir 
#' @param facet_nrow 
#' @param facet_ncol 
#' @param missing_colour 
#' @param present_colour 
#' @param missing_label 
#' @param present_label 
#' @param display_rain_days 
#' @param rain 
#' @param rain_cats 
#'
#' @return
#' @export
#'
#' @examples
inventory_plot <- function(data, date, elements, station = NULL, year = NULL, doy = NULL,  
                           year_doy_plot = FALSE, coord_flip = FALSE, facet_by = NULL, 
                           facet_xsize = 7, facet_ysize = 11,
                           title = "Inventory Plot", subtitle = NULL, caption = NULL, 
                           title_size = NULL,  subtitle_size = NULL, caption_size = NULL, 
                           xaxis_title = NULL, xaxis_title_size = NULL,
                           yaxis_title = NULL, yaxis_title_size = NULL,
                           xaxis_text_size = NULL, xaxis_text_angle = NULL, 
                           yaxis_text_size = NULL, yaxis_text_angle = NULL, 
                           xaxis_scale_from = NULL, xaxis_scale_to = NULL, xaxis_scale_by = NULL, 
                           yaxis_date_format = NULL, yaxis_date_scale_by = NULL, yaxis_date_scale_step = 1,
                           legend_position = NULL, 
                           facet_scales = "fixed", facet_dir = "h", 
                           facet_nrow = NULL, facet_ncol = NULL, 
                           missing_colour = "red",
                           present_colour = "grey",
                           missing_label = "Missing",
                           present_label = "Present",
                           display_rain_days = FALSE, rain = NULL, 
                           rain_cats = list(breaks = c(0, 0.85, Inf), labels = c("Dry", "Rain"), key_colours = c("tan3", "blue"))) {
  
  if (missing(data)) stop("data must be specified.")
  col_names <- names(data)
  if (missing(date)) stop("date column must be specified.")
  if (!date %in% col_names) stop("date column: ", date, " is not a column in data.")
  if (!lubridate::is.Date(data[[date]])) stop("date columns must be of type Date.")
  
  if (missing(elements)) stop("element column(s) must be specified.")
  if (!all(elements %in% col_names)) stop("Some of elements are not columns in the data.")
  
  if (!is.null(station) && !station %in% col_names) stop("station column: ", station, " is not a column in data.")
  
  if (display_rain_days && !is.null(rain) && !rain %in% elements) elements <- c(elements, rain)
  
  is_facet_wrap <- !is.null(facet_nrow) || !is.null(facet_ncol)
  scale_xdate <- !is.null(xaxis_scale_from) || !is.null(xaxis_scale_to) || !is.null(xaxis_scale_by)
  
  # Add year and doy columns if doing year_doy plot
  if(year_doy_plot) {
    if(is.null(year)) {
      year <- ".year"
      data[[year]] <- lubridate::year(data[[date]])
    }
    if(is.null(doy)) {
      doy <- ".doy"
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
  
  key_name <- ".key"
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
        ggplot2::scale_x_continuous(breaks = seq(xaxis_scale_from, xaxis_scale_to, xaxis_scale_by))
    }
    if (!is.null(yaxis_date_scale_by) && !is.null(yaxis_date_format)) {
      g <- g +
        ggplot2::scale_y_date(breaks = seq(min(data[["common_date"]]), 
                                           max(data[["common_date"]]), 
                                           by = paste0(yaxis_date_scale_step, " ", yaxis_date_scale_by)),
                              date_labels = yaxis_date_format)
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
          ggplot2::labs(y = NULL)
      } else if (facet_by == "elements") {
        g <- g + 
          ggplot2::facet_grid(facets = stats::as.formula(paste("variable +", station, "~.")), scales = facet_scales) + 
          theme_blank_y_axis + 
          ggplot2::scale_y_continuous(breaks = NULL) + 
          ggplot2::labs(y = NULL)
      } else if (facet_by == "stations-elements") {
        g <- g + 
          ggplot2::facet_grid(facets = stats::as.formula(paste(station, "~variable")), scales = facet_scales) + 
          theme_blank_y_axis + 
          ggplot2::scale_y_continuous(breaks = NULL) + 
          ggplot2::labs(y = NULL)
      } else if (facet_by == "elements-stations") {
        g <- g + ggplot2::facet_grid(facets = stats::as.formula(paste("variable~", station)), scales = facet_scales) + 
          theme_blank_y_axis + 
          ggplot2::scale_y_continuous(breaks = NULL) + 
          ggplot2::labs(y = NULL)
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
        g + ggplot2::scale_x_date(breaks = paste0(xaxis_scale_by, " year"), 
                                  limits = c(from = as.Date(paste0(xaxis_scale_from, "-01-01")), 
                                             to = as.Date(paste0(xaxis_scale_to, "-12-31"))),
                                  date_labels = "%Y"
        )
    }
  }
  if (coord_flip) {
    g <- g + ggplot2::coord_flip()
  }
  g <- g + 
    ggplot2::xlab(xaxis_title) +
    ggplot2::ylab(yaxis_title) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) + 
    ggplot2::theme(strip.text.x = ggplot2::element_text(margin = ggplot2::margin(1, 0, 1, 0), size = facet_xsize), 
                   strip.text.y = ggplot2::element_text(margin = ggplot2::margin(1, 0, 1, 0), size = facet_ysize), 
                   legend.position = legend_position, 
                   plot.title = ggplot2::element_text(hjust = 0.5, size = title_size), 
                   plot.subtitle = ggplot2::element_text(size = subtitle_size), 
                   plot.caption = ggplot2::element_text(size = caption_size), 
                   axis.text.x = ggplot2::element_text(size = xaxis_text_size, angle = xaxis_text_angle, vjust = 0.6), 
                   axis.title.x = ggplot2::element_text(size = xaxis_title_size), 
                   axis.title.y = ggplot2::element_text(size = yaxis_title_size), 
                   axis.text.y = ggplot2::element_text(size = yaxis_text_size, angle = yaxis_text_angle, hjust = 0.6))
  return(g)
}