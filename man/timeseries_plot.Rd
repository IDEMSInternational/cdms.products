% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeseries_plot.R
\name{timeseries_plot}
\alias{timeseries_plot}
\title{Produce a timeseries graph}
\usage{
timeseries_plot(
  data,
  date_time,
  elements,
  station = NULL,
  facet_by = c("stations", "elements", "stations-elements", "elements-stations",
    "none"),
  type = c("line", "bar"),
  add_points = FALSE,
  add_line_of_best_fit = FALSE,
  se = TRUE,
  add_path = FALSE,
  add_step = FALSE,
  na_rm = FALSE,
  show_legend = NA,
  title = "Timeseries Plot",
  x_title = NULL,
  y_title = NULL
)
}
\arguments{
\item{data}{\code{data.frame} The data.frame to calculate from.}

\item{date_time}{\code{\link[base]{Date}} The name of the date column in \code{data}.}

\item{elements}{\code{character} The name of the elements column in \code{data} to apply the function to.}

\item{station}{\code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
Timeseries plots are calculated separately for each station.}

\item{facet_by}{\code{character(1)} Whether to facet by stations, elements, both, or neither. Options are \code{"stations"}, \code{"elements"}, \code{"station-elements"}, \code{"elements-stations"}, or \code{"none"}.}

\item{type}{\code{character(1)} The type of plot, either "line" or line graphs or "bar" for bar graphs.}

\item{add_points}{\code{logical(1)} If \code{TRUE}, points are added to the plot using  \code{"ggplot2::geom_point()"}.}

\item{add_line_of_best_fit}{\code{logical(1)} If \code{TRUE}, points are added to the plot using  \code{"ggplot2::geom_smooth(method = "lm")"}.}

\item{se}{\code{logical(1)} If \code{TRUE}, the standard error is are added to the line of best fit. Only works if \code{add_line_of_best_fit = TRUE}.}

\item{add_path}{\code{logical(1)} If \code{TRUE}, paths are added to the plot using  \code{"ggplot2::geom_path()"}.}

\item{add_step}{\code{logical(1)} If \code{TRUE}, steps are added to the plot using  \code{"ggplot2::geom_step()"}.}

\item{na_rm}{\code{logical(1)} If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing values are silently removed.}

\item{show_legend}{\code{logical(1)} Should this layer be included in the legends? \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE} never includes, and \code{TRUE} always includes.}

\item{title}{\code{character(1)} The text for the title.}

\item{x_title}{\code{character(1)} The text for the x-axis.}

\item{y_title}{\code{character(1)} The text for the y-axis.}
}
\value{
a \code{ggplot} object.
}
\description{
Returns a timeseries plot using \code{ggplot2} for each element
and station given. Takes a data frame as an input and the relevant columns to
create the plot.
}
\examples{
# Create a time series plot with two elements and facet by station.
data(daily_niger)
daily_niger_1 <- daily_niger \%>\% dplyr::filter(year < 1950)
timeseries_plot(data = daily_niger_1, date_time = "date", elements = c("tmax", "tmin"), 
                station = "station_name", facet_by = "stations")

# Can make additional changes to the plot since the returned object is a \code{ggplot2} object
# for example, to add colour-blind friendly colours instead of the default colours
require(ggplot2)
t1 <- timeseries_plot(data = daily_niger_1, date_time = "date", elements = c("tmax", "tmin"), 
                      station = "station_name", facet_by = "stations")
t1 + ggplot2::scale_colour_discrete(type = c("#E69F00", "#56B4E9"))
}
