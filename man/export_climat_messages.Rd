% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_climat_messages.R
\name{export_climat_messages}
\alias{export_climat_messages}
\title{Export CLIMAT messages file(s) from daily data}
\usage{
export_climat_messages(
  data,
  date_time,
  station_id,
  year = NULL,
  month = NULL,
  mean_pressure_station = NULL,
  mean_pressure_reduced = NULL,
  mean_temp = NULL,
  mean_max_temp = NULL,
  mean_min_temp = NULL,
  mean_vapour_pressure = NULL,
  total_precip = NULL,
  total_sunshine = NULL,
  total_snow_depth = NULL,
  max_ws = NULL,
  min_h_vis = NULL,
  folder = getwd()
)
}
\arguments{
\item{data}{\code{data.frame} The data.frame to calculate from.}

\item{date_time}{\code{\link[base]{Date}} The name of the date column in \code{data}.}

\item{station_id}{\code{character(1)} The name of the station column in \code{data}.}

\item{year}{\code{character(1)} The name of the year column in \code{data}.}

\item{month}{\code{character(1)} The name of the month column in \code{data}.}

\item{mean_pressure_station}{TODO}

\item{mean_pressure_reduced}{TODO}

\item{mean_temp}{TODO}

\item{mean_max_temp}{TODO}

\item{mean_min_temp}{TODO}

\item{mean_vapour_pressure}{TODO}

\item{total_precip}{TODO}

\item{total_sunshine}{TODO}

\item{total_snow_depth}{Daily total snow depth in cm column name}

\item{max_ws}{Daily maximum wind speed in m/s column name}

\item{min_h_vis}{Daily minimum horizontal visibility in m column name}

\item{folder}{TODO}
}
\description{
Export CLIMAT messages file(s) from daily data
}
\examples{
# TODO
}
