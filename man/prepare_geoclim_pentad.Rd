% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_geoclim_pentad.R
\name{prepare_geoclim_pentad}
\alias{prepare_geoclim_pentad}
\title{Prepare pentad data in GeoCLIM format}
\usage{
prepare_geoclim_pentad(
  data,
  year,
  pentad,
  element,
  station_id,
  latitude,
  longitude,
  metadata = NULL,
  join_by = NULL,
  add_cols = NULL
)
}
\arguments{
\item{data}{\code{data.frame} The data.frame to calculate from.}

\item{year}{\code{character(1)} The name of the year column in \code{data}.}

\item{pentad}{\code{character(1)} The name of the pentad column in \code{data}.}

\item{element}{\code{character(1)} The name of the element column in \code{data} to apply the function to.}

\item{station_id}{\code{character(1)} The name of the station column in \code{metadata}, or \code{data} if \code{metadata = NULL}.}

\item{latitude}{\code{character(1)} The name of the latitude column in \code{metadata}, or \code{data} if \code{metadata = NULL}.}

\item{longitude}{\code{character(1)} The name of the longitude column in \code{metadata}, or \code{data} if \code{metadata = NULL}.}

\item{metadata}{\code{data.frame} The metadata data.frame to calculate from.}

\item{join_by}{\code{character} The variable(s) to merge the \code{data} and \code{metadata} data frames.}

\item{add_cols}{\code{character} Names of additional metadata columns that should be included in the output}
}
\value{
A data.frame formatted for use in GeoCLIM
}
\description{
\code{prepare_geoclim_pentad} takes a data frame that is in pentad
format. This data is then rearranged to a format suitable for use in GeoCLIM.
This data frame can be rearranged and exported to a file or connection with
\code{export_geoclim_pentad}.
}
\examples{
# Calculate pentad summaries for the rainfall column
pentad_data <- daily_niger \%>\% dplyr::mutate(pentad = pentad(date))
summary_data <- pentad_data \%>\% dplyr::group_by(station_name, year, pentad) \%>\%
      dplyr::summarise(mean_rain = mean(rain, na.rm = TRUE))
prepare_geoclim_pentad(data = summary_data, year = "year",
                station_id = "station_name",
                pentad = "pentad",
                element = "mean_rain", metadata = stations_niger, 
                join_by = "station_name",
                latitude = "lat", longitude = "long")
}
