% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getStationsList.R
\name{getStationsList}
\alias{getStationsList}
\title{Get stations list}
\usage{
getStationsList(
  p_cod = "ceazamet",
  e_owner = NULL,
  e_cod = NULL,
  user = NULL,
  cols = NULL,
  geo = NULL,
  filename = NULL
)
}
\arguments{
\item{p_cod}{string, The code of the provider, mandatory field (default: ceazamet)}

\item{e_owner}{string, Specify "ceaza" to retrieve only CEAZA stations (default: NULL)}

\item{e_cod}{int or string, Station code (default: NULL)}

\item{user}{string, User name, add your email or user name (default: NULL)}

\item{cols}{string or vector of strings, Columns to retrieve, it must be a subset of c("e_lat", "e_lon", "e_altitud", "e_ultima_lectura", "e_cod", "e_nombre", "e_primera_lectura", "e_cod_provincia") (default: NULL)}

\item{geo}{boolean, Sort stations list by latitude? (TRUE for north to south) (default: NULL)}

\item{filename}{string, path to save the data frame as a CSV file (default: NULL)}
}
\value{
A data frame with the stations list
}
\description{
Get the list of stations available in the CEAZA-Met through the web service provide. Use this output (e_cod) to get sensors and data from the stations by using the \code{getSensorsList} and \code{getSensorData} functions.
}
\examples{
\dontrun{
getStationsList()
getStationsList(geo = 1)
getStationsList(user = "anon@host.com", cols = c(
  "e_lat", "e_lon",
  "e_altitud", "e_ultima_lectura", "e_cod", "e_nombre",
  "e_primera_lectura"
))
getStationsList(
  p_cod = "ceazamet", e_owner = "ceaza",
  user = "anon@host.com"
)
}
}
