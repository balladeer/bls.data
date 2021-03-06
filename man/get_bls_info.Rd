% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_info.R
\name{get_bls_info}
\alias{get_bls_info}
\title{Get descriptive information about a BLS SeriesID.}
\usage{
get_bls_info(series_ids = "LAUCN040010000000005", registration_key = NA)
}
\arguments{
\item{series_ids}{input character vector}

\item{registration_key}{BLS API registration key as a character vector}
}
\value{
nested list with series ids as top-level named elements
}
\description{
You mush supply a registration_key
}
\examples{
get_bls_info()
get_bls_info(series_id=c("LAUCN040010000000005", "LAUCN040010000000006"))
}
