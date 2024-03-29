% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/location_functions.R
\name{geocode_rev}
\alias{geocode_rev}
\title{Reverse geocoding with latitude and longitude}
\usage{
geocode_rev(lat, lon, zoom = 18, email = "", rate_limit = 1, format = "jsonv2")
}
\arguments{
\item{lat}{The latitude of the location (in degrees)}

\item{lon}{The longitude of the location (in degrees)}

\item{zoom}{The desired zoom level from 1-18. The lowest level, 18, is building level.}

\item{email}{If you are making large numbers of request please include an appropriate email
address to identify your requests. See Nominatim's Usage Policy for more details.}

\item{rate_limit}{The time interval to keep between queries, in seconds. If the rate limit is too
low, OpenStreetMaps may reject further requests or even ban your entirely.}

\item{format}{The format of the response. Either "jsonv2", "geojson", or"geocodejson". See
Nomatims documentation for more details.}
}
\value{
A list of information about the location. See \href{https://nominatim.org/release-docs/develop/api/Reverse/#example-with-formatjsonv2}{Nominatim's documentation}
for more details. The response may also be an error message in case of API errors, or \code{NA} if
the client or API is offline.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This functions allows you to extract information about a place based on the latitude and
longitude from the OpenStreetMaps nominatim API.
}
\section{Warning}{
 Do not abuse this function or you will be banned by OpenStreetMap. The maximum
number of requests is around 1 per second. Also make sure not to do too many batch lookups, as
many subsequent requests will get you blocked as well.
}

\examples{
# Frankfurt Airport
geocode_rev(50.037936, 8.5599631)
}
