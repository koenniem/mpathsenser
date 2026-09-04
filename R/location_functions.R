deg2rad <- function(deg) {
  check_arg(deg, "numeric")
  deg * pi / 180
}

rad2deg <- function(rad) {
  check_arg(rad, "numeric")
  rad * 180 / pi
}

#' Calculate the Great-Circle Distance between two points in kilometers
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Calculate the great-circle distance between two points using the Haversine function.
#'
#' @param lon1 The longitude of point 1 in degrees.
#' @param lat1 The latitude of point 1 in degrees.
#' @param lon2 The longitude of point 2 in degrees.
#' @param lat2 The latitude of point 2 in degrees.
#' @param r The average earth radius.
#'
#' @returns A numeric value of the distance between point 1 and 2 in kilometers.
#' @export
#'
#' @examples
#' fra <- c(50.03333, 8.570556) # Frankfurt Airport
#' ord <- c(41.97861, -87.90472) # Chicago O'Hare International Airport
#' haversine(fra[1], fra[2], ord[1], ord[2]) # 6971.059 km
haversine <- function(lat1, lon1, lat2, lon2, r = 6371) {
  check_arg(lat1, "numeric")
  check_arg(lon1, "numeric")
  check_arg(lat2, "numeric")
  check_arg(lon2, "numeric")
  check_arg(r, "numeric")

  p <- pi / 180
  a <- 0.5 -
    cos((lat2 - lat1) * p) / 2 +
    cos(lat1 * p) * cos(lat2 * p) * (1 - cos((lon2 - lon1) * p)) / 2
  return(r * 2 * asin(sqrt(a))) # Equal to 2*R*asin...
}

location_variance <- function(lat, lon) {
  check_arg(lat, "numeric")
  check_arg(lon, "numeric")

  log((stats::sd(lat) * 2 + stats::sd(lon) * 2) + 1)
}

#' Reverse geocoding with latitude and longitude
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This functions allows you to extract information about a place based on the latitude and
#'   longitude from the OpenStreetMaps nominatim API.
#'
#' @param lat The latitude of the location (in degrees)
#' @param lon The longitude of the location (in degrees)
#' @param zoom The desired zoom level from 1-18. The lowest level, 18, is building level.
#' @param email If you are making large numbers of request please include an appropriate email
#'   address to identify your requests. See Nominatim's Usage Policy for more details.
#' @param rate_limit The time interval to keep between queries, in seconds. If the rate limit is too
#'   low, OpenStreetMaps may reject further requests or even ban your entirely.
#' @param format The format of the response. Either "jsonv2", "geojson", or"geocodejson". See
#'   Nomatims documentation for more details.
#'
#' @section Warning: Do not abuse this function or you will be banned by OpenStreetMap. The maximum
#'   number of requests is around 1 per second. Also make sure not to do too many batch lookups, as
#'   many subsequent requests will get you blocked as well.
#'
#' @returns A list of information about the location. See [Nominatim's
#'   documentation](https://nominatim.org/release-docs/develop/api/Reverse/#example-with-formatjsonv2)
#'   for more details. The response may also be an error message in case of API errors, or `NA` if
#'   the client or API is offline.
#' @export
#'
#' @examples
#' # Frankfurt Airport
#' geocode_rev(50.037936, 8.5599631)
geocode_rev <- function(lat, lon, zoom = 18, email = "", rate_limit = 1, format = "jsonv2") {
  check_arg(email, "character", n = 1, allow_null = TRUE)
  check_arg(rate_limit, "double", n = 1)
  check_arg(format, "character", n = 1)
  check_arg(lat, "numeric")
  check_arg(lon, "numeric")

  if (length(lat) != length(lon)) {
    cli::cli_abort("{.arg lat} and {.arg lon} must have equal length.")
  }

  format <- match.arg(format, c("jsonv2", "geojson", "geocodejson"))

  n <- length(lat)

  if (n == 0) {
    return(list())
  }

  # Avoid duplicate API requests
  key <- paste(lat, lon, zoom, sep = "_")
  unique_key <- unique(key)

  results <- vector("list", length(unique_key))
  # names(results) <- unique_key

  base_url <- "https://nominatim.openstreetmap.org/reverse"

  for (i in seq_along(unique_key)) {
    idx <- match(unique_key[i], key)

    query <- paste0(
      base_url,
      "?lat=", lat[idx],
      "&lon=", lon[idx],
      "&zoom=", zoom,
      "&format=", format,
      if (nzchar(email)) {
        paste0("&email=", utils::URLencode(email, reserved = TRUE))
      } else {
        ""
      }
    )

    results[[i]] <- suppressWarnings(
      tryCatch(
        jsonlite::fromJSON(query),
        error = function(e) NA
      )
    )

    if (i < length(unique_key)) {
      Sys.sleep(rate_limit)
    }
  }

  results[match(key, unique_key)]
}
