#' Decrypt GPS data from a curve25519 public key
#'
#' @description `r lifecycle::badge("stable")`
#'
#' By default, the latitude and longitude of the GPS data collected by m-Path Sense will be
#' encrypted using an asymmetric curve25519 key to provide extra protection for these highly
#' sensitive data. This function takes the entire location data set and decrypts its longitude and
#' latitude columns using the provided \code{key}.
#'
#' @param data A (lazy) tibble containing the GPS data
#' @param key A curve25519 public key
#'
#' @return A tibble containing the non-lazy, decrypted GPS data
#' @export
decrypt_gps <- function(data, key) {
  ensure_suggested_package("sodium")

  if (!(is.raw(key) && length(key) == 32) && !is.character(key)) {
    abort(c(
      "`key` must be either in a hex or bin format.",
      i = "Try to use `sodium::hex2bin(key)` or `sodium::bin2hex(key)`",
      x = "Steer clear of `charToRaw(key)`, as this delivers an incorrect key format."
    ))
  }

  if (!is.raw(key)) {
    key <- sodium::hex2bin(key)
  }

  # Make some functions vectorised for neater syntax later on
  vec_hex2bin <- Vectorize(sodium::hex2bin, SIMPLIFY = FALSE)
  vec_simple_decrypt <- Vectorize(sodium::simple_decrypt, vectorize.args = "bin", SIMPLIFY = FALSE)
  vec_raw_to_char <- Vectorize(rawToChar, SIMPLIFY = FALSE)

  # Internal decryption process
  internal_decrypt <- function(hex_vec) {
    hex_vec <- hex_vec %>%
      vec_hex2bin() %>%
      vec_simple_decrypt(key) %>%
      vec_raw_to_char() %>%
      unlist()

    hex_vec
  }

  data <- data %>%
    collect() %>%
    mutate(across(c("latitude", "longitude"), internal_decrypt))

  data
}

deg2rad <- function(deg) {
  check_arg(deg, "double")
  deg * pi / 180
}

rad2deg <- function(rad) {
  check_arg(rad, "double")
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
#' @return A numeric value of the distance between point 1 and 2 in kilometers.
#' @export
#'
#' @examples
#' fra <- c(50.03333, 8.570556) # Frankfurt Airport
#' ord <- c(41.97861, -87.90472) # Chicago O'Hare International Airport
#' haversine(fra[1], fra[2], ord[1], ord[2]) # 6971.059 km
haversine <- function(lat1, lon1, lat2, lon2, r = 6371) {
  check_arg(lat1, "double")
  check_arg(lon1, "double")
  check_arg(lat2, "double")
  check_arg(lon2, "double")
  check_arg(r, "double")

  p <- pi / 180
  a <- 0.5 - cos((lat2 - lat1) * p) / 2 +
    cos(lat1 * p) * cos(lat2 * p) *
      (1 - cos((lon2 - lon1) * p)) / 2
  return(r * 2 * asin(sqrt(a))) # 2*R*asin...
}

location_variance <- function(lat, lon) {
  check_arg(lat, "double")
  check_arg(lon, "double")

  log((stats::sd(lat) * 2 + stats::sd(lon) * 2) + 1)
}

#' Reverse geocoding with latitude and longitude
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This functions allows you to extract information about a place based on the latitude and
#' longitude from the OpenStreetMaps nominatim API.
#'
#' @param lat The latitude of the location (in degrees)
#' @param lon The longitude of the location (in degrees)
#' @param zoom The desired zoom level from 1-18. The lowest level, 18, is building level.
#' @param email If you are making large numbers of request please include an appropriate email
#'   address to identify your requests. See Nominatim's Usage Policy for more details.
#' @param rate_limit The time interval to keep between queries, in seconds. If the rate limit is too
#'   low, OpenStreetMaps may reject further requests or even ban your entirely.
#'
#' @section Warning: Do not abuse this function or you will be banned by OpenStreetMap. The maximum
#'   number of requests is around 1 per second. Also make sure not to do too many batch lookups, as
#'   many subsequent requests will get you blocked as well.
#'
#' @return A list of information about the location. See [Nominatim's
#' documentation](https://nominatim.org/release-docs/develop/api/Reverse/#example-with-formatjsonv2)
#'    for more details.
#' @export
#'
#' @examples
#' # Frankfurt Airport
#' geocode_rev(50.037936, 8.5599631)
geocode_rev <- function(lat, lon, zoom = 18, email = "", rate_limit = 1) {
  check_arg(email, "character", n = 1, allow_null = TRUE)
  check_arg(rate_limit, "double", n = 1)

  base_query <- "https://nominatim.openstreetmap.org/reverse.php?"
  args <- list(
    lat = lat,
    lon = lon,
    email = rep(email, length(lat)),
    zoom = rep(zoom, length(lat)),
    format = rep("jsonv2", length(lat))
  )

  args <- purrr::transpose(args)
  args <- lapply(args, function(x) paste0(names(x), "=", x, collapse = "&"))
  query <- lapply(args, function(x) paste0(base_query, x))
  lapply(query, function(x) {
    res <- jsonlite::fromJSON(x)

    if (length(args) > 1) {
      Sys.sleep(rate_limit)
    }

    res
  })
}
