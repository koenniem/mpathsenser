#'Decrypt GPS data from a curve25519 public key
#'
#'@description `r lifecycle::badge("stable")`
#'
#'  By default, the latitude and longitude of the GPS data collected by m-Path Sense are encrypted
#'  using an asymmetric curve25519 key to provide extra protection for these highly sensitive data.
#'  This function takes a character vector and decrypts its longitude and latitude columns using the
#'  provided \code{key}.
#'
#'@inheritSection import Parallel
#'
#'@param data A character vector containing hexadecimal (i.e. encrypted) data.
#'@param key A curve25519 private key.
#'@param ignore A string with characters to ignore from `data`. See [sodium::hex2bin()].
#'
#'@returns A vector doubles of the decrypted GPS coordinates.
#'@export
#'
#' @examples
#'library(dplyr)
#'library(sodium)
#'# Create some GPS  coordinates.
#'data <- data.frame(
#'  participant_id = "12345",
#'  time = as.POSIXct(c("2022-12-02 12:00:00",
#'                      "2022-12-02 12:00:01",
#'                      "2022-12-02 12:00:02")),
#'  longitude = c("50.12345", "50.23456", "50.34567"),
#'  latitude = c("4.12345", "4.23456", "4.345678")
#')
#'
#'# Generate keypair
#'key <- sodium::keygen()
#'pub <- sodium::pubkey(key)
#'
#'# Encrypt coordinates with pubkey
#'# You do not need to do this for m-Path Sense
#'# as this is already encrypted
#'encrypt <- function(data, pub) {
#'  data <- lapply(data, charToRaw)
#'  data <- lapply(data, function(x) sodium::simple_encrypt(x, pub))
#'  data <- lapply(data, sodium::bin2hex)
#'  data <- unlist(data)
#'  data
#'}
#'data$longitude <- encrypt(data$longitude, pub)
#'data$latitude <- encrypt(data$latitude, pub)
#'
#'# Once the data has been collected, decrypt it using decrypt_gps().
#'data %>%
#'  mutate(longitude = decrypt_gps(longitude, key)) %>%
#'  mutate(latitude = decrypt_gps(latitude, key))
decrypt_gps <- function(data, key, ignore = ":") {
  ensure_suggested_package("sodium")
  check_arg(data, "character")

  # Custom key check: Either raw and length 32, or a character vector
  if (!(is.raw(key) && length(key) == 32) && !is.character(key)) {
    abort(c(
      "`key` must be either a hexadecimal string or a binary vector.",
      i = "Try to use `sodium::hex2bin(key)` or `sodium::bin2hex(key)`",
      x = "Steer clear of `charToRaw(key)`, as this delivers an incorrect key format."
    ))
  }

  if (!is.raw(key)) {
    key <- sodium::hex2bin(key)
  }

  data <- data %>%
    furrr::future_map(sodium::hex2bin, ignore = ignore) %>%
    furrr::future_map(sodium::simple_decrypt, key = key) %>%
    furrr::future_map(rawToChar) %>%
    unlist(recursive = FALSE) %>%
    as.double()

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
#' @returns A numeric value of the distance between point 1 and 2 in kilometers.
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
  return(r * 2 * asin(sqrt(a))) # Equal to 2*R*asin...
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
#' @returns A list of information about the location. See [Nominatim's
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
