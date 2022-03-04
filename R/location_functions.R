#' Decrypt GPS data from a curve25519 public key
#'
#' @param data A (lazy) tibble containing the GPS data
#' @param key A curve25519 public key
#'
#' @return The non-lazy decrypted GPS data
#' @export
decrypt_gps <- function(data, key) {
	if (!is.raw(key) & !is.character(key)) stop("key must be either a character or raw vector")

	if (!is.raw(key)) {
		key <- sodium::hex2bin(key)
	}

	data %>%
		dplyr::collect() %>%
		dplyr::mutate(dplyr::across(c(latitude, longitude), ~ {
			.x %>%
				lapply(sodium::hex2bin) %>%
				lapply(sodium::simple_decrypt, key) %>%
				lapply(rawToChar) %>%
				unlist
		}))
}

deg2rad <- function(deg) {
	deg * pi / 180
}

rad2deg <- function(rad) {
	rad * 180 / pi
}

#' Calculate the Great-Circle Distance between two points in kilometers
#'
#' Calculate the great-circle distance between two points using the Haversine function.
#'
#' @param lon1 The longitude of point 1 in degrees.
#' @param lat1 The latitude of point 1 in degrees.
#' @param lon2 The longitude of point 2 in degrees.
#' @param lat2 The latitude of point 2 in degrees.
#' @param r The average earth radius.
#'
#' @return The distance between point 1 and 2 in kilometers.
#' @export
#'
#' @examples
#' fra <- c(50.03333, 8.570556) # Frankfurt Airport
#' ord <- c(41.97861, -87.90472) # Chicago O'Hare International Airport
#' haversine(fra[1], fra[2], ord[1], ord[2]) # 6971.059 km
haversine <- function(lat1, lon1, lat2, lon2, r = 6371) {
	lat1 <- deg2rad(lat1)
	lon1 <- deg2rad(lon1)
	lat2 <- deg2rad(lat2)
	lon2 <- deg2rad(lon2)

	dlon <- lon2 - lon1
	dlat <- lat2 - lat1
	a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
	c <- 2 * asin(sqrt(a))
	d <- r * c
	d
}

location_variance <- function(lat, lon, time) {
	lat_sd <- stats::sd(lat)
	lon_sd <- stats::sd(lon)
	if(lat_sd == 0 & lon_sd ==0) {
		return(log(1e-09))
	}
	return(log(lat_sd^2 + lon^2))
}

#' Reverse geocoding with latitude and longitude
#'
#' This functions allows you to extract information about a place based on the latitude and
#' longitude from the OpenStreetMaps nominatim API.
#'
#' Warning: DO NOT ABUSE THIS FUNCTION or you will be banned by OpenStreetMap. The maximum number
#' of requests is around 1 per second. Also make sure not to do batch lookups, as many subsequent
#' requests will get you blocked as well.
#'
#' @param lat The latitude of the location (in degrees)
#' @param lon The longitude of the location (in degrees)
#' @param zoom The desired zoom level from 1-18. The lowest level, 18, is building level.
#' @param email If you are making large numbers of request please include an appropriate email
#' address to identify your requests. See Nominatim's Usage Policy for more details.
#'
#' @return A list of information about the location. See [Nominatim's documentation](https://nominatim.org/release-docs/develop/api/Reverse/#example-with-formatjsonv2) for more details.
#' @export
#'
#' @examples
#' # Frankfurt Airport
#' geocode_rev(50.037936, 8.5599631)
geocode_rev <- function(lat, lon, zoom = 18, email=NULL) {
	base_query <- "https://nominatim.openstreetmap.org/reverse.php?"
	args <- list(
		lat = lat,
		lon = lon,
		email = rep(email, length(lat)),
		zoom = rep(zoom, length(lat)),
		format = rep("jsonv2", length(lat))
	)

	args <- purrr::flatten(args)
	args <- purrr::transpose(args)
	args <- lapply(args, function(x) paste0(names(x) , "=", x, collapse = "&"))
	query <- lapply(args, function(x) paste0(base_query, x))
	res <- lapply(query, jsonlite::fromJSON, query)
	jsonlite::fromJSON(paste0(base_query, args))
}