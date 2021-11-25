#' Decrypt GPS data from a curve25519 public key
#'
#' @param data A (lazy) tibble containing the GPS data
#' @param key A curve25519 public key
#'
#' @return The non-lazy decrypted GPS data
#' @export
decrypt_gps <- function(data, key) {
	if (!is.raw(key)) {
		key <- sodium::hex2bin(key)
	}

	data %>%
		dplyr::collect() %>%
		dplyr::mutate(
			dplyr::across(c(latitude, longitude), ~ {
				lapply(.x, sodium::hex2bin) %>%
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
#' @param r The average earth radius (6371 km by default).
#'
#' @return The distance between point 1 and 2 in kilometers.
#' @export
#'
#' @examples
# fra <- c(50.03333, 8.570556) # Frankfurt Airport
# ord <- c(41.97861, -87.90472) # Chicago O'Hare International Airport
# haversine(fra[1], fra[2], ord[1], ord[2]) # 6971.059 km
haversine <- function(lat1, lon1, lat2, lon2, r = 6371) {
	lat1 <- deg2rad(lat1)
	lon1 <- deg2rad(lon1)
	lat2 <- deg2rad(lat2)
	lon2 <- deg2rad(lon2)

	dlon <- lon2 - lon1
	dlat <- lat2 - lat1
	a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
	c <- 2 * atan2(sqrt(a), sqrt(1 - a))
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


