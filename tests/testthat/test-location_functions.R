test_that("decrypt_gps", {
  # Create a new key
  key <- sodium::keygen()
  pubkey <- sodium::pubkey(key)

  data <- tibble::tibble(
    participant_id = "12345",
    longitude = c("50.123456", "50.2345678"),
    latitude = c("4.987654", "4.876543")
  )

  # copy to compare with later
  true <- data

  # Convert to raw
  data$longitude <- lapply(data$longitude, function(x) charToRaw(x))
  data$latitude <- lapply(data$latitude, function(x) charToRaw(x))

  # Encrypt
  data$longitude <- lapply(data$longitude, function(x) sodium::simple_encrypt(x, pubkey))
  data$latitude <- lapply(data$latitude, function(x) sodium::simple_encrypt(x, pubkey))

  # Convert to hex, like in the real data
  key <- sodium::bin2hex(key)
  data$longitude <- vapply(data$longitude, function(x) sodium::bin2hex(x), character(1))
  data$latitude <- vapply(data$latitude, function(x) sodium::bin2hex(x), character(1))

  expect_equal(
    decrypt_gps(data$longitude, key),
    as.double(true$longitude),
    ignore_attr = TRUE
  )

  expect_equal(
    decrypt_gps(data$latitude, key),
    as.double(true$latitude),
    ignore_attr = TRUE
  )

  # Test key format
  expect_error(
    decrypt_gps(data$longitude, charToRaw(key)),
    "`key` must be either a hexadecimal string or a binary vector."
  )

  expect_error(
    decrypt_gps(data$longitude, sodium::hex2bin(substr(key, 1, 20))),
    "`key` must be either a hexadecimal string or a binary vector."
  )
})

test_that("deg2rad", {
  expect_equal(
    deg2rad(100),
    100 * pi / 180
  )
})

test_that("rad2deg", {
  expect_equal(
    rad2deg(100),
    100 * 180 / pi
  )
})

test_that("haversine", {
  fra <- c(50.03333, 8.570556) # Frankfurt Airport
  ord <- c(41.97861, -87.90472) # Chicago O'Hare International Airport
  expect_equal(
    haversine(fra[1], fra[2], ord[1], ord[2]),
    6971.059
  )

  x <- c(50.0359, 5.4253)
  y <- c(58.3838, 3.0412)
  expect_equal(
    haversine(x[1], x[2], y[1], y[2]),
    940.94763
  )
})

test_that("location_variance", {
  data <- tibble::tibble(
    lat = c(50.03333, 41.97861),
    lon = c(8.570556, -87.90472)
  )

  expect_equal(
    location_variance(data$lat, data$lon),
    5.0027895
  )
})

test_that("geocode_rev", {
  data <- tibble::tibble(
    lat = c(50.03333, 41.97861),
    lon = c(8.570556, -87.90472)
  )

  res <- geocode_rev(data$lat, data$lon, email = "koen.niemeijer@kuleuven.be")

  # No errors
  expect_false(any(unlist(lapply(res, names)) == "error"))

  # Correct places
  # Disabled, as too tight checking will lead to a failed test when something on OSM changes
})
