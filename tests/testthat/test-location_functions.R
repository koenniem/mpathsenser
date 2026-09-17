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
  testthat::skip_if_offline("nominatim.openstreetmap.org")
  data <- tibble::tibble(
    lat = c(50.03333, 41.97861),
    lon = c(8.570556, -87.90472)
  )

  res <- geocode_rev(data$lat, data$lon, email = "koen.niemeijer@kuleuven.be")

  # No errors
  expect_false(any(unlist(lapply(res, names)) == "error"))
})
