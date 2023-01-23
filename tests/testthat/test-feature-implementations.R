test_that("feature_duration gives correct output", {
  data <- tibble(
    time = as.POSIXct(c("2022-05-10 10:00:00", "2022-05-10 10:30:00", "2022-05-10 11:00:00")),
    type = c("WALKING", "STILL", "RUNNING")
  )
  categories <-  c("RUNNING", "STILL", "WALKING")

  true <- tibble(
    type = c("RUNNING", "STILL", "WALKING"),
    duration = c(0, 50, 50)
  )

  res <- feature_duration(data = data,
                           by = "type",
                           categories = categories)

  expect_equal(res, true)

  # Multiple measurement at the same time
  # TODO: Make test more complicated
  data <- tibble(
    time = as.POSIXct(c("2022-05-10 10:00:00", "2022-05-10 10:00:00", "2022-05-10 10:30:00",
                        "2022-05-10 10:45:00", "2022-05-10 10:45:00", "2022-05-10 11:00:00")),
    type = c("WALKING", "RUNNING", "STILL", "STILL", "WALKING", "RUNNING")
  )

  true <- tibble(
    type = c("RUNNING", "STILL", "WALKING"),
    duration = c(25, 37.5, 37.5)
  )

  res <- feature_duration(data = data,
                          by = "type",
                          categories = categories)

  expect_equal(res, true)

  # Multiple measurements, one of which is a gap
  data <- tibble(
    time = as.POSIXct(c("2022-05-10 10:00:00", "2022-05-10 10:30:00", "2022-05-10 10:30:00",
                        "2022-05-10 11:00:00")),
    type = c("WALKING", "STILL", "GAP", "RUNNING")
  )
  categories <-  c("RUNNING", "STILL", "WALKING")

  true <- tibble(
    type = c("GAP", "RUNNING", "WALKING"),
    duration = c(50, 0, 50)
  )

  res <- feature_duration(data = data,
                          by = "type",
                          categories = categories)

  expect_equal(res, true)

  # With weights
  data <- tibble(
    time = as.POSIXct(c("2022-05-10 10:00:00", "2022-05-10 10:00:00", "2022-05-10 10:30:00",
                        "2022-05-10 10:30:00", "2022-05-10 10:45:00", "2022-05-10 11:00:00")),
    type = c("WALKING", "STILL", "STILL", "WALKING", "RUNNING", "RUNNING"),
    confidence = c(100, 40, 30, 20, 80, 100)
  )
  categories <-  c("RUNNING", "STILL", "WALKING")

  true <- tibble(
    type = c("RUNNING", "STILL", "WALKING"),
    duration = c(25, 29.285714, 45.714286)
  )

  res <- feature_duration(data = data,
                          by = "type",
                          categories = categories,
                          weight_by = "confidence")

  expect_equal(res, true, tolerance = 0.00001)

  # With weights and gap
  data <- tibble(
    time = as.POSIXct(c("2022-05-10 10:00:00", "2022-05-10 10:00:00", "2022-05-10 10:30:00",
                        "2022-05-10 10:30:00", "2022-05-10 10:45:00", "2022-05-10 11:00:00")),
    type = c("WALKING", "STILL", "GAP", "WALKING", "RUNNING", "RUNNING"),
    confidence = c(100, 40, 100, 20, 80, 100)
  )
  categories <-  c("RUNNING", "STILL", "WALKING")

  true <- tibble(
    type = c("GAP", "RUNNING", "STILL", "WALKING"),
    duration = c(25, 25, 14.2857143, 35.714286)
  )

  res <- feature_duration(data = data,
                          by = "type",
                          categories = categories,
                          weight_by = "confidence")

  expect_equal(res, true, tolerance = 0.00001)
})

test_that("feature_duration grouping works", {
  data <- tibble(
    participant_id = c(rep("12345", 6), rep("23456", 6)),
    time = rep(as.POSIXct(c("2022-05-10 10:00:00", "2022-05-10 10:00:00", "2022-05-10 10:30:00",
                        "2022-05-10 10:30:00", "2022-05-10 10:45:00", "2022-05-10 11:00:00")),
               2),
    type = rep(c("WALKING", "STILL", "GAP", "WALKING", "RUNNING", "RUNNING"), 2),
    confidence = rep(c(100, 40, 100, 20, 80, 100), 2)
  )
  categories <-  c("RUNNING", "STILL", "WALKING")

  true <- tibble(
    participant_id = c(rep("12345", 4), rep("23456", 4)),
    type = rep(c("GAP", "RUNNING", "STILL", "WALKING"), 2),
    duration = rep(c(25, 25, 14.2857143, 35.714286), 2)
  )

  res <- data |>
    group_by(participant_id) |>
    feature_duration(by = "type",
                     categories = categories,
                     weight_by = "confidence") |>
    ungroup()

  expect_equal(res, true, tolerance = 0.00001)
})

# test_that("feature_duration categories work", {
#   data <- tibble(
#     time = integer(0),
#     type = character(0)
#   )
#   categories <- c("RUNNING", "STILL", "WALKING")
#
#   true <- tibble(
#     type = categories,
#     duration = 0
#   )
#
#   res <- feature_duration(data = data,
#                            by = "type",
#                            categories = categories)
#
#   expect_equal(res, true)
# })
