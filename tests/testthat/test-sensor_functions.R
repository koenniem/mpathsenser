# Tests for sensor_functions.R

test_that("get_data", {
  db <- create_sensor_test_db()
  res <- get_data(db, "Activity", "12345", "2021-11-14", "2021-11-14") |>
    dplyr::select(participant_id, time, timezone, confidence, type) |>
    dplyr::collect()
  expect_equal(
    res,
    tibble::tibble(
      participant_id = 12345,
      time = as.POSIXct(
        c("2021-11-14 13:59:59", "2021-11-14 14:00:00", "2021-11-14 14:00:01"),
        tz = "UTC"
      ),
      timezone = rep(NA_character_, 3),
      confidence = c(NA, 100L, 99L),
      type = c(NA, "WALKING", "STILL")
    )
  )

  # Only a start date
  res <- get_data(db, "Device", "12345", "2021-11-14", "2021-11-14") |>
    dplyr::select(
      participant_id,
      time,
      device_id,
      hardware,
      device_name,
      device_manufacturer,
      device_model,
      operating_system,
      platform,
      operating_system_version
    ) |>
    dplyr::collect()
  expect_equal(
    res,
    tibble::tibble(
      participant_id = 12345,
      time = as.POSIXct(c("2021-11-14 13:00:00", "2021-11-14 14:01:00"), tz = "UTC"),
      device_id = c("QKQ1.200628.002", NA),
      hardware = c("qcom", NA),
      device_name = c("gauguin", NA),
      device_manufacturer = c("Xiaomi", NA),
      device_model = c("M2007J17G", NA),
      operating_system = c("REL", NA),
      platform = c("Android", NA),
      operating_system_version = rep(NA_character_, 2)
    )
  )

  # Only an end date
  res <- get_data(db, "Device", "12345", end_date = "2021-11-13") |>
    dplyr::select(participant_id, time, device_id) |>
    dplyr::collect()
  expect_equal(
    res,
    tibble::tibble(
      participant_id = 12345,
      time = as.POSIXct("2021-11-13 13:00:00", tz = "UTC"),
      device_id = "QKQ1.200628.002"
    )
  )

  cleanup_test_db(db)
})

test_that("get_data rejects invalid date strings", {
  db <- create_sensor_test_db()
  on.exit(cleanup_test_db(db), add = TRUE)

  expect_snapshot(
    error = TRUE,
    get_data(db, "Activity", start_date = "2021-02-30")
  )
  expect_snapshot(
    error = TRUE,
    get_data(db, "Activity", end_date = "2021-11-14 trailing")
  )
  expect_error(get_data(db, "Activity", start_date = "foo"), "valid date")
  expect_error(get_data(db, "Activity", start_date = NA), "must be a character")
  expect_error(get_data(db, "Activity", start_date = as.Date(NA)), "must be a non-missing")
  expect_error(
    get_data(db, "Activity", end_date = as.POSIXct(NA, tz = "UTC")),
    "must be a non-missing"
  )
})

test_that("get_data supports date ranges and exact POSIXt bounds", {
  db <- create_sensor_test_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  DBI::dbExecute(db, "SET timezone = 'America/New_York'")

  character_result <- get_data(
    db,
    "Activity",
    participant_id = "12345",
    start_date = "2021-11-14",
    end_date = "2021-11-14"
  ) |>
    dplyr::collect()
  date_result <- get_data(
    db,
    "Activity",
    participant_id = "12345",
    start_date = as.Date("2021-11-14"),
    end_date = as.Date("2021-11-14")
  ) |>
    dplyr::collect()
  instant <- as.POSIXct("2021-11-14 09:00:00", tz = "America/New_York")
  posix_result <- get_data(
    db,
    "Activity",
    participant_id = "12345",
    start_date = instant,
    end_date = instant
  ) |>
    dplyr::collect()

  expect_equal(date_result, character_result)
  expect_equal(posix_result$confidence, 100L)
  expect_equal(
    as.numeric(posix_result$time),
    as.numeric(as.POSIXct("2021-11-14 14:00:00", tz = "UTC"))
  )
})

test_that("get_data uses local wall dates only for _local views", {
  db <- create_sensor_test_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  DBI::dbExecute(db, "SET timezone = 'America/New_York'")
  DBI::dbExecute(db, "UPDATE raw.Activity SET timezone = 'Europe/Brussels'")
  DBI::dbExecute(
    db,
    "INSERT INTO raw.Activity
     (participant_id, time, timezone, confidence, type, source_file_id, source_row_id, source_measurement_id)
     VALUES
       ('12345', '2021-11-14 23:30:00+00', 'Europe/Brussels', 80, 'STILL', 1, 100, 1),
       ('12345', '2021-11-15 00:00:00+00', 'Europe/Brussels', 81, 'STILL', 1, 101, 1)"
  )

  utc_day <- get_data(db, "Activity", "12345", end_date = "2021-11-14") |>
    dplyr::arrange(.data$time) |>
    dplyr::select(confidence, time) |>
    dplyr::collect()
  unfiltered <- get_data(db, "Activity", "12345") |>
    dplyr::select("time") |>
    dplyr::collect()
  local_day <- get_data(db, "Activity_local", "12345", end_date = "2021-11-14") |>
    dplyr::arrange(.data$time) |>
    dplyr::select(confidence) |>
    dplyr::collect()
  with_local_day <- get_data(db, "Activity_with_local", "12345", end_date = "2021-11-14") |>
    dplyr::arrange(.data$time) |>
    dplyr::select(confidence) |>
    dplyr::collect()
  local_instant <- as.POSIXct("2021-11-14 15:00:00", tz = "Europe/Brussels")
  local_exact <- get_data(
    db,
    "Activity_local",
    "12345",
    start_date = local_instant,
    end_date = local_instant
  ) |>
    dplyr::select(confidence) |>
    dplyr::collect()

  expect_equal(utc_day$confidence, c(NA_integer_, 100L, 99L, 80L))
  expect_equal(local_day$confidence, c(NA_integer_, 100L, 99L))
  expect_equal(with_local_day$confidence, c(NA_integer_, 100L, 99L, 80L))
  expect_equal(local_exact$confidence, 100L)
  expect_equal(
    format(utc_day$time, tz = "UTC"),
    c(
      "2021-11-14 13:59:59",
      "2021-11-14 14:00:00",
      "2021-11-14 14:00:01",
      "2021-11-14 23:30:00"
    )
  )
  next_midnight <- as.POSIXct("2021-11-15 00:00:00", tz = "UTC")
  expect_false(any(as.numeric(utc_day$time) == as.numeric(next_midnight)))
  expect_true(any(as.numeric(unfiltered$time) == as.numeric(next_midnight)))
})

test_that("installed_apps", {
  db <- create_sensor_test_db()
  res <- installed_apps(db, "12345")
  true <- tibble::tibble(
    app = c(
      "BBC News",
      "Calculator",
      "Clock",
      "Google News",
      "Google PDF Viewer",
      "Google Play Books",
      "Google Play Games",
      "Google Play Movies & TV",
      "Google Play Music",
      "Google Play Services for AR",
      "Google VR Services",
      "Home",
      "Mobile Device Information Provider",
      "Photos",
      "WhatsApp",
      "m-Path Sense"
    )
  )
  expect_equal(res, true)
  cleanup_test_db(db)
})

test_that("app_category skips missing names and rate-limits requests", {
  requests <- character()
  sleeps <- numeric()
  local_mocked_bindings(
    app_category_impl = function(name, num, exact) {
      requests <<- c(requests, name)
      list(package = paste0("pkg.", name), genre = paste0("genre.", name))
    },
    .package = "mpathsenser"
  )
  local_mocked_bindings(
    Sys.sleep = function(time) {
      sleeps <<- c(sleeps, time)
    },
    .package = "base"
  )
  app_names <- c("first", NA_character_, "second", NA_character_, "third")

  result <- app_category(app_names, rate_limit = 1.5, .progress = FALSE)

  expect_identical(requests, c("first", "second", "third"))
  expect_identical(sleeps, c(1.5, 1.5))
  expect_identical(result$app, app_names)
  expect_equal(result$package, c("pkg.first", NA, "pkg.second", NA, "pkg.third"))
  expect_equal(result$genre, c("genre.first", NA, "genre.second", NA, "genre.third"))
})

test_that("app_category", {
  skip_if_offline("play.google.com")

  # Test whether there is some response
  # This test would break in case of a change in app category or search algorithm, so for now
  # we just check if there is a response, even if it is NA.
  res <- app_category("whatsapp")
  expect_equal(res$app, "whatsapp")
  expect_true(nrow(res) == 1)
  # expect_equal(
  #   res,
  #   data.frame(
  #     app = "whatsapp",
  #     package = "com.whatsapp",
  #     genre = "COMMUNICATION"
  #   )
  # )

  res2 <- app_category("whatsapp", exact = FALSE)
  expect_equal(res, res2)

  res <- app_category(c("whatsapp", "weather"), rate_limit = 1)
  expect_equal(colnames(res), c("app", "package", "genre"))
  expect_true(nrow(res) == 2)

  res <- app_category("joizmfoipjfjjf9803j")
  expect_equal(res$package, NA)
  expect_equal(res$genre, NA)

  expect_equal(app_category("foo", num = 1e9)$package, NA)
})

test_that("device_info", {
  db <- create_sensor_test_db()

  expect_error(device_info(db, participant_id = "12345"), NA)
  res <- device_info(db, participant_id = "12345")
  expect_equal(
    colnames(res),
    c(
      "participant_id",
      "device_id",
      "hardware",
      "device_name",
      "device_manufacturer",
      "device_model",
      "operating_system",
      "platform",
      "operating_system_version",
      "timezone"
    )
  )
  expect_true(nrow(res) > 0)
  cleanup_test_db(db)
})

test_that("moving_average", {
  db <- create_sensor_test_db()

  expect_error(
    moving_average(db, "Accelerometer", cols = "x_mean", participant_id = "12345", n = 2),
    NA
  )
  res <- moving_average(
    db = db,
    sensor = "Accelerometer",
    cols = "x_mean",
    participant_id = "12345",
    n = 2,
    start_date = "2021-11-14",
    end_date = "2021-11-14"
  ) %>%
    dplyr::collect()
  expect_true(nrow(res) > 0)

  cleanup_test_db(db)
})

test_that("identify_gaps", {
  db <- create_sensor_test_db()

  gaps <- identify_gaps(db, "12345", min_gap = 1, sensor = sensors)

  # TODO: Calculate the other gaps by hand
  gaps <- gaps[1:8, ]

  true <- tibble::tibble(
    participant_id = c(12345),
    from = as.POSIXct(
      c(
        "2021-11-13 13:00:00",
        "2021-11-14 13:00:00",
        "2021-11-14 13:59:59",
        "2021-11-14 14:00:00",
        "2021-11-14 14:00:01",
        "2021-11-14 14:00:02",
        "2021-11-14 14:00:10",
        "2021-11-14 14:01:00"
      ),
      tz = "UTC"
    ),
    to = as.POSIXct(
      c(
        "2021-11-14 13:00:00",
        "2021-11-14 13:59:59",
        "2021-11-14 14:00:00",
        "2021-11-14 14:00:01",
        "2021-11-14 14:00:02",
        "2021-11-14 14:00:10",
        "2021-11-14 14:01:00",
        "2021-11-14 14:02:00"
      ),
      tz = "UTC"
    ),
    gap = c(86400, 3599, 1, 1, 1, 8, 50, 60)
  )

  expect_equal(nrow(gaps), nrow(true))
  expect_equal(gaps$participant_id, true$participant_id)
  expect_true(nrow(gaps) > 0)
  cleanup_test_db(db)
})

# add_data
test_that("add_gaps", {
  # Define some data
  dat <- data.frame(
    participant_id = "12345",
    time = as.POSIXct(c("2022-05-10 10:00:00", "2022-05-10 10:30:00", "2022-05-10 11:30:00")),
    type = c("WALKING", "STILL", "RUNNING"),
    confidence = c(80, 100, 20)
  )

  gaps <- data.frame(
    participant_id = "12345",
    from = as.POSIXct(c("2022-05-10 10:05:00", "2022-05-10 10:50:00")),
    to = as.POSIXct(c("2022-05-10 10:20:00", "2022-05-10 11:10:00"))
  )

  # Test by
  expect_error(
    add_gaps(dat, gaps, by = "confidence"),
    "Column `confidence` must be present in both `data` and `gaps`."
  )

  # Define the true data
  true <- tibble::tibble(
    participant_id = "12345",
    time = as.POSIXct(c(
      "2022-05-10 10:00:00",
      "2022-05-10 10:05:00",
      "2022-05-10 10:30:00",
      "2022-05-10 10:50:00",
      "2022-05-10 11:30:00"
    )),
    type = c("WALKING", NA, "STILL", NA, "RUNNING"),
    confidence = c(80, NA, 100, NA, 20)
  )

  true_continue <- tibble::tibble(
    participant_id = "12345",
    time = as.POSIXct(c(
      "2022-05-10 10:00:00",
      "2022-05-10 10:05:00",
      "2022-05-10 10:20:00",
      "2022-05-10 10:30:00",
      "2022-05-10 10:50:00",
      "2022-05-10 11:10:00",
      "2022-05-10 11:30:00"
    )),
    type = c("WALKING", NA, "WALKING", "STILL", NA, "STILL", "RUNNING"),
    confidence = c(80, NA, 80, 100, NA, 100, 20)
  )

  # Check basic functionality
  res <- add_gaps(
    data = dat,
    gaps = gaps,
    by = "participant_id",
    continue = FALSE
  )

  res_continue <- add_gaps(
    data = dat,
    gaps = gaps,
    by = "participant_id",
    continue = TRUE
  )
  expect_identical(res, true)
  expect_identical(res_continue, true_continue)

  # You can use fill if  you want to get rid of those pesky NA's
  res <- add_gaps(
    data = dat,
    gaps = gaps,
    by = "participant_id",
    continue = FALSE,
    fill = list(type = "GAP", confidence = 100)
  )

  res_continue <- add_gaps(
    data = dat,
    gaps = gaps,
    by = "participant_id",
    continue = TRUE,
    fill = list(type = "GAP", confidence = 100)
  )
  true <- tidyr::replace_na(true, list(type = "GAP", confidence = 100))
  true_continue <- tidyr::replace_na(true_continue, list(type = "GAP", confidence = 100))
  expect_identical(res, true)
  expect_identical(res_continue, true_continue)

  # Problems occur when there is no information _before_ the gap
  dat <- data.frame(
    participant_id = c(rep("12345", 4), rep("23456", 4)),
    time = rep(
      as.POSIXct(c(
        "2022-05-10 10:00:00",
        "2022-05-10 10:30:00",
        "2022-05-10 10:30:00",
        "2022-05-10 11:30:00"
      )),
      2
    ),
    event = rep(c("a", "b", "c", "d"), 2),
    event2 = rep(c("a", "b", "c", "d"), 2)
  )

  gaps <- data.frame(
    participant_id = c(rep("12345", 5), rep("23456", 5)),
    from = rep(
      as.POSIXct(c(
        "2022-05-10 09:05:00",
        "2022-05-10 09:20:00",
        "2022-05-10 10:10:00",
        "2022-05-10 10:40:00",
        "2022-05-10 11:00:00"
      )),
      2
    ),
    to = rep(
      as.POSIXct(c(
        "2022-05-10 09:10:00",
        "2022-05-10 09:40:00",
        "2022-05-10 10:20:00",
        "2022-05-10 10:50:00",
        "2022-05-10 11:10:00"
      )),
      2
    )
  )
  res <- add_gaps(
    data = dat,
    gaps = gaps,
    by = "participant_id",
    continue = FALSE,
    fill = list(event = "GAP", event2 = "GAP")
  )
  res_continue <- add_gaps(
    data = dat,
    gaps = gaps,
    by = "participant_id",
    continue = TRUE,
    fill = list(event = "GAP", event2 = "GAP")
  )
  true <- tibble::tibble(
    participant_id = c(rep("12345", 9), rep("23456", 9)),
    time = rep(
      as.POSIXct(c(
        "2022-05-10 09:05:00",
        "2022-05-10 09:20:00",
        "2022-05-10 10:00:00",
        "2022-05-10 10:10:00",
        "2022-05-10 10:30:00",
        "2022-05-10 10:30:00",
        "2022-05-10 10:40:00",
        "2022-05-10 11:00:00",
        "2022-05-10 11:30:00"
      )),
      2
    ),
    event = rep(
      c(
        "GAP",
        "GAP",
        "a",
        "GAP",
        "b",
        "c",
        "GAP",
        "GAP",
        "d"
      ),
      2
    ),
    event2 = event
  )
  true_continue <- tibble::tibble(
    participant_id = c(rep("12345", 16), rep("23456", 16)),
    time = rep(
      as.POSIXct(c(
        "2022-05-10 09:05:00",
        "2022-05-10 09:10:00",
        "2022-05-10 09:20:00",
        "2022-05-10 09:40:00",
        "2022-05-10 10:00:00",
        "2022-05-10 10:10:00",
        "2022-05-10 10:20:00",
        "2022-05-10 10:30:00",
        "2022-05-10 10:30:00",
        "2022-05-10 10:40:00",
        "2022-05-10 10:50:00",
        "2022-05-10 10:50:00",
        "2022-05-10 11:00:00",
        "2022-05-10 11:10:00",
        "2022-05-10 11:10:00",
        "2022-05-10 11:30:00"
      )),
      2
    ),
    event = rep(
      c(
        "GAP",
        NA,
        "GAP",
        NA,
        "a",
        "GAP",
        "a",
        "b",
        "c",
        "GAP",
        "b",
        "c",
        "GAP",
        "b",
        "c",
        "d"
      ),
      2
    ),
    event2 = event
  )
  expect_equal(res, true)
  expect_equal(res_continue, true_continue)

  # Bug: If the end of the gap is exactly equal to the first measurement after the gap, that
  # measurement is replicated instead of the one before the gap.
  dat <- tibble::tibble(
    participant_id = "12345",
    time = as.POSIXct(c(
      "2022-05-10 09:50:00",
      "2022-05-10 10:00:00",
      "2022-05-10 10:10:00",
      "2022-05-10 10:30:00"
    )),
    event = c("a", "b", "c", "d")
  )

  gaps <- tibble::tibble(
    participant_id = "12345",
    from = as.POSIXct("2022-05-10 10:00:00"),
    to = as.POSIXct("2022-05-10 10:10:00")
  )
  res <- add_gaps(
    data = dat,
    gaps = gaps,
    by = "participant_id",
    continue = FALSE
  )
  res_continue <- add_gaps(
    data = dat,
    gaps = gaps,
    by = "participant_id",
    continue = TRUE
  )

  true <- tibble::tibble(
    participant_id = "12345",
    time = as.POSIXct(c(
      "2022-05-10 09:50:00",
      "2022-05-10 10:00:00",
      "2022-05-10 10:00:00",
      "2022-05-10 10:10:00",
      "2022-05-10 10:30:00"
    )),
    event = c("a", "b", NA, "c", "d")
  )
  expect_equal(res, true)
  expect_equal(res_continue, true)
})

test_that("add_gaps handles multi-column and variable-held keys", {
  dat <- tibble::tibble(
    participant_id = c("p1", "p1", "p2"),
    device = c("a", "a", "b"),
    time = as.POSIXct(
      c(
        "2022-05-10 10:00:00",
        "2022-05-10 10:30:00",
        "2022-05-10 10:00:00"
      ),
      tz = "UTC"
    ),
    event = c("early", "late", "other")
  )
  gaps <- tibble::tibble(
    participant_id = c("p1", "p1", "missing"),
    device = c("a", "a", "z"),
    from = as.POSIXct(
      c(
        "2022-05-10 10:10:00",
        "2022-05-10 10:40:00",
        "2022-05-10 10:05:00"
      ),
      tz = "UTC"
    ),
    to = as.POSIXct(
      c(
        "2022-05-10 10:20:00",
        "2022-05-10 10:50:00",
        "2022-05-10 10:15:00"
      ),
      tz = "UTC"
    )
  )
  by_cols <- c("participant_id", "device")

  expected <- tibble::tibble(
    participant_id = c("p1", "p1", "p1", "p1", "p2"),
    device = c("a", "a", "a", "a", "b"),
    time = as.POSIXct(
      c(
        "2022-05-10 10:00:00",
        "2022-05-10 10:10:00",
        "2022-05-10 10:30:00",
        "2022-05-10 10:40:00",
        "2022-05-10 10:00:00"
      ),
      tz = "UTC"
    ),
    event = c("early", NA, "late", NA, "other")
  )
  expected_continue <- tibble::tibble(
    participant_id = c("p1", "p1", "p1", "p1", "p1", "p1", "p2"),
    device = c("a", "a", "a", "a", "a", "a", "b"),
    time = as.POSIXct(
      c(
        "2022-05-10 10:00:00",
        "2022-05-10 10:10:00",
        "2022-05-10 10:20:00",
        "2022-05-10 10:30:00",
        "2022-05-10 10:40:00",
        "2022-05-10 10:50:00",
        "2022-05-10 10:00:00"
      ),
      tz = "UTC"
    ),
    event = c("early", NA, "early", "late", NA, "late", "other")
  )

  for (continue in c(FALSE, TRUE)) {
    target <- if (continue) expected_continue else expected
    expect_identical(
      add_gaps(dat, gaps, by = c("participant_id", "device"), continue = continue),
      target
    )
    expect_identical(
      add_gaps(dat, gaps, by = by_cols, continue = continue),
      target
    )
  }
})
