# Tests for sensor_functions.R

test_that("get_data", {
  db <- create_test_db()
  res <- get_data(db, "Activity", "12345", "2021-11-14", "2021-11-14") %>%
    collect()
  expect_equal(
    res,
    tibble::tibble(
      measurement_id = c(
        "fbf85cd7-6d37-53a8-5c44-ad8fe13ef7ac",
        "ef96364c-d1f4-5f73-ce40-277f078e3d0f",
        "5ba54e77-4bcf-c8d1-17ff-71b9ed908897"
      ),
      participant_id = "12345",
      time = as.POSIXct(
        c("2021-11-14 13:59:59", "2021-11-14 14:00:00", "2021-11-14 14:00:01"),
        tz = "UTC"
      ),
      confidence = c(NA, 100L, 99L),
      type = c(NA, "WALKING", "STILL")
    )
  )

  # Only a start date
  res <- get_data(db, "Device", "12345", "2021-11-14") %>%
    collect()
  expect_equal(
    res,
    tibble::tibble(
      measurement_id = c(
        "ac1230a8-ed5f-4ded-7fca-7693a5ab4124",
        "138b9204-a313-96f3-89de-42bc2ac9d1e9"
      ),
      participant_id = "12345",
      time = as.POSIXct(c("2021-11-14 13:00:00", "2021-11-14 14:01:00"), tz = "UTC"),
      device_id = c("QKQ1.200628.002", NA),
      hardware = c("qcom", NA),
      device_name = c("gauguin", NA),
      device_manufacturer = c("Xiaomi", NA),
      device_model = c("M2007J17G", NA),
      operating_system = c("REL", NA),
      platform = c("Android", NA),
      operating_system_version = rep(NA_character_, 2),
      sdk = rep(NA_character_, 2)
    )
  )

  # Only an end date
  res <- get_data(db, "Device", "12345", end_date = "2021-11-13") %>%
    collect()
  expect_equal(
    res,
    tibble::tibble(
      measurement_id = "bce3c272-3e06-4c84-f533-5bbbeaaac049",
      participant_id = "12345",
      time = as.POSIXct("2021-11-13 13:00:00", tz = "UTC"),
      device_id = "QKQ1.200628.002",
      hardware = "qcom",
      device_name = "gauguin",
      device_manufacturer = "Xiaomi",
      device_model = "M2007J17G",
      operating_system = "REL",
      platform = "Android",
      operating_system_version = NA_character_,
      sdk = NA_character_
    )
  )

  cleanup_test_db(db)
})

test_that("first_date", {
  db <- create_test_db()
  expect_equal(first_date(db, "Device"), as.Date("2021-11-13"))
  expect_equal(first_date(db, "Device", "12345"), as.Date("2021-11-13"))
  cleanup_test_db(db)
})

test_that("last_date", {
  db <- create_test_db()
  expect_equal(last_date(db, "Device"), as.Date("2021-11-14"))
  expect_equal(last_date(db, "Device", "12345"), as.Date("2021-11-14"))
  cleanup_test_db(db)
})

test_that("installed_apps", {
  db <- create_test_db()
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
  db <- create_test_db()

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
      "sdk"
    )
  )
  expect_true(nrow(res) > 0)
  cleanup_test_db(db)
})

test_that("moving_average", {
  db <- create_test_db()

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
  db <- create_test_db()

  gaps <- identify_gaps(db, "12345", min_gap = 1, sensor = sensors)

  true <- tibble::tibble(
    participant_id = c("12345"),
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
    "Column\\(s\\) \"confidence\" must be present in both `data` and `gaps`."
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
