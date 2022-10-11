# Tests for sensor_functions.R
Sys.setenv("TZ" = "UTC")

## get_data ===============
test_that("get_data", {
  db <- open_db(system.file("testdata", package = "mpathsenser"), "test.db")
  res <- get_data(db, "Activity", "12345", "2021-11-14", "2021-11-14") %>%
    dplyr::collect()
  expect_equal(
    res,
    tibble::tibble(
      measurement_id = c(
        "fbf85cd7-6d37-53a8-5c44-ad8fe13ef7ac",
        "ef96364c-d1f4-5f73-ce40-277f078e3d0f",
        "5ba54e77-4bcf-c8d1-17ff-71b9ed908897"
      ),
      participant_id = "12345",
      date = "2021-11-14",
      time = c("13:59:59", "14:00:00", "14:00:01"),
      confidence = c(NA, 100L, 99L),
      type = c(NA, "WALKING", "STILL")
    )
  )

  # Only a start date
  res <- get_data(db, "Device", "12345", "2021-11-14") %>%
    dplyr::collect()
  expect_equal(
    res,
    tibble::tibble(
      measurement_id = c(
        "ac1230a8-ed5f-4ded-7fca-7693a5ab4124",
        "138b9204-a313-96f3-89de-42bc2ac9d1e9"
      ),
      participant_id = "12345",
      date = "2021-11-14",
      time = c("13:00:00", "14:01:00"),
      device_id = c("QKQ1.200628.002", NA),
      hardware = c("qcom", NA),
      device_name = c("gauguin", NA),
      device_manufacturer = c("Xiaomi", NA),
      device_model = c("M2007J17G", NA),
      operating_system = c("REL", NA),
      platform = c("Android", NA)
    )
  )

  # Only an end date
  res <- get_data(db, "Device", "12345", end_date = "2021-11-13") %>%
    dplyr::collect()
  expect_equal(
    res,
    tibble::tibble(
      measurement_id = "bce3c272-3e06-4c84-f533-5bbbeaaac049",
      participant_id = "12345",
      date = "2021-11-13",
      time = "13:00:00",
      device_id = "QKQ1.200628.002",
      hardware = "qcom",
      device_name = "gauguin",
      device_manufacturer = "Xiaomi",
      device_model = "M2007J17G",
      operating_system = "REL",
      platform = "Android"
    )
  )

  DBI::dbDisconnect(db)
  expect_error(get_data(db, "Activity"), "Database connection is not valid")
})

## first_date ===============
test_that("first_date", {
  db <- open_db(system.file("testdata", package = "mpathsenser"), "test.db")
  expect_equal(first_date(db, "Device"), "2021-11-13")
  expect_equal(first_date(db, "Device", "12345"), "2021-11-13")
  DBI::dbDisconnect(db)
})

## last_date ===============
test_that("last_date", {
  db <- open_db(system.file("testdata", package = "mpathsenser"), "test.db")
  expect_equal(last_date(db, "Device"), "2021-11-14")
  expect_equal(last_date(db, "Device", "12345"), "2021-11-14")
  DBI::dbDisconnect(db)
})

## get_installed_apps ===============
test_that("get_installed_apps", {
  db <- open_db(system.file("testdata", package = "mpathsenser"), "test.db")
  res <- get_installed_apps(db, "12345")
  true <- tibble::tibble(app = c(
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
  ))
  expect_equal(res, true)
  DBI::dbDisconnect(db)
})

# add_data
test_that("add_data", {
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

# Define the true data
true <- tibble::tibble(
  participant_id = "12345",
  time = as.POSIXct(c("2022-05-10 10:00:00", "2022-05-10 10:05:00", "2022-05-10 10:20:00",
                      "2022-05-10 10:30:00", "2022-05-10 10:50:00", "2022-05-10 11:10:00",
                      "2022-05-10 11:30:00")),
  type = c("WALKING", NA, "WALKING", "STILL", NA, "STILL", "RUNNING"),
  confidence = c(80, NA, 80, 100, NA, 100, 20)
)

# Check basic functionality
res <- add_gaps(data = dat,
                gaps = gaps,
                by = "participant_id")
expect_identical(res, true)

# You can use fill if  you want to get rid of those pesky NA's
res <- add_gaps(data = dat,
                gaps = gaps,
                by = "participant_id",
                fill = list(type = "GAP", confidence = 100))
true <- tidyr::replace_na(true, list(type = "GAP", confidence = 100))
expect_identical(res, true)
})
