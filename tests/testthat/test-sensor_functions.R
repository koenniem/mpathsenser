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
      measurement_id = c("fbf85cd7-6d37-53a8-5c44-ad8fe13ef7ac",
                         "ef96364c-d1f4-5f73-ce40-277f078e3d0f",
                         "5ba54e77-4bcf-c8d1-17ff-71b9ed908897"),
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
      measurement_id = c("ac1230a8-ed5f-4ded-7fca-7693a5ab4124",
                         "138b9204-a313-96f3-89de-42bc2ac9d1e9"),
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

## link ===============
test_that("link", {
  dat1 <- data.frame(
    time = rep(seq.POSIXt(as.POSIXct("2021-11-14 13:00:00"), by = "1 hour", length.out = 3), 2),
    participant_id = c(rep("12345", 3), rep("23456", 3)),
    item_one = rep(c(40, 50, 60), 2)
  )

  dat2 <- data.frame(
    time = rep(seq.POSIXt(as.POSIXct("2021-11-14 12:50:00"), by = "5 min", length.out = 30), 2),
    participant_id = c(rep("12345", 30), rep("23456", 30)),
    x = rep(1:30, 2)
  )

  res <- link(dat1, dat2, "participant_id", offset_before = 1800)
  true <- tibble::tibble(
    time = rep(c(as.POSIXct("2021-11-14 13:00:00"), as.POSIXct("2021-11-14 14:00:00"),
                 as.POSIXct("2021-11-14 15:00:00")), 2),
    participant_id = c(rep("12345", 3), rep("23456", 3)),
    item_one = rep(c(40, 50, 60), 2),
    data = rep(list(
      tibble::tibble(time = seq.POSIXt(from = as.POSIXct("2021-11-14 12:50:00"),
                                       length.out = 3, by = "5 min"),
                     x = 1:3),
      tibble::tibble(time = seq.POSIXt(from = as.POSIXct("2021-11-14 13:30:00"),
                                       length.out = 7, by = "5 min"),
                     x = 9:15),
      tibble::tibble(time = seq.POSIXt(from = as.POSIXct("2021-11-14 14:30:00"),
                                       length.out = 7, by = "5 min"),
                     x = 21:27)
    ), 2)
  )
  expect_equal(res, true)

  # Scrambled test
  scramble <- function(data) {
    idx <- sample(1:nrow(data), nrow(data))
    data[idx,]
  }
  res <- link(scramble(dat1), scramble(dat2), "participant_id", offset_before = 1800) %>%
    dplyr::arrange(participant_id, time)
  expect_equal(res, true)

  res <- link(dat1, dat2, "participant_id", offset_after = 1800)
  true <- tibble::tibble(
    time = rep(c(as.POSIXct("2021-11-14 13:00:00"), as.POSIXct("2021-11-14 14:00:00"),
                 as.POSIXct("2021-11-14 15:00:00")), 2),
    participant_id = c(rep("12345", 3), rep("23456", 3)),
    item_one = rep(c(40, 50, 60), 2),
    data =  rep(list(
      tibble::tibble(time = seq.POSIXt(from = as.POSIXct("2021-11-14 13:00:00"),
                                       length.out = 7, by = "5 min"),
                     x = 3:9),
      tibble::tibble(time = seq.POSIXt(from = as.POSIXct("2021-11-14 14:00:00"),
                                       length.out = 7, by = "5 min"),
                     x = 15:21),
      tibble::tibble(time = seq.POSIXt(from = as.POSIXct("2021-11-14 15:00:00"),
                                       length.out = 4, by = "5 min"),
                     x = 27:30)
    ), 2)
  )
  expect_equal(res, true)

  res <- link(scramble(dat1), scramble(dat2), "participant_id", offset_after = 1800) %>%
    dplyr::arrange(participant_id, time)
  expect_equal(res, true)

  # Test add_before and add_after
  res <- link(dat1, dat2, "participant_id", offset_before = 1800,
              add_before = TRUE, add_after = TRUE)
  true <- tibble::tibble(
    time = rep(c(as.POSIXct("2021-11-14 13:00:00"), as.POSIXct("2021-11-14 14:00:00"),
                 as.POSIXct("2021-11-14 15:00:00")), 2),
    participant_id = c(rep("12345", 3), rep("23456", 3)),
    item_one = rep(c(40, 50, 60), 2),
    data = rep(list(
      tibble::tibble(time = c(seq.POSIXt(from = as.POSIXct("2021-11-14 12:50:00"),
                                         length.out = 3, by = "5 min"),
                              as.POSIXct("2021-11-14 13:00:00")),
                     x = c(1:3, 4),
                     original_time = c(rep(lubridate::`NA_POSIXct_`, 3),
                                       as.POSIXct("2021-11-14 13:05:00"))),
      tibble::tibble(time = c(as.POSIXct("2021-11-14 13:30:00"),
                              seq.POSIXt(from = as.POSIXct("2021-11-14 13:30:00"),
                                       length.out = 7, by = "5 min"),
                              as.POSIXct("2021-11-14 14:00:00")),
                     x = c(8, 9:15, 16),
                     original_time = c(as.POSIXct("2021-11-14 13:25:00"),
                                       rep(lubridate::`NA_POSIXct_`, 7),
                                       as.POSIXct("2021-11-14 14:05:00"))),
      tibble::tibble(time = c(as.POSIXct("2021-11-14 14:30:00"),
                              seq.POSIXt(from = as.POSIXct("2021-11-14 14:30:00"),
                                       length.out = 7, by = "5 min"),
                              as.POSIXct("2021-11-14 15:00:00")),
                     x = c(20, 21:27, 28),
                     original_time = c(as.POSIXct("2021-11-14 14:25:00"),
                                       rep(lubridate::`NA_POSIXct_`, 7),
                                       as.POSIXct("2021-11-14 15:05:00")))
    ), 2)
  )
  expect_equal(res, true, ignore_attr = TRUE)

  # Test arguments
  expect_error(link(1, dat2, "participant_id", offset_before = 1800), "x must be a data frame")
  expect_error(link(dat1, 1, "participant_id", offset_before = 1800), "y must be a data frame")
  expect_error(link(dat1, dat2, 12345, offset_before = 1800),
               "by must be a character vector of variables to join by")

  expect_error(link(dplyr::mutate(dat1, time = as.character(time)), dat2, offset_before = 1800),
               "column 'time' in x must be a POSIXct")
  expect_error(link(dat1, dplyr::mutate(dat2, time = as.character(time)), offset_before = 1800),
               "column 'time' in y must be a POSIXct")
  expect_error(link(dat1, dat2, offset_before = TRUE),
               "offset_before must be a character vector, numeric vector, or a period")
  expect_error(link(dat1, dat2, offset_before = "1800"),
               paste("Invalid offset specified\\. Try something like '30 minutes', ",
                     "lubridate::minutes\\(30\\)\\, or 1800."))
  expect_error(link(dplyr::select(dat1, -time), dat2, offset_before = 1800),
               "column 'time' must be present in both x and y")
  expect_error(link(dat1, dplyr::select(dat2, -time), offset_before = 1800),
               "column 'time' must be present in both x and y")

  # Bug #6: Test whether original_time is present in all nested data columns
  # Create some data to use
  dat1 <- data.frame(
    time = c(rep(seq.POSIXt(as.POSIXct("2021-11-14 13:00:00"), by = "1 hour", length.out = 3), 2),
             as.POSIXct("2021-11-14 13:00:00"), as.POSIXct("2021-11-14 13:00:00")),
    participant_id = c(rep("12345", 3), rep("23456", 3), "45678", "56789"),
    item_one = c(rep(c(40, 50, 60), 2), 40, 40)
  )

  dat2 <- data.frame(
    time = c(rep(seq.POSIXt(as.POSIXct("2021-11-14 12:50:00"), by = "5 min", length.out = 30), 2),
             seq.POSIXt(as.POSIXct("2021-11-14 12:30:00"), by = "5 min", length.out = 6)),
    participant_id = c(rep("12345", 30), rep("23456", 30), rep("45678", 6)),
    x = c(rep(1:30, 2), 1:6)
  )

  # Link together, make sure to include rows before and after
  res <- link(x = dat1,
              y = dat2,
              by = "participant_id",
              offset_before = 1800,
              add_before = TRUE,
              add_after = TRUE)

  # Use the results, e.g. to filter out the extra added rows by offset_before and offset_after
  # (as you have to do for the number of screen unlocks).
  expect_true(all(purrr::map_lgl(res$data, ~ "original_time" %in% colnames(.x))))


})

## link_db ===============
test_that("link_db", {
  path <- system.file("testdata", package = "mpathsenser")
  db <- open_db(path, "test.db")
  dat1 <- data.frame(
    time = c(as.POSIXct("2021-11-14 13:00:00"), as.POSIXct("2021-11-14 14:00:00"),
             as.POSIXct("2021-11-14 15:00:00")),
    participant_id = "12345",
    item_one = c(40, 50, 60)
  )

  # Check basic functionality
  res <- link_db(db, "Activity", "Connectivity", offset_after = 1800)
  true <- tibble::tibble(
    measurement_id = c("fbf85cd7-6d37-53a8-5c44-ad8fe13ef7ac",
                       "ef96364c-d1f4-5f73-ce40-277f078e3d0f",
                       "5ba54e77-4bcf-c8d1-17ff-71b9ed908897"),
    participant_id = "12345",
    time = as.POSIXct(c("2021-11-14 13:59:59", "2021-11-14 14:00:00", "2021-11-14 14:00:01")),
    confidence = c(NA, 100L, 99L),
    type = c(NA, "WALKING", "STILL"),
    data = list(
      tibble::tibble(
        measurement_id = c("27a5777a-ec41-80de-afa4-d2e7f6b02fcf",
                           "2d430c2a-5b16-1dce-0e2f-c049c44e3729"),
        time = as.POSIXct(c("2021-11-14 14:00:00", "2021-11-14 14:01:00")),
        connectivity_status = c("wifi", NA)
      ),
      tibble::tibble(
        measurement_id = c("27a5777a-ec41-80de-afa4-d2e7f6b02fcf",
                           "2d430c2a-5b16-1dce-0e2f-c049c44e3729"),
        time = as.POSIXct(c("2021-11-14 14:00:00", "2021-11-14 14:01:00")),
        connectivity_status = c("wifi", NA)
      ),
      tibble::tibble(
        measurement_id = "2d430c2a-5b16-1dce-0e2f-c049c44e3729",
        time = as.POSIXct("2021-11-14 14:01:00"),
        connectivity_status = NA_character_
      )
    )
  )
  expect_equal(res, true)

  # Check reverse
  res <- link_db(db, "Activity", "Connectivity", offset_after = 1800, reverse = TRUE)
  true <- tibble::tibble(
    measurement_id = c("27a5777a-ec41-80de-afa4-d2e7f6b02fcf",
                       "2d430c2a-5b16-1dce-0e2f-c049c44e3729"),
    participant_id = "12345",
    time = as.POSIXct(c("2021-11-14 14:00:00", "2021-11-14 14:01:00")),
    connectivity_status = c("wifi", NA),
    data = list(
      tibble::tibble(
        measurement_id = c("ef96364c-d1f4-5f73-ce40-277f078e3d0f",
                           "5ba54e77-4bcf-c8d1-17ff-71b9ed908897"),
        time = as.POSIXct(c("2021-11-14 14:00:00", "2021-11-14 14:00:01")),
        confidence = c(100L, 99L),
        type = c("WALKING", "STILL"),
      ),
      tibble::tibble(
        measurement_id = character(0),
        time = structure(numeric(0), tzone = "", class = c("POSIXct", "POSIXt")),
        confidence = integer(0),
        type = character(0)
      )
    )
  )
  expect_equal(res, true)

  # Check with external data
  res <- link_db(db, "Activity", external = dat1, offset_after = 1800)
  true <- tibble::tibble(
    dat1,
    data = list(
      tibble::tibble(measurement_id = character(0),
                     time = structure(numeric(0), tzone = "", class = c("POSIXct", "POSIXt")),
                     confidence = integer(0L),
                     type = character(0)),
      tibble::tibble(measurement_id = c("ef96364c-d1f4-5f73-ce40-277f078e3d0f",
                                        "5ba54e77-4bcf-c8d1-17ff-71b9ed908897"),
                     time = as.POSIXct(c("2021-11-14 14:00:00", "2021-11-14 14:00:01")),
                     confidence = c(100L, 99L),
                     type = c("WALKING", "STILL")),
      tibble::tibble(measurement_id = character(0),
                     time = structure(numeric(0), tzone = "", class = c("POSIXct", "POSIXt")),
                     confidence = integer(0),
                     type = character(0))
    )
  )
  expect_equal(res, true)

  expect_error(link_db(db, "Activity", 1:10, offset_before = 1800),
               "sensor_two must be a character vector")
  expect_error(link_db(db, "Activity", offset_before = 1800, external = "Bluetooth"),
               "external must be a data frame")
  expect_error(link_db(db, "Activity", "Bluetooth", offset_before = 1800, external = dat1),
               "only a second sensor or an external data frame can be supplied, but not both")
  expect_error(link_db(db, "Activity", offset_before = 1800),
               "either a second sensor or an external data frame must be supplied")
  DBI::dbDisconnect(db)
  expect_error(link_db(db, "Activity", "Bluetooth", offset_before = 1800),
               "Database connection is not valid")

  # Check if ignore_large works
  filename <- tempfile("big", fileext = ".db")
  db <- create_db(NULL, filename)

  # Populate database
  add_study(db, data.frame(study_id = "test-study", data_format = "CARP"))
  add_participant(db, data.frame(study_id = "test-study", participant_id = "12345"))

  sens_value <- seq.int(0, 10, length.out = 50001)
  time_value <- seq.POSIXt(as.POSIXct("2021-11-14 14:00:00.000", format = "%F %H:%M:%OS"),
                           by = "sec",
                           length.out = 50001)
  acc <- data.frame(
    measurement_id = paste0("id_", 1:50001),
    participant_id = "12345",
    date = "2021-11-14",
    time = strftime(time_value, format = "%H:%M:%OS3"),
    x = sens_value,
    y = sens_value,
    z = sens_value
  )

  DBI::dbWriteTable(db, "Accelerometer", acc, overwrite = TRUE)
  DBI::dbWriteTable(db, "Gyroscope", acc, overwrite = TRUE)

  expect_error(
    link_db(db, "Accelerometer", "Gyroscope", offset_after = 30),
    "the total number of rows is higher than 100000. Use ignore_large = TRUE to continue")
  expect_error(link_db(db, "Accelerometer", "Gyroscope", offset_after = 30, ignore_large = TRUE),
               "x and y are identical")

  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(filename)
})


## get_installed_apps ===============
test_that("get_installed_apps", {
  db <- open_db(system.file("testdata", package = "mpathsenser"), "test.db")
  res <- get_installed_apps(db, "12345")
  true <- tibble::tibble(app = c("BBC News",
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
                                 "m-Path Sense"))
  expect_equal(res, true)
  DBI::dbDisconnect(db)
})
