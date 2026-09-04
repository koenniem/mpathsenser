test_that("coverage", {
  db <- create_test_db()

  # Working cases
  expect_s3_class(coverage(db, "12345"), "tbl_df")
  expect_s3_class(coverage(db, "12345", sensor = c("Accelerometer", "Battery")), "tbl_df")
  expect_warning(
    coverage(db, "12345", plot = TRUE),
    "The `plot` argument of `coverage\\(\\)` is deprecated as of mpathsenser 1.1.1."
  )
  expect_s3_class(
    coverage(db, "12345", start_date = "2021-11-13", end_date = "2021-11-14"),
    "tbl_df"
  )

  # Sensors
  expect_error(
    coverage(db, "12345", sensor = "foo"),
    ".*Sensor `foo` could not be found\\..*"
  )

  # participant_id
  expect_error(coverage(db, "foo"), "\\\"foo\\\".* is not a known participant")

  # Frequency
  expect_error(
    coverage(db, "12345", frequency = c(1, 2, 3)),
    "`frequency` must be a named numeric vector"
  )
  expect_error(
    coverage(db, "12345", frequency = c(1, 2, 3), relative = FALSE),
    "`frequency` must be a named numeric vector"
  )
  tmp_freq <- freq
  names(tmp_freq) <- NULL
  expect_error(
    coverage(db, "12345", frequency = tmp_freq),
    "`frequency` must be a named numeric vector"
  )

  # start_date and end_date
  expect_error(
    coverage(db, "12345", start_date = 1, end_date = 2),
    "`start_date` and `end_date` must be `NULL`, a date string, or a .*<Date>"
  )
  expect_error(
    coverage(db, "12345", start_date = "foo", end_date = "bar"),
    "`start_date` and `end_date` must be `NULL`, a date string, or a .*<Date>"
  )

  # Offset
  expect_warning(
    coverage(db, "12345", start_date = "2021-11-14", end_date = "2021-11-14", offset = "1 day"),
    paste0(
      "Argument start_date/end_date and offset cannot be present at the same ",
      "time."
    )
  )
  expect_error(
    coverage(db, "12345", offset = "foo"),
    "`offset` must be .*\\\"None\\\".*, or a day specification like .*\\\"1 day\\\".*"
  )

  # No data present in that period
  expect_no_error(
    coverage(db, "12345", start_date = "2021-11-16", end_date = "2021-11-16")
  )

  # Cleanup
  dbDisconnect(db)
})

test_that("coverage returns correct values for relative and absolute", {
  tmp <- tempfile()
  db <- create_db(NULL, tmp)
  on.exit(
    {
      dbDisconnect(db)
      file.remove(tmp)
    },
    add = TRUE
  )

  add_study(db, "foo", NA)
  add_participant(db, "12345", "foo")

  # 2 days of data
  data <- data.frame(
    participant_id = rep("12345", 8),
    time = as.POSIXct(
      c(
        "2024-01-01 10:00:01",
        "2024-01-01 10:00:02",
        "2024-01-01 10:00:03", # Day 1: 3 in hour 10
        "2024-01-01 11:00:00", # Day 1: 1 in hour 11
        "2024-01-02 10:00:01", # Day 2: 1 in hour 10
        "2024-01-02 12:00:01",
        "2024-01-02 12:00:02",
        "2024-01-02 12:00:03" # Day 2: 3 in hour 12
      ),
      tz = "UTC"
    ),
    timezone = NA_character_,
    source_file_id = 1
  )
  DBI::dbExecute(db, "DELETE FROM Accelerometer")
  DBI::dbWriteTable(
    db,
    DBI::Id(schema = "main", table = "Accelerometer"),
    data,
    append = TRUE
  )

  res_abs <- coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    relative = FALSE
  )

  # Mean for hour 10: (3 + 1) / 2 = 2
  # Mean for hour 11: (1 + 0) / 2 = 0.5
  # Mean for hour 12: (0 + 3) / 2 = 1.5
  # All other hours: 0
  expect_equal(res_abs$coverage[res_abs$hour == 10], 2)
  expect_equal(res_abs$coverage[res_abs$hour == 11], 0.5)
  expect_equal(res_abs$coverage[res_abs$hour == 12], 1.5)
  expect_equal(res_abs$coverage[res_abs$hour == 9], 0)

  # Ensure all 24 hours are present
  expect_equal(nrow(res_abs), 24)

  res_rel <- coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    relative = TRUE
  )

  # frequency for Accelerometer is 720
  expect_equal(res_rel$coverage[res_rel$hour == 10], round(2 / 720, 2))
  expect_equal(res_rel$coverage[res_rel$hour == 11], round(0.5 / 720, 2))
  expect_equal(res_rel$coverage[res_rel$hour == 12], round(1.5 / 720, 2))
  expect_equal(res_rel$coverage[res_rel$hour == 9], 0)
  expect_equal(nrow(res_rel), 24)
})

test_that("coverage filters Heartbeat on Secondary Phone ignores other devices", {
  tmp <- tempfile()
  db <- create_db(NULL, tmp)
  on.exit(
    {
      dbDisconnect(db)
      file.remove(tmp)
    },
    add = TRUE
  )

  add_study(db, "foo", NA)
  add_participant(db, "12345", "foo")

  data <- data.frame(
    participant_id = rep("12345", 3),
    time = as.POSIXct(
      c("2024-01-01 10:00:01", "2024-01-01 10:00:02", "2024-01-01 10:00:03"),
      tz = "UTC"
    ),
    device_role_name = c("Primary Phone", "Secondary Phone", "Primary Watch"),
    timezone = NA_character_,
    source_file_id = 1
  )
  DBI::dbExecute(db, "DELETE FROM Heartbeat")
  DBI::dbWriteTable(
    db,
    DBI::Id(schema = "main", table = "Heartbeat"),
    data,
    append = TRUE
  )

  res <- coverage(
    db,
    "12345",
    sensor = "Heartbeat",
    relative = FALSE,
    frequency = c(Heartbeat = 60)
  )

  # 2 of 3 measurements should remain in hour 10 (Primary Phone and Primary Watch)
  expect_equal(res$coverage[res$hour == 10], 2)
})

test_that("plot.mpathsenser_coverage", {
  db <- create_test_db()

  # Working cases
  expect_s3_class(plot(coverage(db, "12345")), "ggplot")
  expect_s3_class(plot(coverage(db, "12345", relative = FALSE)), "ggplot")

  # Cleanup
  cleanup_test_db(db)
})

test_that("freq", {
  expect_vector(freq, ptype = numeric(), 18)
})
