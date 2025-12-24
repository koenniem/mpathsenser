test_that("coverage", {
  path <- system.file("testdata", package = "mpathsenser")
  db <- open_db(path, db_name = "test.db")

  # Working cases
  expect_s3_class(coverage(db, "12345"), "tbl_df")
  expect_s3_class(coverage(db, "12345", sensor = c("Accelerometer", "Gyroscope")), "tbl_df")
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
  expect_error(coverage(db, "-1"), "Participant_id not known.")

  # Frequency
  expect_error(
    coverage(db, "12345", frequency = c(1, 2, 3)),
    "Frequency must be a named numeric vector"
  )
  expect_error(
    coverage(db, "12345", frequency = c(1, 2, 3), relative = FALSE),
    "Frequency must be a named numeric vector"
  )
  tmp_freq <- freq
  names(tmp_freq) <- NULL
  expect_error(
    coverage(db, "12345", frequency = tmp_freq),
    "Frequency must be a named numeric vector"
  )

  # start_date and end_date
  expect_error(
    coverage(db, "12345", start_date = 1, end_date = 2),
    "start_date and end_date must be NULL, a character string, or date."
  )
  expect_error(
    coverage(db, "12345", start_date = "foo", end_date = "bar"),
    "start_date and end_date must be NULL, a character string, or date."
  )

  # Offset
  expect_warning(
    coverage(db, "12345", start_date = "2021-02-25", end_date = "2021-02-25", offset = "1 day"),
    paste0(
      "Argument start_date/end_date and offset cannot be present at the same ",
      "time."
    )
  )
  expect_error(
    coverage(db, "12345", offset = "foo"),
    "Argument offset must be either \\'None\\', 1 day, or 2, 3, 4, \\.\\.\\. days\\."
  )

  # Cleanup
  dbDisconnect(db)

  # Check sensors that do not exist in the database, but is a valid sensor name
  tmp <- tempfile()
  file.copy(system.file("testdata", "test.db", package = "mpathsenser"), tmp)
  db <- open_db(NULL, tmp)
  DBI::dbRemoveTable(db, "Accelerometer")
  expect_warning(
    coverage(db, "12345", sensor = c("Accelerometer", "Gyroscope")),
    paste0(
      "This sensor does not exist in the database and will be removed from the output: ",
      "`Accelerometer`."
    )
  )
  close_db(db)
  file.remove(tmp)
})

test_that("coverage takes submeasurements without measurement_id into account", {
  # Copy the test database to a temporary path
  tmp <- tempfile()
  db <- create_db(NULL, tmp)

  # Populate the database
  add_study(db, "foo", NA)
  add_participant(db, "12345", "foo")

  data <- data.frame(
    measurement_id = c("a_1", "a_2", "b"),
    participant_id = "12345",
    date = "2025-19-12",
    time = c("14:58:00", "14:58:01", "14:59:00")
  )
  DBI::dbWriteTable(db, "Accelerometer", data, overwrite = TRUE)

  with_ids <- coverage(db, "12345", relative = FALSE)

  # Now remove the IDs
  data$measurement_id <- NULL
  DBI::dbWriteTable(db, "Accelerometer", data, overwrite = TRUE)
  without_ids <- coverage(db, "12345", relative = FALSE)

  # The count should not depend on the measurement_id
  expect_equal(with_ids, without_ids)

  # Cleanup
  dbDisconnect(db)
  file.remove(tmp)
})

test_that("plot.mpathsenser_coverage", {
  path <- system.file("testdata", package = "mpathsenser")
  db <- open_db(path, db_name = "test.db")

  # Working cases
  expect_s3_class(plot(coverage(db, "12345")), "ggplot")
  expect_s3_class(plot(coverage(db, "12345", relative = FALSE)), "ggplot")

  # Cleanup
  dbDisconnect(db)
})

test_that("freq", {
  expect_vector(freq, ptype = numeric(), 11)
})
