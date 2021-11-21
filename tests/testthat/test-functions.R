# Tests for functions.R

test_that("import", {
  path <- system.file("testdata", package = "CARP")
  db <- create_db(path, "test.db", overwrite = TRUE)

  expect_message(import(
    path = path,
    db = db,
    recursive = FALSE
  ), "All files were successfully written to the database.")

  if (DBI::dbIsValid(db)) {
    DBI::dbDisconnect(db)
  }
})

test_that("coverage", {
  path <- system.file("testdata", package = "CARP")
  db <- open_db(path, db_name = "test.db")

  # Working cases
  expect_s3_class(coverage(db, "27624"), "ggplot")
  expect_s3_class(coverage(db, "27624", sensor = c("Accelerometer", "Gyroscope")), "ggplot")
  expect_s3_class(coverage(db, "27624", plot = FALSE), "data.frame")
  expect_s3_class(coverage(db, "27624", start_date = "2021-02-25", end_date = "2021-02-25"), "ggplot")

  # Database
  db2 <- "foo"
  expect_error(coverage(db2, "27624"), "Argument db is not a database connection.")
  db2 <- DBI::dbConnect(RSQLite::SQLite(), "")
  DBI::dbDisconnect(db2)
  expect_error(coverage(db2, "27624"), "Database is invalid.")

  # Sensors
  expect_error(coverage(db, "27624", sensor = "foo"), "Sensor\\(s\\) foo not found.")

  # participant_id
  expect_error(coverage(db, "12345"), "Participant_id not known.")
  expect_error(coverage(db, 12345), "participant_id must be a character string")
  expect_error(coverage(db, c("27624", "12345")), "Only 1 participant per coverage chart allowed")

  # Frequency
  expect_error(coverage(db, "27624", frequency = c(1,2,3)), "Frequency must be a named numeric vector")
  expect_error(coverage(db, "27624", frequency = c(1,2,3), relative = FALSE), "Frequency must be a named numeric vector")
  tmp_freq <- freq
  names(tmp_freq) <- NULL
  expect_error(coverage(db, "27624", frequency = tmp_freq), "Frequency must be a named numeric vector")

  # start_date and end_date
  expect_error(coverage(db, "27624", start_date = 1, end_date = 2),
               "start_date and end_date must be NULL, a character string, or date.")
  expect_error(coverage(db, "27624", start_date = "foo", end_date = "bar"),
               "start_date and end_date must be NULL, a character string, or date.")

  # Offset
  expect_warning(coverage(db, "27624", start_date = "2021-02-25", end_date = "2021-02-25", offset = "1 day"),
                 "Argument start_date/end_date and offset cannot be present at the same time. Ignoring the offset argument.")

  # Cleanup
  RSQLite::dbDisconnect(db)
})

test_that("freq", {
  expect_vector(freq, ptype = numeric(), 11)
})
