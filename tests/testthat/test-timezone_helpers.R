test_that("add_timezones_to_db aborts if Timezone table is missing", {
  opts <- options(mpathsenser.check_missing_sensors = FALSE)
  on.exit(options(opts), add = TRUE)

  db <- create_db(NULL, tempfile("tz_test", fileext = ".db"))

  # Drop the Timezone table to simulate data without timezone measurements
  DBI::dbExecute(db, "DROP TABLE Timezone")

  expect_error(
    add_timezones_to_db(db),
    "The table `Timezone` does not exist in the database."
  )

  close_db(db)
})

test_that("add_timezones_to_db adds timezone column correctly", {
  db <- create_db(NULL, tempfile("tz_test", fileext = ".db"))

  DBI::dbExecute(
    db,
    "INSERT INTO Timezone (participant_id, time, timezone, source_file_id) VALUES
     ('12345', '2021-11-14 13:00:00', 'Europe/Brussels', 1),
     ('12345', '2021-11-14 14:00:00', 'America/New_York', 1)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO Accelerometer (participant_id, time, source_file_id) VALUES
     ('12345', '2021-11-14 13:30:00', 1),
     ('12345', '2021-11-14 14:30:00', 1)"
  )

  add_timezones_to_db(db, sensors = "Accelerometer", .progress = FALSE)

  result <- DBI::dbGetQuery(db, "SELECT timezone FROM Accelerometer ORDER BY time")

  expect_equal(result$timezone, c("Europe/Brussels", "America/New_York"))
  expect_true(all(!is.na(result$timezone)))

  close_db(db)
})

test_that("add_timezones_to_db handles multiple participants independently", {
  db <- create_db(NULL, tempfile("tz_test", fileext = ".db"))

  DBI::dbExecute(
    db,
    "INSERT INTO Timezone (participant_id, time, timezone, source_file_id) VALUES
     ('1', '2024-01-01 00:00:00', 'Europe/Brussels', 1),
     ('1', '2024-01-02 00:00:00', 'America/New_York', 1),
     ('2', '2024-01-01 00:00:00', 'Asia/Tokyo', 1),
     ('2', '2024-01-03 00:00:00', 'Europe/London', 1)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO Accelerometer (participant_id, time, source_file_id) VALUES
     ('1', '2024-01-01 12:00:00', 1),
     ('1', '2024-01-02 12:00:00', 1),
     ('2', '2024-01-01 12:00:00', 1),
     ('2', '2024-01-03 12:00:00', 1)"
  )

  add_timezones_to_db(db, sensors = "Accelerometer", .progress = FALSE)
  result <- DBI::dbGetQuery(db, "SELECT * FROM Accelerometer")

  res1 <- result[result$participant_id == 1, "timezone", drop = TRUE]
  res2 <- result[result$participant_id == 2, "timezone", drop = TRUE]

  expect_setequal(res1, c("Europe/Brussels", "America/New_York"))
  expect_setequal(res2, c("Asia/Tokyo", "Europe/London"))

  close_db(db)
})

test_that("add_timezones_to_db handles travel and repeated DST instants", {
  db <- create_db(NULL, tempfile("tz_test", fileext = ".db"))

  DBI::dbExecute(
    db,
    "INSERT INTO Timezone (participant_id, time, timezone, source_file_id) VALUES
     ('1', '2025-01-01 00:00:00+00', 'Europe/Brussels', 1),
     ('1', '2025-02-01 00:00:00+00', 'America/New_York', 1),
     ('1', '2025-10-26 01:00:00+00', 'Europe/Brussels', 1)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO Activity (participant_id, time, source_file_id) VALUES
     ('1', '2025-01-15 12:00:00+00', 1),
     ('1', '2025-02-15 11:00:00+00', 1),
     ('1', '2025-10-26 00:30:00+00', 1),
     ('1', '2025-10-26 01:30:00+00', 1)"
  )

  add_timezones_to_db(db, sensors = "Activity", .progress = FALSE)
  result <- DBI::dbGetQuery(db, "SELECT time, timezone FROM main.Activity ORDER BY time")

  expect_equal(
    result$timezone,
    c(
      "Europe/Brussels",
      "America/New_York",
      "America/New_York",
      "Europe/Brussels"
    )
  )
  raw_fields <- DBI::dbGetQuery(db, "PRAGMA table_info('Activity')")
  expect_equal(raw_fields$type[raw_fields$name == "time"], "TIMESTAMP WITH TIME ZONE")
  local_fields <- DBI::dbGetQuery(db, "PRAGMA table_info('main.Activity_local')")
  expect_equal(local_fields$type[local_fields$name == "time"], "TIMESTAMP")

  close_db(db)
})

test_that("add_timezones_to_db handles measurements before and after known timezone intervals", {
  db <- create_db(NULL, tempfile("tz_test", fileext = ".db"))

  DBI::dbExecute(
    db,
    "INSERT INTO Timezone (participant_id, time, timezone, source_file_id) VALUES
     ('1', '2024-01-02 00:00:00', 'Europe/Brussels', 1)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO Light (participant_id, time, source_file_id) VALUES
     ('1', '2024-01-01 12:00:00', 1),
     ('1', '2024-01-02 12:00:00', 1),
     ('1', '2024-01-03 12:00:00', 1)"
  )

  add_timezones_to_db(db, sensors = "Light", .progress = FALSE)
  result <- DBI::dbGetQuery(db, "SELECT timezone FROM main.Light")

  # All should have a timezone (first one uses the earliest known, last one
  # the last known timezone)
  expect_true(all(!is.na(result$timezone)))
  expect_true(all(result$timezone == "Europe/Brussels"))

  close_db(db)
})

test_that("add_timezones_to_db works for empty tables", {
  db <- create_db(NULL, tempfile("tz_test", fileext = ".db"))

  DBI::dbExecute(
    db,
    "INSERT INTO Timezone (participant_id, time, timezone, source_file_id) VALUES
     ('1', '2024-01-01 00:00:00', 'Europe/Brussels', 1)"
  )

  expect_silent(add_timezones_to_db(db, sensors = "Pedometer", .progress = FALSE))

  close_db(db)
})

test_that("add_timezones_to_db removes temporary tables afterward", {
  db <- create_db(NULL, tempfile("tz_test", fileext = ".db"))

  DBI::dbExecute(
    db,
    "INSERT INTO Timezone (participant_id, time, timezone, source_file_id) VALUES
     ('1', '2024-01-01 00:00:00', 'Europe/Brussels', 1)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO Light (participant_id, time, source_file_id) VALUES
     ('1', '2024-01-01 01:00:00', 1),
     ('1', '2024-01-01 02:00:00', 1)"
  )

  add_timezones_to_db(db, sensors = "Light", .progress = FALSE)

  tables <- DBI::dbListTables(db)
  expect_false("temp_tz_intervals" %in% tables)

  close_db(db)
})


test_that("add_timezones_to_db preserves existing timezone values", {
  db <- create_db(NULL, tempfile("tz_test", fileext = ".db"))

  DBI::dbExecute(
    db,
    "INSERT INTO Timezone (participant_id, time, timezone, source_file_id) VALUES
     ('1', '2024-01-01 00:00:00', 'Europe/Brussels', 1)"
  )
  # Fresh schemas already carry the timezone column. A pre-existing timezone
  # must be preserved; only NULL cells are populated.
  DBI::dbExecute(
    db,
    "INSERT INTO Accelerometer (participant_id, time, timezone, source_file_id) VALUES
     ('1', '2024-01-01 00:10:00', NULL, 1),
     ('1', '2024-01-01 01:00:00', 'America/New_York', 1),
     ('1', '2024-01-01 02:00:00', NULL, 1)"
  )

  add_timezones_to_db(db, sensors = "Accelerometer", .progress = FALSE)

  result <- DBI::dbGetQuery(db, "SELECT timezone FROM main.Accelerometer")

  # The pre-existing 'America/New_York' must be kept, NULL cells filled with
  # the candidate timezone.
  expect_equal(result$timezone, c("Europe/Brussels", "America/New_York", "Europe/Brussels"))

  close_db(db)
})

test_that("coincident timezone events do not multiply or duplicate rows", {
  db <- create_db(NULL, tempfile("tz_test", fileext = ".db"))

  # Two timezone events at the same instant with different zones is an
  # (otherwise undefined) pathological case; the interval join must still
  # return one timezone per observation and not multiply sensor rows.
  DBI::dbExecute(
    db,
    "INSERT INTO Timezone (participant_id, time, timezone, source_file_id) VALUES
     ('1', '2024-06-01 00:00:00', 'Europe/Brussels', 1),
     ('1', '2024-06-01 00:00:00', 'America/New_York', 2)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO Activity (participant_id, time, source_file_id) VALUES
     ('1', '2024-06-01 00:00:00', 1)"
  )

  add_timezones_to_db(db, sensors = "Activity", .progress = FALSE)

  result <- DBI::dbGetQuery(db, "SELECT participant_id, time, timezone FROM main.Activity")
  expect_equal(nrow(result), 1L)
  expect_false(is.na(result$timezone[1]))

  close_db(db)
})

test_that("add_timezones_to_db is idempotent on an already-normalized table", {
  db <- create_db(NULL, tempfile("tz_test", fileext = ".db"))

  DBI::dbExecute(
    db,
    "INSERT INTO Timezone (participant_id, time, timezone, source_file_id) VALUES
     ('1', '2024-01-01 00:00:00', 'Europe/Brussels', 1)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO Pedometer (participant_id, time, source_file_id) VALUES
     ('1', '2024-01-01 01:00:00', 1)"
  )

  add_timezones_to_db(db, sensors = "Pedometer", .progress = FALSE)
  first <- DBI::dbGetQuery(db, "SELECT timezone FROM main.Pedometer")

  # A second run must leave the already-assigned timezone untouched.
  add_timezones_to_db(db, sensors = "Pedometer", .progress = FALSE)
  second <- DBI::dbGetQuery(db, "SELECT timezone FROM main.Pedometer")

  expect_equal(second, first)
  expect_equal(second$timezone, "Europe/Brussels")

  close_db(db)
})

test_that("canonical tables retain UTC and explicit local views expose local values", {
  db <- create_db(NULL, ":memory:")
  DBI::dbExecute(
    db,
    "INSERT INTO Timezone VALUES ('1', '2025-01-01 00:00:00+00', 'Europe/Brussels', 1)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO Activity (participant_id, time, source_file_id) VALUES ('1', '2025-01-15 11:00:00+00', 1)"
  )
  add_timezones_to_db(db, sensors = "Activity", .progress = FALSE)

  raw <- DBI::dbGetQuery(db, "SELECT time, timezone FROM Activity")
  view <- DBI::dbGetQuery(db, "SELECT time_local, timezone FROM main.Activity_with_local")
  expect_equal(format(raw$time, tz = "UTC"), "2025-01-15 11:00:00")
  expect_equal(view$time_local, as.POSIXct("2025-01-15 12:00:00", tz = "UTC"))
  expect_equal(view$timezone, "Europe/Brussels")
  expect_equal(
    DBI::dbGetQuery(db, "PRAGMA table_info('Activity')")$type[2],
    "TIMESTAMP WITH TIME ZONE"
  )
  expect_equal(
    DBI::dbGetQuery(db, "PRAGMA table_info('main.Activity')")$type[2],
    "TIMESTAMP WITH TIME ZONE"
  )

  DBI::dbExecute(
    db,
    "INSERT INTO Activity (participant_id, time, source_file_id) VALUES ('2', '2025-01-15 11:00:00+00', 1)"
  )
  unmapped <- DBI::dbGetQuery(db, "SELECT time FROM main.Activity WHERE participant_id = '2'")
  expect_equal(unmapped$time, as.POSIXct("2025-01-15 11:00:00", tz = "UTC"))
  close_db(db)
})


test_that("to_local_time handles a single timezone", {
  x <- as.POSIXct("2025-05-10 12:00:00", tz = "UTC")
  result <- to_local_time(x, "Europe/Brussels")

  # If the stored instant 12:00 UTC actually happened in Brussels (UTC+2 in May),
  # the true local time is 14:00 and that's what should be returned (but marked as UTC).
  expect_equal(format(result, tz = "UTC"), "2025-05-10 14:00:00")
  expect_equal(attr(result, "tzone"), "UTC")
})

test_that("to_local_time handles multiple timezones", {
  x <- as.POSIXct(c("2025-05-10 12:00:00", "2025-05-10 12:00:00"), tz = "UTC")
  tzs <- c("Europe/Brussels", "America/New_York")
  result <- to_local_time(x, tzs)

  expect_equal(length(result), 2L)
  # Brussels in May is UTC+2 -> local 14:00 -> should be returned as 14:00 UTC
  expect_equal(format(result[1], tz = "UTC"), "2025-05-10 14:00:00")
  # New York in May is UTC-4 -> local 08:00 -> should be returned as 08:00 UTC
  expect_equal(format(result[2], tz = "UTC"), "2025-05-10 08:00:00")
  expect_equal(attr(result, "tzone"), "UTC")
})

test_that("to_local_time accepts character timestamps", {
  x <- c("2025-05-10 12:00:00", "2025-05-10 12:00:00")
  tzs <- c("Europe/Brussels", "America/New_York")
  result <- to_local_time(x, tzs)

  expect_s3_class(result, "POSIXct")
  expect_equal(attr(result, "tzone"), "UTC")
  expect_equal(format(result[1], tz = "UTC"), "2025-05-10 14:00:00")
  expect_equal(format(result[2], tz = "UTC"), "2025-05-10 08:00:00")
})

test_that("to_local_time throws error for non-POSIX input", {
  expect_no_error(
    to_local_time(1:3, "UTC")
  )

  expect_no_error(
    to_local_time(logical(0), "UTC")
  )

  # Error in as.POSIXct
  expect_error(
    to_local_time("0", "UTC"),
    " must be a vector of class POSIXt or a character coercible"
  )

  expect_error(
    to_local_time(TRUE, "UTC"),
    " must be a vector of class POSIXt or a character coercible"
  )
})

test_that("to_local_time handles vector recycling for timezone", {
  x <- as.POSIXct(c("2025-05-10 00:00:00", "2025-05-10 12:00:00"), tz = "UTC")
  result <- to_local_time(x, "Europe/Brussels") # single tz recycled
  expect_equal(attr(result, "tzone"), "UTC")
  # Brussels is ahead of UTC in May, so both resulting instants should be later than original UTC
  # instants
  expect_true(all(result > x))
})

test_that("to_local_time preserves NA values", {
  x <- as.POSIXct(c("2025-05-10 12:00:00", NA), tz = "UTC")
  tzs <- c("Europe/Brussels", "Europe/Brussels")
  result <- to_local_time(x, tzs)
  expect_true(is.na(result[2]))
})

test_that("to_local_time preserves repeated autumn DST wall-clock times", {
  x <- as.POSIXct(c("2025-10-26 00:30:00", "2025-10-26 01:30:00"), tz = "UTC")
  result <- to_local_time(x, rep("Europe/Brussels", 2))

  expect_equal(format(result, tz = "UTC"), c("2025-10-26 02:30:00", "2025-10-26 02:30:00"))
  expect_equal(attr(result, "tzone"), "UTC")
})

test_that("to_local_time handles DST transition correctly (Europe/Brussels 2025-03-30)", {
  # Before the DST switch: 2025-03-30 00:30:00 UTC -> local 01:30 (CET) -> returned as 01:30 UTC
  x1 <- as.POSIXct("2025-03-30 00:30:00", tz = "UTC")
  res1 <- to_local_time(x1, "Europe/Brussels")
  expect_equal(format(res1, tz = "UTC"), "2025-03-30 01:30:00")

  # After the DST switch instant (01:00 UTC maps to 03:00 local): 2025-03-30 01:30:00 UTC
  # -> local 03:30 (CEST) -> returned as 03:30 UTC (i.e. effectively +2h shift vs original UTC).
  x2 <- as.POSIXct("2025-03-30 01:30:00", tz = "UTC")
  res2 <- to_local_time(x2, "Europe/Brussels")
  expect_equal(format(res2, tz = "UTC"), "2025-03-30 03:30:00")
})
