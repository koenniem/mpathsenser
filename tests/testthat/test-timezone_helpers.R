test_that("add_timezones_to_db aborts if Timezone table is missing", {
  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(
    db,
    "Accelerometer",
    data.frame(
      measurement_id = 1:2,
      participant_id = 1,
      date = c("2024-01-01", "2024-01-01"),
      time = c("10:00:00", "11:00:00")
    )
  )

  expect_error(
    add_timezones_to_db(db),
    "The table `Timezone` does not exist in the database."
  )

  DBI::dbDisconnect(db)
})


test_that("add_timezones_to_db adds timezone column correctly", {
  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  # Create a simple timezone table
  DBI::dbWriteTable(
    db,
    "Timezone",
    data.frame(
      participant_id = 1,
      date = c("2024-01-01", "2024-01-02"),
      time = c("00:00:00", "00:00:00"),
      timezone = c("Europe/Brussels", "America/New_York")
    )
  )

  # Create a mock sensor table
  DBI::dbWriteTable(
    db,
    "Accelerometer",
    data.frame(
      measurement_id = 1:4,
      participant_id = 1,
      date = c(rep("2024-01-01", 3), "2024-01-02"),
      time = c("00:30:00", "12:00:00", "23:59:59", "00:00:01"),
      stringsAsFactors = FALSE
    )
  )

  add_timezones_to_db(db, sensors = "Accelerometer", .progress = FALSE)

  result <- dplyr::tbl(db, "Accelerometer") |> dplyr::collect()

  expect_true("timezone" %in% names(result))
  expect_equal(unique(result$timezone), c("Europe/Brussels", "America/New_York"))
  expect_true(all(!is.na(result$timezone)))

  DBI::dbDisconnect(db)
})


test_that("add_timezones_to_db handles multiple participants independently", {
  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  tz <- data.frame(
    participant_id = c(1, 1, 2, 2),
    date = c("2024-01-01", "2024-01-02", "2024-01-01", "2024-01-03"),
    time = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    timezone = c("Europe/Brussels", "America/New_York", "Asia/Tokyo", "Europe/London")
  )
  DBI::dbWriteTable(db, "Timezone", tz)

  accel <- data.frame(
    measurement_id = 1:4,
    participant_id = c(1, 1, 2, 2),
    date = c("2024-01-01", "2024-01-02", "2024-01-01", "2024-01-03"),
    time = c("12:00:00", "12:00:00", "12:00:00", "12:00:00")
  )
  DBI::dbWriteTable(db, "Accelerometer", accel)

  add_timezones_to_db(db, sensors = "Accelerometer", .progress = FALSE)
  result <- dplyr::tbl(db, "Accelerometer") |> dplyr::collect()

  res1 <- result[result$participant_id == 1, "timezone", drop = TRUE]
  res2 <- result[result$participant_id == 2, "timezone", drop = TRUE]

  expect_setequal(res1, c("Europe/Brussels", "America/New_York"))
  expect_setequal(res2, c("Asia/Tokyo", "Europe/London"))

  DBI::dbDisconnect(db)
})

test_that("add_timezones_to_db handles measurements before and after known timezone intervals", {
  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(
    db,
    "Timezone",
    data.frame(
      participant_id = 1,
      date = "2024-01-02",
      time = "00:00:00",
      timezone = "Europe/Brussels"
    )
  )

  DBI::dbWriteTable(
    db,
    "Light",
    data.frame(
      measurement_id = 1:3,
      participant_id = 1,
      date = c("2024-01-01", "2024-01-02", "2024-01-03"),
      time = c("12:00:00", "12:00:00", "12:00:00")
    )
  )

  add_timezones_to_db(db, sensors = "Light", .progress = FALSE)
  result <- dplyr::tbl(db, "Light") |> dplyr::collect()

  # All should have a timezone (first one uses implicit earliest, last uses infinite end)
  expect_true(all(!is.na(result$timezone)))
  expect_true(all(result$timezone == "Europe/Brussels"))

  DBI::dbDisconnect(db)
})

test_that("add_timezones_to_db works for empty tables", {
  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(
    db,
    "Timezone",
    data.frame(
      participant_id = 1,
      date = "2024-01-01",
      time = "00:00:00",
      timezone = "Europe/Brussels"
    )
  )
  DBI::dbExecute(
    db,
    "CREATE TABLE Gyroscope (measurement_id TEXT, participant_id TEXT, date TEXT, time TEXT)"
  ) # empty table
  expect_silent(add_timezones_to_db(db, sensors = "Gyroscope", .progress = FALSE))
  DBI::dbDisconnect(db)
})

test_that("add_timezones_to_db skips already completed tables", {
  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(
    db,
    "Timezone",
    data.frame(
      participant_id = 1,
      date = "2024-01-01",
      time = "00:00:00",
      timezone = "Europe/Brussels"
    )
  )

  DBI::dbWriteTable(
    db,
    "Accelerometer",
    data.frame(
      measurement_id = 1:2,
      participant_id = 1,
      date = c("2024-01-01", "2024-01-01"),
      time = c("10:00:00", "11:00:00"),
      timezone = c("Europe/Brussels", "Europe/Brussels")
    )
  )

  expect_silent(add_timezones_to_db(db, sensors = "Accelerometer", .progress = FALSE))

  DBI::dbDisconnect(db)
})


test_that("add_timezones_to_db removes temporary timezone table afterward", {
  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(
    db,
    "Timezone",
    data.frame(
      participant_id = 1,
      date = "2024-01-01",
      time = "00:00:00",
      timezone = "Europe/Brussels"
    )
  )

  DBI::dbWriteTable(
    db,
    "Light",
    data.frame(
      measurement_id = 1:2,
      participant_id = 1,
      date = c("2024-01-01", "2024-01-01"),
      time = c("01:00:00", "02:00:00")
    )
  )

  add_timezones_to_db(db, sensors = "Light", .progress = FALSE)

  tables <- DBI::dbListTables(db)
  expect_false("temp_tzs" %in% tables)

  DBI::dbDisconnect(db)
})

test_that("add_timezones_to_db overwrites existing timezone values", {
  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(
    db,
    "Timezone",
    data.frame(
      participant_id = 1,
      date = "2024-01-01",
      time = "00:00:00",
      timezone = "Europe/Brussels"
    )
  )

  DBI::dbWriteTable(
    db,
    "Accelerometer",
    data.frame(
      measurement_id = 1:3,
      participant_id = 1,
      date = rep("2024-01-01", 3),
      time = c("00:10:00", "01:00:00", "02:00:00"),
      timezone = c(NA, "America/New_York", NA)
    )
  )

  add_timezones_to_db(db, sensors = "Accelerometer", .progress = FALSE)

  result <- dplyr::tbl(db, "Accelerometer") |> dplyr::collect()

  # The non-missing timezone should be overwritten
  expect_equal(unique(result$timezone), "Europe/Brussels")

  DBI::dbDisconnect(db)
})

test_that("add_timezones_to_db correctly handles sensors with end_time column", {
  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(
    db,
    "Timezone",
    data.frame(
      participant_id = 1,
      date = c("2024-01-01", "2024-01-02"),
      time = c("00:00:00", "00:00:00"),
      timezone = c("Europe/Brussels", "America/New_York")
    )
  )

  DBI::dbWriteTable(
    db,
    "Light",
    data.frame(
      measurement_id = 1:3,
      participant_id = 1,
      date = c("2024-01-01", "2024-01-01", "2024-01-02"),
      time = c("00:10:00", "23:30:00", "00:10:00"),
      end_time = c("2024-01-01 00:30:00", "2024-01-02 00:10:00", "2024-01-02 00:20:00")
    )
  )

  add_timezones_to_db(db, sensors = "Light", .progress = FALSE)

  result <- dplyr::tbl(db, "Light") |> dplyr::collect()

  # The first two observations overlap with the first timezone,
  # and the last with the second.
  expect_equal(result$timezone, c("Europe/Brussels", "Europe/Brussels", "America/New_York"))

  DBI::dbDisconnect(db)
})

test_that("with_localtime handles single timezone correctly (shifts to local then forces UTC)", {
  x <- as.POSIXct("2025-05-10 12:00:00", tz = "UTC")
  result <- with_localtime(x, "Europe/Brussels")

  # If the stored instant 12:00 UTC actually happened in Brussels (UTC+2 in May),
  # the true local time is 14:00 and that's what should be returned (but marked as UTC).
  expect_equal(format(result, tz = "UTC"), "2025-05-10 14:00:00")
  expect_equal(attr(result, "tzone"), "UTC")
})

test_that("with_localtime handles multiple timezones correctly", {
  x <- as.POSIXct(c("2025-05-10 12:00:00", "2025-05-10 12:00:00"), tz = "UTC")
  tzs <- c("Europe/Brussels", "America/New_York")
  result <- with_localtime(x, tzs)

  expect_equal(length(result), 2L)
  # Brussels in May is UTC+2 -> local 14:00 -> should be returned as 14:00 UTC
  expect_equal(format(result[1], tz = "UTC"), "2025-05-10 14:00:00")
  # New York in May is UTC-4 -> local 08:00 -> should be returned as 08:00 UTC
  expect_equal(format(result[2], tz = "UTC"), "2025-05-10 08:00:00")
  expect_equal(attr(result, "tzone"), "UTC")
})

test_that("with_localtime accepts character timestamps", {
  x <- c("2025-05-10 12:00:00", "2025-05-10 12:00:00")
  tzs <- c("Europe/Brussels", "America/New_York")
  result <- with_localtime(x, tzs)

  expect_s3_class(result, "POSIXct")
  expect_equal(attr(result, "tzone"), "UTC")
  expect_equal(format(result[1], tz = "UTC"), "2025-05-10 14:00:00")
  expect_equal(format(result[2], tz = "UTC"), "2025-05-10 08:00:00")
})

test_that("with_localtime throws error for non-POSIX input", {
  expect_no_error(
    with_localtime(1:3, "UTC")
  )

  expect_no_error(
    with_localtime(logical(0), "UTC")
  )

  # Error in as.POSIXct
  expect_error(
    with_localtime("0", "UTC"),
    " must be a vector of class POSIXt or a character coercible"
  )

  expect_error(
    with_localtime(TRUE, "UTC"),
    " must be a vector of class POSIXt or a character coercible"
  )
})

test_that("with_localtime handles vector recycling for tz (single tz recycled)", {
  x <- as.POSIXct(c("2025-05-10 00:00:00", "2025-05-10 12:00:00"), tz = "UTC")
  result <- with_localtime(x, "Europe/Brussels") # single tz recycled
  expect_equal(attr(result, "tzone"), "UTC")
  # Brussels is ahead of UTC in May, so both resulting instants should be later than original UTC
  # instants
  expect_true(all(result > x))
})

test_that("with_localtime preserves NA values", {
  x <- as.POSIXct(c("2025-05-10 12:00:00", NA), tz = "UTC")
  tzs <- c("Europe/Brussels", "Europe/Brussels")
  result <- with_localtime(x, tzs)
  expect_true(is.na(result[2]))
})

test_that("with_localtime handles DST transition correctly (Europe/Brussels 2025-03-30)", {
  # Before the DST switch: 2025-03-30 00:30:00 UTC -> local 01:30 (CET) -> returned as 01:30 UTC
  x1 <- as.POSIXct("2025-03-30 00:30:00", tz = "UTC")
  res1 <- with_localtime(x1, "Europe/Brussels")
  expect_equal(format(res1, tz = "UTC"), "2025-03-30 01:30:00")

  # After the DST switch instant (01:00 UTC maps to 03:00 local): 2025-03-30 01:30:00 UTC
  # -> local 03:30 (CEST) -> returned as 03:30 UTC (i.e. effectively +2h shift vs original UTC).
  x2 <- as.POSIXct("2025-03-30 01:30:00", tz = "UTC")
  res2 <- with_localtime(x2, "Europe/Brussels")
  expect_equal(format(res2, tz = "UTC"), "2025-03-30 03:30:00")
})
