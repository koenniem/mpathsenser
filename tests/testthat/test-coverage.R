# Test fixtures ----------------------------------------------------------------

# Create a fresh coverage test database with the given participant ids.
coverage_db <- function(participants = "12345") {
  tmp <- tempfile(fileext = ".duckdb")
  db <- create_db(NULL, tmp, shared_home = FALSE)
  DBI::dbExecute(
    db,
    "INSERT INTO Study (study_id, data_format) VALUES ('foo', NULL)"
  )
  for (p in participants) {
    DBI::dbExecute(
      db,
      sprintf(
        "INSERT INTO Participant (participant_id, study_id) VALUES (%s, 'foo')",
        as.character(p)
      )
    )
  }
  db
}

# Insert measurements into a raw sensor table. `timezone` and extra columns
# are optional.
insert_measurements <- function(db, sensor, participant_id, time, ...) {
  n <- length(time)
  extra <- list(...)
  data <- data.frame(
    participant_id = rep(participant_id, length.out = n),
    time = as.POSIXct(time, tz = "UTC"),
    timezone = NA_character_,
    source_file_id = 1,
    source_row_id = seq_len(n),
    source_measurement_id = 1,
    stringsAsFactors = FALSE
  )
  for (nm in names(extra)) {
    data[[nm]] <- extra[[nm]]
  }
  DBI::dbWriteTable(
    db,
    DBI::Id(schema = "raw", table = sensor),
    data,
    append = TRUE
  )
  invisible(db)
}

# A fixture with the classic two-day spread: 3 + 1 observations on day 1 and
# 1 + 3 on day 2.
insert_two_day_accelerometer <- function(db, participant_id = "12345") {
  insert_measurements(
    db,
    "Accelerometer",
    participant_id,
    c(
      "2024-01-01 10:00:01",
      "2024-01-01 10:00:02",
      "2024-01-01 10:00:03",
      "2024-01-01 11:00:00",
      "2024-01-02 10:00:01",
      "2024-01-02 12:00:01",
      "2024-01-02 12:00:02",
      "2024-01-02 12:00:03"
    )
  )
}

# Tests ------------------------------------------------------------------------

test_that("coverage validates its input", {
  db <- coverage_db()
  on.exit(
    {
      dbDisconnect(db)
      file.remove(db@driver@dbdir)
    },
    add = TRUE
  )

  expect_error(
    coverage(db, "foo"),
    "could not be found"
  )
  expect_error(
    coverage(db, "12345", sensor = "foo"),
    ".*Sensor `foo` could not be found\\..*"
  )
  expect_error(
    coverage(db, "12345", by = "fortnight"),
    "should be one of"
  )
  expect_error(
    coverage(db, "12345", cycle = "fortnight"),
    "should be one of"
  )
  expect_error(
    coverage(db, "12345", by = "day", cycle = "day"),
    "must be coarser than"
  )
  expect_error(
    coverage(db, "12345", week_start = 8),
    "must be a whole number between 1"
  )
  expect_error(
    coverage(db, "12345", expected = c(1, 2, 3)),
    "must be a named numeric vector"
  )
  expect_error(
    coverage(db, "12345", expected = c(Accelerometer = -1)),
    "positive, finite intervals"
  )
  expect_error(coverage(db, "12345", frequency = c(Accelerometer = 720)))
  expect_error(coverage(db, "12345", relative = FALSE))
  expect_error(coverage(db, "12345", offset = "1 day"))
  expect_error(
    coverage(db, "12345", start_date = 1, end_date = 2),
    "must be `NULL`, a date string, or a .*<Date>"
  )
})

test_that("coverage returns per-participant hour-of-day profiles", {
  db <- coverage_db()
  on.exit(
    {
      dbDisconnect(db)
      file.remove(db@driver@dbdir)
    },
    add = TRUE
  )
  insert_two_day_accelerometer(db)

  res <- collect(coverage(db, "12345", sensor = "Accelerometer"))

  expect_s3_class(res, "coverage")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 24)
  expect_equal(res$participant_id, rep(12345, 24))
  expect_true(is.factor(res$measure))
  expect_equal(as.character(unique(res$measure)), "Accelerometer")

  # Absolute counts averaged per hour of day: hour 10 = (3 + 1) / 2,
  # hour 11 = (1 + 0) / 2, hour 12 = (0 + 3) / 2.
  expect_equal(res$coverage[res$hour == 10], 2)
  expect_equal(res$coverage[res$hour == 11], 0.5)
  expect_equal(res$coverage[res$hour == 12], 1.5)
  expect_equal(res$coverage[res$hour == 9], 0)

  # Relative coverage, expected = one sample per 5 seconds.
  rel <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = coverage_frequency(Accelerometer = 5)
  ))

  expect_equal(rel$coverage[rel$hour == 12], 1.5)
  expect_equal(rel$coverage[rel$hour == 10], 0)
  expect_equal(nrow(rel), 24)
})

test_that("coverage zero-fills missing days inside the participant span", {
  db <- coverage_db()
  on.exit(
    {
      dbDisconnect(db)
      file.remove(db@driver@dbdir)
    },
    add = TRUE
  )
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c("2024-01-01 09:00:00", "2024-01-03 09:00:00")
  )

  res <- collect(coverage(db, "12345", sensor = "Accelerometer"))

  # Span is 2024-01-01 09:00 to 2024-01-03 09:00: hour 9 occurs on three
  # days with counts 1, 0, 1 -> 2/3.
  expect_equal(res$coverage[res$hour == 9], 0.67)
  expect_equal(res$coverage[res$hour == 8], 0)
  expect_equal(nrow(res), 24)
})

test_that("coverage spans are per participant", {
  db <- coverage_db(c("1", "2"))
  on.exit(
    {
      dbDisconnect(db)
      file.remove(db@driver@dbdir)
    },
    add = TRUE
  )
  insert_measurements(
    db,
    "Accelerometer",
    "1",
    c("2024-01-01 08:00:00", "2024-01-01 09:00:00")
  )
  insert_measurements(
    db,
    "Accelerometer",
    "2",
    "2024-01-03 08:00:00"
  )

  res <- collect(coverage(db, sensor = "Accelerometer"))

  expect_equal(sort(unique(res$participant_id)), c(1, 2))
  expect_equal(nrow(res), 3)
  expect_equal(res$coverage[res$participant_id == 1], c(1, 1))
  expect_equal(res$hour[res$participant_id == 1], c(8L, 9L))
  expect_equal(res$coverage[res$participant_id == 2], 1)
  expect_equal(res$hour[res$participant_id == 2], 8L)
})

test_that("coverage prorates the first and last partial bins", {
  db <- coverage_db()
  on.exit(
    {
      dbDisconnect(db)
      file.remove(db@driver@dbdir)
    },
    add = TRUE
  )
  # 360 samples every 5 seconds from 12:30:00 to 12:59:55.
  times <- as.POSIXct("2024-01-01 12:30:00", tz = "UTC") + seq(0, 1795, by = 5)
  insert_measurements(db, "Accelerometer", "12345", times)

  res <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = coverage_frequency(Accelerometer = 5)
  ))

  # Covered window 12:30:00-12:59:55 = 1795 s -> 359 expected samples for
  # 360 observations.
  expect_equal(nrow(res), 1)
  expect_equal(res$hour, 12L)
  expect_equal(res$coverage, 1)

  # Absolute coverage is not prorated.
  abs_res <- collect(coverage(db, "12345", sensor = "Accelerometer"))
  expect_equal(abs_res$coverage, 360)

  # A single-instant span floors the expected count at one interval.
  db2 <- coverage_db()
  on.exit(
    {
      dbDisconnect(db2)
      file.remove(db2@driver@dbdir)
    },
    add = TRUE
  )
  insert_measurements(db2, "Accelerometer", "12345", "2024-01-01 12:30:00")
  single <- collect(coverage(
    db2,
    "12345",
    sensor = "Accelerometer",
    expected = coverage_frequency(Accelerometer = 5)
  ))
  expect_equal(single$coverage, 1)
})

test_that("coverage computes week profiles with weekday labels", {
  db <- coverage_db()
  on.exit(
    {
      dbDisconnect(db)
      file.remove(db@driver@dbdir)
    },
    add = TRUE
  )
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c(
      "2024-01-01 09:00:00",
      "2024-01-01 09:00:01",
      "2024-01-03 09:00:00"
    )
  )

  res <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    by = "day",
    cycle = "week",
    week_start = 1
  ))

  expect_equal(nrow(res), 3)
  expect_equal(as.character(res$day_of_week), c("Mon", "Tue", "Wed"))
  expect_equal(res$coverage, c(2, 0, 1))

  plain <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    by = "day",
    cycle = "week",
    week_start = 1,
    label = FALSE
  ))
  expect_equal(plain$day_of_week, 1:3)
  expect_false(is.factor(plain$day_of_week))

  # week_start = 7 shifts the position index.
  sunday <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    by = "day",
    cycle = "week",
    week_start = 7,
    label = FALSE
  ))
  expect_equal(sunday$day_of_week, 2:4)
})

test_that("coverage supports month and year positions", {
  db <- coverage_db()
  on.exit(
    {
      dbDisconnect(db)
      file.remove(db@driver@dbdir)
    },
    add = TRUE
  )
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c("2024-01-15 12:00:00", "2024-03-10 12:00:00")
  )

  by_month <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    by = "month",
    cycle = "year"
  ))
  expect_equal(by_month$month, 1:3)
  expect_equal(by_month$coverage, c(1, 0, 1))

  db2 <- coverage_db()
  on.exit(
    {
      dbDisconnect(db2)
      file.remove(db2@driver@dbdir)
    },
    add = TRUE
  )
  insert_measurements(
    db2,
    "Accelerometer",
    "12345",
    c("2024-01-01 12:00:00", "2024-01-03 12:00:00")
  )
  by_day <- collect(coverage(
    db2,
    "12345",
    sensor = "Accelerometer",
    by = "day",
    cycle = "month"
  ))
  expect_equal(by_day$day_of_month, 1:3)
  expect_equal(by_day$coverage, c(1, 0, 1))
})

test_that("coverage returns the full zero-filled series when cycle is NULL", {
  db <- coverage_db()
  on.exit(
    {
      dbDisconnect(db)
      file.remove(db@driver@dbdir)
    },
    add = TRUE
  )
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c("2024-01-01 10:00:00", "2024-01-01 12:00:00")
  )

  res <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    cycle = NULL
  ))

  expect_equal(nrow(res), 3)
  expect_equal(res$time, as.POSIXct(
    c(
      "2024-01-01 10:00:00",
      "2024-01-01 11:00:00",
      "2024-01-01 12:00:00"
    ),
    tz = "UTC"
  ))
  expect_equal(res$coverage, c(1, 0, 1))
})

test_that("coverage bins by participant-local time", {
  db <- coverage_db()
  on.exit(
    {
      dbDisconnect(db)
      file.remove(db@driver@dbdir)
    },
    add = TRUE
  )
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    "2024-01-01 23:30:00",
    timezone = "Europe/Amsterdam"
  )

  local_res <- collect(coverage(db, "12345", sensor = "Accelerometer"))
  utc_res <- collect(coverage(db, "12345", sensor = "Accelerometer", local = FALSE))

  expect_equal(local_res$hour, 0L)
  expect_equal(utc_res$hour, 23L)

  # A missing timezone falls back to UTC.
  db2 <- coverage_db()
  on.exit(
    {
      dbDisconnect(db2)
      file.remove(db2@driver@dbdir)
    },
    add = TRUE
  )
  insert_measurements(db2, "Accelerometer", "12345", "2024-01-01 23:30:00")
  fallback <- collect(coverage(db2, "12345", sensor = "Accelerometer"))
  expect_equal(fallback$hour, 23L)
})

test_that("coverage filters Heartbeat measurements from secondary devices", {
  db <- coverage_db()
  on.exit(
    {
      dbDisconnect(db)
      file.remove(db@driver@dbdir)
    },
    add = TRUE
  )
  insert_measurements(
    db,
    "Heartbeat",
    "12345",
    c("2024-01-01 10:00:01", "2024-01-01 10:00:02", "2024-01-01 10:00:03"),
    device_role_name = c("Primary Phone", "Secondary Phone", "Primary Watch")
  )

  res <- collect(coverage(db, "12345", sensor = "Heartbeat", local = FALSE))
  expect_equal(res$coverage[res$hour == 10], 2)
})

test_that("coverage handles participant selection", {
  db <- coverage_db(c("1", "2", "3"))
  on.exit(
    {
      dbDisconnect(db)
      file.remove(db@driver@dbdir)
    },
    add = TRUE
  )
  insert_measurements(db, "Accelerometer", "1", "2024-01-01 08:00:00")

  all_participants <- collect(coverage(db, sensor = "Accelerometer"))
  expect_equal(unique(all_participants$participant_id), 1)

  expect_warning(
    skipped <- collect(coverage(db, c(1, 2), sensor = "Accelerometer")),
    "No data found for participant.*2"
  )
  expect_equal(unique(skipped$participant_id), 1)

  expect_error(
    collect(coverage(db, 2, sensor = "Accelerometer")),
    "No observations found for participant.*2"
  )
})

test_that("coverage aborts when the database has no participants", {
  db <- create_db(NULL, tempfile(fileext = ".duckdb"), shared_home = FALSE)
  on.exit(
    {
      dbDisconnect(db)
      file.remove(db@driver@dbdir)
    },
    add = TRUE
  )
  expect_error(coverage(db), "does not contain any participants")
})

test_that("coverage filters sensors by expected intervals", {
  db <- coverage_db()
  on.exit(
    {
      dbDisconnect(db)
      file.remove(db@driver@dbdir)
    },
    add = TRUE
  )
  insert_two_day_accelerometer(db)
  insert_measurements(db, "Battery", "12345", "2024-01-01 10:00:00")

  expect_warning(
    res <- collect(coverage(
      db,
      "12345",
      sensor = c("Accelerometer", "Battery"),
      expected = c(Accelerometer = 5),
      local = FALSE
    )),
    "Dropping sensor.*Battery"
  )
  expect_equal(as.character(unique(res$measure)), "Accelerometer")

  expect_error(
    suppressWarnings(coverage(
      db,
      "12345",
      sensor = "Battery",
      expected = c(Accelerometer = 5),
      local = FALSE
    )),
    "No sensors left"
  )
})

test_that("coverage respects start_date and end_date", {
  db <- coverage_db()
  on.exit(
    {
      dbDisconnect(db)
      file.remove(db@driver@dbdir)
    },
    add = TRUE
  )
  insert_two_day_accelerometer(db)

  res <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    start_date = "2024-01-02",
    end_date = "2024-01-02",
    local = FALSE
  ))

  expect_equal(nrow(res), 3)
  # Only day 2 remains; the span runs from the first to the last observation.
  expect_equal(res$hour, c(10L, 11L, 12L))
  expect_equal(res$coverage, c(1, 0, 3))

  expect_error(
    collect(coverage(
      db,
      "12345",
      sensor = "Accelerometer",
      start_date = "2024-01-05",
      end_date = "2024-01-05",
      local = FALSE
    )),
    "No observations found"
  )
})

test_that("coverage_frequency constructs interval vectors", {
  intervals <- coverage_frequency()
  expect_length(intervals, 14)
  expect_named(intervals)
  expect_equal(unname(intervals["Weather"]), 120)
  expect_equal(unname(intervals["Accelerometer"]), 120)

  custom <- coverage_frequency(Weather = 1200)
  expect_equal(unname(custom["Weather"]), 1200)
  expect_equal(unname(custom["Accelerometer"]), 120)

  expect_error(coverage_frequency(Weather = -1), "positive, finite")
  expect_error(coverage_frequency(Weather = NA_real_), "positive, finite")
})

test_that("plot.coverage returns ggplot objects", {
  db <- coverage_db(c("1", "2"))
  on.exit(
    {
      dbDisconnect(db)
      file.remove(db@driver@dbdir)
    },
    add = TRUE
  )
  insert_two_day_accelerometer(db, participant_id = "1")
  insert_measurements(db, "Accelerometer", "2", "2024-01-01 09:00:00")

  expect_s3_class(plot(coverage(db, "1", sensor = "Accelerometer")), "ggplot")
  expect_s3_class(
    plot(coverage(
      db,
      "1",
      sensor = "Accelerometer",
      expected = coverage_frequency(Accelerometer = 5)
    )),
    "ggplot"
  )
  expect_s3_class(
    plot(coverage(db, "1", sensor = "Accelerometer", cycle = NULL)),
    "ggplot"
  )
  expect_s3_class(
    plot(
      coverage(db, "1", sensor = "Accelerometer", cycle = NULL),
      type = "heatmap"
    ),
    "ggplot"
  )
  expect_s3_class(
    plot(
      coverage(db, "1", sensor = "Accelerometer", by = "day", cycle = "week"),
      type = "line"
    ),
    "ggplot"
  )
  expect_s3_class(
    plot(coverage(db, sensor = "Accelerometer")),
    "ggplot"
  )
})
