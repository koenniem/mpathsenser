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
  on.exit(cleanup_test_db(db), add = TRUE)

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
  on.exit(cleanup_test_db(db), add = TRUE)
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
  on.exit(cleanup_test_db(db), add = TRUE)
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
  on.exit(cleanup_test_db(db), add = TRUE)
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
  on.exit(cleanup_test_db(db), add = TRUE)
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
  on.exit(cleanup_test_db(db2), add = TRUE)
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
  on.exit(cleanup_test_db(db), add = TRUE)
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

test_that("coverage aligns week bins and reports week positions", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c("2024-01-03 12:00:00", "2024-01-10 12:00:00")
  )

  monday <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    by = "week",
    cycle = NULL,
    week_start = 1,
    local = FALSE
  ))
  sunday <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    by = "week",
    cycle = NULL,
    week_start = 7,
    local = FALSE
  ))
  year <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    by = "week",
    cycle = "year",
    week_start = 1,
    local = FALSE
  ))
  month <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    by = "week",
    cycle = "month",
    week_start = 1,
    local = FALSE
  ))

  expect_equal(
    monday$time,
    as.POSIXct(c("2024-01-01", "2024-01-08"), tz = "UTC")
  )
  expect_equal(
    sunday$time,
    as.POSIXct(c("2023-12-31", "2024-01-07"), tz = "UTC")
  )
  expect_equal(year$week_of_year, 1:2)
  expect_equal(month$week_of_month, 1:2)
})

test_that("coverage supports month and year positions", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
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
  on.exit(cleanup_test_db(db2), add = TRUE)
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
  on.exit(cleanup_test_db(db), add = TRUE)
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
  on.exit(cleanup_test_db(db), add = TRUE)
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
  on.exit(cleanup_test_db(db2), add = TRUE)
  insert_measurements(db2, "Accelerometer", "12345", "2024-01-01 23:30:00")
  fallback <- collect(coverage(db2, "12345", sensor = "Accelerometer"))
  expect_equal(fallback$hour, 23L)
})

test_that("coverage filters Heartbeat measurements from secondary devices", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
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
  on.exit(cleanup_test_db(db), add = TRUE)
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
  on.exit(cleanup_test_db(db), add = TRUE)
  expect_error(coverage(db), "does not contain any participants")
})

test_that("coverage filters sensors by expected intervals", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
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
  on.exit(cleanup_test_db(db), add = TRUE)
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
  on.exit(cleanup_test_db(db), add = TRUE)
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

# Coverage metric = "time" ---------------------------------------------------

test_that("coverage validates the metric argument", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)

  expect_error(coverage(db, "12345", metric = "coverage"), "should be one of")
  expect_error(
    coverage(db, "12345", metric = c("time", "count")),
    "length 1"
  )
  expect_error(
    coverage(db, "12345", sensor = "Accelerometer", metric = "time"),
    "requires `expected`"
  )
})

test_that("coverage metric count is the default and is preserved", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_two_day_accelerometer(db)

  default <- collect(coverage(db, "12345", sensor = "Accelerometer"))
  explicit <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    metric = "count"
  ))

  expect_equal(default, explicit)
  expect_equal(attr(default, "metric"), "count")

  relative <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = coverage_frequency(Accelerometer = 5),
    metric = "time"
  ))
  expect_equal(attr(relative, "metric"), "time")
})

test_that("coverage time unions bursts instead of counting samples", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  # 600 one-second samples in the first 10 minutes, plus one sample at 13:00
  # that extends the participant span over the full 12:00 hour.
  burst <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC") + seq(0, 599)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c(burst, as.POSIXct("2024-01-01 13:00:00", tz = "UTC"))
  )
  expected <- c(Accelerometer = 5)

  count <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = expected,
    by = "hour",
    cycle = NULL
  ))
  time <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = expected,
    metric = "time",
    by = "hour",
    cycle = NULL
  ))

  expect_equal(
    count$time,
    as.POSIXct(
      c("2024-01-01 12:00:00", "2024-01-01 13:00:00"),
      tz = "UTC"
    )
  )
  # 600 samples of 720 expected vs 604 covered seconds of 3600 eligible.
  expect_equal(count$coverage, c(0.83, 1))
  expect_equal(time$coverage, c(0.17, 1))
})

test_that("coverage time preserves fractional-second intervals", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  times <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")
  insert_measurements(db, "Accelerometer", "12345", times)

  res <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = c(Accelerometer = 0.5),
    metric = "time",
    by = "minute",
    cycle = NULL,
    local = FALSE
  ))

  expect_equal(res$coverage, 1)
})

test_that("coverage time reaches one for a regular sampling stream", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  times <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC") + seq(0, 3595, by = 5)
  insert_measurements(db, "Accelerometer", "12345", times)
  expected <- c(Accelerometer = 5)

  time <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = expected,
    metric = "time",
    by = "hour",
    cycle = NULL
  ))
  count <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = expected,
    by = "hour",
    cycle = NULL
  ))

  expect_equal(nrow(time), 1)
  expect_equal(time$time, as.POSIXct("2024-01-01 12:00:00", tz = "UTC"))
  expect_equal(time$coverage, 1)
  expect_equal(count$coverage, 1)

  # A cycle profile contains the positions present in the participant span.
  profile <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = expected,
    metric = "time",
    by = "hour",
    cycle = "day"
  ))
  expect_equal(nrow(profile), 1)
  expect_equal(profile$hour, 12L)
  expect_equal(profile$coverage, 1)
})

test_that("coverage time deduplicates observations and stays bounded", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  # Two non-overlapping ten-minute intervals are each written twice. A naive
  # sum would report 100%; their union covers half of the eligible span.
  times <- rep(
    as.POSIXct(c("2024-01-01 12:00:00", "2024-01-01 12:30:00"), tz = "UTC"),
    each = 2
  )
  insert_measurements(db, "Accelerometer", "12345", times)

  duplicated <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = c(Accelerometer = 600),
    metric = "time",
    by = "hour",
    cycle = NULL,
    local = FALSE
  ))

  expect_equal(duplicated$coverage, 0.5)
  expect_lte(max(duplicated$coverage), 1)
})

test_that("the interval merge uses the running maximum of prior ends", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(
    con,
    "CREATE TABLE intervals AS SELECT * FROM (VALUES
      (1, 'A', TIMESTAMP '2024-01-01 00:00:00', TIMESTAMP '2024-01-01 00:01:40'),
      (1, 'A', TIMESTAMP '2024-01-01 00:00:50', TIMESTAMP '2024-01-01 00:01:00'),
      (1, 'A', TIMESTAMP '2024-01-01 00:01:10', TIMESTAMP '2024-01-01 00:01:20')
    ) t(participant_id, measure, seg_start, seg_end)"
  )
  query <- sprintf(
    "WITH %s SELECT COUNT(*) AS islands,
     DATE_DIFF('second', MIN(seg_start), MAX(seg_end)) AS span FROM islands",
    .coverage_sql_islands()
  )
  res <- DBI::dbGetQuery(con, query)

  # [0,100) [50,60) [70,80) merge into one island of 100 s. A naive lag(end)
  # merge would split at 70 s and report two islands of 100 + 10 s.
  expect_equal(res$islands, 1)
  expect_equal(res$span, 100)
})

test_that("coverage time clips intervals across bin boundaries", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    as.POSIXct(
      c("2024-01-01 12:00:00", "2024-01-01 12:00:58", "2024-01-01 12:02:00"),
      tz = "UTC"
    )
  )

  res <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = c(Accelerometer = 5),
    metric = "time",
    by = "minute",
    cycle = NULL
  ))

  expect_equal(
    res$time,
    as.POSIXct(
      c("2024-01-01 12:00:00", "2024-01-01 12:01:00", "2024-01-01 12:02:00"),
      tz = "UTC"
    )
  )
  # 12:00 has 5 s from the first interval plus 2 s of the crossing interval,
  # 12:01 the remaining 3 s; 12:02 is a fully covered partial bin.
  expect_equal(res$coverage, c(0.12, 0.05, 1))
})

test_that("coverage time zero-fills gaps inside the participant span", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
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
    expected = c(Accelerometer = 5),
    metric = "time",
    by = "hour",
    cycle = NULL
  ))

  expect_equal(nrow(res), 3)
  expect_equal(res$coverage, c(0, 0, 1))
})

test_that("coverage time supports cycle profiles and cycle NULL", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c(
      "2024-01-01 10:00:00",
      "2024-01-01 12:00:00",
      "2024-01-02 10:00:00",
      "2024-01-02 12:00:00"
    )
  )
  expected <- c(Accelerometer = 5)

  profile <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = expected,
    metric = "time",
    by = "hour",
    cycle = "day"
  ))
  expect_equal(nrow(profile), 24)
  # Hour 12 is a complete five-second terminal bin on the second day and an
  # empty interior bin on the first; the cycle profile averages them.
  expect_equal(profile$coverage[profile$hour == 12], 0.5)
  expect_equal(profile$coverage[profile$hour == 10], 0)
  expect_equal(sum(profile$coverage), 0.5)

  # cycle = NULL covers the month calendar step.
  db2 <- coverage_db()
  on.exit(cleanup_test_db(db2), add = TRUE)
  insert_measurements(
    db2,
    "Accelerometer",
    "12345",
    c("2024-01-15 12:00:00", "2024-03-10 12:00:00")
  )
  monthly <- collect(coverage(
    db2,
    "12345",
    sensor = "Accelerometer",
    expected = expected,
    metric = "time",
    by = "month",
    cycle = NULL
  ))
  expect_equal(nrow(monthly), 3)
  expect_true(all(monthly$coverage <= 1))
})

test_that("coverage time uses per-sensor expected intervals", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  times <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC") + seq(0, 3595, by = 5)
  insert_measurements(db, "Accelerometer", "12345", times)
  insert_measurements(
    db,
    "Battery",
    "12345",
    "2024-01-01 12:30:00"
  )

  res <- collect(coverage(
    db,
    "12345",
    sensor = c("Accelerometer", "Battery"),
    expected = c(Accelerometer = 5, Battery = 60),
    metric = "time",
    by = "hour",
    cycle = NULL
  ))

  expect_equal(nrow(res), 2)
  expect_equal(res$coverage[res$measure == "Accelerometer"], 1)
  expect_equal(res$coverage[res$measure == "Battery"], 0.02)
})

test_that("coverage time separates participants", {
  db <- coverage_db(c("1", "2"))
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "1",
    c("2024-01-01 08:00:00", "2024-01-01 09:00:00")
  )
  insert_measurements(db, "Accelerometer", "2", "2024-01-03 08:00:00")

  res <- collect(coverage(
    db,
    sensor = "Accelerometer",
    expected = c(Accelerometer = 5),
    metric = "time",
    by = "hour",
    cycle = NULL
  ))

  expect_equal(nrow(res), 3)
  expect_equal(
    res$time[res$participant_id == 1],
    as.POSIXct(c("2024-01-01 08:00:00", "2024-01-01 09:00:00"), tz = "UTC")
  )
  expect_equal(res$coverage[res$participant_id == 1], c(0, 1))
  expect_equal(res$coverage[res$participant_id == 2], 1)
})

test_that("coverage time bins by participant-local time", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    "2024-01-01 23:30:00",
    timezone = "Europe/Amsterdam"
  )

  local_res <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = c(Accelerometer = 5),
    metric = "time"
  ))
  utc_res <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = c(Accelerometer = 5),
    metric = "time",
    local = FALSE
  ))

  expect_equal(local_res$hour, 0L)
  expect_equal(utc_res$hour, 23L)
  expect_equal(local_res$coverage, 1)
  expect_equal(utc_res$coverage, 1)

  db2 <- coverage_db()
  on.exit(cleanup_test_db(db2), add = TRUE)
  insert_measurements(db2, "Accelerometer", "12345", "2024-01-01 23:30:00")
  fallback <- collect(coverage(
    db2,
    "12345",
    sensor = "Accelerometer",
    expected = c(Accelerometer = 5),
    metric = "time"
  ))
  expect_equal(fallback$hour, 23L)
})

test_that("coverage local spans survive westward timezone changes", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    as.POSIXct(
      c("2024-01-01 09:00:00", "2024-01-01 11:00:00"),
      tz = "UTC"
    ),
    timezone = c("Europe/Amsterdam", "America/New_York")
  )

  count <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    cycle = NULL
  ))
  time <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = c(Accelerometer = 5),
    metric = "time",
    cycle = NULL
  ))

  expected_bins <- as.POSIXct(
    seq(
      as.POSIXct("2024-01-01 06:00:00", tz = "UTC"),
      as.POSIXct("2024-01-01 10:00:00", tz = "UTC"),
      by = "hour"
    ),
    tz = "UTC"
  )
  expect_equal(count$time, expected_bins)
  expect_equal(count$coverage, c(1, 0, 0, 0, 1))
  expect_equal(time$time, expected_bins)
  expect_equal(time$coverage, c(0, 0, 0, 0, 1))
})

test_that("coverage time filters Heartbeat secondary devices", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Heartbeat",
    "12345",
    c("2024-01-01 10:00:00", "2024-01-01 10:00:05", "2024-01-01 10:00:10"),
    device_role_name = c("Primary Phone", "Secondary Phone", "Primary Watch")
  )

  res <- collect(coverage(
    db,
    "12345",
    sensor = "Heartbeat",
    expected = c(Heartbeat = 5),
    metric = "time",
    local = FALSE
  ))

  # The excluded secondary interval would fill the gap; only 10 of 15 eligible
  # seconds are covered.
  expect_equal(res$coverage, 0.67)
})

test_that("coverage time respects start_date and end_date", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_two_day_accelerometer(db)

  res <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = c(Accelerometer = 5),
    metric = "time",
    local = FALSE,
    start_date = "2024-01-02",
    end_date = "2024-01-02"
  ))

  expect_equal(nrow(res), 3)
  expect_equal(res$hour, c(10L, 11L, 12L))
  # The last bin is the 8 seconds from 12:00:00 to the final interval end
  # (12:00:08), of which the three merged intervals cover 7 seconds.
  expect_equal(res$coverage, c(0, 0, 0.88))

  expect_error(
    collect(coverage(
      db,
      "12345",
      sensor = "Accelerometer",
      expected = c(Accelerometer = 5),
      metric = "time",
      local = FALSE,
      start_date = "2024-01-05",
      end_date = "2024-01-05"
    )),
    "No observations found"
  )
})

test_that("coverage time prorates only participant boundary bins", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  first <- as.POSIXct("2024-01-01 13:50:00", tz = "UTC") + seq(0, 595, by = 5)
  last <- as.POSIXct("2024-01-01 15:00:00", tz = "UTC") + seq(0, 595, by = 5)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c(first, last)
  )

  res <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = c(Accelerometer = 5),
    metric = "time",
    by = "hour",
    cycle = NULL,
    local = FALSE
  ))

  expect_equal(res$coverage, c(1, 0, 1))
})

test_that("coverage time gives partial and single bins their full interval", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  # A half-hour stream inside the 12:00 hour still reaches full coverage of the
  # observed part of the bin.
  times <- as.POSIXct("2024-01-01 12:30:00", tz = "UTC") + seq(0, 1795, by = 5)
  insert_measurements(db, "Accelerometer", "12345", times)
  res <- collect(coverage(
    db,
    "12345",
    sensor = "Accelerometer",
    expected = c(Accelerometer = 5),
    metric = "time",
    cycle = NULL
  ))
  expect_equal(nrow(res), 1)
  expect_equal(res$time, as.POSIXct("2024-01-01 12:00:00", tz = "UTC"))
  expect_equal(res$coverage, 1)

  # A single observation covers its own interval completely.
  db2 <- coverage_db()
  on.exit(cleanup_test_db(db2), add = TRUE)
  insert_measurements(db2, "Accelerometer", "12345", "2024-01-01 12:30:00")
  single <- collect(coverage(
    db2,
    "12345",
    sensor = "Accelerometer",
    expected = c(Accelerometer = 5),
    metric = "time",
    cycle = NULL
  ))
  expect_equal(single$coverage, 1)
})
