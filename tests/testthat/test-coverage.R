test_that("coverage_expected returns overridable sensor intervals", {
  expected <- coverage_expected()

  expect_length(expected, 14)
  expect_named(expected)
  expect_equal(unname(expected["Accelerometer"]), 120)
  expect_equal(unname(expected["Weather"]), 120)
  expect_equal(unname(coverage_expected(Accelerometer = 60)["Accelerometer"]), 60)
  expect_error(coverage_expected(Weather = -1), "positive, finite")
  expect_error(coverage_expected(Weather = NA_real_), "positive, finite")
  # Logical or otherwise non-numeric overrides are rejected before coercion.
  expect_error(coverage_expected(Accelerometer = TRUE), "positive, finite")
  expect_error(coverage_expected(Bogus = 10), "could not be found")
  expect_error(coverage_expected(Accelerometer = 60, accelerometer = 70), "formal argument")
  expect_error(coverage_expected(accelerometer = 1, accelerometer = 2), "more than once")
  expect_error(coverage_expected(60, accelerometer = 70), "formal argument")
  unnamed <- c(rep(list(1), 14), list(2))
  expect_error(do.call(coverage_expected, unnamed), "must be named")

  # Names are matched case-insensitively; output uses canonical capitalisation.
  expect_equal(unname(coverage_expected(accelerometer = 60)["Accelerometer"]), 60)
  expect_equal(
    names(coverage_expected(appusage = 10, weather = 20)),
    names(coverage_expected())
  )
})

test_that("coverage functions validate their inputs", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)

  expect_error(coverage_frequency(db, sensor = "Accelerometer", by = NULL), "cannot be.*NULL")
  expect_error(coverage_frequency(db, sensor = "Accelerometer", by = 0), "positive, finite")
  expect_error(
    coverage_frequency(db, sensor = "Accelerometer", by = "fortnight"),
    "should be one of"
  )
  expect_error(
    coverage_frequency(db, sensor = "Accelerometer", week_start = 8),
    "whole number between"
  )
  expect_error(coverage_proportional(db, sensor = "Accelerometer"), "absent but must be supplied")
  expect_error(
    coverage_proportional(db, expected = c(Accelerometer = -1)),
    "at least one microsecond"
  )
  expect_error(
    coverage_proportional(db, expected = c(Accelerometer = 1, Accelerometer = 2)),
    "duplicate sensor names"
  )
  unnamed_expected <- 1
  names(unnamed_expected) <- NA_character_
  expect_error(coverage_proportional(db, expected = unnamed_expected), "named numeric vector")
  expect_error(
    coverage_frequency(db, sensor = "Accelerometer", start_date = as.Date(NA)),
    "must be.*date"
  )
  expect_error(
    coverage_proportional(db, expected = c(Accelerometer = 1), metric = "duration"),
    "should be one of"
  )
  expect_error(
    coverage_proportional(db, expected = c(Accelerometer = 1), metric = "time"),
    "should be one of"
  )
  expect_error(
    coverage_proportional(db, expected = c(Accelerometer = 1), by = 0),
    "positive, finite"
  )
  expect_error(coverage_frequency(db, participant_id = "missing"), "could not be found")
  expect_error(coverage_frequency(db, sensor = "missing"), "could not be found")
})

test_that("coverage_frequency counts distinct measurements in zero-filled bins", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c(
      "2024-01-01 10:00:00",
      "2024-01-01 10:00:09",
      "2024-01-01 10:00:09",
      "2024-01-01 10:00:10",
      "2024-01-01 10:00:29"
    )
  )

  res <- collect(coverage_frequency(
    db,
    "12345",
    sensor = "Accelerometer",
    by = 10,
    local = FALSE
  ))

  expect_s3_class(res, "coverage")
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("participant_id", "time", "sensor", "coverage"))
  expect_equal(
    res$time,
    as.POSIXct(
      c("2024-01-01 10:00:00", "2024-01-01 10:00:10", "2024-01-01 10:00:20"),
      tz = "UTC"
    )
  )
  expect_equal(res$coverage, c(2, 1, 1))
  expect_equal(attr(res, "coverage_type"), "frequency")
  expect_null(attr(res, "expected"))

  hourly <- collect(coverage_frequency(db, "12345", sensor = "Accelerometer", local = FALSE))
  expect_equal(hourly$coverage, 4)
  expect_equal(attr(hourly, "by"), c(Accelerometer = "hour"))
  minute <- collect(coverage_frequency(
    db,
    "12345",
    sensor = "Accelerometer",
    by = "minute",
    local = FALSE
  ))
  expect_equal(nrow(minute), 1)
  expect_equal(minute$coverage, 4)
  expect_equal(
    collect(coverage_frequency(
      db,
      "12345",
      sensor = c("Accelerometer", "Accelerometer"),
      by = 10,
      local = FALSE
    )),
    res
  )
})

test_that("coverage_frequency uses participant spans and zero-fills other sensors", {
  db <- coverage_db(c("1", "2"))
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "1",
    c("2024-01-01 00:00:00", "2024-01-01 00:02:00")
  )
  insert_measurements(db, "Battery", "1", "2024-01-01 00:00:00")
  insert_measurements(db, "Accelerometer", "2", "2024-01-02 00:00:00")

  res <- collect(coverage_frequency(
    db,
    sensor = c("Accelerometer", "Battery"),
    by = 60,
    local = FALSE
  ))
  p1 <- res[res$participant_id == 1, ]
  p2 <- res[res$participant_id == 2, ]

  expect_equal(nrow(p1), 6)
  expect_equal(nrow(p2), 2)
  expect_equal(p1$coverage[p1$sensor == "Battery"], c(1, 0, 0))
  expect_equal(p1$coverage[p1$sensor == "Accelerometer"], c(1, 0, 1))
  expect_equal(p2$coverage, c(1, 0))
})

test_that("coverage_frequency supports calendar months and configurable weeks", {
  expect_equal(
    .coverage_period_seconds(as.POSIXct("2024-02-15", tz = "UTC"), "month"),
    29 * 86400
  )
  expect_equal(
    .coverage_period_seconds(as.POSIXct("2024-06-15", tz = "UTC"), "year"),
    366 * 86400
  )
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c("2024-01-31 23:59:00", "2024-02-01 00:00:00", "2024-02-29 23:59:00", "2024-03-01 00:00:00")
  )

  monthly <- collect(coverage_frequency(
    db,
    "12345",
    sensor = "Accelerometer",
    by = "month",
    local = FALSE
  ))
  expect_equal(
    monthly$time,
    as.POSIXct(c("2024-01-01", "2024-02-01", "2024-03-01"), tz = "UTC")
  )
  expect_equal(monthly$coverage, c(1, 2, 1))
  year_plot <- plot(monthly, cycle = "year")
  expect_equal(year_plot$data$month, 1:3)
  thirty_day_bins <- coverage_frequency(
    db,
    sensor = "Accelerometer",
    by = 30 * 86400,
    local = FALSE
  )
  expect_error(plot(thirty_day_bins, cycle = "month"), "must be coarser")

  weekly_db <- coverage_db()
  on.exit(cleanup_test_db(weekly_db), add = TRUE)
  insert_measurements(
    weekly_db,
    "Accelerometer",
    "12345",
    c(
      "2024-01-01 00:00:00",
      "2024-01-07 00:00:00",
      "2024-01-08 00:00:00",
      "2024-01-15 00:00:00"
    )
  )
  monday <- collect(coverage_frequency(
    weekly_db,
    sensor = "Accelerometer",
    by = "week",
    week_start = 1,
    local = FALSE
  ))
  sunday <- collect(coverage_frequency(
    weekly_db,
    sensor = "Accelerometer",
    by = "week",
    week_start = 7,
    local = FALSE
  ))
  expect_equal(
    monday$time,
    as.POSIXct(c("2024-01-01", "2024-01-08", "2024-01-15"), tz = "UTC")
  )
  expect_equal(monday$coverage, c(2, 1, 1))
  expect_equal(
    sunday$time,
    as.POSIXct(c("2023-12-31", "2024-01-07", "2024-01-14"), tz = "UTC")
  )
  expect_equal(sunday$coverage, c(1, 2, 1))
  month_profile <- plot(monday, cycle = "month")
  expect_equal(month_profile$data$week_of_month, 1:3)
})

test_that("coverage_proportional uses each expected interval as its default bin", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c("2024-01-01 00:00:00", "2024-01-01 00:01:00", "2024-01-01 00:02:00")
  )
  insert_measurements(
    db,
    "Battery",
    "12345",
    c("2024-01-01 00:00:00", "2024-01-01 00:02:00")
  )

  res <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 60, Battery = 120),
    sensor = c("Accelerometer", "Battery"),
    local = FALSE
  ))

  expect_equal(nrow(res[res$sensor == "Accelerometer", ]), 3)
  expect_equal(nrow(res[res$sensor == "Battery", ]), 2)
  expect_equal(res$coverage, rep(1, 5))
  expect_equal(attr(res, "by"), c(Accelerometer = 60, Battery = 120))

  wide <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 60, Battery = 120),
    sensor = c("Accelerometer", "Battery"),
    by = 180,
    local = FALSE
  ))
  expect_equal(nrow(wide), 2)
  expect_equal(wide$coverage[wide$sensor == "Accelerometer"], 1.5)
  expect_equal(wide$coverage[wide$sensor == "Battery"], 2)
})

test_that("coverage_proportional interval unions overlaps in the last bin", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c("2024-01-01 00:00:00", "2024-01-01 00:00:09", "2024-01-01 00:00:09")
  )

  res <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 10),
    sensor = "Accelerometer",
    by = 15,
    metric = "interval",
    local = FALSE
  ))

  # The second observation's window reaches 00:00:19, but the output stops at
  # its own bin; the overlapping windows union to full coverage there.
  expect_equal(nrow(res), 1)
  expect_equal(res$coverage, 1)
  expect_lte(max(res$coverage), 1)
  expect_equal(attr(res, "metric"), "interval")

  default_bins <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 10),
    sensor = "Accelerometer",
    metric = "interval",
    local = FALSE
  ))
  expect_equal(nrow(default_bins), 1)
  expect_equal(default_bins$coverage, 1)
  expect_equal(attr(default_bins, "by"), c(Accelerometer = 10))
})

test_that("proportional metrics widen bins that are shorter than sensor intervals", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c("2024-01-01 00:00:00", "2024-01-01 00:02:00", "2024-01-01 00:04:00")
  )
  insert_measurements(
    db,
    "Battery",
    "12345",
    c(
      "2024-01-01 00:00:00",
      "2024-01-01 00:01:00",
      "2024-01-01 00:02:00",
      "2024-01-01 00:03:00",
      "2024-01-01 00:04:00"
    )
  )
  expected <- c(Accelerometer = 120, Battery = 60)

  for (metric in c("count", "interval", "bin")) {
    expect_warning(
      result <- coverage_proportional(
        db,
        expected = expected,
        sensor = names(expected),
        by = "minute",
        metric = metric,
        local = FALSE
      ),
      "Accelerometer: minute.*120 seconds"
    )
    expect_equal(
      attr(result, "by"),
      list(Accelerometer = 120, Battery = "minute")
    )

    result <- collect(result)
    for (sensor in names(expected)) {
      times <- sort(as.numeric(result$time[result$sensor == sensor]))
      expect_gte(length(times), 3)
      expect_equal(
        diff(times),
        rep(unname(expected[[sensor]]), length(times) - 1)
      )
    }
    if (identical(metric, "count")) {
      expect_s3_class(plot(result, cycle = "hour"), "ggplot")
    }
  }

  expect_no_warning(
    default_bins <- coverage_proportional(
      db,
      expected = expected,
      sensor = names(expected),
      local = FALSE
    )
  )
  expect_equal(attr(default_bins, "by"), expected)

  expect_warning(
    numeric_bins <- coverage_proportional(
      db,
      expected = c(Accelerometer = 120),
      sensor = "Accelerometer",
      by = 60,
      local = FALSE
    ),
    "60 seconds"
  )
  expect_equal(attr(numeric_bins, "by"), c(Accelerometer = 120))
})

test_that("calendar month bins keep actual durations instead of widening", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(db, "Accelerometer", "12345", "2024-01-01 00:00:00")

  # Divergence from the old 2.0.0 beta: it warned "shortest month: 28 days"
  # and widened the month to a fixed 30-day bin; calendar units now keep
  # their actual boundaries.
  expect_no_warning(
    result <- collect(coverage_proportional(
      db,
      expected = c(Accelerometer = 30 * 86400),
      sensor = "Accelerometer",
      by = "month",
      local = FALSE
    ))
  )
  expect_equal(attr(result, "coverage_type"), "proportional")
  expect_equal(attr(result, "by"), c(Accelerometer = "month"))

  # Actual month durations: one observation per day across March 2024, with
  # the local DST spring-forward inside the month.
  split_db <- coverage_db()
  on.exit(cleanup_test_db(split_db), add = TRUE)
  insert_measurements(
    split_db,
    "Accelerometer",
    "12345",
    seq(
      as.POSIXct("2024-03-01 12:00:00", tz = "UTC"),
      as.POSIXct("2024-03-31 12:00:00", tz = "UTC"),
      by = "day"
    ),
    timezone = "Europe/Amsterdam"
  )
  count <- collect(coverage_proportional(
    split_db,
    expected = c(Accelerometer = 86400),
    sensor = "Accelerometer",
    by = "month",
    metric = "count",
    local = TRUE
  ))
  # 31 daily numerators over the month cell's two offset parts: 29d13h floors
  # to 29.54 opportunities, the 11h part to 1, so 31 / 30.54 rounds to 1.02.
  expect_equal(count$coverage, 1.02)
})

test_that("coverage_proportional supports calendar month intervals", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    seq(
      as.POSIXct("2024-01-01", tz = "UTC"),
      as.POSIXct("2024-03-01", tz = "UTC"),
      by = "day"
    )
  )
  monthly_count <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 86400),
    sensor = "Accelerometer",
    by = "month",
    local = FALSE
  ))
  expect_equal(monthly_count$coverage, c(1, 1, 1))

  db2 <- coverage_db()
  on.exit(cleanup_test_db(db2), add = TRUE)
  insert_measurements(
    db2,
    "Accelerometer",
    "12345",
    c("2024-01-31 23:59:50", "2024-03-01 00:00:00")
  )

  monthly <- collect(coverage_proportional(
    db2,
    expected = c(Accelerometer = 20),
    sensor = "Accelerometer",
    by = "month",
    metric = "interval",
    local = FALSE
  ))
  expect_equal(
    monthly$time,
    as.POSIXct(c("2024-01-01", "2024-02-01", "2024-03-01"), tz = "UTC")
  )
  expect_equal(monthly$coverage, c(1, 0, 1))
})

test_that("bin coverage counts each occupied slot once at its start", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c("2024-01-01 00:00:30", "2024-01-01 00:01:20", "2024-01-01 00:01:20")
  )

  res <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 60),
    sensor = "Accelerometer",
    by = 60,
    metric = "bin",
    local = FALSE
  ))

  expect_equal(
    res$time,
    as.POSIXct(c("2024-01-01 00:00:00", "2024-01-01 00:01:00"), tz = "UTC")
  )
  expect_equal(res$coverage, c(1, NA_real_))
  expect_equal(attr(res, "metric"), "bin")
  profile <- plot(res, cycle = "hour")
  line <- plot(res, cycle = NULL, type = "line")
  expect_s3_class(profile, "ggplot")
  expect_equal(line$labels$y, "Proportion of occupied expected slots")
})

test_that("bin coverage uses partial spans and exact non-dividing denominators", {
  partial_db <- coverage_db()
  on.exit(cleanup_test_db(partial_db), add = TRUE)
  insert_measurements(
    partial_db,
    "Accelerometer",
    "12345",
    c("2024-01-01 13:30:00", "2024-01-01 13:40:00", "2024-01-01 13:50:00")
  )

  partial <- collect(coverage_proportional(
    partial_db,
    expected = c(Accelerometer = 600),
    sensor = "Accelerometer",
    by = "hour",
    metric = "bin",
    local = FALSE
  ))
  expect_equal(partial$time, as.POSIXct("2024-01-01 13:00:00", tz = "UTC"))
  expect_equal(partial$coverage, 1)

  grid_db <- coverage_db()
  on.exit(cleanup_test_db(grid_db), add = TRUE)
  slot_starts <- as.POSIXct("2024-01-01 00:03:00", tz = "UTC") +
    seq(0, by = 7 * 60, length.out = 27)
  insert_measurements(
    grid_db,
    "Accelerometer",
    "12345",
    slot_starts[-c(2, 10)]
  )

  hourly <- collect(coverage_proportional(
    grid_db,
    expected = c(Accelerometer = 7 * 60),
    sensor = "Accelerometer",
    by = "hour",
    metric = "bin",
    local = FALSE
  ))
  expect_equal(hourly$coverage[1:2], c(0.89, 0.88))
})

test_that("bin coverage extrapolates grids across the shared participant span", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c("2024-01-01 00:00:00", "2024-01-01 00:04:00")
  )
  insert_measurements(
    db,
    "Battery",
    "12345",
    c("2024-01-01 00:02:00", "2024-01-01 00:04:00")
  )

  res <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 120, Battery = 120, Weather = 120),
    sensor = c("Accelerometer", "Battery", "Weather"),
    metric = "bin",
    local = FALSE
  ))

  expect_equal(nrow(res), 9)
  expect_equal(res$coverage[res$sensor == "Accelerometer"], c(1, 0, 1))
  expect_equal(res$coverage[res$sensor == "Battery"], c(0, 1, 1))
  expect_equal(res$coverage[res$sensor == "Weather"], c(0, 0, 0))
})

test_that("bin coverage uses the selected local or UTC time axis", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c("2024-01-01 23:30:00", "2024-01-01 23:31:00"),
    timezone = "Europe/Amsterdam"
  )

  local <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 60),
    sensor = "Accelerometer",
    metric = "bin"
  ))
  utc <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 60),
    sensor = "Accelerometer",
    metric = "bin",
    local = FALSE
  ))

  expect_equal(lubridate::hour(local$time), c(0L, 0L))
  expect_equal(lubridate::hour(utc$time), c(23L, 23L))
  expect_equal(local$coverage, c(1, 1))
  expect_equal(utc$coverage, c(1, 1))
})

test_that("coverage_proportional drops sensors without expected intervals", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(db, "Accelerometer", "12345", "2024-01-01 00:00:00")
  insert_measurements(db, "Battery", "12345", "2024-01-01 00:00:00")

  expect_warning(
    res <- collect(coverage_proportional(
      db,
      expected = c(Accelerometer = 60),
      sensor = c("Accelerometer", "Battery"),
      local = FALSE
    )),
    "Dropping sensor.*Battery"
  )
  expect_equal(as.character(unique(res$sensor)), "Accelerometer")

  expect_error(
    suppressWarnings(coverage_proportional(
      db,
      expected = c(Accelerometer = 60),
      sensor = "Battery"
    )),
    "No sensors left"
  )
})

test_that("coverage bins in local or UTC time and filters Heartbeat and dates", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    "2024-01-01 23:30:00",
    timezone = "Europe/Amsterdam"
  )

  local <- collect(coverage_frequency(
    db,
    "12345",
    sensor = "Accelerometer",
    by = 3600
  ))
  utc <- collect(coverage_frequency(
    db,
    "12345",
    sensor = "Accelerometer",
    by = 3600,
    local = FALSE
  ))
  expect_equal(lubridate::hour(local$time), 0L)
  expect_equal(lubridate::hour(utc$time), 23L)

  db2 <- coverage_db()
  on.exit(cleanup_test_db(db2), add = TRUE)
  insert_measurements(
    db2,
    "Accelerometer",
    "12345",
    as.POSIXct(c("2024-01-01 09:00:00", "2024-01-01 11:00:00"), tz = "UTC"),
    timezone = c("Europe/Amsterdam", "America/New_York")
  )
  local_span <- collect(coverage_frequency(
    db2,
    "12345",
    sensor = "Accelerometer",
    by = 3600
  ))
  # Each instant is binned in its own timezone: 09:00Z is 10:00 in Amsterdam,
  # 11:00Z is 06:00 in New York, and the intervening local 11:00 bin is empty.
  expect_equal(
    format(local_span$time, "%H:%M:%S"),
    c("06:00:00", "10:00:00", "11:00:00")
  )
  expect_equal(local_span$coverage, c(1, 1, 0))

  insert_measurements(
    db,
    "Heartbeat",
    "12345",
    c("2024-01-01 10:00:00", "2024-01-01 10:00:10", "2024-01-02 10:00:00"),
    device_role_name = c("Primary Phone", "Secondary Phone", "Primary Phone")
  )
  heartbeat <- collect(coverage_frequency(
    db,
    "12345",
    sensor = "Heartbeat",
    by = 60,
    local = FALSE,
    start_date = "2024-01-02",
    end_date = "2024-01-02"
  ))
  expect_equal(nrow(heartbeat), 1)
  expect_equal(heartbeat$coverage, 1)
})

test_that("coverage reports unknown OS and marks unsupported iOS sensors NA", {
  db <- coverage_db(c("1", "2"))
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(db, "Accelerometer", "1", c("2024-01-01 00:00:00", "2024-01-01 00:01:00"))
  insert_measurements(db, "Accelerometer", "2", "2024-01-01 00:00:00")
  insert_measurements(db, "Device", "1", "2024-01-01 00:00:00", platform = "iOS")
  insert_measurements(db, "Device", "2", "2024-01-01 00:00:00", platform = "Android")

  res <- collect(coverage_frequency(
    db,
    sensor = c("Accelerometer", "AppUsage", "Light", "Memory", "Screen"),
    by = 60,
    local = FALSE
  ))
  ios <- res[
    res$participant_id == 1 & res$sensor %in% c("AppUsage", "Light", "Memory", "Screen"),
  ]
  android <- res[
    res$participant_id == 2 & res$sensor %in% c("AppUsage", "Light", "Memory", "Screen"),
  ]

  expect_equal(nrow(ios), 8)
  expect_equal(ios$coverage, rep(NA_real_, 8))
  expect_equal(nrow(android), 4)
  expect_equal(android$coverage, rep(0, 4))

  db2 <- coverage_db()
  on.exit(cleanup_test_db(db2), add = TRUE)
  insert_measurements(db2, "Accelerometer", "12345", "2024-01-01 00:00:00")
  expect_warning(
    unknown_os <- coverage_frequency(
      db2,
      sensor = c("Accelerometer", "AppUsage"),
      by = 60,
      local = FALSE
    ),
    "Operating system is unknown"
  )
  unknown_res <- collect(unknown_os)
  expect_equal(unknown_res$coverage[unknown_res$sensor == "AppUsage"], 0)
})

test_that("cycle positions use one natural calendar position", {
  expect_equal(.coverage_cycle_position("hour", c(Accelerometer = 120)), "minute")
  expect_equal(.coverage_cycle_position("day", c(Accelerometer = 120)), "hour")
  expect_equal(.coverage_cycle_position("week", c(Accelerometer = 120)), "day_of_week")
  expect_equal(.coverage_cycle_position("month", c(Accelerometer = 120)), "day_of_month")
  expect_equal(.coverage_cycle_position("year", c(Accelerometer = 120)), "month")
  expect_equal(
    .coverage_cycle_position("month", c(Accelerometer = "week")),
    "week_of_month"
  )
  expect_equal(
    .coverage_cycle_position("year", c(Accelerometer = "week")),
    "week_of_year"
  )
})

test_that("cycle profiles collapse mixed and fine bin widths", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  times <- c("2024-01-01 00:10:00", "2024-01-01 10:10:00", "2024-01-02 11:10:00")
  insert_measurements(db, "Accelerometer", "12345", times)
  insert_measurements(db, "Battery", "12345", times)

  res <- coverage_proportional(
    db,
    expected = c(Accelerometer = 120, Battery = 160),
    participant_id = "12345",
    sensor = c("Accelerometer", "Battery"),
    local = FALSE
  )
  heat <- plot(res, cycle = "day", type = "heatmap")
  line <- plot(res, cycle = "day", type = "line")

  expect_equal(nrow(heat$data), 48)
  expect_equal(sort(unique(heat$data$hour)), 0:23)
  expect_equal(nrow(ggplot2::ggplot_build(heat)$layout$layout), 1)
  expect_equal(nrow(line$data), 48)
  expect_equal(sort(unique(line$data$hour)), 0:23)
  expect_no_warning(ggplot2::ggplot_build(line))
})

test_that("coverage results can be summarized and plotted by cycle", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c(
      "2024-01-01 00:00:00",
      "2024-01-01 10:00:00",
      "2024-01-02 11:00:00",
      "2024-01-03 00:00:00"
    )
  )

  frequency <- coverage_frequency(
    db,
    "12345",
    sensor = "Accelerometer",
    by = 3600,
    local = FALSE
  )
  profile <- frequency |>
    mutate(date = as.Date(.data$time), hour = lubridate::hour(.data$time)) |>
    summarise(
      coverage = mean(.data$coverage, na.rm = TRUE),
      .by = c("participant_id", "sensor", "date", "hour")
    ) |>
    collect()
  plotted <- plot(frequency, cycle = "day")
  series_plot <- plot(frequency, cycle = NULL)
  default_plot <- plot(frequency)
  collected_plot <- plot(collect(frequency), cycle = "day")

  expect_s3_class(plotted, "ggplot")
  expect_s3_class(series_plot, "ggplot")
  expect_s3_class(default_plot, "ggplot")
  expect_equal(default_plot$data$time, series_plot$data$time)
  expect_s3_class(collected_plot, "ggplot")
  expect_equal(nrow(plotted$data), 24)
  expect_equal(nrow(series_plot$data), 49)
  expect_equal(
    profile$coverage[profile$date == as.Date("2024-01-01") & profile$hour == 10],
    1
  )
  expect_equal(
    profile$coverage[profile$date == as.Date("2024-01-02") & profile$hour == 11],
    1
  )
  week_plot <- plot(frequency, cycle = "week", week_start = 1)
  week_plain <- plot(frequency, cycle = "week", label = FALSE, week_start = 1)
  # Compare the numeric weekday positions so the assertion is locale-independent;
  # the labelled variant must cover the same three days.
  expect_setequal(as.integer(week_plot$data$day_of_week), 1:3)
  expect_setequal(unique(week_plain$data$day_of_week), 1:3)
  expect_error(plot(frequency, cycle = "decade"), "should be one of")
  expect_error(plot(frequency, cycle = "hour"), "must be coarser")
})

test_that("coverage handles participants with no data and empty databases", {
  db <- coverage_db(c("1", "2"))
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(db, "Accelerometer", "1", "2024-01-01 08:00:00")

  expect_warning(
    res <- collect(coverage_frequency(
      db,
      participant_id = c("1", "2"),
      sensor = "Accelerometer",
      by = 60,
      local = FALSE
    )),
    "No data found for participant.*2"
  )
  expect_equal(unique(res$participant_id), 1)
  expect_error(
    collect(coverage_frequency(db, participant_id = 2, sensor = "Accelerometer")),
    "No observations found"
  )

  empty <- create_db(NULL, tempfile(fileext = ".duckdb"), shared_home = FALSE)
  on.exit(cleanup_test_db(empty), add = TRUE)
  expect_error(coverage_frequency(empty), "does not contain any participants")
})

# -- local time, date windows, and DST behaviour --------------------------------

test_that("end_date excludes the next midnight exactly", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c("2024-01-01 10:00:00", "2024-01-02 00:00:00", "2024-01-02 00:00:01")
  )

  # Divergence: the old UTC window admitted exactly the next midnight.
  res <- collect(coverage_frequency(
    db,
    sensor = "Accelerometer",
    by = 3600,
    local = FALSE,
    start_date = "2024-01-01",
    end_date = "2024-01-01"
  ))
  expect_equal(res$time, as.POSIXct("2024-01-01 10:00:00", tz = "UTC"))
  expect_equal(res$coverage, 1)
})

test_that("local date windows follow the applicable local calendar day", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    as.POSIXct(c("2023-12-31 23:00:00", "2024-01-01 23:00:00", "2024-01-02 00:00:00"), tz = "UTC"),
    timezone = "Pacific/Auckland"
  )

  # Divergence: the old implementation filtered UTC calendar days in both modes.
  local <- collect(coverage_frequency(
    db,
    sensor = "Accelerometer",
    by = 3600,
    local = TRUE,
    start_date = "2024-01-01",
    end_date = "2024-01-01"
  ))
  # 23:00Z is 12:00 local on 2024-01-01; 2024-01-02 00:00Z is the next day.
  expect_equal(lubridate::hour(local$time), 12L)
  expect_equal(local$coverage, 1)

  utc_result <- collect(coverage_frequency(
    db,
    sensor = "Accelerometer",
    by = 3600,
    local = FALSE,
    start_date = "2024-01-01",
    end_date = "2024-01-01"
  ))
  expect_equal(lubridate::hour(utc_result$time), 23L)
  expect_equal(utc_result$coverage, 1)
})

test_that("a missing timezone inherits the applicable episode timezone", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    as.POSIXct(c("2024-01-01 22:00:00", "2024-01-01 23:00:00"), tz = "UTC"),
    timezone = c("Pacific/Auckland", NA)
  )

  local <- collect(coverage_frequency(
    db,
    sensor = "Accelerometer",
    by = 3600,
    local = TRUE,
    start_date = "2024-01-02",
    end_date = "2024-01-02"
  ))
  expect_equal(lubridate::hour(local$time), 11:12)
  expect_equal(local$coverage, c(1, 1))
})

test_that("timezone move: episodes assign each instant locally", {
  db <- timezone_fixture_db()
  on.exit(cleanup_test_db(db), add = TRUE)

  # The old wall-clock hull dropped the second observation.
  frequency <- collect(coverage_frequency(
    db,
    sensor = "Accelerometer",
    by = 3600,
    local = TRUE
  ))
  participant_1 <- frequency[frequency$participant_id == 1, ]
  expect_equal(format(participant_1$time, "%H:%M:%S"), c("06:00:00", "10:00:00", "11:00:00"))
  expect_equal(participant_1$coverage, c(1, 1, 0))

  interval <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 3600),
    sensor = "Accelerometer",
    by = 3600,
    metric = "interval",
    local = TRUE
  ))
  participant_1 <- interval[interval$participant_id == 1, ]
  expect_equal(format(participant_1$time, "%H:%M:%S"), c("06:00:00", "10:00:00", "11:00:00"))
  expect_equal(participant_1$coverage, c(1, 1, 0))

  bin <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 3600),
    sensor = "Accelerometer",
    by = 3600,
    metric = "bin",
    local = TRUE
  ))
  participant_1 <- bin[bin$participant_id == 1, ]
  expect_equal(format(participant_1$time, "%H:%M:%S"), c("06:00:00", "10:00:00", "11:00:00"))
  expect_equal(participant_1$coverage, c(1, 1, 0))
})

test_that("fall-back fold sums both cells without a cap", {
  db <- timezone_fixture_db()
  on.exit(cleanup_test_db(db), add = TRUE)

  frequency <- collect(coverage_frequency(
    db,
    sensor = "Accelerometer",
    by = 3600,
    local = TRUE
  ))
  participant_2 <- frequency[frequency$participant_id == 2, ]
  expect_equal(format(participant_2$time, "%H:%M:%S"), "02:00:00")
  expect_equal(participant_2$coverage, 2)

  interval <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 3600),
    sensor = "Accelerometer",
    by = 3600,
    metric = "interval",
    local = TRUE
  ))
  participant_2 <- interval[interval$participant_id == 2, ]
  expect_equal(participant_2$coverage, 1)

  count <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 3600),
    sensor = "Accelerometer",
    by = 3600,
    metric = "count",
    local = TRUE
  ))
  participant_2 <- count[count$participant_id == 2, ]
  # One distinct observation per fold cell and one floored opportunity each:
  # 2 / 2.
  expect_equal(participant_2$coverage, 1)

  bin <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 3600),
    sensor = "Accelerometer",
    by = 3600,
    metric = "bin",
    local = TRUE
  ))
  participant_2 <- bin[bin$participant_id == 2, ]
  expect_equal(participant_2$coverage, 1)
})

test_that("spring-forward gap cell is present and empty", {
  db <- timezone_fixture_db()
  on.exit(cleanup_test_db(db), add = TRUE)

  frequency <- collect(coverage_frequency(
    db,
    sensor = "Accelerometer",
    by = 3600,
    local = TRUE
  ))
  participant_3 <- frequency[frequency$participant_id == 3, ]
  expect_equal(format(participant_3$time, "%H:%M:%S"), c("01:00:00", "02:00:00", "03:00:00"))
  expect_equal(participant_3$coverage, c(1, 0, 1))

  count <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 3600),
    sensor = "Accelerometer",
    by = 3600,
    metric = "count",
    local = TRUE
  ))
  participant_3 <- count[count$participant_id == 3, ]
  expect_equal(participant_3$coverage, c(1, 0, 1))

  interval <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 3600),
    sensor = "Accelerometer",
    by = 3600,
    metric = "interval",
    local = TRUE
  ))
  participant_3 <- interval[interval$participant_id == 3, ]
  expect_equal(participant_3$coverage, c(1, NA_real_, 1))

  bin <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 3600),
    sensor = "Accelerometer",
    by = 3600,
    metric = "bin",
    local = TRUE
  ))
  participant_3 <- bin[bin$participant_id == 3, ]
  expect_equal(participant_3$coverage, c(1, NA_real_, 1))
})

test_that("gap cells handle several participants with spring-forward gaps", {
  db <- coverage_db(participants = c("1", "2"))
  on.exit(cleanup_test_db(db), add = TRUE)
  for (participant in c("1", "2")) {
    insert_measurements(
      db,
      "Accelerometer",
      participant,
      as.POSIXct(c("2024-03-31 00:30:00", "2024-03-31 01:30:00"), tz = "UTC"),
      timezone = "Europe/Amsterdam"
    )
  }

  res <- collect(coverage_frequency(
    db,
    sensor = "Accelerometer",
    by = 3600,
    local = TRUE
  ))
  gaps <- res[format(res$time, "%Y-%m-%d %H:%M:%S") == "2024-03-31 02:00:00", ]
  expect_equal(nrow(gaps), 2)
  expect_equal(sort(gaps$participant_id), c(1, 2))
  expect_equal(gaps$coverage, c(0, 0))
})

test_that("missing timezone inherits or falls back to UTC", {
  db <- timezone_fixture_db()
  on.exit(cleanup_test_db(db), add = TRUE)

  # Participant 4: leading NULL falls back to UTC until Amsterdam starts, then
  # the trailing NULLs inherit Amsterdam.
  frequency <- collect(coverage_frequency(
    db,
    sensor = "Accelerometer",
    by = 3600,
    local = TRUE
  ))
  participant_4 <- frequency[frequency$participant_id == 4, ]
  expect_equal(
    format(participant_4$time, "%H:%M:%S"),
    c("08:00:00", "10:00:00", "11:00:00", "12:00:00")
  )
  expect_equal(participant_4$coverage, rep(1, 4))

  all_missing <- coverage_db()
  on.exit(cleanup_test_db(all_missing), add = TRUE)
  insert_measurements(all_missing, "Accelerometer", "12345", "2024-01-01 09:30:00")
  result <- collect(coverage_frequency(
    all_missing,
    "12345",
    sensor = "Accelerometer",
    by = 3600
  ))
  expect_equal(format(result$time, "%H:%M:%S"), "09:00:00")
  expect_equal(result$coverage, 1)
})

test_that("zone transitions land on the exact DST instant", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)

  episodes <- tibble::tibble(
    participant_id = 1L,
    ep_start = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    ep_end = as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
    timezone = "America/New_York"
  )
  transitions <- mpathsenser:::.coverage_zone_transitions(db, episodes)
  expect_equal(
    format(transitions[["America/New_York"]], "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    c("2024-03-10 07:00:00", "2024-11-03 06:00:00")
  )
})

test_that("local bins with a zero-offset timezone stay on the UTC clock", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c("2024-01-01 09:30:00", "2024-01-01 10:15:00", "2024-01-02 09:05:00"),
    timezone = "Africa/Abidjan"
  )

  # Africa/Abidjan never leaves UTC+0, so local bins equal UTC bins and the
  # zero-offset fast path must produce the UTC bins.
  local <- collect(coverage_frequency(
    db,
    "12345",
    sensor = "Accelerometer",
    by = 3600,
    local = TRUE
  ))
  utc <- collect(coverage_frequency(
    db,
    "12345",
    sensor = "Accelerometer",
    by = 3600,
    local = FALSE
  ))
  expect_equal(
    format(local$time, "%Y-%m-%d %H:%M:%S"),
    format(utc$time, "%Y-%m-%d %H:%M:%S")
  )
  expect_equal(local$coverage, utc$coverage)
})

test_that("timezone data with no wall-clock meaning skips the local machinery", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  times <- c(
    "2024-01-01 09:30:00",
    "2024-01-01 10:15:00",
    "2024-01-02 09:05:00",
    "2024-01-02 11:45:00"
  )

  resolved <- function(timezone) {
    insert_measurements(
      db,
      "Accelerometer",
      "12345",
      times,
      timezone = timezone
    )
    validated <- .coverage_validate_frequency(
      db,
      "12345",
      "Accelerometer",
      by = 3600,
      local = TRUE,
      start_date = NULL,
      end_date = NULL,
      week_start = 7
    )
    .coverage_resolve_local(.coverage_normalise_spec(validated))
  }

  # NULL, 'UTC', and empty values all behave as UTC, so the probe must
  # resolve local = TRUE to the UTC path while keeping the request.
  for (timezone in list(NA_character_, "UTC", c("", NA, "UTC", "UTC"))) {
    DBI::dbExecute(db, "DELETE FROM raw.Accelerometer")
    spec <- resolved(timezone)
    expect_false(spec$local)
    expect_true(spec$local_requested)
  }

  # A real zone must keep the episode and segment machinery.
  DBI::dbExecute(db, "DELETE FROM raw.Accelerometer")
  spec <- resolved("Europe/Amsterdam")
  expect_true(spec$local)
  expect_true(spec$local_requested)
})

test_that("non-UTC bins stay local when the timezone probe resolves", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c("2024-01-01 09:30:00", "2024-01-01 10:15:00", "2024-01-02 09:05:00"),
    timezone = "Europe/Amsterdam"
  )

  local <- collect(coverage_frequency(
    db,
    "12345",
    sensor = "Accelerometer",
    by = 3600,
    local = TRUE
  ))
  utc <- collect(coverage_frequency(
    db,
    "12345",
    sensor = "Accelerometer",
    by = 3600,
    local = FALSE
  ))
  # Amsterdam is UTC+1 in January: the observed local bins must read as one
  # hour later wall-clock time than their UTC instants, proving the
  # machinery was not skipped.
  expect_equal(
    format(local$time[local$coverage > 0], "%H:%M:%S"),
    c("10:00:00", "11:00:00", "10:00:00")
  )
})

test_that("the timezone probe ignores dates but respects participants", {
  db <- coverage_db(c("12345", "67890"))
  on.exit(cleanup_test_db(db), add = TRUE)
  # A non-UTC row before the window can be inherited by NULL rows inside it,
  # so the probe must not apply the date filter to the timezone column.
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c("2023-12-31 23:00:00", "2024-01-02 12:00:00"),
    timezone = c("Europe/Amsterdam", NA)
  )
  insert_measurements(
    db,
    "Accelerometer",
    "67890",
    c("2023-12-31 23:00:00", "2024-01-02 12:00:00"),
    timezone = NA_character_
  )

  resolved <- function(participant_id) {
    validated <- .coverage_validate_frequency(
      db,
      participant_id,
      "Accelerometer",
      by = 3600,
      local = TRUE,
      start_date = "2024-01-02",
      end_date = "2024-01-02",
      week_start = 7
    )
    .coverage_resolve_local(.coverage_normalise_spec(validated))
  }
  expect_true(resolved("12345")$local)
  # The probe is scoped to the requested participant, so the other
  # participant's non-UTC rows cannot force the machinery here.
  expect_false(resolved("67890")$local)
})

test_that("the timezone probe respects the Heartbeat secondary-device filter", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Heartbeat",
    "12345",
    c("2024-01-01 09:30:00", "2024-01-01 10:15:00"),
    timezone = "Europe/Amsterdam",
    device_role_name = "Secondary"
  )
  validated <- .coverage_validate_frequency(
    db,
    "12345",
    "Heartbeat",
    by = 3600,
    local = TRUE,
    start_date = NULL,
    end_date = NULL,
    week_start = 7
  )
  spec <- .coverage_resolve_local(.coverage_normalise_spec(validated))
  # Secondary Heartbeat rows are not coverage observations, so their timezone
  # must not keep the local machinery alive.
  expect_false(spec$local)
})

test_that("cycle = day de-duplicates the hour position", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c("2024-01-01 00:00:00", "2024-01-01 10:00:00", "2024-01-02 11:00:00")
  )
  frequency <- coverage_frequency(db, "12345", sensor = "Accelerometer", by = 3600, local = FALSE)

  # Divergence from the old 2.0.0 beta: it added a second ~hour facet for
  # cycle = "day"; the hour position is now the x-axis only.
  built <- ggplot2::ggplot_build(plot(frequency, cycle = "day"))
  expect_equal(nrow(built$layout$layout), 1)
  expect_equal(built$plot$labels$x, "hour")

  # The same de-duplication applies to proportional metrics with a sub-day bin.
  interval <- coverage_proportional(
    db,
    expected = c(Accelerometer = 3600),
    sensor = "Accelerometer",
    by = 3600,
    metric = "interval",
    local = FALSE
  )
  expect_equal(
    nrow(ggplot2::ggplot_build(plot(interval, cycle = "day"))$layout$layout),
    1
  )
})

test_that("expected names are case-insensitive in coverage_proportional", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(db, "Accelerometer", "12345", "2024-01-01 00:00:00")

  res <- collect(coverage_proportional(
    db,
    expected = c(accelerometer = 60),
    local = FALSE
  ))
  expect_equal(as.character(unique(res$sensor)), "Accelerometer")
  # Divergence: the old implementation accepted the mixed-case duplicate.
  expect_error(
    coverage_proportional(
      db,
      expected = c(Accelerometer = 60, accelerometer = 70),
      local = FALSE
    ),
    "duplicate sensor names"
  )
})

test_that("interval and bin stop at the last observation before a DST gap", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    "2024-03-10 06:30:00",
    timezone = "America/New_York"
  )

  interval <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 3600),
    sensor = "Accelerometer",
    by = 3600,
    metric = "interval",
    local = TRUE
  ))
  expect_equal(format(interval$time, "%H:%M:%S"), "01:00:00")
  expect_equal(interval$coverage, 1)

  bin <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 3600),
    sensor = "Accelerometer",
    by = 3600,
    metric = "bin",
    local = TRUE
  ))
  expect_equal(format(bin$time, "%H:%M:%S"), "01:00:00")
  expect_equal(bin$coverage, 1)
})

test_that("gap cells use the DuckDB time_bucket origin for fixed bins", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    as.POSIXct(c("2024-03-31 00:30:00", "2024-03-31 01:30:00"), tz = "UTC"),
    timezone = "Europe/Amsterdam"
  )

  res <- collect(coverage_frequency(
    db,
    sensor = "Accelerometer",
    by = 420,
    local = TRUE
  ))
  expect_equal(
    format(res$time, "%H:%M:%S"),
    c(
      "01:29:00",
      "01:36:00",
      "01:43:00",
      "01:50:00",
      "01:57:00",
      "02:04:00",
      "02:11:00",
      "02:18:00",
      "02:25:00",
      "02:32:00",
      "02:39:00",
      "02:46:00",
      "02:53:00",
      "03:00:00",
      "03:07:00",
      "03:14:00",
      "03:21:00",
      "03:28:00"
    )
  )
  expect_equal(res$coverage, c(1, 0, 0, 0, 0, rep(0, 8), 0, 0, 0, 0, 1))
})

test_that("batched offset lookups stay aligned with their input rows", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  times <- as.POSIXct(
    c(
      "2024-01-15 09:00:00",
      "2024-03-10 06:30:00",
      "2024-01-15 00:00:00",
      "2024-10-27 01:30:00"
    ),
    tz = "UTC"
  )
  zones <- c("Europe/Amsterdam", "America/New_York", "UTC", "Europe/Amsterdam")
  offsets <- mpathsenser:::.coverage_offsets_at(db, zones, times)$offset_seconds
  expect_equal(offsets, c(3600, -18000, 0, 3600))
})

test_that("sensor view suffixes are accepted case-insensitively", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(db, "Accelerometer", "12345", "2024-01-01 00:00:00")

  res <- collect(coverage_frequency(
    db,
    sensor = "Accelerometer_WITH_LOCAL",
    by = 3600,
    local = TRUE
  ))
  expect_equal(as.character(unique(res$sensor)), "Accelerometer")
})

test_that("a DST transition exactly at the episode end has no extra tail", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  # The terminal episode is extended to max(T) + E = 07:00Z, exactly the
  # spring-forward instant, so the post-transition tail has zero duration.
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    "2024-03-10 06:00:00",
    timezone = "America/New_York"
  )

  for (metric in c("interval", "bin")) {
    res <- collect(coverage_proportional(
      db,
      expected = c(Accelerometer = 3600),
      sensor = "Accelerometer",
      by = 3600,
      metric = metric,
      local = TRUE
    ))
    expect_equal(format(res$time, "%H:%M:%S"), "01:00:00")
    expect_equal(res$coverage, 1)
  }
})

test_that("empty date-filter results error identically in UTC and local mode", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    "2024-01-15 23:30:00",
    timezone = "Europe/Amsterdam"
  )

  for (local in c(TRUE, FALSE)) {
    expect_error(
      collect(coverage_frequency(
        db,
        "12345",
        sensor = "Accelerometer",
        by = 3600,
        local = local,
        start_date = "2030-01-01",
        end_date = "2030-01-01"
      )),
      "No observations found"
    )
  }
})

test_that("conflicting timezone values at one instant follow the current tie-break", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)

  # When rows share a UTC instant and disagree on the timezone, the largest
  # timezone name wins (here Europe/Amsterdam over America/New_York), so the
  # episode boundaries are deterministic.
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    as.POSIXct(c("2024-01-15 12:00:00", "2024-01-15 12:00:00"), tz = "UTC"),
    timezone = c("America/New_York", "Europe/Amsterdam")
  )
  res <- collect(coverage_frequency(
    db,
    sensor = "Accelerometer",
    by = 3600,
    local = TRUE
  ))
  expect_equal(format(res$time, "%H:%M:%S"), "13:00:00")
  expect_equal(res$coverage, 1)
})

# -- additional hand-checked cases -----------------------------------------------

test_that("coverage_frequency reports every selected sensor", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(db, "Accelerometer", "12345", "2024-01-01 00:00:00")

  expect_warning(
    res <- collect(coverage_frequency(db, by = 3600, local = FALSE)),
    "Operating system is unknown"
  )
  expect_equal(nrow(res), length(sensors))
  expect_equal(sum(res$coverage), 1)
})

test_that("coverage_frequency keeps duplicates and single instants", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(db, "Accelerometer", "12345", "2024-01-01 00:00:00")
  insert_measurements(
    db,
    "Battery",
    "12345",
    c("2024-01-01 00:00:00", "2024-01-01 00:00:05", "2024-01-01 00:00:05", "2024-01-01 00:01:00")
  )

  single <- collect(coverage_frequency(
    db,
    "12345",
    sensor = "Accelerometer",
    by = 60,
    local = FALSE
  ))
  expect_equal(single$coverage, 1)

  oversampled <- collect(coverage_frequency(
    db,
    "12345",
    sensor = "Battery",
    by = 60,
    local = FALSE
  ))
  expect_equal(
    oversampled$time,
    as.POSIXct(c("2024-01-01 00:00:00", "2024-01-01 00:01:00"), tz = "UTC")
  )
  expect_equal(oversampled$coverage, c(2, 1))
})

test_that("interval unions nested observations with the running maximum", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(db, "Accelerometer", "12345", c("2024-01-01 00:00:00", "2024-01-01 00:00:02"))

  res <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 10),
    sensor = "Accelerometer",
    by = 10,
    metric = "interval",
    local = FALSE
  ))
  expect_equal(format(res$time, "%H:%M:%S"), "00:00:00")
  expect_equal(res$coverage, 1)
})

test_that("proportional endpoints are one bin for a single instant", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(db, "Accelerometer", "12345", "2024-01-01 00:00:00")

  for (metric in c("count", "interval", "bin")) {
    res <- collect(coverage_proportional(
      db,
      expected = c(Accelerometer = 60),
      sensor = "Accelerometer",
      metric = metric,
      local = FALSE
    ))
    expect_equal(nrow(res), 1, info = metric)
    expect_equal(res$coverage, 1, info = metric)
  }
})

test_that("interval and bin stop at the last measurement's bin", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(db, "Accelerometer", "12345", "2024-01-01 00:00:30")

  # The expected window reaches 00:01:30, but the observation's own bin is
  # 00:00, so the trailing window is folded into that bin.
  for (metric in c("interval", "bin")) {
    res <- collect(coverage_proportional(
      db,
      expected = c(Accelerometer = 60),
      sensor = "Accelerometer",
      metric = metric,
      local = FALSE
    ))
    expect_equal(format(res$time, "%H:%M:%S"), "00:00:00", info = metric)
    expect_equal(res$coverage, 1, info = metric)
  }

  # Daily observations at noon: the last row is the last measured day, not the
  # day the final expected window reaches into.
  daily_db <- coverage_db()
  on.exit(cleanup_test_db(daily_db), add = TRUE)
  insert_measurements(
    daily_db,
    "Accelerometer",
    "12345",
    as.POSIXct("2024-01-01 12:00:00", tz = "UTC") + seq(0, by = 86400, length.out = 3)
  )
  for (metric in c("interval", "bin")) {
    res <- collect(coverage_proportional(
      daily_db,
      expected = c(Accelerometer = 86400),
      sensor = "Accelerometer",
      by = 86400,
      metric = metric,
      local = FALSE
    ))
    expect_equal(
      format(res$time, "%Y-%m-%d"),
      c("2024-01-01", "2024-01-02", "2024-01-03"),
      info = metric
    )
    expect_equal(res$coverage, c(1, 1, 1), info = metric)
  }
})

test_that("collect rounds, factorises, and keeps the coverage metadata", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(db, "Accelerometer", "12345", "2024-01-01 00:00:00")
  insert_measurements(db, "Battery", "12345", c("2024-01-01 00:00:30", "2024-01-01 00:01:30"))

  res <- collect(coverage_frequency(
    db,
    "12345",
    sensor = c("Accelerometer", "Battery"),
    by = "minute",
    local = FALSE
  ))

  expect_s3_class(res, "coverage")
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("participant_id", "time", "sensor", "coverage"))
  expect_identical(levels(res$sensor), c("Battery", "Accelerometer"))
  expect_equal(res$coverage, round(res$coverage, 2))
  expect_equal(attr(res, "coverage_type"), "frequency")
  expect_equal(attr(res, "by"), c(Accelerometer = "minute", Battery = "minute"))
  expect_null(attr(res, "expected"))
  expect_null(attr(res, "metric"))
  expect_equal(attr(res, "participant_id"), "12345")
  expect_false(attr(res, "local"))
  # Collecting an already-collected result must not stack the class.
  expect_identical(class(collect(res)), class(res))
})

test_that("coverage output names the sensor column", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(db, "Battery", "12345", "2024-01-01 00:00:00")
  insert_measurements(db, "Accelerometer", "12345", "2024-01-01 00:00:00")

  res <- collect(coverage_frequency(
    db,
    "12345",
    sensor = c("Battery", "Accelerometer")
  ))
  expect_named(res, c("participant_id", "time", "sensor", "coverage"))
  expect_identical(levels(res$sensor), c("Battery", "Accelerometer"))
})

test_that("multiple participants are averaged by default and can be faceted", {
  db <- coverage_db(participants = c("1", "2"))
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "1",
    c("2024-01-01 10:00:00", "2024-01-02 10:00:00")
  )
  insert_measurements(
    db,
    "Accelerometer",
    "2",
    c(
      "2024-01-01 10:00:00",
      "2024-01-01 10:01:00",
      "2024-01-01 10:02:00",
      "2024-01-02 10:00:00",
      "2024-01-02 10:01:00",
      "2024-01-02 10:02:00"
    )
  )

  frequency <- coverage_frequency(
    db,
    sensor = "Accelerometer",
    by = "hour",
    local = FALSE
  )
  averaged <- plot(frequency, cycle = "day")
  faceted <- plot(frequency, cycle = "day", aggregate = NULL)

  expect_equal(nrow(averaged$data), 24)
  expect_equal(averaged$data$coverage[averaged$data$hour == 10], 2)
  expect_equal(averaged$labels$title, "Coverage across participants")
  expect_equal(nrow(faceted$data), 48)
  expect_equal(nrow(ggplot2::ggplot_build(faceted)$layout$layout), 2)
  expect_equal(faceted$labels$title, "Coverage per participant")
})

test_that("bar plots show participant lanes per sensor", {
  db <- coverage_db(participants = c("1", "2"))
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "1",
    c("2024-01-01 00:00:00", "2024-01-01 10:00:00", "2024-01-01 23:00:00")
  )
  insert_measurements(
    db,
    "Accelerometer",
    "2",
    c(
      "2024-01-01 00:00:00",
      "2024-01-01 10:00:00",
      "2024-01-01 10:01:00",
      "2024-01-01 10:02:00",
      "2024-01-01 23:00:00"
    )
  )
  insert_measurements(
    db,
    "Battery",
    "1",
    c("2024-01-01 00:00:00", "2024-01-01 23:00:00")
  )
  insert_measurements(
    db,
    "Battery",
    "2",
    c("2024-01-01 00:00:00", "2024-01-01 23:00:00")
  )

  frequency <- coverage_frequency(
    db,
    sensor = c("Accelerometer", "Battery"),
    by = "hour",
    local = FALSE
  )
  bar <- plot(frequency, cycle = "day", type = "bar")
  averaged <- plot(frequency, cycle = "day", type = "bar", aggregate = "sensor")

  expect_equal(bar$labels$y, "Participant")
  expect_equal(bar$labels$fill, "Scaled count")
  expect_equal(nrow(ggplot2::ggplot_build(bar)$layout$layout), 2)
  expect_equal(nrow(bar$data), 2 * 2 * 24)
  expect_equal(levels(bar$data$participant_id), c("2", "1"))
  expect_equal(
    bar$data$scaled_coverage[
      bar$data$sensor == "Accelerometer" &
        bar$data$participant_id == "1" &
        bar$data$hour == 10
    ],
    1 / 3
  )
  expect_equal(nrow(averaged$data), 2 * 24)
  expect_equal(averaged$labels$fill, "Mean scaled count")

  interval <- coverage_proportional(
    db,
    expected = c(Accelerometer = 3600, Battery = 3600),
    sensor = c("Accelerometer", "Battery"),
    by = "hour",
    metric = "interval",
    local = FALSE
  )
  proportional <- plot(interval, cycle = "day", type = "bar")
  proportional_averaged <- plot(
    interval,
    cycle = "day",
    type = "bar",
    aggregate = "sensor"
  )
  expect_equal(proportional$labels$fill, "Proportion")
  expect_equal(nrow(proportional_averaged$data), 2 * 24)

  expect_error(
    plot(frequency, type = "bar", aggregate = "participant_id"),
    "cannot aggregate across participants"
  )
  expect_error(
    plot(frequency, type = "heatmap", aggregate = "sensor"),
    "only supported for type"
  )
})

test_that("averaging keeps all-NA sensor coverage missing", {
  db <- coverage_db(participants = c("1", "2"))
  on.exit(cleanup_test_db(db), add = TRUE)
  for (participant in c("1", "2")) {
    insert_measurements(
      db,
      "Device",
      participant,
      "2024-01-01 00:00:00",
      platform = "iOS"
    )
    insert_measurements(
      db,
      "Screen",
      participant,
      c("2024-01-01 00:00:00", "2024-01-01 23:00:00")
    )
  }

  frequency <- coverage_frequency(
    db,
    sensor = "Screen",
    by = "hour",
    local = FALSE
  )
  averaged <- plot(frequency, cycle = "day")
  expect_true(all(is.na(averaged$data$coverage)))
})

test_that("plots support the line and heatmap types", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(
    db,
    "Accelerometer",
    "12345",
    c("2024-01-01 00:00:00", "2024-01-01 10:00:00", "2024-01-02 11:00:00", "2024-01-03 00:00:00")
  )
  frequency <- coverage_frequency(db, "12345", sensor = "Accelerometer", by = 3600, local = FALSE)

  series <- plot(frequency)
  heatmap <- plot(frequency, type = "heatmap")
  expect_true(any(vapply(series$layers, \(l) inherits(l$geom, "GeomLine"), logical(1))))
  expect_true(any(vapply(heatmap$layers, \(l) inherits(l$geom, "GeomTile"), logical(1))))
  expect_equal(series$labels$x, "Time")
  expect_equal(series$labels$y, "Measurements")
  expect_equal(heatmap$labels$fill, "Scaled count")
  expect_equal(max(heatmap$data$scaled_coverage), 1)

  count <- coverage_proportional(
    db,
    expected = c(Accelerometer = 3600),
    sensor = "Accelerometer",
    by = 3600,
    metric = "count",
    local = FALSE
  )
  interval <- coverage_proportional(
    db,
    expected = c(Accelerometer = 3600),
    sensor = "Accelerometer",
    by = 3600,
    metric = "interval",
    local = FALSE
  )
  bin <- coverage_proportional(
    db,
    expected = c(Accelerometer = 3600),
    sensor = "Accelerometer",
    by = 3600,
    metric = "bin",
    local = FALSE
  )
  expect_equal(plot(count)$labels$y, "Proportion of expected measurements")
  expect_equal(plot(interval)$labels$y, "Proportion of time covered")
  expect_equal(
    plot(bin, cycle = NULL, type = "line")$labels$y,
    "Proportion of occupied expected slots"
  )
  expect_equal(plot(count, cycle = "day")$labels$fill, "Proportion")
  expect_error(plot(frequency, type = "scatter"), "should be one of")
})
