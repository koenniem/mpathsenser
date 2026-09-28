coverage_db <- function(participants = "12345") {
  db <- create_db(NULL, tempfile(fileext = ".duckdb"), shared_home = FALSE)
  DBI::dbExecute(
    db,
    "INSERT INTO Study (study_id, data_format) VALUES ('foo', NULL)"
  )
  for (participant in participants) {
    DBI::dbExecute(
      db,
      sprintf(
        "INSERT INTO Participant (participant_id, study_id) VALUES (%s, 'foo')",
        as.character(participant)
      )
    )
  }
  db
}

insert_measurements <- function(db, sensor, participant_id, time, ...) {
  n <- length(time)
  extra <- list(...)
  data <- data.frame(
    participant_id = rep(participant_id, length.out = n),
    time = as.POSIXct(time, tz = "UTC"),
    timezone = NA_character_,
    source_file_id = 1,
    source_row_id = seq_len(n),
    source_measurement_id = 1
  )
  for (name in names(extra)) {
    data[[name]] <- extra[[name]]
  }
  DBI::dbWriteTable(
    db,
    DBI::Id(schema = "raw", table = sensor),
    data,
    append = TRUE
  )
  invisible(db)
}

test_that("coverage_expected returns overridable sensor intervals", {
  expected <- coverage_expected()

  expect_length(expected, 14)
  expect_named(expected)
  expect_equal(unname(expected["Accelerometer"]), 120)
  expect_equal(unname(expected["Weather"]), 120)
  expect_equal(unname(coverage_expected(Accelerometer = 60)["Accelerometer"]), 60)
  expect_error(coverage_expected(Weather = -1), "positive, finite")
  expect_error(coverage_expected(Weather = NA_real_), "positive, finite")
})

test_that("coverage functions validate their inputs", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)

  expect_error(coverage_frequency(db, sensor = "Accelerometer", by = NULL), "cannot be.*NULL")
  expect_error(coverage_frequency(db, sensor = "Accelerometer", by = 0), "positive, finite")
  expect_error(coverage_frequency(db, sensor = "Accelerometer", by = "fortnight"), "should be one of")
  expect_error(coverage_frequency(db, sensor = "Accelerometer", week_start = 8), "whole number between")
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
  expect_named(res, c("participant_id", "time", "measure", "coverage"))
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
  expect_equal(p1$coverage[p1$measure == "Battery"], c(1, 0, 0))
  expect_equal(p1$coverage[p1$measure == "Accelerometer"], c(1, 0, 1))
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

  expect_equal(nrow(res[res$measure == "Accelerometer", ]), 3)
  expect_equal(nrow(res[res$measure == "Battery", ]), 2)
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
  expect_equal(wide$coverage[wide$measure == "Accelerometer"], 1.5)
  expect_equal(wide$coverage[wide$measure == "Battery"], 2)
})

test_that("coverage_proportional interval unions overlaps and splits by bin", {
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

  expect_equal(nrow(res), 2)
  expect_equal(res$coverage, c(1, 1))
  expect_lte(max(res$coverage), 1)
  expect_equal(attr(res, "metric"), "interval")

  default_bins <- collect(coverage_proportional(
    db,
    expected = c(Accelerometer = 10),
    sensor = "Accelerometer",
    metric = "interval",
    local = FALSE
  ))
  expect_equal(nrow(default_bins), 2)
  expect_equal(default_bins$coverage, c(1, 1))
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
    for (measure in names(expected)) {
      times <- sort(as.numeric(result$time[result$measure == measure]))
      expect_gte(length(times), 3)
      expect_equal(
        diff(times),
        rep(unname(expected[[measure]]), length(times) - 1)
      )
    }
    if (identical(metric, "count")) {
      expect_s3_class(plot(result, cycle = "hour"), "ggplot")
    }
  }

  expect_no_warning(default_bins <- coverage_proportional(
    db,
    expected = expected,
    sensor = names(expected),
    local = FALSE
  ))
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

test_that("calendar month bins widen when a month can be shorter than expected", {
  db <- coverage_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  insert_measurements(db, "Accelerometer", "12345", "2024-01-01 00:00:00")

  expect_warning(
    result <- collect(coverage_proportional(
      db,
      expected = c(Accelerometer = 30 * 86400),
      sensor = "Accelerometer",
      by = "month",
      local = FALSE
    )),
    "shortest month: 28 days"
  )
  expect_equal(attr(result, "by"), c(Accelerometer = 30 * 86400))
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
  expect_equal(res$coverage[res$measure == "Accelerometer"], c(1, 0, 1))
  expect_equal(res$coverage[res$measure == "Battery"], c(0, 1, 1))
  expect_equal(res$coverage[res$measure == "Weather"], c(0, 0, 0))
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
  expect_equal(as.character(unique(res$measure)), "Accelerometer")

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
  expect_equal(
    local_span$time,
    as.POSIXct(
      c(
        "2024-01-01 06:00:00",
        "2024-01-01 07:00:00",
        "2024-01-01 08:00:00",
        "2024-01-01 09:00:00",
        "2024-01-01 10:00:00"
      ),
      tz = "UTC"
    )
  )
  expect_equal(local_span$coverage, c(1, 0, 0, 0, 1))

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
  ios <- res[res$participant_id == 1 & res$measure %in% c("AppUsage", "Light", "Memory", "Screen"), ]
  android <- res[res$participant_id == 2 & res$measure %in% c("AppUsage", "Light", "Memory", "Screen"), ]

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
  expect_equal(unknown_res$coverage[unknown_res$measure == "AppUsage"], 0)
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
      .by = c("participant_id", "measure", "date", "hour")
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
  expect_setequal(as.character(week_plot$data$day_of_week), c("Mon", "Tue", "Wed"))
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
