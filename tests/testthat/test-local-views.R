test_that("create_db creates local views for every physical sensor table", {
  db <- create_db(NULL, ":memory:")
  on.exit(close_db(db), add = TRUE)

  # Physical sensor tables live in the raw schema; main holds the metadata
  # tables, the main.<sensor> views, and the _local/_with_local views.
  raw_tables <- DBI::dbGetQuery(
    db,
    "SELECT table_name
     FROM information_schema.tables
     WHERE table_schema = 'raw' AND table_type = 'BASE TABLE'"
  )$table_name
  main_tables <- DBI::dbGetQuery(
    db,
    "SELECT table_name
     FROM information_schema.tables
     WHERE table_schema = 'main' AND table_type = 'BASE TABLE'"
  )$table_name
  views <- DBI::dbGetQuery(
    db,
    "SELECT table_name
     FROM information_schema.tables
     WHERE table_schema = 'main' AND table_type = 'VIEW'"
  )$table_name

  expect_setequal(main_tables, c("Study", "Participant", "ProcessedFiles"))
  expect_setequal(raw_tables, mpathsenser::sensors)
  expect_setequal(
    views,
    c(sensors, paste0(raw_tables, "_local"), paste0(raw_tables, "_with_local"))
  )

  # None of the main views (base, _local, or _with_local) may expose the
  # internal provenance columns; they only live in the raw physical tables.
  leaks <- DBI::dbGetQuery(
    db,
    "SELECT DISTINCT table_name
     FROM information_schema.columns
     WHERE table_schema = 'main'
       AND column_name IN ('source_file_id', 'source_row_id', 'source_measurement_id')"
  )$table_name
  expect_length(leaks, 0)

  raw_cols <- DBI::dbGetQuery(
    db,
    "SELECT column_name FROM information_schema.columns
     WHERE table_schema = 'raw' AND table_name = 'Accelerometer'
     ORDER BY ordinal_position"
  )$column_name
  expect_true(all(c("source_file_id", "source_row_id", "source_measurement_id") %in% raw_cols))

  DBI::dbExecute(
    db,
    "INSERT INTO raw.Timezone (participant_id, time, timezone, source_file_id, source_row_id, source_measurement_id)
     VALUES ('1', '2025-01-01 00:00:00+00', 'Europe/Brussels', 1, 1, 1)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO raw.Accelerometer (participant_id, time, source_file_id, source_row_id, source_measurement_id)
     VALUES ('1', '2025-01-15 11:00:00+00', 1, 1, 1)"
  )
  add_timezones_to_db(db, sensors = "Accelerometer", .progress = FALSE)
  result <- dplyr::tbl(db, "Accelerometer") |>
    dplyr::mutate(time = to_local_time(time, timezone)) |>
    dplyr::collect()
  expect_equal(format(result$time, tz = "UTC"), "2025-01-15 12:00:00")

  view_result <- get_data(db, "Accelerometer_with_local") |>
    dplyr::collect()
  expect_equal(format(view_result$time_local, tz = "UTC"), "2025-01-15 12:00:00")
  expect_equal(nrow(collect_local(get_data(db, "Accelerometer_with_local"))), 1L)
  expect_equal(
    format(collect_local(get_data(db, "Accelerometer"))$time, tz = "UTC"),
    "2025-01-15 12:00:00"
  )
})

test_that("legacy local wall-clock timestamps are not shifted by the views", {
  db <- create_db(NULL, ":memory:")
  on.exit(close_db(db), add = TRUE)

  DBI::dbExecute(
    db,
    "INSERT INTO Study (study_id) VALUES ('s')"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO Participant (participant_id, study_id) VALUES ('1', 's')"
  )
  # sense_version 5 (<= 6): period_start was stored as a local wall-clock value.
  DBI::dbExecute(
    db,
    "INSERT INTO ProcessedFiles (file_id, file_name, participant_id, sense_version)
     VALUES (1, 'a.json', '1', 5)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO raw.AppUsage (
       participant_id, time, period_start, timezone,
       source_file_id, source_row_id, source_measurement_id
     )
     VALUES ('1', '2025-01-15 11:00:00+00', '2025-01-15 12:00:00+00', 'Europe/Brussels', 1, 1, 1)"
  )

  # time shifts to local (11:00 UTC -> 12:00 Brussels); period_start keeps its
  # historical wall-clock value (12:00) rather than being shifted again.
  view <- DBI::dbGetQuery(
    db,
    "SELECT time, period_start, time_local, period_start_local FROM AppUsage_with_local"
  )
  expect_equal(format(view$time_local, tz = "UTC"), "2025-01-15 12:00:00")
  expect_equal(format(view$period_start_local, tz = "UTC"), "2025-01-15 12:00:00")
  # The canonical table still holds the (UTC-masquerading) original value.
  expect_equal(format(view$period_start, tz = "UTC"), "2025-01-15 12:00:00")
})

test_that("open_db reopens a database read-only without recreating views", {
  path <- tempfile("mpathsenser_views", fileext = ".db")
  db <- create_db(path)
  DBI::dbExecute(db, "INSERT INTO Study (study_id) VALUES ('s')")
  DBI::dbExecute(db, "INSERT INTO Participant (participant_id, study_id) VALUES ('1', 's')")
  close_db(db)
  on.exit(unlink(path), add = TRUE)

  dbro <- open_db(path, read_only = TRUE)
  on.exit(close_db(dbro), add = TRUE)
  expect_true(DBI::dbIsValid(dbro))
  expect_equal(
    nrow(DBI::dbGetQuery(dbro, "SELECT * FROM AppUsage_local WHERE FALSE")),
    0L
  )
})
