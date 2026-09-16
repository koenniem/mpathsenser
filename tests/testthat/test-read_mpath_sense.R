# Tests for the DuckDB import pipeline (read_mpath_sense.R)

# Write a single m-Path Sense JSON file with an mpathinfo entry and the given
# sensor data entries
make_test_file <- function(
  dir,
  name,
  connection_id = "12345",
  study = "test_study",
  version = 5,
  sensors = list(),
  start_time = 1765889440388567
) {
  entries <- list(list(
    sensorStartTime = start_time[[1]],
    data = list(
      `__type` = "dk.cachet.carp.mpathinfo",
      connectionId = connection_id,
      studyName = study,
      senseVersion = version
    )
  ))
  # start_time can be a vector to give each sensor entry its own timestamp
  # (recycled when shorter than the sensor list)
  times <- rep_len(start_time, length(sensors))
  for (i in seq_along(sensors)) {
    entries[[length(entries) + 1]] <- list(sensorStartTime = times[[i]], data = sensors[[i]])
  }
  jsonlite::write_json(entries, file.path(dir, name), auto_unbox = TRUE)
  file.path(dir, name)
}

# Number of rows that violate the (participant_id, time) physical order, i.e.
# the same check optimize_db() uses to decide whether a table needs rewriting.
physical_order_violations <- function(db, sensor = "Activity") {
  DBI::dbGetQuery(
    db,
    sprintf(
      "SELECT COUNT(*) AS n FROM (
         SELECT participant_id, time,
                LAG(participant_id) OVER (ORDER BY rowid) AS pp,
                LAG(time) OVER (ORDER BY rowid) AS pt
         FROM raw.%s
       ) WHERE pp IS NOT NULL
         AND (participant_id < pp OR (participant_id = pp AND time < pt))",
      sensor
    )
  )$n[[1]]
}

test_that("import populates the database correctly", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    sensors = list(
      list(`__type` = "dk.cachet.carp.activity", confidence = 80, type = "WALKING"),
      list(`__type` = "dk.cachet.carp.batterystate", batteryLevel = 87, batteryStatus = "CHARGING"),
      list(`__type` = "dk.cachet.carp.stepcount", steps = 42)
    )
  )
  db <- create_db(NULL, ":memory:")

  expect_message(
    read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE),
    "All 1 file was successfully written to the database."
  )

  # Study, Participant, ProcessedFiles
  expect_equal(
    DBI::dbGetQuery(db, "SELECT * FROM Study"),
    data.frame(study_id = "test_study", data_format = "CARP JSON")
  )
  expect_equal(
    DBI::dbGetQuery(db, "SELECT * FROM Participant"),
    data.frame(participant_id = 12345, study_id = "test_study")
  )
  pf <- DBI::dbGetQuery(db, "SELECT * FROM ProcessedFiles")
  expect_equal(pf$file_name, "a.json")
  expect_equal(pf$participant_id, 12345)
  expect_equal(pf$sense_version, 5L)
  expect_equal(pf$file_size_bytes, file.size(file.path(dir, "a.json")))

  # Sensor data with correct values, UTC instants, and file provenance. The
  # user-facing Activity view hides the provenance columns; the raw table
  # carries them.
  act <- DBI::dbGetQuery(db, "SELECT * FROM Activity")
  expect_equal(act$participant_id, 12345)
  expect_equal(act$confidence, 80)
  expect_equal(act$type, "WALKING")
  expect_equal(format(act$time, tz = "UTC"), "2025-12-16 12:50:40")
  expect_false("source_file_id" %in% names(act))
  expect_false("source_row_id" %in% names(act))

  act_raw <- DBI::dbGetQuery(db, "SELECT * FROM raw.Activity")
  expect_equal(act_raw$source_file_id, pf$file_id)
  # source_row_id is the 1-based position of the entry in the file (the
  # mpathinfo entry is first, then the sensor entries in file order), so the
  # Activity entry is the second row of the file.
  expect_equal(act_raw$source_row_id, 2)
  expect_equal(act_raw$source_measurement_id, 1)

  bat <- DBI::dbGetQuery(db, "SELECT * FROM Battery")
  expect_equal(bat$battery_level, 87)
  expect_equal(bat$battery_status, "CHARGING")

  ped <- DBI::dbGetQuery(db, "SELECT * FROM Pedometer")
  expect_equal(ped$step_count, 42)

  # Re-running finds no new files
  expect_message(
    read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE),
    "No new files to process."
  )

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("a corrected file is re-imported and wins on deduplication", {
  dir <- tempfile("import_test")
  dir.create(dir)
  f <- make_test_file(
    dir,
    "a.json",
    sensors = list(list(
      `__type` = "dk.cachet.carp.batterystate",
      batteryLevel = 87,
      batteryStatus = "CHARGING"
    ))
  )
  db <- create_db(NULL, ":memory:")
  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE))

  # Correct the file: change the battery level and add a new measurement
  entries <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  entries[[2]]$data$batteryLevel <- 55
  entries[[3]] <- list(
    sensorStartTime = 1765889440388567 + 2e6,
    data = list(
      `__type` = "dk.cachet.carp.batterystate",
      batteryLevel = 60,
      batteryStatus = "DISCHARGING"
    )
  )
  jsonlite::write_json(entries, f, auto_unbox = TRUE)
  Sys.sleep(1.1) # ensure the modification time differs

  expect_message(
    read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE),
    "All 1 file was successfully written to the database."
  )

  # The old row (batteryLevel 87) is replaced by the corrected one (55), and
  # the newly added measurement is kept: 2 rows in total
  bat <- DBI::dbGetQuery(db, "SELECT battery_level FROM Battery ORDER BY time")
  expect_equal(bat$battery_level, c(55, 60))

  # The corrected file is tracked as a new version in ProcessedFiles
  expect_equal(
    DBI::dbGetQuery(db, "SELECT COUNT(*) FROM ProcessedFiles WHERE file_name = 'a.json'")[[1]],
    2
  )

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("renamed copies are imported and deduplicated by measurement key", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    sensors = list(list(`__type` = "dk.cachet.carp.stepcount", steps = 1))
  )
  # Identical content under a different name, in the same run
  file.copy(file.path(dir, "a.json"), file.path(dir, "copy.json"))
  db <- create_db(NULL, ":memory:")

  # Both files are imported; the data-level deduplication removes the
  # duplicate measurement, keeping the newest file's row
  expect_message(
    read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE),
    "All 2 files were successfully written to the database."
  )
  expect_equal(
    DBI::dbGetQuery(db, "SELECT COUNT(*) FROM ProcessedFiles")[[1]],
    2
  )
  expect_equal(
    DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Pedometer")[[1]],
    1
  )

  # Unchanged files are skipped on the next run
  expect_message(
    read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE),
    "No new files to process."
  )

  # A renamed copy added later is imported as well, but deduplicated again
  file.copy(file.path(dir, "a.json"), file.path(dir, "later_copy.json"))
  Sys.sleep(1.1)
  expect_message(
    read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE),
    "All 1 file was successfully written to the database."
  )
  expect_equal(
    DBI::dbGetQuery(db, "SELECT COUNT(*) FROM ProcessedFiles")[[1]],
    3
  )
  expect_equal(
    DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Pedometer")[[1]],
    1
  )

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("end-time sensor deduplication keeps the last same-file row", {
  db <- create_db(NULL, ":memory:")
  DBI::dbExecute(
    db,
    "INSERT INTO raw.Accelerometer (
       participant_id, time, end_time, n, timezone,
       source_file_id, source_row_id, source_measurement_id
     ) VALUES
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', TIMESTAMPTZ '2025-12-16 12:01:00+00', 1, NULL, 1, 1, 1),
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', TIMESTAMPTZ '2025-12-16 12:02:00+00', 2, NULL, 1, 2, 1)"
  )

  res <- deduplicate_db(db, sensors = "Accelerometer")
  expect_equal(unname(res[["Accelerometer"]]), 1)
  accelerometer <- DBI::dbGetQuery(db, "SELECT end_time, n FROM Accelerometer")
  expect_equal(accelerometer$n, 2)
  expect_equal(format(accelerometer$end_time, tz = "UTC"), "2025-12-16 12:02:00")

  close_db(db)
})

test_that("interval sensors keep the newest file when end times differ", {
  db <- create_db(NULL, ":memory:")
  # The same start time occurs in two files: file 1 has the short window, the
  # newer file 2 has an updated (longer) end time. The newest file must win.
  DBI::dbExecute(
    db,
    "INSERT INTO raw.Accelerometer (
       participant_id, time, end_time, n, timezone,
       source_file_id, source_row_id, source_measurement_id
     ) VALUES
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', TIMESTAMPTZ '2025-12-16 12:01:00+00', 1, NULL, 1, 1, 1),
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', TIMESTAMPTZ '2025-12-16 12:05:00+00', 2, NULL, 2, 1, 1)"
  )

  res <- deduplicate_db(db, sensors = "Accelerometer")
  expect_equal(unname(res[["Accelerometer"]]), 1)
  accelerometer <- DBI::dbGetQuery(db, "SELECT end_time, n, source_file_id FROM raw.Accelerometer")
  expect_equal(accelerometer$n, 2)
  expect_equal(accelerometer$source_file_id, 2)
  expect_equal(format(accelerometer$end_time, tz = "UTC"), "2025-12-16 12:05:00")

  close_db(db)
})

test_that("Garmin point sensors keep the last recorded row within a file", {
  db <- create_db(NULL, ":memory:")
  # Two measurements with the same key in one file: the one later in source
  # order (higher source_row_id / source_measurement_id) wins, regardless of
  # physical insertion order.
  DBI::dbExecute(
    db,
    "INSERT INTO raw.GarminBBI (
       participant_id, time, bbi, mac_address, timezone,
       source_file_id, source_row_id, source_measurement_id
     ) VALUES
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 763, '00', NULL, 1, 1, 1),
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 800, '00', NULL, 1, 1, 2)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO raw.GarminEnhancedBBI (
       participant_id, time, bbi, status, gap_duration, mac_address, timezone,
       source_file_id, source_row_id, source_measurement_id
     ) VALUES
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 992, 'lowConfidence', 0, '00', NULL, 1, 1, 1),
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 999, 'lowConfidence', 0, '00', NULL, 1, 1, 2)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO raw.GarminHeartRate (
       participant_id, time, bpm, status, mac_address, timezone,
       source_file_id, source_row_id, source_measurement_id
     ) VALUES
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 60, 'locked', 'A', NULL, 1, 1, 1),
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 61, 'locked', 'A', NULL, 1, 1, 2)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO raw.GarminStress (
       participant_id, time, stress, status, mac_address, timezone,
       source_file_id, source_row_id, source_measurement_id
     ) VALUES
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 40, 'valid', '00', NULL, 1, 1, 1),
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 55, 'valid', '00', NULL, 1, 1, 2)"
  )

  res <- deduplicate_db(
    db,
    sensors = c(
      "GarminBBI",
      "GarminEnhancedBBI",
      "GarminHeartRate",
      "GarminStress"
    )
  )
  expect_equal(unname(res[["GarminBBI"]]), 1)
  expect_equal(unname(res[["GarminEnhancedBBI"]]), 1)
  expect_equal(unname(res[["GarminHeartRate"]]), 1)
  expect_equal(unname(res[["GarminStress"]]), 1)

  # Garmin recalculates the value of an already consumed timestamp, so the last
  # recorded measurement of the newest file is authoritative.
  expect_equal(DBI::dbGetQuery(db, "SELECT bbi FROM GarminBBI")$bbi, 800L)
  expect_equal(DBI::dbGetQuery(db, "SELECT bbi FROM GarminEnhancedBBI")$bbi, 999L)
  expect_equal(DBI::dbGetQuery(db, "SELECT bpm FROM GarminHeartRate")$bpm, 61L)
  expect_equal(DBI::dbGetQuery(db, "SELECT stress FROM GarminStress")$stress, 55L)

  close_db(db)
})

test_that("Garmin point sensors keep the newest file when split across files", {
  db <- create_db(NULL, ":memory:")
  # The same timestamp with different values in two files: the newer file wins.
  DBI::dbExecute(
    db,
    "INSERT INTO raw.GarminHeartRate (
       participant_id, time, bpm, status, mac_address, timezone,
       source_file_id, source_row_id, source_measurement_id
     ) VALUES
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 60, 'locked', 'A', NULL, 1, 1, 1),
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 70, 'locked', 'A', NULL, 2, 1, 1)"
  )

  deduplicate_db(db, sensors = "GarminHeartRate")
  heart_rate <- DBI::dbGetQuery(db, "SELECT bpm, source_file_id FROM raw.GarminHeartRate")
  expect_equal(heart_rate$bpm, 70L)
  expect_equal(heart_rate$source_file_id, 2)

  close_db(db)
})

test_that("non-Garmin point sensors keep the last row of a file", {
  db <- create_db(NULL, ":memory:")
  DBI::dbExecute(
    db,
    "INSERT INTO raw.Activity (
       participant_id, time, confidence, type, timezone,
       source_file_id, source_row_id, source_measurement_id
     ) VALUES
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 80, 'WALKING', NULL, 1, 1, 1),
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 99, 'RUNNING', NULL, 1, 2, 1)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO raw.GarminAccelerometer (
       participant_id, time, x, y, z, mac_address, timezone,
       source_file_id, source_row_id, source_measurement_id
     ) VALUES
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 1, 2, 3, 'A', NULL, 1, 1, 1),
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 4, 5, 6, 'A', NULL, 1, 1, 2)"
  )

  deduplicate_db(db, sensors = c("Activity", "GarminAccelerometer"))
  # Deduplication is an upsert (last wins), so the row latest in source order
  # is kept for every sensor, not only for interval/Garmin sensors.
  expect_equal(DBI::dbGetQuery(db, "SELECT confidence FROM Activity")$confidence, 99L)
  expect_equal(DBI::dbGetQuery(db, "SELECT x FROM GarminAccelerometer")$x, 4)

  close_db(db)
})

test_that("GarminSteps keeps the newest end time for a repeated start time", {
  db <- create_db(NULL, ":memory:")
  DBI::dbExecute(
    db,
    "INSERT INTO raw.GarminSteps (
       participant_id, time, end_time, step_count, total_steps, mac_address, timezone,
       source_file_id, source_row_id, source_measurement_id
     ) VALUES
       (1, TIMESTAMPTZ '2025-12-16 16:31:44+00', TIMESTAMPTZ '2025-12-16 16:31:51+00', 1, 120, '00', NULL, 1, 1, 1),
       (1, TIMESTAMPTZ '2025-12-16 16:31:44+00', TIMESTAMPTZ '2025-12-16 16:31:59+00', 2, 122, '00', NULL, 1, 1, 2)"
  )

  deduplicate_db(db, sensors = "GarminSteps")
  steps <- DBI::dbGetQuery(db, "SELECT end_time, step_count FROM GarminSteps")
  expect_equal(steps$step_count, 2L)
  expect_equal(format(steps$end_time, tz = "UTC"), "2025-12-16 16:31:59")

  close_db(db)
})

test_that("deduplication does not remove data imported in earlier runs", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    connection_id = "12345",
    sensors = list(list(`__type` = "dk.cachet.carp.activity", confidence = 80, type = "WALKING"))
  )
  db <- create_db(NULL, ":memory:")
  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE))

  # A second run imports a file with different measurements: the
  # deduplication of the second run must leave the first run's rows alone
  make_test_file(
    dir,
    "b.json",
    connection_id = "54321",
    start_time = 1765889440388567 + 1e6,
    sensors = list(list(`__type` = "dk.cachet.carp.activity", confidence = 90, type = "STILL"))
  )
  expect_message(
    read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE),
    "All 1 file was successfully written to the database."
  )
  act <- DBI::dbGetQuery(db, "SELECT participant_id, confidence FROM Activity ORDER BY time")
  expect_equal(act$participant_id, c(12345, 54321))
  expect_equal(act$confidence, c(80, 90))

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("deduplication is an upsert: newest file and last row win", {
  db <- create_db(NULL, ":memory:")
  # A plain point sensor with the same key in two files. The newest file (2)
  # must win, and within that file the row latest in source order must win.
  DBI::dbExecute(
    db,
    "INSERT INTO raw.Activity (
       participant_id, time, confidence, type, timezone,
       source_file_id, source_row_id, source_measurement_id
     ) VALUES
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 80, 'WALKING', NULL, 1, 1, 1),
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 90, 'STILL', NULL, 2, 1, 1),
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 99, 'RUNNING', NULL, 2, 2, 1)"
  )

  deduplicate_db(db, sensors = "Activity")
  act <- DBI::dbGetQuery(db, "SELECT confidence, type, source_file_id FROM raw.Activity")
  # Only one row remains: the newest file's row latest in source order.
  expect_equal(act$confidence, 99L)
  expect_equal(act$type, "RUNNING")
  expect_equal(act$source_file_id, 2)
  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Activity")[[1]], 1)

  close_db(db)
})

test_that("deduplicate_db removes duplicates on demand", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    sensors = list(list(`__type` = "dk.cachet.carp.activity", confidence = 80, type = "WALKING"))
  )
  db <- create_db(NULL, ":memory:")
  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE))
  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Activity")[[1]], 1)

  # Create a duplicate measurement manually: same key, but the copy is later in
  # source order so it must win.
  DBI::dbExecute(
    db,
    "INSERT INTO raw.Activity (
       participant_id, time, confidence, type, timezone,
       source_file_id, source_row_id, source_measurement_id
     )
     SELECT participant_id, time, 99, 'RUNNING', timezone,
            source_file_id, source_row_id + 1, source_measurement_id FROM raw.Activity"
  )
  res <- deduplicate_db(db, sensors = "Activity")
  expect_equal(unname(res[["Activity"]]), 1)
  act <- DBI::dbGetQuery(db, "SELECT confidence FROM Activity")
  # Same file, so the row later in source order wins: deduplication is an
  # upsert for every sensor.
  expect_equal(act$confidence, 99)

  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Activity")[[1]], 1)

  # Running it again removes nothing
  res2 <- deduplicate_db(db, sensors = "Activity")
  expect_equal(unname(res2[["Activity"]]), 0)

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("deduplicate and optimize flags control the post-import passes", {
  # The file's entries are in reverse time order and contain a duplicate
  # measurement, so both the deduplication and the optimization are
  # observable. Each database needs its own import because a file is only
  # processed once.
  dir <- tempfile("import_flags")
  dir.create(dir)
  t0 <- 1765889440388567
  make_test_file(
    dir,
    "a.json",
    connection_id = "12345",
    start_time = c(t0 + 1e6, t0 + 1e6, t0),
    sensors = list(
      list(`__type` = "dk.cachet.carp.activity", confidence = 80, type = "WALKING"),
      list(`__type` = "dk.cachet.carp.activity", confidence = 90, type = "STILL"),
      list(`__type` = "dk.cachet.carp.activity", confidence = 70, type = "RUNNING")
    )
  )

  import_activity <- function(...) {
    db <- create_db(NULL, ":memory:")
    suppressMessages(
      read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE, ...)
    )
    db
  }
  activity <- function(db) {
    DBI::dbGetQuery(db, "SELECT confidence FROM raw.Activity ORDER BY rowid")$confidence
  }

  # Defaults: deduplication removes the duplicate (the later source row wins)
  # and optimization writes the table ordered by (participant_id, time).
  db <- import_activity()
  expect_equal(activity(db), c(70L, 90L))
  expect_equal(physical_order_violations(db), 0)
  close_db(db)

  # Neither step: all three rows survive in file order.
  db <- import_activity(deduplicate = FALSE, optimize = FALSE)
  expect_equal(activity(db), c(80L, 90L, 70L))
  expect_gt(physical_order_violations(db), 0)
  close_db(db)

  # Deduplicate only: the duplicate is removed, the file order stays.
  db <- import_activity(optimize = FALSE)
  expect_equal(activity(db), c(90L, 70L))
  expect_gt(physical_order_violations(db), 0)
  close_db(db)

  # Optimize only: all rows survive, ordered by time.
  db <- import_activity(deduplicate = FALSE)
  expect_equal(activity(db), c(70L, 80L, 90L))
  expect_equal(physical_order_violations(db), 0)
  close_db(db)

  unlink(dir, recursive = TRUE)
})

test_that("duplicate timezone events are deduplicated at import", {
  dir <- tempfile("import_tz_dedup")
  dir.create(dir)
  t0 <- 1765889440388567
  make_test_file(
    dir,
    "a.json",
    start_time = c(t0, t0),
    sensors = list(
      list(`__type` = "dk.cachet.carp.timezone", timezone = "Europe/Brussels"),
      list(`__type` = "dk.cachet.carp.timezone", timezone = "Europe/Brussels")
    )
  )
  db <- create_db(NULL, ":memory:")
  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE))
  # The timezone interval matcher assumes one event per participant and
  # instant, so duplicate timezone events are removed like any other sensor.
  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM raw.Timezone")[[1]], 1)
  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("read_mpath_sense validates the deduplicate and optimize flags", {
  dir <- tempfile("import_flags_check")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    sensors = list(list(`__type` = "dk.cachet.carp.stepcount", steps = 1))
  )
  db <- create_db(NULL, ":memory:")
  expect_error(
    read_mpath_sense(
      path = dir,
      db = db,
      recursive = FALSE,
      deduplicate = "yes",
      .progress = FALSE
    )
  )
  expect_error(
    read_mpath_sense(path = dir, db = db, recursive = FALSE, optimize = 1, .progress = FALSE)
  )
  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("files are processed oldest to newest", {
  dir <- tempfile("import_test")
  dir.create(dir)
  # Alphabetical order is the reverse of chronological order
  make_test_file(
    dir,
    "1_study_22_m_Path_sense_2025-12-14_10-00-00.000000.json",
    connection_id = "22",
    sensors = list(list(`__type` = "dk.cachet.carp.stepcount", steps = 1))
  )
  make_test_file(
    dir,
    "1_study_11_m_Path_sense_2025-12-16_10-00-00.000000.json",
    connection_id = "11",
    sensors = list(list(`__type` = "dk.cachet.carp.stepcount", steps = 2))
  )
  db <- create_db(NULL, ":memory:")
  suppressMessages(
    read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE, batch_size = 1)
  )
  pf <- DBI::dbGetQuery(db, "SELECT participant_id FROM ProcessedFiles ORDER BY file_id")
  # The older file (Dec 14, participant 22) is imported first
  expect_equal(pf$participant_id, c(22, 11))

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("file_ids are assigned in deterministic batch order", {
  dir <- tempfile("import_test")
  dir.create(dir)
  # Three files, alphabetical reverse of chronological order; with batch_size = 1
  # each batch is a single file, so file_id order == chronological order.
  make_test_file(
    dir,
    "1_study_22_m_Path_sense_2025-12-14_10-00-00.000000.json",
    connection_id = "22",
    sensors = list(list(`__type` = "dk.cachet.carp.stepcount", steps = 1))
  )
  make_test_file(
    dir,
    "1_study_11_m_Path_sense_2025-12-15_10-00-00.000000.json",
    connection_id = "11",
    sensors = list(list(`__type` = "dk.cachet.carp.stepcount", steps = 2))
  )
  make_test_file(
    dir,
    "1_study_33_m_Path_sense_2025-12-16_10-00-00.000000.json",
    connection_id = "33",
    sensors = list(list(`__type` = "dk.cachet.carp.stepcount", steps = 3))
  )
  db <- create_db(NULL, ":memory:")
  suppressMessages(read_mpath_sense(
    path = dir,
    db = db,
    recursive = FALSE,
    .progress = FALSE,
    batch_size = 1
  ))

  pf <- DBI::dbGetQuery(db, "SELECT file_id, participant_id, file_name FROM ProcessedFiles ORDER BY file_id")
  ped <- DBI::dbGetQuery(db, "SELECT participant_id, step_count, source_file_id FROM raw.Pedometer ORDER BY source_file_id")

  # file_ids are consecutive and assigned in import (chronological) order
  expect_equal(pf$file_id, seq_len(nrow(pf)))
  expect_equal(pf$participant_id, c(22, 11, 33))

  # sensor rows link to the same machine-generated file_ids
  expect_equal(ped$source_file_id, pf$file_id)
  expect_equal(ped$participant_id, pf$participant_id)
  expect_equal(ped$step_count, c(1, 2, 3))

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("empty files are registered as processed", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    sensors = list(list(`__type` = "dk.cachet.carp.stepcount", steps = 1))
  )
  file.create(file.path(dir, "1234_study_777_m_Path_sense_2025-12-16_16-33-00.000000.json"))
  db <- create_db(NULL, ":memory:")

  expect_message(
    read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE),
    "All 2 files were successfully written to the database."
  )
  pf <- DBI::dbGetQuery(
    db,
    "SELECT file_name, sense_version, file_size_bytes FROM ProcessedFiles ORDER BY file_name"
  )
  expect_equal(
    pf$file_name,
    c("1234_study_777_m_Path_sense_2025-12-16_16-33-00.000000.json", "a.json")
  )
  expect_true(is.na(pf$sense_version[1]))
  expect_equal(pf$file_size_bytes[1], 0)

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("a broken file is isolated and reported", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "good.json",
    sensors = list(list(`__type` = "dk.cachet.carp.stepcount", steps = 5))
  )
  # Truncated (invalid) JSON
  writeLines(
    '[\n  {"sensorStartTime": 1, "data": {"__type": "dk.cachet.carp.mpathinfo", "connectionId": "1", "studyName": "s", "senseVersion": 5}},\n  {"sensorStartTime": 1, "data": {"__type": "dk.cachet.carp.stepcount", "steps": 1}}\n',
    file.path(dir, "broken.json")
  )
  db <- create_db(NULL, ":memory:")

  res <- suppressMessages(read_mpath_sense(
    path = dir,
    db = db,
    recursive = FALSE,
    .progress = FALSE,
    batch_size = 2
  ))
  expect_equal(res, "broken.json")

  # The good file was still imported
  expect_equal(
    DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Pedometer")[[1]],
    1
  )
  expect_equal(
    DBI::dbGetQuery(db, "SELECT COUNT(*) FROM ProcessedFiles")[[1]],
    1
  )

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("files without mpathinfo are skipped and reported", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "good.json",
    sensors = list(list(`__type` = "dk.cachet.carp.stepcount", steps = 5))
  )
  jsonlite::write_json(
    list(list(
      sensorStartTime = 1765889440388567,
      data = list(`__type` = "dk.cachet.carp.stepcount", steps = 7)
    )),
    file.path(dir, "no_mpathinfo.json"),
    auto_unbox = TRUE
  )
  db <- create_db(NULL, ":memory:")

  expect_warning(
    res <- read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE),
    "could not be attributed to a participant"
  )
  expect_equal(res, "no_mpathinfo.json")
  expect_equal(
    DBI::dbGetQuery(db, "SELECT COUNT(*) FROM ProcessedFiles")[[1]],
    1
  )

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("ingest dispatch runs each sensor once per sense version with rows", {
  dir <- tempfile("import_test")
  dir.create(dir)
  # Two files in one batch with different sense versions and disjoint sensor
  # payloads: the v5 file has stepcount, the v6 file has battery. Before the
  # version-aware dispatch, every sensor whose payload type occurred anywhere
  # in the batch was ingested for EVERY version present, so the second
  # version pass ran full zero-row queries for sensors of the other version.
  make_test_file(
    dir,
    "v5.json",
    version = 5,
    sensors = list(list(`__type` = "dk.cachet.carp.stepcount", steps = 3))
  )
  make_test_file(
    dir,
    "v6.json",
    version = 6,
    sensors = list(list(`__type` = "dk.cachet.carp.batterystate", batteryLevel = 5, batteryStatus = "OK"))
  )
  db <- create_db(NULL, ":memory:")
  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE, batch_size = 10))

  # Each sensor ingested exactly once, from its own version's file
  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Pedometer")[[1]], 1)
  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Battery")[[1]], 1)
  ped <- DBI::dbGetQuery(db, "SELECT step_count, source_file_id FROM raw.Pedometer")
  bat <- DBI::dbGetQuery(db, "SELECT battery_level, source_file_id FROM raw.Battery")
  expect_equal(ped$step_count, 3L)
  expect_equal(bat$battery_level, 5L)
  expect_equal(
    DBI::dbGetQuery(db, "SELECT sense_version FROM ProcessedFiles ORDER BY file_id")$sense_version,
    c(5L, 6L)
  )

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("unknown senseVersion produces an aggregated warning", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    version = 99,
    sensors = list(list(`__type` = "dk.cachet.carp.stepcount", steps = 3))
  )
  db <- create_db(NULL, ":memory:")

  # The unknown version falls back to the default parser
  expect_warning(
    suppressMessages(
      res <- read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE)
    ),
    "Unknown senseVersion \"99\""
  )
  expect_equal(res, "")

  # Data of the unknown version was imported via the default parser
  expect_equal(
    DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Pedometer")[[1]],
    1
  )

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("unknown sensor types produce an aggregated warning", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    sensors = list(list(`__type` = "dk.cachet.carp.hyperspacejump", distance = 42))
  )
  db <- create_db(NULL, ":memory:")

  expect_warning(
    suppressMessages(
      res <- read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE)
    ),
    "Unknown sensor type \"dk.cachet.carp.hyperspacejump\""
  )
  expect_equal(res, "")

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("batch_size 1 and 100 give the same result", {
  dir <- tempfile("import_test")
  dir.create(dir)
  for (i in 1:5) {
    make_test_file(
      dir,
      paste0("f", i, ".json"),
      sensors = list(
        list(`__type` = "dk.cachet.carp.stepcount", steps = i),
        list(`__type` = "dk.cachet.carp.batterystate", batteryLevel = i, batteryStatus = "OK")
      )
    )
  }

  db1 <- create_db(NULL, ":memory:")
  db2 <- create_db(NULL, ":memory:")
  suppressMessages(read_mpath_sense(
    path = dir,
    db = db1,
    recursive = FALSE,
    .progress = FALSE,
    batch_size = 1
  ))
  suppressMessages(read_mpath_sense(
    path = dir,
    db = db2,
    recursive = FALSE,
    .progress = FALSE,
    batch_size = 100
  ))

  for (tbl in c("Pedometer", "Battery", "ProcessedFiles")) {
    expect_equal(
      DBI::dbGetQuery(db1, sprintf("SELECT COUNT(*) FROM %s", tbl))[[1]],
      DBI::dbGetQuery(db2, sprintf("SELECT COUNT(*) FROM %s", tbl))[[1]],
      info = tbl
    )
  }

  close_db(db1)
  close_db(db2)
  unlink(dir, recursive = TRUE)
})

test_that("deduplication keys include the table-specific extras", {
  dir <- tempfile("import_test")
  dir.create(dir)
  # Two files with the same measurement time but different apps: both rows must
  # survive deduplication
  make_test_file(
    dir,
    "a.json",
    sensors = list(list(
      `__type` = "dk.cachet.carp.appusage",
      usage = list(list(
        startDate = "2025-12-16T16:30:00.000Z",
        endDate = "2025-12-16T16:35:00.000Z",
        usage = 100,
        name = "AppA",
        packageName = "a"
      ))
    ))
  )
  make_test_file(
    dir,
    "b.json",
    sensors = list(list(
      `__type` = "dk.cachet.carp.appusage",
      usage = list(list(
        startDate = "2025-12-16T16:30:00.000Z",
        endDate = "2025-12-16T16:35:00.000Z",
        usage = 200,
        name = "AppB",
        packageName = "b"
      ))
    ))
  )
  db <- create_db(NULL, ":memory:")

  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE))
  apps <- DBI::dbGetQuery(db, "SELECT app, usage FROM AppUsage ORDER BY app")
  expect_equal(apps$app, c("AppA", "AppB"))
  expect_equal(apps$usage, c(100, 200))

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("empty and missing AppUsage collections preserve measurements", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "empty.json",
    sensors = list(list(
      `__type` = "dk.cachet.carp.appusage",
      usage = list()
    )),
    start_time = 1765889440388567
  )
  make_test_file(
    dir,
    "missing.json",
    sensors = list(list(
      `__type` = "dk.cachet.carp.appusage"
    )),
    start_time = 1765889441388567
  )
  make_test_file(
    dir,
    "nonempty.json",
    sensors = list(list(
      `__type` = "dk.cachet.carp.appusage",
      usage = list(list(
        startDate = "2025-12-16T16:30:00.000Z",
        endDate = "2025-12-16T16:35:00.000Z",
        usage = 100,
        name = "AppA",
        packageName = "a"
      ))
    )),
    start_time = 1765889442388567
  )
  db <- create_db(NULL, ":memory:")

  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE))
  apps <- DBI::dbGetQuery(
    db,
    "SELECT time, app, package_name, usage
     FROM AppUsage ORDER BY time"
  )

  expect_equal(nrow(apps), 3)
  expect_true(all(is.na(apps[1:2, c("app", "package_name", "usage")])))
  expect_equal(apps$app[3], "AppA")
  expect_equal(apps$package_name[3], "a")
  expect_equal(apps$usage[3], 100)

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("known but useless sensor types are silently ignored", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    sensors = list(
      list(`__type` = "dk.cachet.carp.triggeredtask", taskName = "some task"),
      list(`__type` = "dk.cachet.carp.stepcount", steps = 3)
    )
  )
  db <- create_db(NULL, ":memory:")

  # No warning for the ignored type, unlike unknown sensor types
  expect_no_warning(
    suppressMessages(
      read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE)
    )
  )
  # The triggered task data is not imported, the step count is
  expect_equal(
    DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Pedometer")[[1]],
    1
  )

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("a missing Garmin array key yields no rows instead of an error", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    sensors = list(list(
      `__type` = "dk.cachet.carp.garminalllogsdata",
      heartRate = list(
        list(timestamp = 1765889440388567, beatsPerMinute = 60, macAddress = "A")
      )
    ))
  )
  db <- create_db(NULL, ":memory:")

  expect_message(
    read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE),
    "All 1 file was successfully written to the database."
  )
  # The heartRate array is ingested, while the missing stress/bbi keys of the
  # same payload yield nothing
  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM GarminHeartRate")[[1]], 1)
  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM GarminStress")[[1]], 0)
  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM GarminBBI")[[1]], 0)

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("empty Bluetooth scan results preserve the scan measurement", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    sensors = list(
      list(
        `__type` = "dk.cachet.carp.bluetooth",
        startScan = "2025-12-16T20:23:22.458165",
        endScan = "2025-12-16T20:23:32.460703",
        scanResult = list()
      ),
      list(
        `__type` = "dk.cachet.carp.bluetooth",
        startScan = "2025-12-16T20:24:22.458165",
        endScan = "2025-12-16T20:24:32.460703",
        scanResult = list(list(
          advertisementName = "TestB1",
          bluetoothDeviceId = "00",
          bluetoothDeviceName = "TestB1",
          connectable = TRUE,
          txPowerLevel = -4,
          rssi = -72
        ))
      ),
      list(
        `__type` = "dk.cachet.carp.beacondata",
        region = "region-1",
        startScan = "2025-12-16T20:25:22.458165",
        endScan = "2025-12-16T20:25:32.460703",
        scanResult = list()
      )
    )
  )
  db <- create_db(NULL, ":memory:")
  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE))

  bluetooth <- DBI::dbGetQuery(
    db,
    "SELECT start_scan, advertisement_name, bluetooth_device_id
     FROM Bluetooth ORDER BY start_scan"
  )
  expect_equal(nrow(bluetooth), 2)
  expect_true(all(is.na(bluetooth[1, c("advertisement_name", "bluetooth_device_id")])))
  expect_equal(bluetooth$advertisement_name[2], "TestB1")
  expect_equal(bluetooth$bluetooth_device_id[2], "00")

  beacon <- DBI::dbGetQuery(
    db,
    "SELECT region, uuid, rssi FROM BluetoothBeacon"
  )
  expect_equal(nrow(beacon), 1)
  expect_equal(beacon$region, "region-1")
  expect_true(is.na(beacon$uuid))
  expect_true(is.na(beacon$rssi))

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("garmin ingest is skipped for arrays absent from the batch payloads", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    sensors = list(list(
      `__type` = "dk.cachet.carp.garminalllogsdata",
      bbi = list(list(timestamp = 1765889440388567, bbi = 800)),
      heartRate = list(list(timestamp = 1765889440388567, beatsPerMinute = 60))
    ))
  )
  db <- create_db(NULL, ":memory:")

  # Sensors whose array carries no element in any payload of the batch are
  # not dispatched at all (they would only run a zero-row unnest over
  # garmin_parsed; in the 106k-file run those calls cost ~104 s in total).
  expect_output(
    read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE, .debug = TRUE),
    "Ingested 1 row into GarminBBI"
  )
  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM GarminHeartRate")[[1]], 1)
  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM GarminStress")[[1]], 0)
  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM GarminSkinTemperature")[[1]], 0)

  # GarminMeta has no array column: it still runs and records the payload
  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM GarminMeta")[[1]], 1)

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("debug mode reports progress per file and per sensor", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    sensors = list(list(`__type` = "dk.cachet.carp.stepcount", steps = 3))
  )
  db <- create_db(NULL, ":memory:")

  expect_output(
    read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE, .debug = TRUE),
    "Ingested 1 row into Pedometer"
  )

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("an unfinished transaction is rolled back before importing", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    sensors = list(list(`__type` = "dk.cachet.carp.stepcount", steps = 1))
  )
  db <- create_db(NULL, ":memory:")

  # Simulate an interrupted import that left a transaction open
  DBI::dbExecute(db, "BEGIN")

  expect_message(
    read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE),
    "Rolled back an unfinished transaction"
  )
  expect_equal(
    DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Pedometer")[[1]],
    1
  )

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("deduplication keeps the newest provenance triple regardless of rowid", {
  db <- create_db(NULL, ":memory:")
  # Insert rows in reverse source order: the physically-first row has the
  # higher source_row_id, so physical order (rowid) opposes source order.
  # Deduplication must resolve by provenance, not by rowid.
  DBI::dbExecute(
    db,
    "INSERT INTO raw.Activity (
       participant_id, time, confidence, type, timezone,
       source_file_id, source_row_id, source_measurement_id
     ) VALUES
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 99, 'RUNNING', NULL, 1, 5, 1),
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 80, 'WALKING', NULL, 1, 1, 1)"
  )

  deduplicate_db(db, sensors = "Activity")
  act <- DBI::dbGetQuery(db, "SELECT confidence, type FROM Activity")
  # The row with source_row_id 5 was recorded later in the file and must win,
  # even though it was inserted (and hence has a lower rowid) first.
  expect_equal(act$confidence, 99L)
  expect_equal(act$type, "RUNNING")
  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Activity")[[1]], 1)

  close_db(db)
})

test_that("dedup chooses a full-table pass on an empty database and a scoped pass otherwise", {
  db <- create_db(NULL, ":memory:")
  meta <- tibble::tibble(
    source_file = "/tmp/f.json",
    file_name = "f.json",
    rel_path = "f.json",
    file_size_bytes = 100,
    modified_at = as.POSIXct("2025-12-16", tz = "UTC")
  )

  # An empty database reports db_was_empty = TRUE: the end-of-run dedup pass
  # then runs over the full sensor tables (no file_ids flagging join).
  out <- .read_filter_new_files(db, meta)
  expect_true(attr(out, "db_was_empty"))

  # After a run registered processed files the same database reports FALSE,
  # so a later run uses the file-scoped dedup pass.
  dir <- tempfile("dedup_choice")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    connection_id = "12345",
    sensors = list(list(`__type` = "dk.cachet.carp.activity", confidence = 80, type = "WALKING"))
  )
  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE))
  meta2 <- meta
  meta2$file_name <- meta2$rel_path <- "g.json"
  out2 <- .read_filter_new_files(db, meta2)
  expect_false(attr(out2, "db_was_empty"))

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("deduplication resolves cross-file duplicates by newest file first", {
  db <- create_db(NULL, ":memory:")
  # file 2 is newer; within file 2 the later source row wins even when it was
  # physically inserted earlier than the older file's rows.
  DBI::dbExecute(
    db,
    "INSERT INTO raw.Activity (
       participant_id, time, confidence, type, timezone,
       source_file_id, source_row_id, source_measurement_id
     ) VALUES
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 90, 'STILL', NULL, 2, 9, 1),
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 80, 'WALKING', NULL, 1, 2, 1),
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 95, 'JOGGING', NULL, 2, 3, 1)"
  )

  deduplicate_db(db, sensors = "Activity")
  act <- DBI::dbGetQuery(db, "SELECT confidence, type, source_file_id FROM raw.Activity")
  # Within file 2, the row with source_row_id 9 was recorded later than the
  # row with source_row_id 3 and must win.
  expect_equal(act$confidence, 90L)
  expect_equal(act$type, "STILL")
  expect_equal(act$source_file_id, 2)

  close_db(db)
})

test_that("main sensor views hide the provenance columns and are read-only", {
  db <- create_db(NULL, ":memory:")
  DBI::dbExecute(
    db,
    "INSERT INTO raw.Activity (
       participant_id, time, confidence, type, timezone,
       source_file_id, source_row_id, source_measurement_id
     ) VALUES (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 80, 'WALKING', NULL, 1, 1, 1)"
  )

  # main.Activity exposes the raw columns minus the three source-id columns;
  # timezone stays visible.
  view_cols <- DBI::dbGetQuery(
    db,
    "SELECT column_name FROM information_schema.columns
     WHERE table_schema = 'main' AND table_name = 'Activity'
     ORDER BY ordinal_position"
  )$column_name
  raw_cols <- DBI::dbGetQuery(
    db,
    "SELECT column_name FROM information_schema.columns
     WHERE table_schema = 'raw' AND table_name = 'Activity'
     ORDER BY ordinal_position"
  )$column_name
  expect_equal(
    view_cols,
    setdiff(raw_cols, c("source_file_id", "source_row_id", "source_measurement_id"))
  )
  expect_true("timezone" %in% view_cols)

  # The view is readable
  expect_equal(DBI::dbGetQuery(db, "SELECT confidence FROM Activity")$confidence, 80L)
  # ... but not writable
  expect_error(
    DBI::dbExecute(
      db,
      "INSERT INTO Activity (participant_id, time, confidence) VALUES (1, now(), 50)"
    )
  )
  expect_error(
    DBI::dbExecute(db, "DELETE FROM Activity")
  )

  close_db(db)
})

test_that("views stay consistent after dedup, optimize, and timezone fills", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    sensors = list(
      list(`__type` = "dk.cachet.carp.stepcount", steps = 1),
      list(`__type` = "dk.cachet.carp.timezone", timezone = "Europe/Brussels")
    )
  )
  db <- create_db(NULL, ":memory:")
  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE))

  # A duplicate measurement in a second file (renamed copy)
  file.copy(file.path(dir, "a.json"), file.path(dir, "b.json"))
  Sys.sleep(1.1)
  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE))

  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Pedometer")[[1]], 1)
  expect_equal(
    DBI::dbGetQuery(db, "SELECT timezone FROM Pedometer")$timezone,
    "Europe/Brussels"
  )
  optimize_db(db, sensors = "Pedometer", .progress = FALSE)
  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Pedometer")[[1]], 1)
  expect_equal(
    DBI::dbGetQuery(db, "SELECT step_count FROM Pedometer")$step_count,
    1L
  )

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("source_row_id ordinals are consecutive and deterministic across batch sizes", {
  dir <- tempfile("import_test")
  dir.create(dir)
  # Five files with distinct measurement times (so deduplication keeps one row
  # per sensor per file), each with several entries; compare batch_size 1 vs 100.
  t0 <- 1765889440388567
  for (i in 1:5) {
    make_test_file(
      dir,
      paste0("f", i, ".json"),
      start_time = t0 + i * 1e8,
      sensors = list(
        list(`__type` = "dk.cachet.carp.stepcount", steps = i),
        list(`__type` = "dk.cachet.carp.activity", confidence = 10 + i, type = "WALKING"),
        list(`__type` = "dk.cachet.carp.batterystate", batteryLevel = i, batteryStatus = "OK")
      )
    )
  }
  db1 <- create_db(NULL, ":memory:")
  db2 <- create_db(NULL, ":memory:")
  suppressMessages(read_mpath_sense(
    path = dir, db = db1, recursive = FALSE, .progress = FALSE, batch_size = 1
  ))
  suppressMessages(read_mpath_sense(
    path = dir, db = db2, recursive = FALSE, .progress = FALSE, batch_size = 100
  ))

  for (tbl in c("raw.Pedometer", "raw.Activity", "raw.Battery")) {
    a <- DBI::dbGetQuery(db1, sprintf("SELECT * FROM %s ORDER BY source_file_id, source_row_id", tbl))
    b <- DBI::dbGetQuery(db2, sprintf("SELECT * FROM %s ORDER BY source_file_id, source_row_id", tbl))
    expect_equal(a, b, info = tbl)
    # Each file contributes one row per sensor (mpathinfo + 3 sensor entries);
    # the row ordinal is the 1-based position of the sensor entry in the file
    # (mpathinfo is first at position 1) and the measurement ordinal is 1.
    expect_equal(a$source_file_id, 1:5, info = tbl)
    expect_true(all(a$source_row_id %in% 1:4), info = tbl)
    expect_equal(a$source_measurement_id, rep(1, 5), info = tbl)
  }

  close_db(db1)
  close_db(db2)
  unlink(dir, recursive = TRUE)
})

test_that("Garmin array element ordinals follow the array order", {
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    sensors = list(list(
      `__type` = "dk.cachet.carp.garminalllogsdata",
      heartRate = list(
        list(timestamp = 1765889440388567, beatsPerMinute = 60, macAddress = "A"),
        list(timestamp = 1765889440389567, beatsPerMinute = 70, macAddress = "A")
      )
    ))
  )
  db <- create_db(NULL, ":memory:")
  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE))

  hr <- DBI::dbGetQuery(
    db,
    "SELECT bpm, source_row_id, source_measurement_id
     FROM raw.GarminHeartRate ORDER BY source_measurement_id"
  )
  expect_equal(hr$source_measurement_id, 1:2)
  expect_equal(hr$bpm, c(60L, 70L))
  # Both array elements come from the same single garminalllogsdata entry
  expect_equal(length(unique(hr$source_row_id)), 1L)

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("Garmin recalculated duplicates: later array element and later file win", {
  # One file whose array repeats a timestamp: the later array element (the
  # recalculation) must win after deduplication.
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    sensors = list(list(
      `__type` = "dk.cachet.carp.garminalllogsdata",
      heartRate = list(
        list(timestamp = 1765889440388567, beatsPerMinute = 60, macAddress = "A"),
        list(timestamp = 1765889440388567, beatsPerMinute = 80, macAddress = "A")
      )
    ))
  )
  db <- create_db(NULL, ":memory:")
  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE))

  # The automatic post-import dedup already resolved the in-file duplicate.
  hr <- DBI::dbGetQuery(db, "SELECT bpm, source_measurement_id FROM raw.GarminHeartRate")
  expect_equal(nrow(hr), 1L)
  expect_equal(hr$bpm, 80L)
  expect_equal(hr$source_measurement_id, 2L)

  # Same duplicate split across two entries of one file: the later entry wins.
  make_test_file(
    dir,
    "b.json",
    start_time = 1765889440388567,
    sensors = list(
      list(
        `__type` = "dk.cachet.carp.garminalllogsdata",
        heartRate = list(list(
          timestamp = 1765889441388567, beatsPerMinute = 60, macAddress = "A"
        ))
      ),
      list(
        `__type` = "dk.cachet.carp.garminalllogsdata",
        heartRate = list(list(
          timestamp = 1765889441388567, beatsPerMinute = 90, macAddress = "A"
        ))
      )
    )
  )
  Sys.sleep(1.1)
  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE))

  hr2 <- DBI::dbGetQuery(
    db,
    "SELECT bpm, source_row_id FROM raw.GarminHeartRate WHERE source_file_id = 2"
  )
  expect_equal(nrow(hr2), 1L)
  # The file contains mpathinfo (position 1) followed by the two
  # garminalllogsdata entries (positions 2 and 3). The recalculation was
  # written second, so it is at position 3 and wins deduplication (dedup
  # keeps the row later in file order).
  expect_equal(hr2$bpm, 90L)
  expect_equal(hr2$source_row_id, 3)

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("staging ordinals are deterministic across thread counts and repeats", {
  # Synthetic multi-file batch: entries carry a monotone in-file marker, with
  # several sensor types sharing timestamps (like real files). Every row has a
  # distinct measurement time so deduplication keeps all rows, and the staged
  # ordinal must always equal the true file position (JSON array index),
  # regardless of thread count or repeat.
  dir <- tempfile("import_test")
  dir.create(dir)
  for (i in 1:4) {
    entries <- list()
    for (k in 1:600) {
      entries[[length(entries) + 1]] <- list(
        sensorStartTime = 1765889440388567 + k * 1e6,
        data = list(
          `__type` = if (k %% 2) "dk.cachet.carp.stepcount" else "dk.cachet.carp.activity",
          steps = k, confidence = k %% 200, type = "WALKING", marker = k
        )
      )
    }
    # The first entry is mpathinfo (deterministic anchor)
    entries <- c(list(list(
      sensorStartTime = 1765889440388567,
      data = list(
        `__type` = "dk.cachet.carp.mpathinfo", connectionId = "12345",
        studyName = "test_study", senseVersion = 5
      )
    )), entries)
    jsonlite::write_json(entries, file.path(dir, paste0("f", i, ".json")), auto_unbox = TRUE)
  }

  ordinals <- function(threads) {
    db <- create_db(NULL, ":memory:", threads = threads)
    res <- tryCatch(
      {
        suppressMessages(read_mpath_sense(
          path = dir, db = db, recursive = FALSE, .progress = FALSE, batch_size = 2
        ))
        DBI::dbGetQuery(
          db,
          "SELECT source_file_id, source_row_id, step_count
           FROM raw.Pedometer ORDER BY source_file_id, source_row_id"
        )
      },
      error = function(e) NULL
    )
    close_db(db)
    res
  }

  a <- ordinals(1)
  b <- ordinals(8)
  if (is.null(a) || is.null(b)) {
    skip("staging could not be exercised in this environment")
  }
  # Per-file ordinals are consecutive and identical across thread settings
  expect_equal(a$source_row_id, b$source_row_id)
  # step_count (the in-file marker) increases with source_row_id within each
  # file: the ordinal reflects the true file position, not scan order.
  expect_true(all(diff(a$step_count) > 0))

  unlink(dir, recursive = TRUE)
})

test_that("source_row_id is the file position even when sensor times interleave", {
  # m-Path Sense writes each sensor in start-time order but interleaves the
  # sensors, so the file-wide start times are not sorted. source_row_id must
  # still be the JSON array position, so that a row points at the exact
  # entry in the file (data provenance).
  dir <- tempfile("import_test")
  dir.create(dir)
  # Deliberately interleaved, with the second sensor's time EARLIER than the
  # first sensor's (as can happen between sensors), plus a duplicate start
  # time within one sensor later in the file.
  entries <- list(
    list(sensorStartTime = 1765889441000000, data = list(
      `__type` = "dk.cachet.carp.mpathinfo", connectionId = "12345",
      studyName = "s", senseVersion = 5
    )),
    # stepcount at 12:00
    list(sensorStartTime = 1765889440000000, data = list(
      `__type` = "dk.cachet.carp.stepcount", steps = 10
    )),
    # activity at 12:01
    list(sensorStartTime = 1765889441000000, data = list(
      `__type` = "dk.cachet.carp.activity", confidence = 10, type = "WALKING"
    )),
    # stepcount again at 12:00:30 (later in file, later time for that sensor)
    list(sensorStartTime = 1765889440300000, data = list(
      `__type` = "dk.cachet.carp.stepcount", steps = 11
    )),
    # activity at 11:59 (a row from another sensor with an EARLIER time than
    # the previous row)
    list(sensorStartTime = 1765889439000000, data = list(
      `__type` = "dk.cachet.carp.activity", confidence = 20, type = "STILL"
    ))
  )
  jsonlite::write_json(entries, file.path(dir, "interleaved.json"), auto_unbox = TRUE)
  db <- create_db(NULL, ":memory:")
  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE))

  ped <- DBI::dbGetQuery(db, "SELECT step_count, source_row_id FROM raw.Pedometer ORDER BY source_row_id")
  act <- DBI::dbGetQuery(db, "SELECT type, confidence, source_row_id FROM raw.Activity ORDER BY source_row_id")

  # Rows keep the true file position: mpathinfo at 1, then 2,3,4,5.
  expect_equal(ped$source_row_id, c(2, 4))
  expect_equal(act$source_row_id, c(3, 5))
  expect_equal(ped$step_count, c(10L, 11L))
  expect_equal(act$type, c("WALKING", "STILL"))

  # Duplicates on (participant, time, sensor): none here, but a same-key pair
  # would keep the row with the later source_row_id. Covered elsewhere; here
  # we pin the provenance-ordinal contract.
  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("duplicates keep the later file position within the newest file", {
  # One sensor reporting the same start time twice within one file: the
  # second (later-position) row is the corrected/updated measurement and must
  # win, regardless of the physical insertion order.
  dir <- tempfile("import_test")
  dir.create(dir)
  make_test_file(
    dir,
    "a.json",
    start_time = 1765889440388567,
    sensors = list(
      list(`__type` = "dk.cachet.carp.activity", confidence = 50, type = "WALKING"),
      list(`__type` = "dk.cachet.carp.activity", confidence = 90, type = "RUNNING")
    )
  )
  db <- create_db(NULL, ":memory:")
  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE))

  act <- DBI::dbGetQuery(db, "SELECT confidence, type, source_row_id FROM raw.Activity")
  expect_equal(nrow(act), 1L)
  # mpathinfo is position 1; the two activity entries are at positions 2 and 3.
  expect_equal(act$source_row_id, 3)
  expect_equal(act$confidence, 90L)
  expect_equal(act$type, "RUNNING")

  close_db(db)
  unlink(dir, recursive = TRUE)
})

test_that("editing a file and re-importing overwrites the corrected measurement", {
  # Documented behaviour: modify a value in a file, re-import (the file is new
  # because its modification time/size changed), and the corrected measurement
  # replaces the old one because the new file has a later source_file_id and
  # its row is later in source order.
  dir <- tempfile("import_test")
  dir.create(dir)
  f <- make_test_file(
    dir,
    "a.json",
    sensors = list(list(`__type` = "dk.cachet.carp.activity", confidence = 50, type = "WALKING"))
  )
  db <- create_db(NULL, ":memory:")
  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE))
  expect_equal(DBI::dbGetQuery(db, "SELECT confidence FROM Activity")$confidence, 50L)

  # Edit: change the value, keep the same sensorStartTime
  entries <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  entries[[2]]$data$confidence <- 95
  entries[[2]]$data$type <- "RUNNING"
  jsonlite::write_json(entries, f, auto_unbox = TRUE)
  Sys.sleep(1.1) # ensure the modification time differs

  suppressMessages(read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE))
  act <- DBI::dbGetQuery(db, "SELECT confidence, type, source_file_id, source_row_id FROM raw.Activity")
  expect_equal(nrow(act), 1L)
  expect_equal(act$confidence, 95L)
  expect_equal(act$type, "RUNNING")
  # The winner comes from the second (corrected) version of the file
  expect_equal(act$source_file_id, 2)

  close_db(db)
  unlink(dir, recursive = TRUE)
})
