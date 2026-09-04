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
    sensorStartTime = start_time,
    data = list(
      `__type` = "dk.cachet.carp.mpathinfo",
      connectionId = connection_id,
      studyName = study,
      senseVersion = version
    )
  ))
  for (s in sensors) {
    entries[[length(entries) + 1]] <- list(sensorStartTime = start_time, data = s)
  }
  jsonlite::write_json(entries, file.path(dir, name), auto_unbox = TRUE)
  file.path(dir, name)
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

  # Sensor data with correct values, UTC instants, and file provenance
  act <- DBI::dbGetQuery(db, "SELECT * FROM Activity")
  expect_equal(act$participant_id, 12345)
  expect_equal(act$confidence, 80)
  expect_equal(act$type, "WALKING")
  expect_equal(format(act$time, tz = "UTC"), "2025-12-16 12:50:40")
  expect_equal(act$source_file_id, pf$file_id)

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
    "INSERT INTO Accelerometer (
       participant_id, time, end_time, n, timezone, source_file_id
     ) VALUES
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', TIMESTAMPTZ '2025-12-16 12:01:00+00', 1, NULL, 1),
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', TIMESTAMPTZ '2025-12-16 12:02:00+00', 2, NULL, 1)"
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
    "INSERT INTO Accelerometer (
       participant_id, time, end_time, n, timezone, source_file_id
     ) VALUES
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', TIMESTAMPTZ '2025-12-16 12:01:00+00', 1, NULL, 1),
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', TIMESTAMPTZ '2025-12-16 12:05:00+00', 2, NULL, 2)"
  )

  res <- deduplicate_db(db, sensors = "Accelerometer")
  expect_equal(unname(res[["Accelerometer"]]), 1)
  accelerometer <- DBI::dbGetQuery(db, "SELECT end_time, n, source_file_id FROM Accelerometer")
  expect_equal(accelerometer$n, 2)
  expect_equal(accelerometer$source_file_id, 2)
  expect_equal(format(accelerometer$end_time, tz = "UTC"), "2025-12-16 12:05:00")

  close_db(db)
})

test_that("Garmin point sensors keep the last recorded row within a file", {
  db <- create_db(NULL, ":memory:")
  DBI::dbExecute(
    db,
    "INSERT INTO GarminBBI (participant_id, time, bbi, mac_address, timezone, source_file_id) VALUES
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 763, '00', NULL, 1),
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 800, '00', NULL, 1)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO GarminEnhancedBBI (participant_id, time, bbi, status, gap_duration, mac_address, timezone, source_file_id) VALUES
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 992, 'lowConfidence', 0, '00', NULL, 1),
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 999, 'lowConfidence', 0, '00', NULL, 1)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO GarminHeartRate (participant_id, time, bpm, status, mac_address, timezone, source_file_id) VALUES
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 60, 'locked', 'A', NULL, 1),
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 61, 'locked', 'A', NULL, 1)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO GarminStress (participant_id, time, stress, status, mac_address, timezone, source_file_id) VALUES
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 40, 'valid', '00', NULL, 1),
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 55, 'valid', '00', NULL, 1)"
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
    "INSERT INTO GarminHeartRate (participant_id, time, bpm, status, mac_address, timezone, source_file_id) VALUES
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 60, 'locked', 'A', NULL, 1),
       (1, TIMESTAMPTZ '2025-12-16 16:30:00+00', 70, 'locked', 'A', NULL, 2)"
  )

  deduplicate_db(db, sensors = "GarminHeartRate")
  heart_rate <- DBI::dbGetQuery(db, "SELECT bpm, source_file_id FROM GarminHeartRate")
  expect_equal(heart_rate$bpm, 70L)
  expect_equal(heart_rate$source_file_id, 2)

  close_db(db)
})

test_that("non-Garmin point sensors keep the last row of a file", {
  db <- create_db(NULL, ":memory:")
  DBI::dbExecute(
    db,
    "INSERT INTO Activity (participant_id, time, confidence, type, timezone, source_file_id) VALUES
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 80, 'WALKING', NULL, 1),
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 99, 'RUNNING', NULL, 1)"
  )
  DBI::dbExecute(
    db,
    "INSERT INTO GarminAccelerometer (participant_id, time, x, y, z, mac_address, timezone, source_file_id) VALUES
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 1, 2, 3, 'A', NULL, 1),
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 4, 5, 6, 'A', NULL, 1)"
  )

  deduplicate_db(db, sensors = c("Activity", "GarminAccelerometer"))
  # Deduplication is an upsert (last wins), so the last recorded row is kept
  # for every sensor, not only for interval/Garmin sensors.
  expect_equal(DBI::dbGetQuery(db, "SELECT confidence FROM Activity")$confidence, 99L)
  expect_equal(DBI::dbGetQuery(db, "SELECT x FROM GarminAccelerometer")$x, 4)

  close_db(db)
})

test_that("GarminSteps keeps the newest end time for a repeated start time", {
  db <- create_db(NULL, ":memory:")
  DBI::dbExecute(
    db,
    "INSERT INTO GarminSteps (
       participant_id, time, end_time, step_count, total_steps, mac_address, timezone, source_file_id
     ) VALUES
       (1, TIMESTAMPTZ '2025-12-16 16:31:44+00', TIMESTAMPTZ '2025-12-16 16:31:51+00', 1, 120, '00', NULL, 1),
       (1, TIMESTAMPTZ '2025-12-16 16:31:44+00', TIMESTAMPTZ '2025-12-16 16:31:59+00', 2, 122, '00', NULL, 1)"
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
  # must win, and within that file the last recorded row must win.
  DBI::dbExecute(
    db,
    "INSERT INTO Activity (participant_id, time, confidence, type, timezone, source_file_id) VALUES
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 80, 'WALKING', NULL, 1),
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 90, 'STILL', NULL, 2),
       (1, TIMESTAMPTZ '2025-12-16 12:00:00+00', 99, 'RUNNING', NULL, 2)"
  )

  deduplicate_db(db, sensors = "Activity")
  act <- DBI::dbGetQuery(db, "SELECT confidence, type, source_file_id FROM Activity")
  # Only one row remains: the newest file's last row.
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

  # Create a duplicate measurement manually
  DBI::dbExecute(
    db,
    "INSERT INTO Activity (participant_id, time, confidence, type, timezone, source_file_id)
     SELECT participant_id, time, 99, 'RUNNING', timezone, source_file_id FROM Activity"
  )
  res <- deduplicate_db(db, sensors = "Activity")
  expect_equal(unname(res[["Activity"]]), 1)
  act <- DBI::dbGetQuery(db, "SELECT confidence FROM Activity")
  # Same file, so the last row (highest rowid) wins: deduplication is an
  # upsert for every sensor.
  expect_equal(act$confidence, 99)

  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Activity")[[1]], 1)

  # Running it again removes nothing
  res2 <- deduplicate_db(db, sensors = "Activity")
  expect_equal(unname(res2[["Activity"]]), 0)

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
  ped <- DBI::dbGetQuery(db, "SELECT participant_id, step_count, source_file_id FROM Pedometer ORDER BY source_file_id")

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
    read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE, debug = TRUE),
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
