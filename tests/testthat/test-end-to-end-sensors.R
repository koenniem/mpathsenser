# End-to-end checks using the checked-in sensor fixture.
# These assertions compare JSON fixture values with the physical main tables.

sensor_fixture_path <- function() {
  local <- file.path(testthat::test_path(), "..", "..", "inst", "testdata", "tests.json")
  if (file.exists(local)) {
    return(local)
  }
  system.file("testdata", "tests.json", package = "mpathsenser")
}

import_sensor_fixture <- function() {
  dir <- tempfile("mpathsenser_fixture")
  dir.create(dir)
  file.copy(sensor_fixture_path(), file.path(dir, "tests.json"))
  db <- create_db(NULL, ":memory:")
  testthat::expect_warning(
    read_mpath_sense(
      path = dir,
      db = db,
      recursive = FALSE,
      .progress = FALSE
    ),
    "Unknown sensor type"
  )
  unlink(dir, recursive = TRUE)
  db
}

expected_fixture_counts <- c(
  Accelerometer = 1L,
  Activity = 1L,
  AppUsage = 3L,
  Battery = 1L,
  Bluetooth = 2L,
  BluetoothBeacon = 2L,
  Connectivity = 1L,
  Device = 1L,
  Error = 1L,
  GarminAccelerometer = 2L,
  GarminActigraphy = 6L,
  GarminBBI = 2L,
  GarminEnhancedBBI = 2L,
  GarminGyroscope = 2L,
  GarminHeartRate = 2L,
  GarminMeta = 1L,
  GarminRespiration = 2L,
  GarminSkinTemperature = 2L,
  GarminSPO2 = 2L,
  GarminSteps = 2L,
  GarminStress = 2L,
  GarminWristStatus = 1L,
  GarminZeroCrossing = 2L,
  Heartbeat = 1L,
  Light = 1L,
  Location = 1L,
  Memory = 1L,
  Pedometer = 1L,
  Screen = 1L,
  Timezone = 1L,
  Weather = 1L,
  Wifi = 1L
)

test_that("the complete sensor fixture is imported with the expected row counts", {
  db <- import_sensor_fixture()
  on.exit(close_db(db), add = TRUE)

  for (sensor in names(expected_fixture_counts)) {
    actual <- DBI::dbGetQuery(
      db,
      sprintf("SELECT COUNT(*) AS n FROM %s", sensor)
    )$n
    expect_equal(actual, expected_fixture_counts[[sensor]], info = sensor)
  }

  expect_equal(
    DBI::dbGetQuery(db, "SELECT participant_id, study_id FROM Participant"),
    data.frame(participant_id = 12345, study_id = "DemoStudy")
  )
  expect_equal(
    DBI::dbGetQuery(db, "SELECT file_name, sense_version FROM ProcessedFiles"),
    data.frame(file_name = "tests.json", sense_version = 5L)
  )
})

test_that("scalar sensor values survive the JSON to DuckDB round trip", {
  db <- import_sensor_fixture()
  on.exit(close_db(db), add = TRUE)

  checks <- list(
    Activity = c("confidence = 70", "type = 'WALKING'"),
    Battery = c("battery_level = 25", "battery_status = 'charging'"),
    Connectivity = c("connectivity_status = 'wifi'"),
    Error = c("message = 'Test error message'"),
    Heartbeat = c("period = 1", "device_role_name = 'garminSmartwatch'"),
    Pedometer = c("step_count = 18987"),
    Screen = c("screen_event = 'SCREEN_OFF'"),
    Timezone = c("timezone = 'Europe/Brussels'"),
    Wifi = c("ip = '10.12.34.56'")
  )

  for (sensor in names(checks)) {
    values <- DBI::dbGetQuery(
      db,
      sprintf("SELECT %s FROM %s", paste(checks[[sensor]], collapse = ", "), sensor)
    )
    expect_equal(nrow(values), 1L, info = sensor)
  }

  app_usage <- DBI::dbGetQuery(
    db,
    "SELECT app, package_name, usage FROM AppUsage ORDER BY app"
  )
  expect_equal(app_usage$app, c("kuleuven", "music", "whatsapp"))
  expect_equal(
    app_usage$package_name,
    c("io.m_Path_Sense.kuleuven", "com.spotify.music", "com.whatsapp")
  )
  expect_equal(app_usage$usage, c(55000000, 11000000, 139000000))

  bluetooth <- DBI::dbGetQuery(
    db,
    "SELECT advertisement_name, bluetooth_device_id, connectable, rssi, tx_power_level
     FROM Bluetooth WHERE bluetooth_device_id IS NOT NULL"
  )
  expect_equal(bluetooth$advertisement_name, "TestB1")
  expect_equal(bluetooth$bluetooth_device_id, "00")
  expect_true(bluetooth$connectable)
  expect_equal(bluetooth$rssi, -72L)
  expect_equal(bluetooth$tx_power_level, -4L)
})

test_that("GarminWristStatus is imported correctly", {
  db <- import_sensor_fixture()
  on.exit(close_db(db), add = TRUE)

  wrist <- DBI::dbGetQuery(
    db,
    "SELECT participant_id, status, mac_address
     FROM GarminWristStatus"
  )
  expect_equal(nrow(wrist), 1L)
  expect_equal(wrist$participant_id, 12345)
  expect_equal(wrist$status, "ON_WRIST")
  expect_equal(wrist$mac_address, "00")
})

test_that("Garmin array values survive the JSON to DuckDB round trip", {
  db <- import_sensor_fixture()
  on.exit(close_db(db), add = TRUE)

  wrist <- DBI::dbGetQuery(
    db,
    "SELECT participant_id, time, status, mac_address
     FROM GarminWristStatus"
  )
  expect_equal(nrow(wrist), 1L)
  expect_equal(wrist$participant_id, 12345)
  expect_equal(format(wrist$time, tz = "UTC"), "2025-12-16 16:53:18")
  expect_equal(wrist$status, "ON_WRIST")
  expect_equal(wrist$mac_address, "00")

  stress <- DBI::dbGetQuery(
    db,
    "SELECT time, stress, status, mac_address
     FROM GarminStress ORDER BY time"
  )
  expect_equal(stress$stress, c(40L, 23L))
  expect_equal(stress$status, c("valid", "valid"))
  expect_equal(stress$mac_address, c("00", "00"))

  heart_rate <- DBI::dbGetQuery(
    db,
    "SELECT bpm, status, mac_address FROM GarminHeartRate ORDER BY time"
  )
  expect_equal(heart_rate$bpm, c(76L, 74L))
  expect_equal(heart_rate$status, c("locked", "locked"))

  # GarminSPO2 reads the 'spo2' array key (lowercase) and the 'spo2Reading'
  # field of each element.
  spo2 <- DBI::dbGetQuery(db, "SELECT spo2, mac_address FROM GarminSPO2 ORDER BY time")
  expect_equal(spo2$spo2, c(95L, 98L))
  expect_true(all(spo2$mac_address == "00"))

  accelerometer <- DBI::dbGetQuery(
    db,
    "SELECT x, y, z, mac_address FROM GarminAccelerometer ORDER BY time"
  )
  expect_equal(accelerometer$x, c(113.3744, 118.0369), tolerance = 1e-4)
  expect_equal(accelerometer$y, c(437.6353, 440.0922), tolerance = 1e-4)
  expect_equal(accelerometer$z, c(-906.8707, -873.2643), tolerance = 1e-4)
  expect_true(all(accelerometer$mac_address == "00"))

  # GarminZeroCrossing reads the deadband field (lowercase key). The fixture's
  # first element spells it 'deadband' (matching the schema); the second spells
  # it 'deadBand' (an old, misspelled variant) and yields NULL.
  zero_crossing <- DBI::dbGetQuery(
    db,
    "SELECT total_energy, n_zero_crossing, deadband FROM GarminZeroCrossing ORDER BY rowid"
  )
  expect_equal(zero_crossing$total_energy, c(1234L, 1235L))
  expect_equal(zero_crossing$n_zero_crossing, c(4L, 2L))
  expect_equal(zero_crossing$deadband, c(1000L, NA_integer_))

  steps <- DBI::dbGetQuery(
    db,
    "SELECT step_count, total_steps FROM GarminSteps ORDER BY time"
  )
  expect_equal(steps$step_count, c(NA_integer_, 3L))
  expect_equal(steps$total_steps, c(122L, 119L))

  bbi <- DBI::dbGetQuery(db, "SELECT bbi FROM GarminBBI ORDER BY time")
  expect_equal(bbi$bbi, c(763L, 775L))

  enhanced <- DBI::dbGetQuery(
    db,
    "SELECT bbi, status, gap_duration FROM GarminEnhancedBBI ORDER BY time"
  )
  expect_equal(enhanced$bbi, c(992L, 859L))
  expect_equal(enhanced$status, c("lowConfidence", "lowConfidence"))
  expect_equal(enhanced$gap_duration, c(0L, 0L))
})

test_that("sensor columns use the declared DuckDB types", {
  db <- import_sensor_fixture()
  on.exit(close_db(db), add = TRUE)

  types <- DBI::dbGetQuery(
    db,
    "SELECT table_name, column_name, data_type
     FROM information_schema.columns
     WHERE table_schema = 'main'"
  )
  expect_true(all(types$data_type != "JSON" | types$column_name == "device_data"))
  expect_equal(
    types$data_type[types$table_name == "GarminWristStatus" & types$column_name == "time"],
    "TIMESTAMP WITH TIME ZONE"
  )
  expect_equal(
    types$data_type[types$table_name == "GarminWristStatus" & types$column_name == "status"],
    "VARCHAR"
  )
  expect_equal(
    types$data_type[types$table_name == "GarminWristStatus" & types$column_name == "mac_address"],
    "VARCHAR"
  )
})
