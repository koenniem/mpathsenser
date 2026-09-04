test_that("registered ingest functions generate complete SQL templates", {
  registry <- new_sensor_registry()

  for (sensor in names(registry)) {
    sql <- registry[[sensor]]$fun(5L)

    expect_type(sql, "character")
    expect_length(sql, 1L)
    expect_match(sql, paste0("INSERT INTO ", sensor, "[ (]"))
    expect_no_match(sql, "%s")
    expect_match(sql, "m\\.sense_version = 5")
  }
})

test_that("ingest functions use staged payload types", {
  registry <- new_sensor_registry()

  for (sensor in names(registry)) {
    sql <- registry[[sensor]]$fun(6L)
    payload_type <- registry[[sensor]]$type

    expect_match(sql, "payload_type")
    expect_match(sql, payload_type, fixed = TRUE)
    expect_match(sql, "m\\.sense_version = 6")
  }
})

test_that("scalar ingest SQL executes against a staging fixture", {
  db <- create_db(NULL, ":memory:")
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE raw_staging AS
     SELECT * FROM (VALUES
       (1765889440388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.activity\", \"confidence\": 80, \"type\": \"WALKING\"}',
        'activity.json', 'dk.cachet.carp.activity', 1::BIGINT)
     ) v(sensorStartTime, sensorEndTime, data, source_file, payload_type, rn)"
  )
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE file_id_map AS
     SELECT * FROM (VALUES ('activity.json', 1, 1::UBIGINT, 5::BIGINT))
     v(source_file, participant_id, file_id, sense_version)"
  )

  sql <- ingest_activity(5L)
  DBI::dbExecute(db, sql)

  activity <- DBI::dbGetQuery(db, "SELECT participant_id, confidence, type FROM Activity")
  expect_equal(activity$participant_id, 1)
  expect_equal(activity$confidence, 80L)
  expect_equal(activity$type, "WALKING")

  close_db(db)
})

test_that("AppUsage ingest preserves empty, missing, and populated collections", {
  db <- create_db(NULL, ":memory:")
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE raw_staging AS
     SELECT * FROM (VALUES
       (1765889440388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.appusage\", \"usage\": []}',
        'empty.json', 'dk.cachet.carp.appusage', 1::BIGINT),
       (1765889441388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.appusage\"}',
        'missing.json', 'dk.cachet.carp.appusage', 2::BIGINT),
       (1765889442388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.appusage\", \"usage\": [{\"usage\": 100, \"name\": \"AppA\", \"packageName\": \"a\"}]}',
        'full.json', 'dk.cachet.carp.appusage', 3::BIGINT)
     ) v(sensorStartTime, sensorEndTime, data, source_file, payload_type, rn)"
  )
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE file_id_map AS
     SELECT * FROM (VALUES
       ('empty.json', 1, 1::UBIGINT, 5::BIGINT),
       ('missing.json', 1, 2::UBIGINT, 5::BIGINT),
       ('full.json', 1, 3::UBIGINT, 5::BIGINT)
     ) v(source_file, participant_id, file_id, sense_version)"
  )

  sql <- ingest_appusage(5L)
  DBI::dbExecute(db, sql)

  apps <- DBI::dbGetQuery(
    db,
    "SELECT source_file_id, app, package_name, usage
     FROM AppUsage ORDER BY source_file_id"
  )
  expect_equal(nrow(apps), 3L)
  expect_true(all(is.na(apps[1:2, c("app", "package_name", "usage")])))
  expect_equal(apps$app[3], "AppA")
  expect_equal(apps$package_name[3], "a")
  expect_equal(apps$usage[3], 100)

  close_db(db)
})

test_that("AppUsage replaces epoch last foreground timestamps with NULL", {
  db <- create_db(NULL, ":memory:")
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE raw_staging AS
     SELECT * FROM (VALUES
       (1765889440388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.appusage\", \"usage\": [
          {\"name\": \"Epoch\", \"lastForeground\": \"1970-01-01T00:00:00.000\"},
          {\"name\": \"Offset\", \"lastForeground\": \"1970-01-01T01:00:00.000\"},
          {\"name\": \"Real\", \"lastForeground\": \"2025-12-16T16:30:00.000Z\"}
        ]}',
        'appusage.json', 'dk.cachet.carp.appusage', 1::BIGINT)
     ) v(sensorStartTime, sensorEndTime, data, source_file, payload_type, rn)"
  )
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE file_id_map AS
     SELECT * FROM (VALUES ('appusage.json', 1, 1::UBIGINT, 5::BIGINT))
     v(source_file, participant_id, file_id, sense_version)"
  )

  DBI::dbExecute(db, ingest_appusage(5L))
  apps <- DBI::dbGetQuery(
    db,
    "SELECT app, last_foreground FROM AppUsage ORDER BY app"
  )
  expect_true(all(is.na(apps$last_foreground[1:2])))
  expect_equal(format(apps$last_foreground[3], tz = "UTC"), "2025-12-16 16:30:00")

  close_db(db)
})

test_that("typed array ingest executes", {
  db <- create_db(NULL, ":memory:")
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE raw_staging AS
     SELECT * FROM (VALUES
       (1765889440388567::BIGINT,
        '{\"wristStatus\": [{\"timestamp\": 1, \"status\": \"ON_WRIST\", \"macAddress\": \"m\"}]}',
        'f', 'dk.cachet.carp.garminalllogsdata')
     ) v(sensorStartTime, data, source_file, payload_type)"
  )
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE file_id_map AS SELECT * FROM (VALUES ('f', 1, 1::UBIGINT, 5::BIGINT)) v(source_file, participant_id, file_id, sense_version)"
  )
  DBI::dbExecute(db, ingest_garmin_wriststatus(5L))
  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) n FROM GarminWristStatus")$n, 1L)
  close_db(db)
})

test_that("Bluetooth ingest preserves empty and populated scan results", {
  db <- create_db(NULL, ":memory:")
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE raw_staging AS
     SELECT * FROM (VALUES
       (1765889440388567::BIGINT,
        '{\"__type\": \"dk.cachet.carp.bluetooth\", \"scanResult\": []}',
        'empty.json', 'dk.cachet.carp.bluetooth', 1::BIGINT),
       (1765889441388567::BIGINT,
        '{\"__type\": \"dk.cachet.carp.bluetooth\", \"scanResult\": [{\"rssi\": -72}]}',
        'full.json', 'dk.cachet.carp.bluetooth', 2::BIGINT)
     ) v(sensorStartTime, data, source_file, payload_type, rn)"
  )
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE file_id_map AS
     SELECT * FROM (VALUES
       ('empty.json', 1, 1::UBIGINT, 5::BIGINT),
       ('full.json', 1, 2::UBIGINT, 5::BIGINT)
     ) v(source_file, participant_id, file_id, sense_version)"
  )

  sql <- ingest_bluetooth(5L)
  DBI::dbExecute(db, sql)

  bluetooth <- DBI::dbGetQuery(
    db,
    "SELECT source_file_id, rssi FROM Bluetooth ORDER BY source_file_id"
  )
  expect_equal(nrow(bluetooth), 2L)
  expect_true(is.na(bluetooth$rssi[1]))
  expect_equal(bluetooth$rssi[2], -72L)

  close_db(db)
})
