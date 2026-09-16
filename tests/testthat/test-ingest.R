test_that("registered ingest functions generate complete SQL templates", {
  registry <- new_sensor_registry()
  garmin <- vapply(registry, \(x) x[["type"]], character(1)) ==
    "dk.cachet.carp.garminalllogsdata"

  for (sensor in names(registry)) {
    sql <- registry[[sensor]]$fun(5L)

    expect_type(sql, "character")
    expect_length(sql, 1L)
    expect_match(sql, paste0("INSERT INTO raw.", sensor, "[ (]"))
    expect_no_match(sql, "%s")
    if (garmin[[sensor]]) {
      # Garmin ingest statements read the batch's single-parse temp table
      # (garmin_parsed, built by .read_garmin_parse_sql()) and no longer scan
      # raw_staging themselves or filter by payload type or sense version
      expect_match(sql, "FROM garmin_parsed")
      expect_no_match(sql, "raw_staging|payload_type|sense_version")
    } else {
      expect_match(sql, "m\\.sense_version = 5")
    }
  }
})

test_that("ingest functions use staged payload types", {
  registry <- new_sensor_registry()
  garmin <- vapply(registry, \(x) x[["type"]], character(1)) ==
    "dk.cachet.carp.garminalllogsdata"

  for (sensor in names(registry)) {
    sql <- registry[[sensor]]$fun(6L)
    payload_type <- registry[[sensor]]$type

    if (garmin[[sensor]]) {
      expect_match(sql, "garmin_parsed")
    } else {
      expect_match(sql, "payload_type")
      expect_match(sql, payload_type, fixed = TRUE)
      expect_match(sql, "m\\.sense_version = 6")
    }
  }
})

test_that("collection-expanding ingests prefilter raw_staging by payload type", {
  # Bluetooth/BluetoothBeacon/Connectivity expand per-payload JSON arrays in
  # a CROSS JOIN LATERAL. DuckDB does not push the payload_type filter below
  # such a lateral (the lateral expression is evaluated for every staged
  # row), so each call used to JSON-parse the whole batch data column
  # (~240-330 ms per call on a ~200 MB staging batch, measured). The SQL
  # therefore reads raw_staging through a derived table that filters by
  # payload type first; the lateral then only sees matching payloads.
  # AppUsage (LEFT JOIN LATERAL) and the garmin_parsed CTAS (plain scalar
  # lateral) do not need this and must stay untouched.
  for (sensor in c("Bluetooth", "BluetoothBeacon", "Connectivity")) {
    sql <- new_sensor_registry()[[sensor]]$fun(6L)
    payload_type <- new_sensor_registry()[[sensor]]$type
    expect_match(
      sql,
      sprintf(
        "FROM \\(SELECT \\* FROM raw_staging WHERE payload_type = '%s'\\) s",
        payload_type
      ),
      fixed = FALSE
    )
    expect_no_match(sql, "FROM raw_staging s")
  }
  for (f in c("ingest_appusage", ".read_garmin_parse_sql")) {
    sql <- if (f == "ingest_appusage") ingest_appusage(6L) else .read_garmin_parse_sql(6L)
    expect_match(sql, "FROM raw_staging s")
  }
})

test_that("phone Accelerometer ingest parses each payload once", {
  # The accelerationfeatures payload holds ~42 summary features. The ingest
  # used to read each with its own data->>'...' extraction; DuckDB parses the
  # VARCHAR JSON document once per ->> expression, so every row was parsed
  # once per feature (~60-85 us/row on a real corpus, ~24 s for 372k rows).
  # The statement must now read the features from one typed json_transform
  # of each row (parsed once), applied only to payloads of this type/version
  # (the derived-table prefilter: a lateral/transform on raw_staging would
  # otherwise run on every staged row, including multi-hundred-KB Garmin
  # payloads).
  sql <- ingest_accelerometer(6L)
  expect_no_match(sql, "data->>")
  expect_match(sql, "json_transform\\(s\\.data")
  # filter + version guard live inside the derived table, before the lateral
  expect_match(sql, "FROM raw_staging s")
  expect_match(sql, "JOIN file_id_map m ON m.source_file = s.source_file", fixed = TRUE)
  expect_match(sql, "payload_type = 'dk.cachet.carp.accelerationfeatures'", fixed = TRUE)
  expect_match(sql, "m.sense_version = 6", fixed = TRUE)
  expect_match(sql, "CROSS JOIN LATERAL", fixed = TRUE)
  # no ->> remains; features come from the transform struct (spot checks)
  expect_match(sql, "CAST\\(j\\.p\\.count AS INTEGER\\)")
  expect_match(sql, "CAST\\(j\\.p\\.xMean AS REAL\\)")
  expect_match(sql, "CAST\\(j\\.p\\.signalMagnitudeArea AS REAL\\)")
  # target column list and value list are generated from one payload-key
  # order, so they stay positionally aligned
  expect_match(sql, "\\(\n        participant_id, time, end_time,")
})

test_that("garmin_parse SQL builds one typed transform per staged payload", {
  sql <- .read_garmin_parse_sql(6L)
  expect_match(sql, "CREATE OR REPLACE TEMP TABLE garmin_parsed")
  expect_match(sql, "json_transform\\(s.data")
  expect_match(sql, "payload_type = 'dk.cachet.carp.garminalllogsdata'")
  expect_match(sql, "m\\.sense_version = 6")
  expect_no_match(sql, "%s")
  # every Garmin array schema appears in the transform
  for (key in c("heartRate", "stress", "steps", "bbi", "enhancedBbi",
    "gyroscope", "accelerometer", "respiration", "skinTemperature",
    "spo2", "wristStatus", "zeroCrossing", "actigraphy1", "actigraphy2",
    "actigraphy3")) {
    expect_match(sql, sprintf('\"%s\"', key), fixed = TRUE)
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
     ) v(sensorStartTime, sensorEndTime, data, source_file, payload_type, source_row_id)"
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
     ) v(sensorStartTime, sensorEndTime, data, source_file, payload_type, source_row_id)"
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
     FROM raw.AppUsage ORDER BY source_file_id"
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
     ) v(sensorStartTime, sensorEndTime, data, source_file, payload_type, source_row_id)"
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
    "SELECT app, last_foreground FROM raw.AppUsage ORDER BY app"
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
        'f', 'dk.cachet.carp.garminalllogsdata', 1::BIGINT)
     ) v(sensorStartTime, data, source_file, payload_type, source_row_id)"
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
     ) v(sensorStartTime, data, source_file, payload_type, source_row_id)"
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
    "SELECT source_file_id, rssi FROM raw.Bluetooth ORDER BY source_file_id"
  )
  expect_equal(nrow(bluetooth), 2L)
  expect_true(is.na(bluetooth$rssi[1]))
  expect_equal(bluetooth$rssi[2], -72L)

  close_db(db)
})
