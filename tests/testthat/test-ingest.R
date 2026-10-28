test_that("normalized payload types are unique", {
  types <- c(.read_known_types(), ignored_sensor_types)
  expect_identical(types, unique(types))
  expect_length(grep("^dk\\.cachet\\.carp\\.", types), 0L)
})

test_that("registered ingest functions generate complete SQL templates", {
  registry <- new_sensor_registry()
  garmin <- vapply(registry, \(x) x[["type"]], character(1)) == "garminalllogsdata"

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
      # A Garmin array sensor expands exactly the array column its registry
      # entry names; the same field drives the empty-array skip in
      # read_mpath_sense(), so it must not drift from the SQL.
      for (col in registry[[sensor]]$array) {
        expect_match(sql, sprintf("UNNEST\\(g\\.%s\\)", col))
      }
    } else {
      expect_match(sql, "m\\.sense_version = 5")
    }
  }
})

test_that("ingest functions use staged payload types", {
  registry <- new_sensor_registry()
  garmin <- vapply(registry, \(x) x[["type"]], character(1)) == "garminalllogsdata"

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
  expect_match(sql, "payload_type = 'accelerationfeatures'", fixed = TRUE)
  expect_match(sql, "m.sense_version = 6", fixed = TRUE)
  expect_match(sql, "CROSS JOIN LATERAL", fixed = TRUE)
  # no ->> remains; features come from the transform struct (spot checks)
  expect_match(sql, "CAST\\(j\\.p\\.count AS UINTEGER\\)")
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
  expect_match(sql, "payload_type = 'garminalllogsdata'")
  expect_match(sql, "m\\.sense_version = 6")
  expect_no_match(sql, "%s")
  # every Garmin array schema appears in the transform
  for (key in c(
    "heartRate",
    "stress",
    "steps",
    "bbi",
    "enhancedBbi",
    "gyroscope",
    "accelerometer",
    "respiration",
    "skinTemperature",
    "spo2",
    "wristStatus",
    "zeroCrossing",
    "actigraphy1",
    "actigraphy2",
    "actigraphy3"
  )) {
    expect_match(sql, sprintf('\"%s\"', key), fixed = TRUE)
  }
})

test_that("scalar ingest SQL executes against a staging fixture", {
  db <- create_db(NULL, ":memory:", shared_home = FALSE)
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE raw_staging AS
     SELECT * FROM (VALUES
       (1765889440388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.activity\", \"confidence\": 80, \"type\": \"WALKING\"}',
        'activity.json', 'activity', 1::BIGINT)
     ) v(sensorStartTime, sensorEndTime, data, source_file, payload_type, source_row_id)"
  )
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE file_id_map AS
     SELECT * FROM (VALUES ('activity.json', 1, 1::UBIGINT, 5::BIGINT))
     v(source_file, participant_id, file_id, sense_version)"
  )

  sql <- new_sensor_registry()[["Activity"]]$fun(5L)
  DBI::dbExecute(db, sql)

  activity <- DBI::dbGetQuery(db, "SELECT participant_id, confidence, type FROM Activity")
  expect_equal(activity$participant_id, 1)
  expect_equal(activity$confidence, 80L)
  expect_equal(activity$type, "WALKING")

  close_db(db)
})

test_that("Location and Weather parse each payload once", {
  location <- ingest_location(6L)
  expect_no_match(location, "data->>")
  expect_match(location, "json_extract_string\\(s\\.data")
  expect_match(location, "payload_type = 'location'", fixed = TRUE)
  expect_match(location, "CROSS JOIN LATERAL")

  weather <- ingest_weather(6L)
  expect_no_match(weather, "data->>")
  expect_match(weather, "json_extract_string\\(s\\.data")
  expect_match(weather, "payload_type = 'weather'", fixed = TRUE)
  expect_match(weather, "CROSS JOIN LATERAL")
})

test_that("Location ingest handles full, missing, and string-valued payloads", {
  db <- create_db(NULL, ":memory:", shared_home = FALSE)
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE raw_staging AS
     SELECT * FROM (VALUES
       (1765889440388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.location\", \"latitude\": 50.1, \"longitude\": 4.2, \"altitude\": 10.5, \"accuracy\": 3.2, \"verticalAccuracy\": 1.1, \"speed\": 1.4, \"speedAccuracy\": 0.2, \"heading\": 180, \"headingAccuracy\": 5, \"isMock\": false, \"elapsedRealtimeNanos\": 1234567890123, \"elapsedRealtimeUncertaintyNanos\": 10}',
        'full.json', 'location', 1::BIGINT),
       (1765889441388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.location\"}',
        'missing.json', 'location', 2::BIGINT),
       (1765889442388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.location\", \"latitude\": \"50.5\", \"longitude\": \"4.6\", \"altitude\": \"10\", \"accuracy\": \"3\", \"verticalAccuracy\": \"1\", \"speed\": \"1\", \"speedAccuracy\": \"0\", \"heading\": \"180\", \"headingAccuracy\": \"5\", \"isMock\": \"true\", \"elapsedRealtimeNanos\": \"123\", \"elapsedRealtimeUncertaintyNanos\": \"10\"}',
        'strings.json', 'location', 3::BIGINT)
     ) v(sensorStartTime, sensorEndTime, data, source_file, payload_type, source_row_id)"
  )
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE file_id_map AS
     SELECT * FROM (VALUES
       ('full.json', 1, 1::UBIGINT, 6::BIGINT),
       ('missing.json', 1, 2::UBIGINT, 6::BIGINT),
       ('strings.json', 1, 3::UBIGINT, 6::BIGINT)
     ) v(source_file, participant_id, file_id, sense_version)"
  )

  DBI::dbExecute(db, ingest_location(6L))
  location <- DBI::dbGetQuery(
    db,
    "SELECT source_row_id, latitude, longitude, altitude, is_mock,
            elapsed_realtime_nanos
     FROM raw.Location ORDER BY source_row_id"
  )
  expect_equal(location$source_row_id, 1:3)
  expect_equal(location$latitude, c(50.1, NA, 50.5))
  expect_equal(location$longitude, c(4.2, NA, 4.6))
  expect_equal(location$altitude, c(10.5, NA, 10), tolerance = 1e-4)
  expect_equal(location$is_mock, c(FALSE, NA, TRUE))
  expect_equal(location$elapsed_realtime_nanos, c(1234567890123, NA, 123))

  close_db(db)
})

test_that("Location ingest still rejects malformed numeric fields", {
  db <- create_db(NULL, ":memory:", shared_home = FALSE)
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE raw_staging AS
     SELECT * FROM (VALUES
       (1765889440388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.location\", \"latitude\": \"not-a-number\"}',
        'bad.json', 'location', 1::BIGINT)
     ) v(sensorStartTime, sensorEndTime, data, source_file, payload_type, source_row_id)"
  )
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE file_id_map AS
     SELECT * FROM (VALUES ('bad.json', 1, 1::UBIGINT, 6::BIGINT))
     v(source_file, participant_id, file_id, sense_version)"
  )

  expect_error(DBI::dbExecute(db, ingest_location(6L)), "Could not convert")
  close_db(db)
})

test_that("Weather ingest handles ISO strings, epochs, and missing fields", {
  db <- create_db(NULL, ":memory:", shared_home = FALSE)
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE raw_staging AS
     SELECT * FROM (VALUES
       (1765889440388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.weather\", \"country\": \"BE\", \"areaName\": \"Leuven\", \"weatherMain\": \"Clouds\", \"weatherDescription\": \"broken clouds\", \"sunrise\": \"2025-12-16T11:49:12\", \"sunset\": \"2025-12-16T19:46:33\", \"latitude\": 50.9, \"longitude\": 4.7, \"pressure\": 938, \"windSpeed\": 4.9, \"windDegree\": 173, \"humidity\": 60, \"cloudiness\": 74, \"rainLastHour\": 0.1, \"rainLast3Hours\": 0.2, \"snowLastHour\": 0, \"snowLast3Hours\": 0, \"temperature\": 12.4, \"tempMin\": 11.8, \"tempMax\": 14.2}',
        'full.json', 'weather', 1::BIGINT),
       (1765889441388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.weather\"}',
        'missing.json', 'weather', 2::BIGINT),
       (1765889442388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.weather\", \"sunrise\": 1760000000, \"sunset\": 1760040000, \"pressure\": \"938.5\"}',
        'epochs.json', 'weather', 3::BIGINT)
     ) v(sensorStartTime, sensorEndTime, data, source_file, payload_type, source_row_id)"
  )
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE file_id_map AS
     SELECT * FROM (VALUES
       ('full.json', 1, 1::UBIGINT, 6::BIGINT),
       ('missing.json', 1, 2::UBIGINT, 6::BIGINT),
       ('epochs.json', 1, 3::UBIGINT, 6::BIGINT)
     ) v(source_file, participant_id, file_id, sense_version)"
  )

  DBI::dbExecute(db, ingest_weather(6L))
  weather <- DBI::dbGetQuery(
    db,
    "SELECT source_row_id, country, sunrise, sunset, pressure, temperature
     FROM raw.Weather ORDER BY source_row_id"
  )
  expect_equal(weather$source_row_id, 1:3)
  expect_equal(weather$country, c("BE", NA, NA))
  expect_equal(
    format(weather$sunrise, tz = "UTC"),
    c("2025-12-16 11:49:12", NA, "2025-10-09 08:53:20")
  )
  expect_equal(
    format(weather$sunset, tz = "UTC"),
    c("2025-12-16 19:46:33", NA, "2025-10-09 20:00:00")
  )
  expect_equal(weather$pressure, c(938, NA, 938.5), tolerance = 1e-4)
  expect_equal(weather$temperature, c(12.4, NA, NA), tolerance = 1e-4)

  close_db(db)
})

test_that("Weather ingest still rejects malformed numeric fields", {
  db <- create_db(NULL, ":memory:", shared_home = FALSE)
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE raw_staging AS
     SELECT * FROM (VALUES
       (1765889440388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.weather\", \"pressure\": \"not-a-number\"}',
        'bad.json', 'weather', 1::BIGINT)
     ) v(sensorStartTime, sensorEndTime, data, source_file, payload_type, source_row_id)"
  )
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE file_id_map AS
     SELECT * FROM (VALUES ('bad.json', 1, 1::UBIGINT, 6::BIGINT))
     v(source_file, participant_id, file_id, sense_version)"
  )

  expect_error(DBI::dbExecute(db, ingest_weather(6L)), "Could not convert")
  close_db(db)
})

test_that("Location and Weather map fields by name, not JSON key order", {
  db <- create_db(NULL, ":memory:", shared_home = FALSE)
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE raw_staging AS
     SELECT * FROM (VALUES
       (1765889440388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.location\", \"extra\": \"ignored\", \"elapsedRealtimeUncertaintyNanos\": 10, \"isMock\": false, \"heading\": 180, \"altitude\": 76.9, \"longitude\": 4.7, \"accuracy\": 87.8, \"latitude\": 50.8, \"speed\": 1.4, \"verticalAccuracy\": 1.1, \"speedAccuracy\": 0.2, \"headingAccuracy\": 5, \"elapsedRealtimeNanos\": 123}',
        'location.json', 'location', 1::BIGINT),
       (1765889441388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.weather\", \"temperature\": 12.4, \"sunset\": \"2025-12-16T19:46:33\", \"country\": \"BE\", \"pressure\": 938, \"latitude\": 50.9, \"windDegree\": 173, \"longitude\": 4.7, \"humidity\": 60, \"areaName\": \"Leuven\", \"weatherDescription\": \"broken clouds\", \"cloudiness\": 74, \"sunrise\": \"2025-12-16T11:49:12\", \"weatherMain\": \"Clouds\", \"windSpeed\": 4.9, \"rainLastHour\": 0.1, \"rainLast3Hours\": 0.2, \"snowLastHour\": 0, \"snowLast3Hours\": 0, \"tempMin\": 11.8, \"tempMax\": 14.2}',
        'weather.json', 'weather', 1::BIGINT)
     ) v(sensorStartTime, sensorEndTime, data, source_file, payload_type, source_row_id)"
  )
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE file_id_map AS
     SELECT * FROM (VALUES
       ('location.json', 1, 1::UBIGINT, 6::BIGINT),
       ('weather.json', 1, 2::UBIGINT, 6::BIGINT)
     ) v(source_file, participant_id, file_id, sense_version)"
  )

  DBI::dbExecute(db, ingest_location(6L))
  DBI::dbExecute(db, ingest_weather(6L))

  location <- DBI::dbGetQuery(
    db,
    "SELECT latitude, longitude, altitude, accuracy, speed, vertical_accuracy,
            heading, is_mock, elapsed_realtime_nanos
     FROM raw.Location"
  )
  expect_equal(location$latitude, 50.8)
  expect_equal(location$longitude, 4.7)
  expect_equal(location$altitude, 76.9, tolerance = 1e-4)
  expect_equal(location$accuracy, 87.8, tolerance = 1e-4)
  expect_equal(location$speed, 1.4, tolerance = 1e-4)
  expect_equal(location$vertical_accuracy, 1.1, tolerance = 1e-4)
  expect_equal(location$heading, 180, tolerance = 1e-4)
  expect_false(location$is_mock)
  expect_equal(location$elapsed_realtime_nanos, 123)

  weather <- DBI::dbGetQuery(
    db,
    "SELECT country, area_name, weather_main, weather_description, sunrise,
            sunset, latitude, longitude, pressure, temperature
     FROM raw.Weather"
  )
  expect_equal(weather$country, "BE")
  expect_equal(weather$area_name, "Leuven")
  expect_equal(weather$weather_main, "Clouds")
  expect_equal(weather$weather_description, "broken clouds")
  expect_equal(format(weather$sunrise, tz = "UTC"), "2025-12-16 11:49:12")
  expect_equal(format(weather$sunset, tz = "UTC"), "2025-12-16 19:46:33")
  expect_equal(weather$latitude, 50.9, tolerance = 1e-4)
  expect_equal(weather$longitude, 4.7, tolerance = 1e-4)
  expect_equal(weather$pressure, 938, tolerance = 1e-4)
  expect_equal(weather$temperature, 12.4, tolerance = 1e-4)

  close_db(db)
})

test_that("AppUsage ingest preserves empty, missing, and populated collections", {
  db <- create_db(NULL, ":memory:", shared_home = FALSE)
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE raw_staging AS
     SELECT * FROM (VALUES
       (1765889440388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.appusage\", \"usage\": []}',
        'empty.json', 'appusage', 1::BIGINT),
       (1765889441388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.appusage\"}',
        'missing.json', 'appusage', 2::BIGINT),
       (1765889442388567::BIGINT, NULL::BIGINT,
        '{\"__type\": \"dk.cachet.carp.appusage\", \"usage\": [{\"usage\": 100, \"name\": \"AppA\", \"packageName\": \"a\"}]}',
        'full.json', 'appusage', 3::BIGINT)
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
  db <- create_db(NULL, ":memory:", shared_home = FALSE)
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
        'appusage.json', 'appusage', 1::BIGINT)
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
  db <- create_db(NULL, ":memory:", shared_home = FALSE)
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE raw_staging AS
     SELECT * FROM (VALUES
       (1765889440388567::BIGINT,
        '{\"wristStatus\": [{\"timestamp\": 1, \"status\": \"ON_WRIST\", \"macAddress\": \"m\"}]}',
        'f', 'garminalllogsdata', 1::BIGINT)
     ) v(sensorStartTime, data, source_file, payload_type, source_row_id)"
  )
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE file_id_map AS SELECT * FROM (VALUES ('f', 1, 1::UBIGINT, 5::BIGINT)) v(source_file, participant_id, file_id, sense_version)"
  )
  # The batch loop parses the Garmin payloads once into garmin_parsed; the
  # per-sensor ingest then expands its own array column.
  DBI::dbExecute(db, .read_garmin_parse_sql(5L))
  DBI::dbExecute(db, new_sensor_registry()[["GarminWristStatus"]]$fun(5L))
  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) n FROM GarminWristStatus")$n, 1L)
  close_db(db)
})

test_that("Bluetooth ingest preserves empty and populated scan results", {
  db <- create_db(NULL, ":memory:", shared_home = FALSE)
  DBI::dbExecute(
    db,
    "CREATE TEMP TABLE raw_staging AS
     SELECT * FROM (VALUES
       (1765889440388567::BIGINT,
        '{\"__type\": \"dk.cachet.carp.bluetooth\", \"scanResult\": []}',
        'empty.json', 'bluetooth', 1::BIGINT),
       (1765889441388567::BIGINT,
        '{\"__type\": \"dk.cachet.carp.bluetooth\", \"scanResult\": [{\"rssi\": -72}]}',
        'full.json', 'bluetooth', 2::BIGINT)
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
