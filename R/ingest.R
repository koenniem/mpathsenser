# Ingest functions of the read_mpath_sense() pipeline.
#
# Each function builds the SQL statement that inserts the data of one sensor
# from the raw_staging temp table into the corresponding sensor table, using
# file_id_map to link each row to its source file (participant, senseVersion,
# file_id). The statements are executed by .read_ingest(), which optionally
# chunks them with LIMIT/OFFSET to bound memory usage. All timestamps are
# stored as UTC instants in DuckDB; observation timezones are populated after
# ingestion by the normalization step.

# Feature columns of the accelerationfeatures payload (phone accelerometer
# summaries). The old ingest read each of these with a separate
# data->>'...' extraction; DuckDB parses the VARCHAR JSON document once per
# ->> expression, so a 46-field row was parsed ~46 times (~60-85 us/row,
# ~24 s over a real corpus of 372k rows). Reading the fields from one typed
# json_transform (below) parses each row exactly once (~5 us/row). Integer
# features are listed here; everything else in accel_payload_order is a
# double feature.
accel_int_fields <- c(
  "count", "xNegCount", "yNegCount", "zNegCount",
  "xPosCount", "yPosCount", "zPosCount", "xAboveMean", "yAboveMean", "zAboveMean"
)
# dbdef.sql maps these payload keys to the raw.Accelerometer column names;
# keep in sync with the INSERT target list below.
accel_col_map <- c(
  xMean = "x_mean", yMean = "y_mean", zMean = "z_mean",
  xMedian = "x_median", yMedian = "y_median", zMedian = "z_median",
  xStd = "x_std", yStd = "y_std", zStd = "z_std",
  xAad = "x_aad", yAad = "y_aad", zAad = "z_aad",
  xMin = "x_min", yMin = "y_min", zMin = "z_min",
  xMax = "x_max", yMax = "y_max", zMax = "z_max",
  xMaxMinDiff = "x_max_min_diff", yMaxMinDiff = "y_max_min_diff",
  zMaxMinDiff = "z_max_min_diff",
  xMad = "x_mad", yMad = "y_mad", zMad = "z_mad",
  xIqr = "x_iqr", yIqr = "y_iqr", zIqr = "z_iqr",
  xEnergy = "x_energy", yEnergy = "y_energy", zEnergy = "z_energy",
  avgResultAcceleration = "avg_res_acc", signalMagnitudeArea = "sma",
  count = "n", xNegCount = "x_neg_n", yNegCount = "y_neg_n",
  zNegCount = "z_neg_n", xPosCount = "x_pos_n", yPosCount = "y_pos_n",
  zPosCount = "z_pos_n", xAboveMean = "x_above_mean",
  yAboveMean = "y_above_mean", zAboveMean = "z_above_mean"
)

# Column layout shared by the INSERT target list and the value list of the
# SELECT below, in one canonical payload-key order (the raw.Accelerometer
# column names come from accel_col_map). Keep accel_col_map in sync with
# dbdef.sql.
accel_payload_order <- c(
  "xMean", "yMean", "zMean", "xMedian", "yMedian", "zMedian",
  "xStd", "yStd", "zStd", "xAad", "yAad", "zAad",
  "xMin", "yMin", "zMin", "xMax", "yMax", "zMax",
  "xMaxMinDiff", "yMaxMinDiff", "zMaxMinDiff",
  "xMad", "yMad", "zMad", "xIqr", "yIqr", "zIqr",
  "xNegCount", "yNegCount", "zNegCount", "xPosCount", "yPosCount", "zPosCount",
  "xAboveMean", "yAboveMean", "zAboveMean",
  "xEnergy", "yEnergy", "zEnergy", "avgResultAcceleration", "signalMagnitudeArea",
  "count"
)
# Value expressions per feature, mirroring the historical data->>'...' CASTs:
# doubles and integers are read from the single typed transform (j.p.<key>).
.accel_feature_exprs <- function(prefix = "j.p.") {
  is_int <- accel_payload_order %in% accel_int_fields
  type <- ifelse(is_int, "INTEGER", "REAL")
  setNames(
    sprintf("CAST(%s%s AS %s)", prefix, accel_payload_order, type),
    unname(accel_col_map[accel_payload_order])
  )
}

ingest_accelerometer <- function(sense_version) {
  schema <- sprintf(
    "{%s}",
    paste0(
      sprintf('"%s": "DOUBLE"', accel_payload_order),
      collapse = ", "
    )
  )
  exprs <- .accel_feature_exprs()
  target_cols <- names(exprs)
  sprintf(
    "INSERT INTO raw.Accelerometer (
        participant_id, time, end_time,
        %s,
        source_file_id, source_row_id, source_measurement_id
      )
      SELECT
        s.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        to_timestamp(CAST(s.sensorEndTime AS BIGINT) / 1000000.0),
        %s,
        s.file_id, s.source_row_id AS source_row_id, 1 AS source_measurement_id
      FROM (
        SELECT s.source_row_id, s.sensorStartTime, s.sensorEndTime, s.data,
               m.participant_id, m.file_id
        FROM raw_staging s
        JOIN file_id_map m ON m.source_file = s.source_file
        WHERE s.payload_type = 'dk.cachet.carp.accelerationfeatures'
          AND %s
          AND s.sensorStartTime IS NOT NULL
      ) s
      CROSS JOIN LATERAL (
        SELECT json_transform(s.data, '%s') AS p
      ) j
",
    paste0(target_cols, collapse = ", "),
    paste0(exprs, collapse = ",\n        "),
    .read_version_filter(sense_version),
    schema
  )
}

ingest_activity <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.Activity (participant_id, time, confidence, type, source_file_id, source_row_id, source_measurement_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'confidence' AS INTEGER),
        CAST(s.data->>'type' AS TEXT),
        m.file_id, s.source_row_id AS source_row_id, 1 AS source_measurement_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      WHERE s.payload_type = 'dk.cachet.carp.activity'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .read_version_filter(sense_version)
  )
}

# Origin timestamps indicate that the foreground time was unavailable. Allow a
# day around the Unix epoch to cover offsets introduced by local timezones.
ingest_appusage <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.AppUsage (
        participant_id, time, end_time, period_start, period_end,
        usage, app, package_name, last_foreground, source_file_id, source_row_id, source_measurement_id
      )
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        to_timestamp(CAST(s.sensorEndTime AS BIGINT) / 1000000.0),
        %s,
        %s,
        CAST(apps.value->>'usage' AS BIGINT),
        CAST(apps.value->>'name' AS TEXT),
        CAST(apps.value->>'packageName' AS TEXT),
        CASE
          WHEN apps.value->>'lastForeground' IS NULL THEN NULL
          WHEN abs(epoch(CAST(apps.value->>'lastForeground' AS TIMESTAMPTZ))) <= 86400 THEN NULL
          ELSE %s
        END,
        m.file_id, s.source_row_id AS source_row_id,
        -- AppUsage collections are JSON objects (not arrays), so elements have
        -- no inherent position; ordering by the object key gives a
        -- deterministic per-entry ordinal, which is all dedup needs.
        ROW_NUMBER() OVER (PARTITION BY s.source_file, s.source_row_id ORDER BY apps.key) AS source_measurement_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      LEFT JOIN LATERAL json_each(s.data->'usage') AS apps ON TRUE
      WHERE s.payload_type = 'dk.cachet.carp.appusage'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .source_timestamp_import_sql("apps.value->>'startDate'", "AppUsage", "period_start"),
    .source_timestamp_import_sql("apps.value->>'endDate'", "AppUsage", "period_end"),
    .source_timestamp_import_sql("apps.value->>'lastForeground'", "AppUsage", "last_foreground"),
    .read_version_filter(sense_version)
  )
}

ingest_battery <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.Battery (participant_id, time, battery_level, battery_status, source_file_id, source_row_id, source_measurement_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'batteryLevel' AS INTEGER),
        CAST(s.data->>'batteryStatus' AS TEXT),
        m.file_id, s.source_row_id AS source_row_id, 1 AS source_measurement_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      WHERE s.payload_type = 'dk.cachet.carp.batterystate'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .read_version_filter(sense_version)
  )
}

# raw_staging rows of one payload type, as a derived table. DuckDB does not
# push a WHERE filter on raw_staging below a CROSS JOIN LATERAL that reads
# s.data (EXPLAIN ANALYZE verified): the lateral expression is evaluated for
# every staged row, so the JSON of the whole batch data column is parsed per
# call, including multi-hundred-KB Garmin payloads (measured ~240-330 ms per
# call on a ~200 MB staging batch, flat across calls). Pre-filtering in a
# derived table keeps the JSON work to the payloads that can match (measured
# ~11 ms). Sensors whose SQL reads s.data only in the projection (scalar
# ->> accesses), in a LEFT JOIN LATERAL (AppUsage), or in a plain scalar
# lateral (the garmin_parsed CTAS) do not need this: the filter is applied
# before the per-row JSON work in those plans.
.read_staging_payloads <- function(payload_type) {
  sprintf(
    "FROM (SELECT * FROM raw_staging WHERE payload_type = '%s') s",
    payload_type
  )
}


ingest_bluetooth <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.Bluetooth (
        participant_id, time, start_scan, end_scan,
        advertisement_name, bluetooth_device_id, bluetooth_device_name,
        connectable, rssi, tx_power_level, source_file_id, source_row_id, source_measurement_id
      )
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        %s,
        %s,
        CAST(j.l[g.i].advertisementName AS TEXT),
        CAST(j.l[g.i].bluetoothDeviceId AS TEXT),
        CAST(j.l[g.i].bluetoothDeviceName AS TEXT),
        CAST(j.l[g.i].connectable AS BOOLEAN),
        CAST(j.l[g.i].rssi AS INTEGER),
        CAST(j.l[g.i].txPowerLevel AS INTEGER),
        m.file_id, s.source_row_id AS source_row_id, g.i AS source_measurement_id
      %s
      JOIN file_id_map m ON s.source_file = m.source_file
      CROSS JOIN LATERAL (
        SELECT %s AS l
      ) j
      -- Empty or missing scanResult still yields one row (with NULL scan
      -- fields) so the scan measurement itself is preserved; this differs
      -- from the Garmin/Connectivity ingests, where a missing array key
      -- yields zero rows.
      CROSS JOIN LATERAL range(1, GREATEST(COALESCE(len(j.l), 0), 1) + 1) AS g(i)
      WHERE s.sensorStartTime IS NOT NULL
        AND %s
",
    .source_timestamp_import_sql("s.data->>'startScan'", "Bluetooth", "start_scan"),
    .source_timestamp_import_sql("s.data->>'endScan'", "Bluetooth", "end_scan"),
    .read_staging_payloads("dk.cachet.carp.bluetooth"),
    .read_json_array_typed("s.data", array_schemas[["Bluetooth"]], key = "scanResult"),
    .read_version_filter(sense_version)
  )
}


ingest_bluetooth_beacon <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.BluetoothBeacon (
        participant_id, time, region, uuid, rssi, major, minor, accuracy, proximity, source_file_id, source_row_id, source_measurement_id
      )
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'region' AS TEXT),
        CAST(j.l[g.i].uuid AS TEXT),
        CAST(j.l[g.i].rssi AS INTEGER),
        CAST(j.l[g.i].major AS INTEGER),
        CAST(j.l[g.i].minor AS INTEGER),
        CAST(j.l[g.i].accuracy AS REAL),
        CAST(j.l[g.i].proximity AS TEXT),
        m.file_id, s.source_row_id AS source_row_id, g.i AS source_measurement_id
      %s
      JOIN file_id_map m ON s.source_file = m.source_file
      CROSS JOIN LATERAL (
        SELECT %s AS l
      ) j
      -- As for Bluetooth above: an empty scanResult preserves the
      -- measurement as one NULL row.
      CROSS JOIN LATERAL range(1, GREATEST(COALESCE(len(j.l), 0), 1) + 1) AS g(i)
      WHERE s.sensorStartTime IS NOT NULL
        AND %s
",
    .read_staging_payloads("dk.cachet.carp.beacondata"),
    .read_json_array_typed("s.data", array_schemas[["BluetoothBeacon"]], key = "scanResult"),
    .read_version_filter(sense_version)
  )
}


ingest_connectivity <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.Connectivity (participant_id, time, connectivity_status, source_file_id, source_row_id, source_measurement_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        e,
        m.file_id, s.source_row_id AS source_row_id, pos AS source_measurement_id
      %s
      JOIN file_id_map m ON s.source_file = m.source_file
      CROSS JOIN LATERAL (
        SELECT CASE WHEN json_type(s.data->'connectivityStatus') = 'ARRAY'
                    THEN CAST(json_transform(s.data->'connectivityStatus', '[\"VARCHAR\"]') AS VARCHAR[])
                    WHEN (s.data->'connectivityStatus') IS NULL THEN CAST([] AS VARCHAR[])
                    ELSE [CAST(s.data->>'connectivityStatus' AS VARCHAR)] END AS l
      ) j,
      LATERAL UNNEST(j.l) WITH ORDINALITY AS t(e, pos)
      WHERE s.sensorStartTime IS NOT NULL
        AND %s
",
    .read_staging_payloads("dk.cachet.carp.connectivity"),
    .read_version_filter(sense_version)
  )
}


ingest_device <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.Device (
        participant_id, time, device_id, hardware,
        device_name, device_manufacturer, device_model, operating_system,
        platform, operating_system_version, device_data, source_file_id, source_row_id, source_measurement_id
      )
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'deviceId' AS TEXT),
        CAST(s.data->>'hardware' AS TEXT),
        CAST(s.data->>'deviceName' AS TEXT),
        CAST(s.data->>'deviceManufacturer' AS TEXT),
        CAST(s.data->>'deviceModel' AS TEXT),
        CAST(s.data->>'operatingSystem' AS TEXT),
        CAST(s.data->>'platform' AS TEXT),
        COALESCE(s.data->'deviceData'->'version'->>'release', s.data->'deviceData'->>'systemVersion'),
        s.data->'deviceData',
        m.file_id, s.source_row_id AS source_row_id, 1 AS source_measurement_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      WHERE s.payload_type = 'dk.cachet.carp.deviceinformation'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .read_version_filter(sense_version)
  )
}

ingest_error <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.Error (participant_id, time, message, source_file_id, source_row_id, source_measurement_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'message' AS TEXT),
        m.file_id, s.source_row_id AS source_row_id, 1 AS source_measurement_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      WHERE s.payload_type = 'dk.cachet.carp.error'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .read_version_filter(sense_version)
  )
}

# ---------------------------------------------------------------------------
# Garmin ingest: one parse per garminalllogsdata payload, feeding every
# Garmin sensor table.
#
# All Garmin arrays live in a single `garminalllogsdata` payload per entry,
# so the per-sensor statements used to each run their own json_transform over
# every staged payload. That parsed each payload once per sensor statement
# (14+ times per batch, also for arrays that are absent from all payloads),
# which dominated Garmin ingest time. Instead, the batch loop first builds a
# `garmin_parsed` temp table that transforms each payload exactly once into
# typed columns (one per array, plus the meta fields), and the ingest
# statements below then only unnest their own column. Absent array keys
# become NULL (and thus zero rows) at no extra cost.
#
# The typed transform keeps memory bounded: the parsed representation costs
# tens of bytes per element instead of the ~1.5-2 KB per element of parsed
# JSON values, and the CTAS streams the transform output straight into the
# temp table.

# SQL for the CREATE TEMP TABLE garmin_parsed statement of one batch (and
# sense version, when the batch mixes versions). Columns: participant_id,
# file_id, source_row_id, sensorStartTime, fromTime, toTime, entryCounts
# (struct of the per-sensor entry counts), and one LIST column per Garmin
# array (heartRate, stress, steps, bbi, enhancedBbi, gyroscope,
# accelerometer, respiration, skinTemperature, spo2, wristStatus,
# zeroCrossing, actigraphy1, actigraphy2, actigraphy3).
# garmin_parsed array column(s) read by each Garmin ingest function.
# GarminMeta is absent on purpose: it derives one row per payload and has no
# array column. Keep in sync with the UNNEST(g.<col>) expressions of the
# ingest functions and with the array_schemas keys of
# .read_garmin_parse_sql().
garmin_sensor_array_cols <- list(
  GarminAccelerometer = "accelerometer",
  GarminActigraphy = c("actigraphy1", "actigraphy2", "actigraphy3"),
  GarminBBI = "bbi",
  GarminEnhancedBBI = "enhancedBbi",
  GarminGyroscope = "gyroscope",
  GarminHeartRate = "heartRate",
  GarminRespiration = "respiration",
  GarminSkinTemperature = "skinTemperature",
  GarminSPO2 = "spo2",
  GarminSteps = "steps",
  GarminStress = "stress",
  GarminWristStatus = "wristStatus",
  GarminZeroCrossing = "zeroCrossing"
)

.read_garmin_parse_sql <- function(sense_version) {
  # Schema of the full payload object. entryCounts counts are BIGINT;
  # fromTime/toTime stay VARCHAR so the GarminMeta legacy conversion can
  # distinguish epoch-millisecond numbers from ISO instants, exactly as when
  # reading them with ->> from the raw JSON.
  arr <- array_schemas
  ec <- c(
    "accelerometer", "actigraphy1", "actigraphy2", "actigraphy3",
    "bbi", "enhancedBbi", "gyroscope", "heartRate", "respiration",
    "skinTemperature", "spo2", "steps", "stress", "wristStatus", "zeroCrossing"
  )
  schema <- paste0(
    '{"fromTime": "VARCHAR", "toTime": "VARCHAR"',
    ', "entryCounts": {', paste0('"', ec, '": "BIGINT"', collapse = ", "), "}",
    ', "heartRate": ', arr$GarminHeartRate,
    ', "stress": ', arr$GarminStress,
    ', "steps": ', arr$GarminSteps,
    ', "bbi": ', arr$GarminBBI,
    ', "enhancedBbi": ', arr$GarminEnhancedBBI,
    ', "gyroscope": ', arr$GarminGyroscope,
    ', "accelerometer": ', arr$GarminAccelerometer,
    ', "respiration": ', arr$GarminRespiration,
    ', "skinTemperature": ', arr$GarminSkinTemperature,
    ', "spo2": ', arr$GarminSPO2,
    ', "wristStatus": ', arr$GarminWristStatus,
    ', "zeroCrossing": ', arr$GarminZeroCrossing,
    ', "actigraphy1": ', arr$GarminActigraphy,
    ', "actigraphy2": ', arr$GarminActigraphy,
    ', "actigraphy3": ', arr$GarminActigraphy,
    "}"
  )
  arr_cols <- c(
    "heartRate", "stress", "steps", "bbi", "enhancedBbi", "gyroscope",
    "accelerometer", "respiration", "skinTemperature", "spo2", "wristStatus",
    "zeroCrossing", "actigraphy1", "actigraphy2", "actigraphy3"
  )
  cols <- paste0(
    "m.participant_id, m.file_id, s.source_row_id, s.sensorStartTime,",
    " p.fromTime, p.toTime, p.entryCounts",
    paste0(", p.", arr_cols, collapse = "")
  )
  sprintf(
    "CREATE OR REPLACE TEMP TABLE garmin_parsed AS
     SELECT %s
     FROM raw_staging s
     JOIN file_id_map m ON m.source_file = s.source_file
     CROSS JOIN LATERAL (
       SELECT json_transform(s.data, '%s') AS p
     ) j
     WHERE s.payload_type = 'dk.cachet.carp.garminalllogsdata'
       AND %s
       AND s.sensorStartTime IS NOT NULL",
    cols,
    schema,
    .read_version_filter(sense_version)
  )
}

ingest_garmin_meta <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.GarminMeta (
        participant_id, time, time_from, time_to,
        n_accelerometer, n_actigraphy_1, n_actigraphy_2, n_actigraphy_3,
        n_bbi, n_enhanced_bbi, n_gyroscope, n_heartrate, n_respiration,
        n_skin_temperature, n_spo2, n_steps, n_stress, n_wrist_status, n_zero_crossing,
        source_file_id, source_row_id, source_measurement_id
      )
      SELECT
        g.participant_id,
        to_timestamp(CAST(g.sensorStartTime AS BIGINT) / 1000000.0),
        %s,
        %s,
        CAST(g.entryCounts.accelerometer AS INTEGER),
        CAST(g.entryCounts.actigraphy1 AS INTEGER),
        CAST(g.entryCounts.actigraphy2 AS INTEGER),
        CAST(g.entryCounts.actigraphy3 AS INTEGER),
        CAST(g.entryCounts.bbi AS INTEGER),
        CAST(g.entryCounts.enhancedBbi AS INTEGER),
        CAST(g.entryCounts.gyroscope AS INTEGER),
        CAST(g.entryCounts.heartRate AS INTEGER),
        CAST(g.entryCounts.respiration AS INTEGER),
        CAST(g.entryCounts.skinTemperature AS INTEGER),
        CAST(g.entryCounts.spo2 AS INTEGER),
        CAST(g.entryCounts.steps AS INTEGER),
        CAST(g.entryCounts.stress AS INTEGER),
        CAST(g.entryCounts.wristStatus AS INTEGER),
        CAST(g.entryCounts.zeroCrossing AS INTEGER),
        g.file_id, g.source_row_id AS source_row_id, 1 AS source_measurement_id
      FROM garmin_parsed g
",
    .source_timestamp_import_sql(
      "COALESCE(to_timestamp(TRY_CAST(g.fromTime AS BIGINT) / 1000.0), TRY_CAST(g.fromTime AS TIMESTAMPTZ))",
      "GarminMeta",
      "time_from"
    ),
    .source_timestamp_import_sql(
      "COALESCE(to_timestamp(TRY_CAST(g.toTime AS BIGINT) / 1000.0), TRY_CAST(g.toTime AS TIMESTAMPTZ))",
      "GarminMeta",
      "time_to"
    )
  )
}

ingest_garmin_heartrate <- function(sense_version) {
  "INSERT INTO raw.GarminHeartRate (
      participant_id, time, bpm, status, mac_address, source_file_id, source_row_id, source_measurement_id
    )
    SELECT
      g.participant_id,
      to_timestamp(CAST(e.timestamp AS BIGINT) / 1000.0),
      NULLIF(TRY_CAST(e.beatsPerMinute AS BIGINT), -1),
      CAST(e.status AS TEXT),
      CAST(e.macAddress AS TEXT),
      g.file_id, g.source_row_id AS source_row_id, pos AS source_measurement_id
    FROM garmin_parsed g
    CROSS JOIN LATERAL UNNEST(g.heartRate) WITH ORDINALITY AS t(e, pos)
    WHERE (e.timestamp) IS NOT NULL"
}

ingest_garmin_stress <- function(sense_version) {
  "INSERT INTO raw.GarminStress (
      participant_id, time, stress, status, mac_address, source_file_id, source_row_id, source_measurement_id
    )
    SELECT
      g.participant_id,
      to_timestamp(CAST(e.timestamp AS BIGINT) / 1000.0),
      NULLIF(TRY_CAST(e.stressScore AS BIGINT), -1),
      CAST(e.status AS TEXT),
      CAST(e.macAddress AS TEXT),
      g.file_id, g.source_row_id AS source_row_id, pos AS source_measurement_id
    FROM garmin_parsed g
    CROSS JOIN LATERAL UNNEST(g.stress) WITH ORDINALITY AS t(e, pos)
    WHERE (e.timestamp) IS NOT NULL"
}

ingest_garmin_steps <- function(sense_version) {
  "INSERT INTO raw.GarminSteps (
      participant_id, time, end_time, step_count, total_steps, mac_address, source_file_id, source_row_id, source_measurement_id
    )
    SELECT
      g.participant_id,
      to_timestamp(CAST(e.startTimestamp AS BIGINT) / 1000.0),
      to_timestamp(CAST(e.endTimestamp AS BIGINT) / 1000.0),
      NULLIF(TRY_CAST(e.stepCount AS BIGINT), -1),
      NULLIF(TRY_CAST(e.totalSteps AS BIGINT), -1),
      CAST(e.macAddress AS TEXT),
      g.file_id, g.source_row_id AS source_row_id, pos AS source_measurement_id
    FROM garmin_parsed g
    CROSS JOIN LATERAL UNNEST(g.steps) WITH ORDINALITY AS t(e, pos)
    WHERE (e.startTimestamp) IS NOT NULL"
}

ingest_garmin_bbi <- function(sense_version) {
  "INSERT INTO raw.GarminBBI (
      participant_id, time, bbi, mac_address, source_file_id, source_row_id, source_measurement_id
    )
    SELECT
      g.participant_id,
      to_timestamp(CAST(e.timestamp AS BIGINT) / 1000.0),
      NULLIF(TRY_CAST(e.bbi AS BIGINT), -1),
      CAST(e.macAddress AS TEXT),
      g.file_id, g.source_row_id AS source_row_id, pos AS source_measurement_id
    FROM garmin_parsed g
    CROSS JOIN LATERAL UNNEST(g.bbi) WITH ORDINALITY AS t(e, pos)
    WHERE (e.timestamp) IS NOT NULL"
}

ingest_garmin_enhanced_bbi <- function(sense_version) {
  "INSERT INTO raw.GarminEnhancedBBI (
      participant_id, time, bbi, status, gap_duration, mac_address, source_file_id, source_row_id, source_measurement_id
    )
    SELECT
      g.participant_id,
      to_timestamp(CAST(e.timestamp AS BIGINT) / 1000.0),
      NULLIF(TRY_CAST(e.bbi AS BIGINT), -1),
      CAST(e.status AS TEXT),
      CAST(e.gapDuration AS INTEGER),
      CAST(e.macAddress AS TEXT),
      g.file_id, g.source_row_id AS source_row_id, pos AS source_measurement_id
    FROM garmin_parsed g
    CROSS JOIN LATERAL UNNEST(g.enhancedBbi) WITH ORDINALITY AS t(e, pos)
    WHERE (e.timestamp) IS NOT NULL"
}

ingest_garmin_gyroscope <- function(sense_version) {
  "INSERT INTO raw.GarminGyroscope (
      participant_id, time, x, y, z, mac_address, source_file_id, source_row_id, source_measurement_id
    )
    SELECT
      g.participant_id,
      to_timestamp(CAST(e.timestamp AS BIGINT) / 1000.0),
      CAST(e.xValue AS REAL),
      CAST(e.yValue AS REAL),
      CAST(e.zValue AS REAL),
      CAST(e.macAddress AS TEXT),
      g.file_id, g.source_row_id AS source_row_id, pos AS source_measurement_id
    FROM garmin_parsed g
    CROSS JOIN LATERAL UNNEST(g.gyroscope) WITH ORDINALITY AS t(e, pos)
    WHERE (e.timestamp) IS NOT NULL"
}

ingest_garmin_accelerometer <- function(sense_version) {
  "INSERT INTO raw.GarminAccelerometer (
      participant_id, time, x, y, z, mac_address, source_file_id, source_row_id, source_measurement_id
    )
    SELECT
      g.participant_id,
      to_timestamp(CAST(e.timestamp AS BIGINT) / 1000.0),
      CAST(e.xValue AS REAL),
      CAST(e.yValue AS REAL),
      CAST(e.zValue AS REAL),
      CAST(e.macAddress AS TEXT),
      g.file_id, g.source_row_id AS source_row_id, pos AS source_measurement_id
    FROM garmin_parsed g
    CROSS JOIN LATERAL UNNEST(g.accelerometer) WITH ORDINALITY AS t(e, pos)
    WHERE (e.timestamp) IS NOT NULL"
}

ingest_garmin_respiration <- function(sense_version) {
  "INSERT INTO raw.GarminRespiration (
      participant_id, time, bpm, status, mac_address, source_file_id, source_row_id, source_measurement_id
    )
    SELECT
      g.participant_id,
      to_timestamp(CAST(e.timestamp AS BIGINT) / 1000.0),
      CAST(e.breathsPerMinute AS REAL),
      CAST(e.status AS TEXT),
      CAST(e.macAddress AS TEXT),
      g.file_id, g.source_row_id AS source_row_id, pos AS source_measurement_id
    FROM garmin_parsed g
    CROSS JOIN LATERAL UNNEST(g.respiration) WITH ORDINALITY AS t(e, pos)
    WHERE (e.timestamp) IS NOT NULL"
}

ingest_garmin_skintemperature <- function(sense_version) {
  "INSERT INTO raw.GarminSkinTemperature (
      participant_id, time, temperature, status, mac_address, source_file_id, source_row_id, source_measurement_id
    )
    SELECT
      g.participant_id,
      to_timestamp(CAST(e.timestamp AS BIGINT) / 1000.0),
      CAST(e.temperature AS REAL),
      CAST(e.status AS TEXT),
      CAST(e.macAddress AS TEXT),
      g.file_id, g.source_row_id AS source_row_id, pos AS source_measurement_id
    FROM garmin_parsed g
    CROSS JOIN LATERAL UNNEST(g.skinTemperature) WITH ORDINALITY AS t(e, pos)
    WHERE (e.timestamp) IS NOT NULL"
}

ingest_garmin_spo2 <- function(sense_version) {
  "INSERT INTO raw.GarminSPO2 (
      participant_id, time, spo2, mac_address, source_file_id, source_row_id, source_measurement_id
    )
    SELECT
      g.participant_id,
      to_timestamp(CAST(e.timestamp AS BIGINT) / 1000.0),
      NULLIF(TRY_CAST(e.spo2Reading AS BIGINT), -1),
      CAST(e.macAddress AS TEXT),
      g.file_id, g.source_row_id AS source_row_id, pos AS source_measurement_id
    FROM garmin_parsed g
    CROSS JOIN LATERAL UNNEST(g.spo2) WITH ORDINALITY AS t(e, pos)
    WHERE (e.timestamp) IS NOT NULL"
}

ingest_garmin_wriststatus <- function(sense_version) {
  "INSERT INTO raw.GarminWristStatus (
      participant_id, time, status, mac_address, source_file_id, source_row_id, source_measurement_id
    )
    SELECT
      g.participant_id,
      to_timestamp(CAST(e.timestamp AS BIGINT) / 1000.0),
      CAST(e.status AS TEXT),
      CAST(e.macAddress AS TEXT),
      g.file_id, g.source_row_id AS source_row_id, pos AS source_measurement_id
    FROM garmin_parsed g
    CROSS JOIN LATERAL UNNEST(g.wristStatus) WITH ORDINALITY AS t(e, pos)
    WHERE (e.timestamp) IS NOT NULL"
}

ingest_garmin_zerocrossing <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.GarminZeroCrossing (
        participant_id, time, end_time, total_energy, n_zero_crossing, deadband, mac_address, source_file_id, source_row_id, source_measurement_id
      )
      SELECT
        g.participant_id,
        to_timestamp(CAST(e.startTimestamp AS BIGINT) / 1000.0),
        to_timestamp(CAST(e.endTimestamp AS BIGINT) / 1000.0),
        %s,
        %s,
        CAST(e.deadband AS INTEGER),
        CAST(e.macAddress AS TEXT),
        g.file_id, g.source_row_id AS source_row_id, pos AS source_measurement_id
      FROM garmin_parsed g
      CROSS JOIN LATERAL UNNEST(g.zeroCrossing) WITH ORDINALITY AS t(e, pos)
      WHERE (e.startTimestamp) IS NOT NULL",
    .read_null_neg("e.totalEnergy"),
    .read_null_neg("e.zeroCrossingCount")
  )
}

ingest_garmin_actigraphy <- function(sense_version) {
  branch <- function(key, offset) {
    sprintf(
      "SELECT g.participant_id, g.file_id, g.source_row_id AS source_row_id,
              e.startTimestamp AS startTimestamp,
              e.endTimestamp AS endTimestamp,
              e.instance AS instance,
              e.totalEnergy AS totalEnergy,
              e.zeroCrossingCount AS zeroCrossingCount,
              e.timeAboveThreshold AS timeAboveThreshold,
              e.macAddress AS macAddress,
              %d + pos AS source_measurement_id
       FROM garmin_parsed g
       CROSS JOIN LATERAL UNNEST(g.%s) WITH ORDINALITY AS t(e, pos)
       WHERE (e.startTimestamp) IS NOT NULL",
      offset,
      key
    )
  }
  sprintf(
    "INSERT INTO raw.GarminActigraphy (
        participant_id, time, end_time, instance, total_energy, n_zero_crossing, time_above_threshold, mac_address, source_file_id, source_row_id, source_measurement_id
      )
      SELECT
        u.participant_id,
        to_timestamp(CAST(u.startTimestamp AS BIGINT) / 1000.0),
        to_timestamp(CAST(u.endTimestamp AS BIGINT) / 1000.0),
        CAST(u.instance AS TEXT),
        %s,
        %s,
        CAST(u.timeAboveThreshold AS REAL),
        CAST(u.macAddress AS TEXT),
        u.file_id, u.source_row_id AS source_row_id, u.source_measurement_id
      FROM (
        %s
        UNION ALL
        %s
        UNION ALL
        %s
      ) u
      WHERE (u.startTimestamp) IS NOT NULL",
    .read_null_neg("u.totalEnergy", "DOUBLE"),
    .read_null_neg("u.zeroCrossingCount"),
    branch("actigraphy1", 0),
    branch("actigraphy2", 1000000000),
    branch("actigraphy3", 2000000000)
  )
}
ingest_heartbeat <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.Heartbeat (participant_id, time, period, device_type, device_role_name, source_file_id, source_row_id, source_measurement_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'period' AS INTEGER),
        CAST(s.data->>'deviceType' AS TEXT),
        CAST(s.data->>'deviceRoleName' AS TEXT),
        m.file_id, s.source_row_id AS source_row_id, 1 AS source_measurement_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      WHERE s.payload_type = 'dk.cachet.carp.heartbeat'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .read_version_filter(sense_version)
  )
}

ingest_light <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.Light (participant_id, time, end_time, mean_lux, std_lux, min_lux, max_lux, source_file_id, source_row_id, source_measurement_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        to_timestamp(CAST(s.sensorEndTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'meanLux' AS REAL),
        CAST(s.data->>'stdLux' AS REAL),
        CAST(s.data->>'minLux' AS REAL),
        CAST(s.data->>'maxLux' AS REAL),
        m.file_id, s.source_row_id AS source_row_id, 1 AS source_measurement_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      WHERE s.payload_type = 'dk.cachet.carp.ambientlight'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .read_version_filter(sense_version)
  )
}

ingest_location <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.Location (
        participant_id, time, latitude, longitude,
        altitude, accuracy, vertical_accuracy, speed, speed_accuracy,
        heading, heading_accuracy, is_mock,
        elapsed_realtime_nanos, elapsed_realtime_uncertainty_nanos, source_file_id, source_row_id, source_measurement_id
      )
      SELECT
        m.participant_id,
        %s,
        CAST(s.data->>'latitude' AS DOUBLE),
        CAST(s.data->>'longitude' AS DOUBLE),
        CAST(s.data->>'altitude' AS REAL),
        CAST(s.data->>'accuracy' AS REAL),
        CAST(s.data->>'verticalAccuracy' AS REAL),
        CAST(s.data->>'speed' AS REAL),
        CAST(s.data->>'speedAccuracy' AS REAL),
        CAST(s.data->>'heading' AS REAL),
        CAST(s.data->>'headingAccuracy' AS REAL),
        CAST(s.data->>'isMock' AS BOOLEAN),
        CAST(s.data->>'elapsedRealtimeNanos' AS UBIGINT),
        CAST(s.data->>'elapsedRealtimeUncertaintyNanos' AS UBIGINT),
        m.file_id, s.source_row_id AS source_row_id, 1 AS source_measurement_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      WHERE s.payload_type = 'dk.cachet.carp.location'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .source_timestamp_import_sql(
      "to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0)",
      "Location",
      "time"
    ),
    .read_version_filter(sense_version)
  )
}

ingest_memory <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.Memory (participant_id, time, free_physical_memory, free_virtual_memory, source_file_id, source_row_id, source_measurement_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'freePhysicalMemory' AS BIGINT),
        CAST(s.data->>'freeVirtualMemory' AS BIGINT),
        m.file_id, s.source_row_id AS source_row_id, 1 AS source_measurement_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      WHERE s.payload_type = 'dk.cachet.carp.freememory'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .read_version_filter(sense_version)
  )
}

ingest_pedometer <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.Pedometer (participant_id, time, step_count, source_file_id, source_row_id, source_measurement_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'steps' AS INTEGER),
        m.file_id, s.source_row_id AS source_row_id, 1 AS source_measurement_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      WHERE s.payload_type = 'dk.cachet.carp.stepcount'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .read_version_filter(sense_version)
  )
}

ingest_screen <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.Screen (participant_id, time, screen_event, source_file_id, source_row_id, source_measurement_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'screenEvent' AS TEXT),
        m.file_id, s.source_row_id AS source_row_id, 1 AS source_measurement_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      WHERE s.payload_type = 'dk.cachet.carp.screenevent'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .read_version_filter(sense_version)
  )
}

ingest_timezone <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.Timezone (participant_id, time, timezone, source_file_id, source_row_id, source_measurement_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'timezone' AS TEXT),
        m.file_id, s.source_row_id AS source_row_id, 1 AS source_measurement_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      WHERE s.payload_type = 'dk.cachet.carp.timezone'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .read_version_filter(sense_version)
  )
}

ingest_weather <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.Weather (
        participant_id, time, country, area_name,
        weather_main, weather_description, sunrise, sunset, latitude, longitude,
        pressure, wind_speed, wind_degree, humidity, cloudiness,
        rain_last_hour, rain_last_3hours, snow_last_hour, snow_last_3hours,
        temperature, temp_min, temp_max, source_file_id, source_row_id, source_measurement_id
      )
      SELECT
        m.participant_id,
        %s,
        CAST(s.data->>'country' AS TEXT),
        CAST(s.data->>'areaName' AS TEXT),
        CAST(s.data->>'weatherMain' AS TEXT),
        CAST(s.data->>'weatherDescription' AS TEXT),
        %s,
        %s,
        CAST(s.data->>'latitude' AS DOUBLE),
        CAST(s.data->>'longitude' AS DOUBLE),
        CAST(s.data->>'pressure' AS REAL),
        CAST(s.data->>'windSpeed' AS REAL),
        CAST(s.data->>'windDegree' AS REAL),
        CAST(s.data->>'humidity' AS REAL),
        CAST(s.data->>'cloudiness' AS REAL),
        CAST(s.data->>'rainLastHour' AS REAL),
        CAST(s.data->>'rainLast3Hours' AS REAL),
        CAST(s.data->>'snowLastHour' AS REAL),
        CAST(s.data->>'snowLast3Hours' AS REAL),
        CAST(s.data->>'temperature' AS REAL),
        CAST(s.data->>'tempMin' AS REAL),
        CAST(s.data->>'tempMax' AS REAL),
        m.file_id, s.source_row_id AS source_row_id, 1 AS source_measurement_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      WHERE s.payload_type = 'dk.cachet.carp.weather'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .source_timestamp_import_sql(
      "to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0)",
      "Weather",
      "time"
    ),
    .source_timestamp_import_sql(
      "COALESCE(to_timestamp(TRY_CAST(s.data->>'sunrise' AS DOUBLE)), TRY_CAST(s.data->>'sunrise' AS TIMESTAMPTZ))",
      "Weather",
      "sunrise"
    ),
    .source_timestamp_import_sql(
      "COALESCE(to_timestamp(TRY_CAST(s.data->>'sunset' AS DOUBLE)), TRY_CAST(s.data->>'sunset' AS TIMESTAMPTZ))",
      "Weather",
      "sunset"
    ),
    .read_version_filter(sense_version)
  )
}

ingest_wifi <- function(sense_version) {
  sprintf(
    "INSERT INTO raw.Wifi (participant_id, time, ssid, bssid, ip, source_file_id, source_row_id, source_measurement_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'ssid' AS TEXT),
        CAST(s.data->>'bssid' AS TEXT),
        CAST(s.data->>'ip' AS TEXT),
        m.file_id, s.source_row_id AS source_row_id, 1 AS source_measurement_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      WHERE s.payload_type = 'dk.cachet.carp.wifi'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .read_version_filter(sense_version)
  )
}

# ---------------------------------------------------------------------------
