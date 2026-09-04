# Ingest functions of the read_mpath_sense() pipeline.
#
# Each function builds the SQL statement that inserts the data of one sensor
# from the raw_staging temp table into the corresponding sensor table, using
# file_id_map to link each row to its source file (participant, senseVersion,
# file_id). The statements are executed by .read_ingest(), which optionally
# chunks them with LIMIT/OFFSET to bound memory usage. All timestamps are
# stored as UTC instants in DuckDB; observation timezones are populated after
# ingestion by the normalization step.

ingest_accelerometer <- function(sense_version) {
  sprintf(
    "INSERT INTO Accelerometer (
        participant_id, time, end_time, n,
        x_mean, y_mean, z_mean, x_median, y_median, z_median,
        x_std, y_std, z_std, x_aad, y_aad, z_aad,
        x_min, y_min, z_min, x_max, y_max, z_max,
        x_max_min_diff, y_max_min_diff, z_max_min_diff,
        x_mad, y_mad, z_mad, x_iqr, y_iqr, z_iqr,
        x_neg_n, y_neg_n, z_neg_n, x_pos_n, y_pos_n, z_pos_n,
        x_above_mean, y_above_mean, z_above_mean,
        x_energy, y_energy, z_energy, avg_res_acc, sma,
        source_file_id
      )
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        to_timestamp(CAST(s.sensorEndTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'count' AS INTEGER),
        CAST(s.data->>'xMean' AS REAL), CAST(s.data->>'yMean' AS REAL), CAST(s.data->>'zMean' AS REAL),
        CAST(s.data->>'xMedian' AS REAL), CAST(s.data->>'yMedian' AS REAL), CAST(s.data->>'zMedian' AS REAL),
        CAST(s.data->>'xStd' AS REAL), CAST(s.data->>'yStd' AS REAL), CAST(s.data->>'zStd' AS REAL),
        CAST(s.data->>'xAad' AS REAL), CAST(s.data->>'yAad' AS REAL), CAST(s.data->>'zAad' AS REAL),
        CAST(s.data->>'xMin' AS REAL), CAST(s.data->>'yMin' AS REAL), CAST(s.data->>'zMin' AS REAL),
        CAST(s.data->>'xMax' AS REAL), CAST(s.data->>'yMax' AS REAL), CAST(s.data->>'zMax' AS REAL),
        CAST(s.data->>'xMaxMinDiff' AS REAL), CAST(s.data->>'yMaxMinDiff' AS REAL), CAST(s.data->>'zMaxMinDiff' AS REAL),
        CAST(s.data->>'xMad' AS REAL), CAST(s.data->>'yMad' AS REAL), CAST(s.data->>'zMad' AS REAL),
        CAST(s.data->>'xIqr' AS REAL), CAST(s.data->>'yIqr' AS REAL), CAST(s.data->>'zIqr' AS REAL),
        CAST(s.data->>'xNegCount' AS INTEGER), CAST(s.data->>'yNegCount' AS INTEGER), CAST(s.data->>'zNegCount' AS INTEGER),
        CAST(s.data->>'xPosCount' AS INTEGER), CAST(s.data->>'yPosCount' AS INTEGER), CAST(s.data->>'zPosCount' AS INTEGER),
        CAST(s.data->>'xAboveMean' AS INTEGER), CAST(s.data->>'yAboveMean' AS INTEGER), CAST(s.data->>'zAboveMean' AS INTEGER),
        CAST(s.data->>'xEnergy' AS REAL), CAST(s.data->>'yEnergy' AS REAL), CAST(s.data->>'zEnergy' AS REAL),
        CAST(s.data->>'avgResultAcceleration' AS REAL), CAST(s.data->>'signalMagnitudeArea' AS REAL),
        m.file_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      WHERE s.payload_type = 'dk.cachet.carp.accelerationfeatures'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .read_version_filter(sense_version)
  )
}

ingest_activity <- function(sense_version) {
  sprintf(
    "INSERT INTO Activity (participant_id, time, confidence, type, source_file_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'confidence' AS INTEGER),
        CAST(s.data->>'type' AS TEXT),
        m.file_id
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
    "INSERT INTO AppUsage (
        participant_id, time, end_time, period_start, period_end,
        usage, app, package_name, last_foreground, source_file_id
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
        m.file_id
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
    "INSERT INTO Battery (participant_id, time, battery_level, battery_status, source_file_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'batteryLevel' AS INTEGER),
        CAST(s.data->>'batteryStatus' AS TEXT),
        m.file_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      WHERE s.payload_type = 'dk.cachet.carp.batterystate'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .read_version_filter(sense_version)
  )
}

ingest_bluetooth <- function(sense_version) {
  sprintf(
    "INSERT INTO Bluetooth (
        participant_id, time, start_scan, end_scan,
        advertisement_name, bluetooth_device_id, bluetooth_device_name,
        connectable, rssi, tx_power_level, source_file_id
      )
      SELECT
        u.participant_id,
        to_timestamp(CAST(u.sensorStartTime AS BIGINT) / 1000000.0),
        %s,
        %s,
        CAST(u.res.advertisementName AS TEXT),
        CAST(u.res.bluetoothDeviceId AS TEXT),
        CAST(u.res.bluetoothDeviceName AS TEXT),
        CAST(u.res.connectable AS BOOLEAN),
        CAST(u.res.rssi AS INTEGER),
        CAST(u.res.txPowerLevel AS INTEGER),
        u.file_id
      FROM (
        SELECT m.participant_id, m.file_id, m.sense_version, s.sensorStartTime,
        s.data->>'startScan' AS start_scan,
        s.data->>'endScan' AS end_scan,
        res
        FROM raw_staging s
        JOIN file_id_map m ON s.source_file = m.source_file
        LEFT JOIN LATERAL UNNEST(%s) AS scan(res) ON TRUE
        WHERE s.payload_type = 'dk.cachet.carp.bluetooth'
          AND %s

      ) u
      WHERE u.sensorStartTime IS NOT NULL",
    .source_timestamp_import_sql("u.start_scan", "Bluetooth", "start_scan", "u"),
    .source_timestamp_import_sql("u.end_scan", "Bluetooth", "end_scan", "u"),
    .read_json_array_typed("s.data", array_schemas[["Bluetooth"]], key = "scanResult"),
    .read_version_filter(sense_version)
  )
}

ingest_bluetooth_beacon <- function(sense_version) {
  sprintf(
    "INSERT INTO BluetoothBeacon (
        participant_id, time, region, uuid, rssi, major, minor, accuracy, proximity, source_file_id
      )
      SELECT
        u.participant_id,
        to_timestamp(CAST(u.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(u.region AS TEXT),
        CAST(u.beacons.uuid AS TEXT),
        CAST(u.beacons.rssi AS INTEGER),
        CAST(u.beacons.major AS INTEGER),
        CAST(u.beacons.minor AS INTEGER),
        CAST(u.beacons.accuracy AS REAL),
        CAST(u.beacons.proximity AS TEXT),
        u.file_id
      FROM (
        SELECT m.participant_id, m.file_id, s.sensorStartTime,
        s.data->>'region' AS region,
        s.data->>'startScan' AS start_scan,
        s.data->>'endScan' AS end_scan,
        beacons
        FROM raw_staging s
        JOIN file_id_map m ON s.source_file = m.source_file
        LEFT JOIN LATERAL UNNEST(%s) AS scan(beacons) ON TRUE
        WHERE s.payload_type = 'dk.cachet.carp.beacondata'
          AND %s

      ) u
      WHERE u.sensorStartTime IS NOT NULL",
    .read_json_array_typed("s.data", array_schemas[["BluetoothBeacon"]], key = "scanResult"),
    .read_version_filter(sense_version)
  )
}

ingest_connectivity <- function(sense_version) {
  sprintf(
    "INSERT INTO Connectivity (participant_id, time, connectivity_status, source_file_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        status,
        m.file_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file,
      UNNEST(
        CASE WHEN json_type(s.data->'connectivityStatus') = 'ARRAY'
             THEN CAST(json_transform(s.data->'connectivityStatus', '[\"VARCHAR\"]') AS VARCHAR[])
             WHEN (s.data->'connectivityStatus') IS NULL THEN CAST([] AS VARCHAR[])
             ELSE [CAST(s.data->>'connectivityStatus' AS VARCHAR)] END
      ) AS cs(status)
      WHERE s.payload_type = 'dk.cachet.carp.connectivity'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .read_version_filter(sense_version)
  )
}

ingest_device <- function(sense_version) {
  sprintf(
    "INSERT INTO Device (
        participant_id, time, device_id, hardware,
        device_name, device_manufacturer, device_model, operating_system,
        platform, operating_system_version, device_data, source_file_id
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
        m.file_id
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
    "INSERT INTO Error (participant_id, time, message, source_file_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'message' AS TEXT),
        m.file_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      WHERE s.payload_type = 'dk.cachet.carp.error'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .read_version_filter(sense_version)
  )
}

ingest_garmin_meta <- function(sense_version) {
  sprintf(
    "INSERT INTO GarminMeta (
        participant_id, time, time_from, time_to,
        n_accelerometer, n_actigraphy_1, n_actigraphy_2, n_actigraphy_3,
        n_bbi, n_enhanced_bbi, n_gyroscope, n_heartrate, n_respiration,
        n_skin_temperature, n_spo2, n_steps, n_stress, n_wrist_status, n_zero_crossing,
        source_file_id
      )
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        %s,
        %s,
        CAST(s.data->'entryCounts'->>'accelerometer' AS INTEGER),
        CAST(s.data->'entryCounts'->>'actigraphy1' AS INTEGER),
        CAST(s.data->'entryCounts'->>'actigraphy2' AS INTEGER),
        CAST(s.data->'entryCounts'->>'actigraphy3' AS INTEGER),
        CAST(s.data->'entryCounts'->>'bbi' AS INTEGER),
        CAST(s.data->'entryCounts'->>'enhancedBbi' AS INTEGER),
        CAST(s.data->'entryCounts'->>'gyroscope' AS INTEGER),
        CAST(s.data->'entryCounts'->>'heartRate' AS INTEGER),
        CAST(s.data->'entryCounts'->>'respiration' AS INTEGER),
        CAST(s.data->'entryCounts'->>'skinTemperature' AS INTEGER),
        CAST(s.data->'entryCounts'->>'spo2' AS INTEGER),
        CAST(s.data->'entryCounts'->>'steps' AS INTEGER),
        CAST(s.data->'entryCounts'->>'stress' AS INTEGER),
        CAST(s.data->'entryCounts'->>'wristStatus' AS INTEGER),
        CAST(s.data->'entryCounts'->>'zeroCrossing' AS INTEGER),
        m.file_id
      FROM raw_staging s
      JOIN file_id_map m ON s.source_file = m.source_file
      WHERE s.payload_type = 'dk.cachet.carp.garminalllogsdata'
        AND %s
        AND s.sensorStartTime IS NOT NULL
",
    .source_timestamp_import_sql(
      "COALESCE(to_timestamp(TRY_CAST(s.data->>'fromTime' AS BIGINT) / 1000.0), TRY_CAST(s.data->>'fromTime' AS TIMESTAMPTZ))",
      "GarminMeta",
      "time_from"
    ),
    .source_timestamp_import_sql(
      "COALESCE(to_timestamp(TRY_CAST(s.data->>'toTime' AS BIGINT) / 1000.0), TRY_CAST(s.data->>'toTime' AS TIMESTAMPTZ))",
      "GarminMeta",
      "time_to"
    ),
    .read_version_filter(sense_version)
  )
}

ingest_garmin_heartrate <- function(sense_version) {
  sprintf(
    "INSERT INTO GarminHeartRate (participant_id, time, bpm, status, mac_address, source_file_id)
      SELECT
        u.participant_id,
        to_timestamp(CAST(u.hr.timestamp AS BIGINT) / 1000.0),
        %s,
        CAST(u.hr.status AS TEXT),
        CAST(u.hr.macAddress AS TEXT),
        u.file_id
      FROM (
        SELECT m.participant_id, m.file_id, s.sensorStartTime,
               UNNEST(%s) AS hr
        FROM raw_staging s
        JOIN file_id_map m ON s.source_file = m.source_file
        WHERE s.payload_type = 'dk.cachet.carp.garminalllogsdata'
          AND %s

      ) u
      WHERE (u.hr.timestamp) IS NOT NULL",
    .read_null_neg("u.hr.beatsPerMinute"),
    .read_json_array_typed("s.data", array_schemas[["GarminHeartRate"]], key = "heartRate"),
    .read_version_filter(sense_version)
  )
}

ingest_garmin_stress <- function(sense_version) {
  sprintf(
    "INSERT INTO GarminStress (participant_id, time, stress, status, mac_address, source_file_id)
      SELECT
        u.participant_id,
        to_timestamp(CAST(u.st.timestamp AS BIGINT) / 1000.0),
        %s,
        CAST(u.st.status AS TEXT),
        CAST(u.st.macAddress AS TEXT),
        u.file_id
      FROM (
        SELECT m.participant_id, m.file_id, s.sensorStartTime,
               UNNEST(%s) AS st
        FROM raw_staging s
        JOIN file_id_map m ON s.source_file = m.source_file
        WHERE s.payload_type = 'dk.cachet.carp.garminalllogsdata'
          AND %s

      ) u
      WHERE (u.st.timestamp) IS NOT NULL",
    .read_null_neg("u.st.stressScore"),
    .read_json_array_typed("s.data", array_schemas[["GarminStress"]], key = "stress"),
    .read_version_filter(sense_version)
  )
}

ingest_garmin_steps <- function(sense_version) {
  sprintf(
    "INSERT INTO GarminSteps (participant_id, time, end_time, step_count, total_steps, mac_address, source_file_id)
      SELECT
        u.participant_id,
        to_timestamp(CAST(u.st.startTimestamp AS BIGINT) / 1000.0),
        to_timestamp(CAST(u.st.endTimestamp AS BIGINT) / 1000.0),
        %s,
        %s,
        CAST(u.st.macAddress AS TEXT),
        u.file_id
      FROM (
        SELECT m.participant_id, m.file_id, s.sensorStartTime,
               UNNEST(%s) AS st
        FROM raw_staging s
        JOIN file_id_map m ON s.source_file = m.source_file
        WHERE s.payload_type = 'dk.cachet.carp.garminalllogsdata'
          AND %s

      ) u
      WHERE (u.st.startTimestamp) IS NOT NULL",
    .read_null_neg("u.st.stepCount"),
    .read_null_neg("u.st.totalSteps"),
    .read_json_array_typed("s.data", array_schemas[["GarminSteps"]], key = "steps"),
    .read_version_filter(sense_version)
  )
}

ingest_garmin_bbi <- function(sense_version) {
  sprintf(
    "INSERT INTO GarminBBI (participant_id, time, bbi, mac_address, source_file_id)
      SELECT
        u.participant_id,
        to_timestamp(CAST(u.b.timestamp AS BIGINT) / 1000.0),
        %s,
        CAST(u.b.macAddress AS TEXT),
        u.file_id
      FROM (
        SELECT m.participant_id, m.file_id, s.sensorStartTime,
               UNNEST(%s) AS b
        FROM raw_staging s
        JOIN file_id_map m ON s.source_file = m.source_file
        WHERE s.payload_type = 'dk.cachet.carp.garminalllogsdata'
          AND %s

      ) u
      WHERE (u.b.timestamp) IS NOT NULL",
    .read_null_neg("u.b.bbi"),
    .read_json_array_typed("s.data", array_schemas[["GarminBBI"]], key = "bbi"),
    .read_version_filter(sense_version)
  )
}

ingest_garmin_enhanced_bbi <- function(sense_version) {
  sprintf(
    "INSERT INTO GarminEnhancedBBI (participant_id, time, bbi, status, gap_duration, mac_address, source_file_id)
      SELECT
        u.participant_id,
        to_timestamp(CAST(u.eb.timestamp AS BIGINT) / 1000.0),
        %s,
        CAST(u.eb.status AS TEXT),
        CAST(u.eb.gapDuration AS INTEGER),
        CAST(u.eb.macAddress AS TEXT),
        u.file_id
      FROM (
        SELECT m.participant_id, m.file_id, s.sensorStartTime,
               UNNEST(%s) AS eb
        FROM raw_staging s
        JOIN file_id_map m ON s.source_file = m.source_file
        WHERE s.payload_type = 'dk.cachet.carp.garminalllogsdata'
          AND %s

      ) u
      WHERE (u.eb.timestamp) IS NOT NULL",
    .read_null_neg("u.eb.bbi"),
    .read_json_array_typed("s.data", array_schemas[["GarminEnhancedBBI"]], key = "enhancedBbi"),
    .read_version_filter(sense_version)
  )
}

ingest_garmin_gyroscope <- function(sense_version) {
  sprintf(
    "INSERT INTO GarminGyroscope (participant_id, time, x, y, z, mac_address, source_file_id)
      SELECT
        u.participant_id,
        to_timestamp(CAST(u.g.timestamp AS BIGINT) / 1000.0),
        CAST(u.g.xValue AS REAL),
        CAST(u.g.yValue AS REAL),
        CAST(u.g.zValue AS REAL),
        CAST(u.g.macAddress AS TEXT),
        u.file_id
      FROM (
        SELECT m.participant_id, m.file_id, s.sensorStartTime,
               UNNEST(%s) AS g
        FROM raw_staging s
        JOIN file_id_map m ON s.source_file = m.source_file
        WHERE s.payload_type = 'dk.cachet.carp.garminalllogsdata'
          AND %s

      ) u
      WHERE (u.g.timestamp) IS NOT NULL",
    .read_json_array_typed("s.data", array_schemas[["GarminGyroscope"]], key = "gyroscope"),
    .read_version_filter(sense_version)
  )
}

ingest_garmin_accelerometer <- function(sense_version) {
  sprintf(
    "INSERT INTO GarminAccelerometer (participant_id, time, x, y, z, mac_address, source_file_id)
      SELECT
        u.participant_id,
        to_timestamp(CAST(u.acc.timestamp AS BIGINT) / 1000.0),
        CAST(u.acc.xValue AS REAL),
        CAST(u.acc.yValue AS REAL),
        CAST(u.acc.zValue AS REAL),
        CAST(u.acc.macAddress AS TEXT),
        u.file_id
      FROM (
        SELECT m.participant_id, m.file_id, s.sensorStartTime,
               UNNEST(%s) AS acc
        FROM raw_staging s
        JOIN file_id_map m ON s.source_file = m.source_file
        WHERE s.payload_type = 'dk.cachet.carp.garminalllogsdata'
          AND %s

      ) u
      WHERE (u.acc.timestamp) IS NOT NULL",
    .read_json_array_typed("s.data", array_schemas[["GarminAccelerometer"]], key = "accelerometer"),
    .read_version_filter(sense_version)
  )
}

ingest_garmin_respiration <- function(sense_version) {
  sprintf(
    "INSERT INTO GarminRespiration (participant_id, time, bpm, status, mac_address, source_file_id)
      SELECT
        u.participant_id,
        to_timestamp(CAST(u.r.timestamp AS BIGINT) / 1000.0),
        CAST(u.r.breathsPerMinute AS REAL),
        CAST(u.r.status AS TEXT),
        CAST(u.r.macAddress AS TEXT),
        u.file_id
      FROM (
        SELECT m.participant_id, m.file_id, s.sensorStartTime,
               UNNEST(%s) AS r
        FROM raw_staging s
        JOIN file_id_map m ON s.source_file = m.source_file
        WHERE s.payload_type = 'dk.cachet.carp.garminalllogsdata'
          AND %s

      ) u
      WHERE (u.r.timestamp) IS NOT NULL",
    .read_json_array_typed("s.data", array_schemas[["GarminRespiration"]], key = "respiration"),
    .read_version_filter(sense_version)
  )
}

ingest_garmin_skintemperature <- function(sense_version) {
  sprintf(
    "INSERT INTO GarminSkinTemperature (participant_id, time, temperature, status, mac_address, source_file_id)
      SELECT
        u.participant_id,
        to_timestamp(CAST(u.t.timestamp AS BIGINT) / 1000.0),
        CAST(u.t.temperature AS REAL),
        CAST(u.t.status AS TEXT),
        CAST(u.t.macAddress AS TEXT),
        u.file_id
      FROM (
        SELECT m.participant_id, m.file_id, s.sensorStartTime,
               UNNEST(%s) AS t
        FROM raw_staging s
        JOIN file_id_map m ON s.source_file = m.source_file
        WHERE s.payload_type = 'dk.cachet.carp.garminalllogsdata'
          AND %s

      ) u
      WHERE (u.t.timestamp) IS NOT NULL",
    .read_json_array_typed(
      "s.data",
      array_schemas[["GarminSkinTemperature"]],
      key = "skinTemperature"
    ),
    .read_version_filter(sense_version)
  )
}

ingest_garmin_spo2 <- function(sense_version) {
  sprintf(
    "INSERT INTO GarminSPO2 (participant_id, time, spo2, mac_address, source_file_id)
      SELECT
        u.participant_id,
        to_timestamp(CAST(u.sp.timestamp AS BIGINT) / 1000.0),
        %s,
        CAST(u.sp.macAddress AS TEXT),
        u.file_id
      FROM (
        SELECT m.participant_id, m.file_id, s.sensorStartTime,
               UNNEST(%s) AS sp
        FROM raw_staging s
        JOIN file_id_map m ON s.source_file = m.source_file
        WHERE s.payload_type = 'dk.cachet.carp.garminalllogsdata'
          AND %s

      ) u
      WHERE (u.sp.timestamp) IS NOT NULL",
    .read_null_neg("u.sp.spo2Reading"),
    .read_json_array_typed("s.data", array_schemas[["GarminSPO2"]], key = "spo2"),
    .read_version_filter(sense_version)
  )
}

ingest_garmin_wriststatus <- function(sense_version) {
  sprintf(
    "INSERT INTO GarminWristStatus (participant_id, time, status, mac_address, source_file_id)
      SELECT
        u.participant_id,
        to_timestamp(CAST(u.w.timestamp AS BIGINT) / 1000.0),
        CAST(u.w.status AS TEXT),
        CAST(u.w.macAddress AS TEXT),
        u.file_id
      FROM (
        SELECT m.participant_id, m.file_id, s.sensorStartTime,
               UNNEST(%s) AS w
        FROM raw_staging s
        JOIN file_id_map m ON s.source_file = m.source_file
        WHERE s.payload_type = 'dk.cachet.carp.garminalllogsdata'
          AND %s

      ) u
      WHERE (u.w.timestamp) IS NOT NULL",
    .read_json_array_typed("s.data", array_schemas[["GarminWristStatus"]], key = "wristStatus"),
    .read_version_filter(sense_version)
  )
}

ingest_garmin_zerocrossing <- function(sense_version) {
  sprintf(
    "INSERT INTO GarminZeroCrossing (
        participant_id, time, end_time, total_energy, n_zero_crossing, deadband, mac_address, source_file_id
      )
      SELECT
        u.participant_id,
        to_timestamp(CAST(u.zc.startTimestamp AS BIGINT) / 1000.0),
        to_timestamp(CAST(u.zc.endTimestamp AS BIGINT) / 1000.0),
        %s,
        %s,
        CAST(u.zc.deadband AS INTEGER),
        CAST(u.zc.macAddress AS TEXT),
        u.file_id
      FROM (
        SELECT m.participant_id, m.file_id, s.sensorStartTime,
               UNNEST(%s) AS zc
        FROM raw_staging s
        JOIN file_id_map m ON s.source_file = m.source_file
        WHERE s.payload_type = 'dk.cachet.carp.garminalllogsdata'
          AND %s

      ) u
      WHERE (u.zc.startTimestamp) IS NOT NULL",
    .read_null_neg("u.zc.totalEnergy"),
    .read_null_neg("u.zc.zeroCrossingCount"),
    .read_json_array_typed("s.data", array_schemas[["GarminZeroCrossing"]], key = "zeroCrossing"),
    .read_version_filter(sense_version)
  )
}

ingest_garmin_actigraphy <- function(sense_version) {
  branch <- function(key) {
    sprintf(
      "SELECT m.participant_id, m.file_id,
              UNNEST(%s) AS act
       FROM raw_staging
       JOIN file_id_map m ON raw_staging.source_file = m.source_file
       WHERE payload_type = 'dk.cachet.carp.garminalllogsdata'
         AND %s
 ",
      .read_json_array_typed("data", array_schemas[["GarminActigraphy"]], key = key),
      .read_version_filter(sense_version)
    )
  }
  sprintf(
    "INSERT INTO GarminActigraphy (
        participant_id, time, end_time, instance, total_energy, n_zero_crossing, time_above_threshold, mac_address, source_file_id
      )
      SELECT
        u.participant_id,
        to_timestamp(CAST(u.act.startTimestamp AS BIGINT) / 1000.0),
        to_timestamp(CAST(u.act.endTimestamp AS BIGINT) / 1000.0),
        CAST(u.act.instance AS TEXT),
        %s,
        %s,
        CAST(u.act.timeAboveThreshold AS REAL),
        CAST(u.act.macAddress AS TEXT),
        u.file_id
      FROM (
        %s
        UNION ALL
        %s
        UNION ALL
        %s
      ) u
      WHERE (u.act.startTimestamp) IS NOT NULL",
    .read_null_neg("u.act.totalEnergy", "DOUBLE"),
    .read_null_neg("u.act.zeroCrossingCount"),
    branch("actigraphy1"),
    branch("actigraphy2"),
    branch("actigraphy3")
  )
}

ingest_heartbeat <- function(sense_version) {
  sprintf(
    "INSERT INTO Heartbeat (participant_id, time, period, device_type, device_role_name, source_file_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'period' AS INTEGER),
        CAST(s.data->>'deviceType' AS TEXT),
        CAST(s.data->>'deviceRoleName' AS TEXT),
        m.file_id
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
    "INSERT INTO Light (participant_id, time, end_time, mean_lux, std_lux, min_lux, max_lux, source_file_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        to_timestamp(CAST(s.sensorEndTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'meanLux' AS REAL),
        CAST(s.data->>'stdLux' AS REAL),
        CAST(s.data->>'minLux' AS REAL),
        CAST(s.data->>'maxLux' AS REAL),
        m.file_id
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
    "INSERT INTO Location (
        participant_id, time, latitude, longitude,
        altitude, accuracy, vertical_accuracy, speed, speed_accuracy,
        heading, heading_accuracy, is_mock,
        elapsed_realtime_nanos, elapsed_realtime_uncertainty_nanos, source_file_id
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
        m.file_id
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
    "INSERT INTO Memory (participant_id, time, free_physical_memory, free_virtual_memory, source_file_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'freePhysicalMemory' AS BIGINT),
        CAST(s.data->>'freeVirtualMemory' AS BIGINT),
        m.file_id
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
    "INSERT INTO Pedometer (participant_id, time, step_count, source_file_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'steps' AS INTEGER),
        m.file_id
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
    "INSERT INTO Screen (participant_id, time, screen_event, source_file_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'screenEvent' AS TEXT),
        m.file_id
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
    "INSERT INTO Timezone (participant_id, time, timezone, source_file_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'timezone' AS TEXT),
        m.file_id
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
    "INSERT INTO Weather (
        participant_id, time, country, area_name,
        weather_main, weather_description, sunrise, sunset, latitude, longitude,
        pressure, wind_speed, wind_degree, humidity, cloudiness,
        rain_last_hour, rain_last_3hours, snow_last_hour, snow_last_3hours,
        temperature, temp_min, temp_max, source_file_id
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
        m.file_id
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
    "INSERT INTO Wifi (participant_id, time, ssid, bssid, ip, source_file_id)
      SELECT
        m.participant_id,
        to_timestamp(CAST(s.sensorStartTime AS BIGINT) / 1000000.0),
        CAST(s.data->>'ssid' AS TEXT),
        CAST(s.data->>'bssid' AS TEXT),
        CAST(s.data->>'ip' AS TEXT),
        m.file_id
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
