-- Derived local-time views for canonical sensor tables.
-- Canonical timestamps are absolute UTC TIMESTAMPTZ instants in main.<sensor>.
-- Each sensor gains two views:
--
--   <sensor>_with_local  all columns, plus one localized column per timestamp
--   <sensor>_local       timestamps replaced by their localized wall-clock value
--
-- Most views read the user-facing main.<sensor> view (not the raw table), so
-- the internal provenance columns (source_file_id, source_row_id,
-- source_measurement_id) are hidden from query results, exactly as in the base
-- sensor views. The exception is the views that need the legacy
-- sense_version <= 6 behaviour (AppUsage, Bluetooth, Location, Weather):
-- those read raw.<sensor> (excluding the provenance columns) joined against
-- ProcessedFiles for the sense version. The raw schema tables remain the
-- physical storage and keep the provenance columns for internal use and for
-- technically inclined users who query them directly.
--
-- Legacy timestamp workaround: m-Path Sense versions <= 6 stored a few
-- timestamps (AppUsage period_start/period_end/last_foreground, Bluetooth
-- start_scan/end_scan, Location time, Weather time/sunrise/sunset) as local
-- wall-clock values rather than UTC instants. Those columns are represented as
-- UTC TIMESTAMPTZ in the canonical table (via AT TIME ZONE 'UTC'
-- at import time) but must NOT be shifted again when producing local values;
-- the CASE below keeps their historical clock value.
-- This workaround is removed once these timestamps are truly UTC.

-- Local-time representation helper.
-- The R function to_local_time() is translated by dbplyr to this signature
-- inside a lazy query, and the view definitions below call it directly.

CREATE OR REPLACE MACRO to_local_time(ts, tz) AS
  CASE WHEN ts IS NULL THEN NULL
       ELSE ts AT TIME ZONE COALESCE(tz, 'UTC') END;

CREATE OR REPLACE VIEW Accelerometer_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local,
    to_local_time(s.end_time, s.timezone) AS end_time_local
    FROM main.Accelerometer s;

CREATE OR REPLACE VIEW Accelerometer_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time,
    to_local_time(s.end_time, s.timezone) AS end_time
)
    FROM main.Accelerometer s;
CREATE OR REPLACE VIEW Activity_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.Activity s;

CREATE OR REPLACE VIEW Activity_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.Activity s;
CREATE OR REPLACE VIEW AppUsage_with_local AS
SELECT s.* EXCLUDE (source_file_id, source_row_id, source_measurement_id),
    to_local_time(s.time, s.timezone) AS time_local,
    to_local_time(s.end_time, s.timezone) AS end_time_local,
    CASE WHEN pf.sense_version <= 6 THEN s.period_start AT TIME ZONE 'UTC'
       ELSE to_local_time(s.period_start, s.timezone) END AS period_start_local,
    CASE WHEN pf.sense_version <= 6 THEN s.period_end AT TIME ZONE 'UTC'
       ELSE to_local_time(s.period_end, s.timezone) END AS period_end_local,
    CASE WHEN pf.sense_version <= 6 THEN s.last_foreground AT TIME ZONE 'UTC'
       ELSE to_local_time(s.last_foreground, s.timezone) END AS last_foreground_local
    FROM raw.AppUsage s LEFT JOIN ProcessedFiles pf ON pf.file_id = s.source_file_id;

CREATE OR REPLACE VIEW AppUsage_local AS
SELECT s.* EXCLUDE (source_file_id, source_row_id, source_measurement_id) REPLACE (
    to_local_time(s.time, s.timezone) AS time,
    to_local_time(s.end_time, s.timezone) AS end_time,
    CASE WHEN pf.sense_version <= 6 THEN s.period_start AT TIME ZONE 'UTC'
       ELSE to_local_time(s.period_start, s.timezone) END AS period_start,
    CASE WHEN pf.sense_version <= 6 THEN s.period_end AT TIME ZONE 'UTC'
       ELSE to_local_time(s.period_end, s.timezone) END AS period_end,
    CASE WHEN pf.sense_version <= 6 THEN s.last_foreground AT TIME ZONE 'UTC'
       ELSE to_local_time(s.last_foreground, s.timezone) END AS last_foreground
)
    FROM raw.AppUsage s LEFT JOIN ProcessedFiles pf ON pf.file_id = s.source_file_id;
CREATE OR REPLACE VIEW Battery_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.Battery s;

CREATE OR REPLACE VIEW Battery_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.Battery s;
CREATE OR REPLACE VIEW Bluetooth_with_local AS
SELECT s.* EXCLUDE (source_file_id, source_row_id, source_measurement_id),
    to_local_time(s.time, s.timezone) AS time_local,
    CASE WHEN pf.sense_version <= 6 THEN s.start_scan AT TIME ZONE 'UTC'
       ELSE to_local_time(s.start_scan, s.timezone) END AS start_scan_local,
    CASE WHEN pf.sense_version <= 6 THEN s.end_scan AT TIME ZONE 'UTC'
       ELSE to_local_time(s.end_scan, s.timezone) END AS end_scan_local
    FROM raw.Bluetooth s LEFT JOIN ProcessedFiles pf ON pf.file_id = s.source_file_id;

CREATE OR REPLACE VIEW Bluetooth_local AS
SELECT s.* EXCLUDE (source_file_id, source_row_id, source_measurement_id) REPLACE (
    to_local_time(s.time, s.timezone) AS time,
    CASE WHEN pf.sense_version <= 6 THEN s.start_scan AT TIME ZONE 'UTC'
       ELSE to_local_time(s.start_scan, s.timezone) END AS start_scan,
    CASE WHEN pf.sense_version <= 6 THEN s.end_scan AT TIME ZONE 'UTC'
       ELSE to_local_time(s.end_scan, s.timezone) END AS end_scan
)
    FROM raw.Bluetooth s LEFT JOIN ProcessedFiles pf ON pf.file_id = s.source_file_id;
CREATE OR REPLACE VIEW BluetoothBeacon_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.BluetoothBeacon s;

CREATE OR REPLACE VIEW BluetoothBeacon_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.BluetoothBeacon s;
CREATE OR REPLACE VIEW Connectivity_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.Connectivity s;

CREATE OR REPLACE VIEW Connectivity_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.Connectivity s;
CREATE OR REPLACE VIEW Device_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.Device s;

CREATE OR REPLACE VIEW Device_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.Device s;
CREATE OR REPLACE VIEW Error_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.Error s;

CREATE OR REPLACE VIEW Error_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.Error s;
CREATE OR REPLACE VIEW GarminAccelerometer_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.GarminAccelerometer s;

CREATE OR REPLACE VIEW GarminAccelerometer_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.GarminAccelerometer s;
CREATE OR REPLACE VIEW GarminActigraphy_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local,
    to_local_time(s.end_time, s.timezone) AS end_time_local
    FROM main.GarminActigraphy s;

CREATE OR REPLACE VIEW GarminActigraphy_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time,
    to_local_time(s.end_time, s.timezone) AS end_time
)
    FROM main.GarminActigraphy s;
CREATE OR REPLACE VIEW GarminBBI_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.GarminBBI s;

CREATE OR REPLACE VIEW GarminBBI_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.GarminBBI s;
CREATE OR REPLACE VIEW GarminEnhancedBBI_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.GarminEnhancedBBI s;

CREATE OR REPLACE VIEW GarminEnhancedBBI_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.GarminEnhancedBBI s;
CREATE OR REPLACE VIEW GarminGyroscope_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.GarminGyroscope s;

CREATE OR REPLACE VIEW GarminGyroscope_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.GarminGyroscope s;
CREATE OR REPLACE VIEW GarminHeartRate_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.GarminHeartRate s;

CREATE OR REPLACE VIEW GarminHeartRate_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.GarminHeartRate s;
CREATE OR REPLACE VIEW GarminMeta_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local,
    to_local_time(s.time_from, s.timezone) AS time_from_local,
    to_local_time(s.time_to, s.timezone) AS time_to_local
    FROM main.GarminMeta s;

CREATE OR REPLACE VIEW GarminMeta_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time,
    to_local_time(s.time_from, s.timezone) AS time_from,
    to_local_time(s.time_to, s.timezone) AS time_to
)
    FROM main.GarminMeta s;
CREATE OR REPLACE VIEW GarminRespiration_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.GarminRespiration s;

CREATE OR REPLACE VIEW GarminRespiration_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.GarminRespiration s;
CREATE OR REPLACE VIEW GarminSPO2_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.GarminSPO2 s;

CREATE OR REPLACE VIEW GarminSPO2_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.GarminSPO2 s;
CREATE OR REPLACE VIEW GarminSkinTemperature_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.GarminSkinTemperature s;

CREATE OR REPLACE VIEW GarminSkinTemperature_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.GarminSkinTemperature s;
CREATE OR REPLACE VIEW GarminSteps_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local,
    to_local_time(s.end_time, s.timezone) AS end_time_local
    FROM main.GarminSteps s;

CREATE OR REPLACE VIEW GarminSteps_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time,
    to_local_time(s.end_time, s.timezone) AS end_time
)
    FROM main.GarminSteps s;
CREATE OR REPLACE VIEW GarminStress_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.GarminStress s;

CREATE OR REPLACE VIEW GarminStress_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.GarminStress s;
CREATE OR REPLACE VIEW GarminWristStatus_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.GarminWristStatus s;

CREATE OR REPLACE VIEW GarminWristStatus_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.GarminWristStatus s;
CREATE OR REPLACE VIEW GarminZeroCrossing_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local,
    to_local_time(s.end_time, s.timezone) AS end_time_local
    FROM main.GarminZeroCrossing s;

CREATE OR REPLACE VIEW GarminZeroCrossing_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time,
    to_local_time(s.end_time, s.timezone) AS end_time
)
    FROM main.GarminZeroCrossing s;
CREATE OR REPLACE VIEW Heartbeat_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.Heartbeat s;

CREATE OR REPLACE VIEW Heartbeat_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.Heartbeat s;
CREATE OR REPLACE VIEW Light_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local,
    to_local_time(s.end_time, s.timezone) AS end_time_local
    FROM main.Light s;

CREATE OR REPLACE VIEW Light_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time,
    to_local_time(s.end_time, s.timezone) AS end_time
)
    FROM main.Light s;
CREATE OR REPLACE VIEW Location_with_local AS
SELECT s.* EXCLUDE (source_file_id, source_row_id, source_measurement_id),
    CASE WHEN pf.sense_version <= 6 THEN s.time AT TIME ZONE 'UTC'
       ELSE to_local_time(s.time, s.timezone) END AS time_local
    FROM raw.Location s LEFT JOIN ProcessedFiles pf ON pf.file_id = s.source_file_id;

CREATE OR REPLACE VIEW Location_local AS
SELECT s.* EXCLUDE (source_file_id, source_row_id, source_measurement_id) REPLACE (
    CASE WHEN pf.sense_version <= 6 THEN s.time AT TIME ZONE 'UTC'
       ELSE to_local_time(s.time, s.timezone) END AS time
)
    FROM raw.Location s LEFT JOIN ProcessedFiles pf ON pf.file_id = s.source_file_id;
CREATE OR REPLACE VIEW Memory_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.Memory s;

CREATE OR REPLACE VIEW Memory_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.Memory s;
CREATE OR REPLACE VIEW Pedometer_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.Pedometer s;

CREATE OR REPLACE VIEW Pedometer_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.Pedometer s;
CREATE OR REPLACE VIEW Screen_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.Screen s;

CREATE OR REPLACE VIEW Screen_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.Screen s;
CREATE OR REPLACE VIEW Timezone_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.Timezone s;

CREATE OR REPLACE VIEW Timezone_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.Timezone s;
CREATE OR REPLACE VIEW Weather_with_local AS
SELECT s.* EXCLUDE (source_file_id, source_row_id, source_measurement_id),
    CASE WHEN pf.sense_version <= 6 THEN s.time AT TIME ZONE 'UTC'
       ELSE to_local_time(s.time, s.timezone) END AS time_local,
    CASE WHEN pf.sense_version <= 6 THEN s.sunrise AT TIME ZONE 'UTC'
       ELSE to_local_time(s.sunrise, s.timezone) END AS sunrise_local,
    CASE WHEN pf.sense_version <= 6 THEN s.sunset AT TIME ZONE 'UTC'
       ELSE to_local_time(s.sunset, s.timezone) END AS sunset_local
    FROM raw.Weather s LEFT JOIN ProcessedFiles pf ON pf.file_id = s.source_file_id;

CREATE OR REPLACE VIEW Weather_local AS
SELECT s.* EXCLUDE (source_file_id, source_row_id, source_measurement_id) REPLACE (
    CASE WHEN pf.sense_version <= 6 THEN s.time AT TIME ZONE 'UTC'
       ELSE to_local_time(s.time, s.timezone) END AS time,
    CASE WHEN pf.sense_version <= 6 THEN s.sunrise AT TIME ZONE 'UTC'
       ELSE to_local_time(s.sunrise, s.timezone) END AS sunrise,
    CASE WHEN pf.sense_version <= 6 THEN s.sunset AT TIME ZONE 'UTC'
       ELSE to_local_time(s.sunset, s.timezone) END AS sunset
)
    FROM raw.Weather s LEFT JOIN ProcessedFiles pf ON pf.file_id = s.source_file_id;
CREATE OR REPLACE VIEW Wifi_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM main.Wifi s;

CREATE OR REPLACE VIEW Wifi_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM main.Wifi s;

