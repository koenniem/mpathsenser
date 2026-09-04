-- Derived local-time views for canonical sensor tables.
-- Canonical timestamps are absolute UTC TIMESTAMPTZ instants in main.<sensor>.
-- Each sensor gains two views:
--
--   <sensor>_with_local  all columns, plus one localized column per timestamp
--   <sensor>_local       timestamps replaced by their localized wall-clock value
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
    FROM Accelerometer s;

CREATE OR REPLACE VIEW Accelerometer_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time,
    to_local_time(s.end_time, s.timezone) AS end_time
)
    FROM Accelerometer s;
CREATE OR REPLACE VIEW Activity_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM Activity s;

CREATE OR REPLACE VIEW Activity_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM Activity s;
CREATE OR REPLACE VIEW AppUsage_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local,
    to_local_time(s.end_time, s.timezone) AS end_time_local,
    CASE WHEN pf.sense_version <= 6 THEN s.period_start AT TIME ZONE 'UTC'
       ELSE to_local_time(s.period_start, s.timezone) END AS period_start_local,
    CASE WHEN pf.sense_version <= 6 THEN s.period_end AT TIME ZONE 'UTC'
       ELSE to_local_time(s.period_end, s.timezone) END AS period_end_local,
    CASE WHEN pf.sense_version <= 6 THEN s.last_foreground AT TIME ZONE 'UTC'
       ELSE to_local_time(s.last_foreground, s.timezone) END AS last_foreground_local
    FROM AppUsage s LEFT JOIN ProcessedFiles pf ON pf.file_id = s.source_file_id;

CREATE OR REPLACE VIEW AppUsage_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time,
    to_local_time(s.end_time, s.timezone) AS end_time,
    CASE WHEN pf.sense_version <= 6 THEN s.period_start AT TIME ZONE 'UTC'
       ELSE to_local_time(s.period_start, s.timezone) END AS period_start,
    CASE WHEN pf.sense_version <= 6 THEN s.period_end AT TIME ZONE 'UTC'
       ELSE to_local_time(s.period_end, s.timezone) END AS period_end,
    CASE WHEN pf.sense_version <= 6 THEN s.last_foreground AT TIME ZONE 'UTC'
       ELSE to_local_time(s.last_foreground, s.timezone) END AS last_foreground
)
    FROM AppUsage s LEFT JOIN ProcessedFiles pf ON pf.file_id = s.source_file_id;
CREATE OR REPLACE VIEW Battery_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM Battery s;

CREATE OR REPLACE VIEW Battery_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM Battery s;
CREATE OR REPLACE VIEW Bluetooth_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local,
    CASE WHEN pf.sense_version <= 6 THEN s.start_scan AT TIME ZONE 'UTC'
       ELSE to_local_time(s.start_scan, s.timezone) END AS start_scan_local,
    CASE WHEN pf.sense_version <= 6 THEN s.end_scan AT TIME ZONE 'UTC'
       ELSE to_local_time(s.end_scan, s.timezone) END AS end_scan_local
    FROM Bluetooth s LEFT JOIN ProcessedFiles pf ON pf.file_id = s.source_file_id;

CREATE OR REPLACE VIEW Bluetooth_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time,
    CASE WHEN pf.sense_version <= 6 THEN s.start_scan AT TIME ZONE 'UTC'
       ELSE to_local_time(s.start_scan, s.timezone) END AS start_scan,
    CASE WHEN pf.sense_version <= 6 THEN s.end_scan AT TIME ZONE 'UTC'
       ELSE to_local_time(s.end_scan, s.timezone) END AS end_scan
)
    FROM Bluetooth s LEFT JOIN ProcessedFiles pf ON pf.file_id = s.source_file_id;
CREATE OR REPLACE VIEW BluetoothBeacon_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM BluetoothBeacon s;

CREATE OR REPLACE VIEW BluetoothBeacon_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM BluetoothBeacon s;
CREATE OR REPLACE VIEW Connectivity_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM Connectivity s;

CREATE OR REPLACE VIEW Connectivity_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM Connectivity s;
CREATE OR REPLACE VIEW Device_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM Device s;

CREATE OR REPLACE VIEW Device_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM Device s;
CREATE OR REPLACE VIEW Error_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM Error s;

CREATE OR REPLACE VIEW Error_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM Error s;
CREATE OR REPLACE VIEW GarminAccelerometer_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM GarminAccelerometer s;

CREATE OR REPLACE VIEW GarminAccelerometer_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM GarminAccelerometer s;
CREATE OR REPLACE VIEW GarminActigraphy_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local,
    to_local_time(s.end_time, s.timezone) AS end_time_local
    FROM GarminActigraphy s;

CREATE OR REPLACE VIEW GarminActigraphy_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time,
    to_local_time(s.end_time, s.timezone) AS end_time
)
    FROM GarminActigraphy s;
CREATE OR REPLACE VIEW GarminBBI_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM GarminBBI s;

CREATE OR REPLACE VIEW GarminBBI_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM GarminBBI s;
CREATE OR REPLACE VIEW GarminEnhancedBBI_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM GarminEnhancedBBI s;

CREATE OR REPLACE VIEW GarminEnhancedBBI_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM GarminEnhancedBBI s;
CREATE OR REPLACE VIEW GarminGyroscope_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM GarminGyroscope s;

CREATE OR REPLACE VIEW GarminGyroscope_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM GarminGyroscope s;
CREATE OR REPLACE VIEW GarminHeartRate_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM GarminHeartRate s;

CREATE OR REPLACE VIEW GarminHeartRate_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM GarminHeartRate s;
CREATE OR REPLACE VIEW GarminMeta_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local,
    to_local_time(s.time_from, s.timezone) AS time_from_local,
    to_local_time(s.time_to, s.timezone) AS time_to_local
    FROM GarminMeta s;

CREATE OR REPLACE VIEW GarminMeta_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time,
    to_local_time(s.time_from, s.timezone) AS time_from,
    to_local_time(s.time_to, s.timezone) AS time_to
)
    FROM GarminMeta s;
CREATE OR REPLACE VIEW GarminRespiration_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM GarminRespiration s;

CREATE OR REPLACE VIEW GarminRespiration_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM GarminRespiration s;
CREATE OR REPLACE VIEW GarminSPO2_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM GarminSPO2 s;

CREATE OR REPLACE VIEW GarminSPO2_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM GarminSPO2 s;
CREATE OR REPLACE VIEW GarminSkinTemperature_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM GarminSkinTemperature s;

CREATE OR REPLACE VIEW GarminSkinTemperature_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM GarminSkinTemperature s;
CREATE OR REPLACE VIEW GarminSteps_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local,
    to_local_time(s.end_time, s.timezone) AS end_time_local
    FROM GarminSteps s;

CREATE OR REPLACE VIEW GarminSteps_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time,
    to_local_time(s.end_time, s.timezone) AS end_time
)
    FROM GarminSteps s;
CREATE OR REPLACE VIEW GarminStress_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM GarminStress s;

CREATE OR REPLACE VIEW GarminStress_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM GarminStress s;
CREATE OR REPLACE VIEW GarminWristStatus_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM GarminWristStatus s;

CREATE OR REPLACE VIEW GarminWristStatus_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM GarminWristStatus s;
CREATE OR REPLACE VIEW GarminZeroCrossing_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local,
    to_local_time(s.end_time, s.timezone) AS end_time_local
    FROM GarminZeroCrossing s;

CREATE OR REPLACE VIEW GarminZeroCrossing_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time,
    to_local_time(s.end_time, s.timezone) AS end_time
)
    FROM GarminZeroCrossing s;
CREATE OR REPLACE VIEW Heartbeat_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM Heartbeat s;

CREATE OR REPLACE VIEW Heartbeat_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM Heartbeat s;
CREATE OR REPLACE VIEW Light_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local,
    to_local_time(s.end_time, s.timezone) AS end_time_local
    FROM Light s;

CREATE OR REPLACE VIEW Light_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time,
    to_local_time(s.end_time, s.timezone) AS end_time
)
    FROM Light s;
CREATE OR REPLACE VIEW Location_with_local AS
SELECT s.*,
    CASE WHEN pf.sense_version <= 6 THEN s.time AT TIME ZONE 'UTC'
       ELSE to_local_time(s.time, s.timezone) END AS time_local
    FROM Location s LEFT JOIN ProcessedFiles pf ON pf.file_id = s.source_file_id;

CREATE OR REPLACE VIEW Location_local AS
SELECT s.* REPLACE (
    CASE WHEN pf.sense_version <= 6 THEN s.time AT TIME ZONE 'UTC'
       ELSE to_local_time(s.time, s.timezone) END AS time
)
    FROM Location s LEFT JOIN ProcessedFiles pf ON pf.file_id = s.source_file_id;
CREATE OR REPLACE VIEW Memory_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM Memory s;

CREATE OR REPLACE VIEW Memory_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM Memory s;
CREATE OR REPLACE VIEW Pedometer_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM Pedometer s;

CREATE OR REPLACE VIEW Pedometer_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM Pedometer s;
CREATE OR REPLACE VIEW Screen_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM Screen s;

CREATE OR REPLACE VIEW Screen_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM Screen s;
CREATE OR REPLACE VIEW Timezone_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM Timezone s;

CREATE OR REPLACE VIEW Timezone_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM Timezone s;
CREATE OR REPLACE VIEW Weather_with_local AS
SELECT s.*,
    CASE WHEN pf.sense_version <= 6 THEN s.time AT TIME ZONE 'UTC'
       ELSE to_local_time(s.time, s.timezone) END AS time_local,
    CASE WHEN pf.sense_version <= 6 THEN s.sunrise AT TIME ZONE 'UTC'
       ELSE to_local_time(s.sunrise, s.timezone) END AS sunrise_local,
    CASE WHEN pf.sense_version <= 6 THEN s.sunset AT TIME ZONE 'UTC'
       ELSE to_local_time(s.sunset, s.timezone) END AS sunset_local
    FROM Weather s LEFT JOIN ProcessedFiles pf ON pf.file_id = s.source_file_id;

CREATE OR REPLACE VIEW Weather_local AS
SELECT s.* REPLACE (
    CASE WHEN pf.sense_version <= 6 THEN s.time AT TIME ZONE 'UTC'
       ELSE to_local_time(s.time, s.timezone) END AS time,
    CASE WHEN pf.sense_version <= 6 THEN s.sunrise AT TIME ZONE 'UTC'
       ELSE to_local_time(s.sunrise, s.timezone) END AS sunrise,
    CASE WHEN pf.sense_version <= 6 THEN s.sunset AT TIME ZONE 'UTC'
       ELSE to_local_time(s.sunset, s.timezone) END AS sunset
)
    FROM Weather s LEFT JOIN ProcessedFiles pf ON pf.file_id = s.source_file_id;
CREATE OR REPLACE VIEW Wifi_with_local AS
SELECT s.*,
    to_local_time(s.time, s.timezone) AS time_local
    FROM Wifi s;

CREATE OR REPLACE VIEW Wifi_local AS
SELECT s.* REPLACE (
    to_local_time(s.time, s.timezone) AS time
)
    FROM Wifi s;

