# Versioned sensor registry of the read_mpath_sense() pipeline.
#
# The registry is keyed by the senseVersion found in the mpathinfo entry;
# the "default" parser set is used for versions that are not (yet)
# registered. Each entry maps a sensor name to the CARP payload type it
# ingests and the ingest function, which takes the senseVersion and returns
# the SQL statement to execute.

# Build a registry of ingest functions for a given senseVersion
new_sensor_registry <- function() {
  list(
    "Accelerometer" = list(
      type = "dk.cachet.carp.accelerationfeatures",
      fun = ingest_accelerometer
    ),
    "Activity" = list(type = "dk.cachet.carp.activity", fun = ingest_activity),
    "AppUsage" = list(type = "dk.cachet.carp.appusage", fun = ingest_appusage),
    "Battery" = list(type = "dk.cachet.carp.batterystate", fun = ingest_battery),
    "Bluetooth" = list(
      type = "dk.cachet.carp.bluetooth",
      fun = ingest_bluetooth
    ),
    "BluetoothBeacon" = list(
      type = "dk.cachet.carp.beacondata",
      fun = ingest_bluetooth_beacon
    ),
    "Connectivity" = list(
      type = "dk.cachet.carp.connectivity",
      fun = ingest_connectivity
    ),
    "Device" = list(type = "dk.cachet.carp.deviceinformation", fun = ingest_device),
    "Error" = list(type = "dk.cachet.carp.error", fun = ingest_error),
    "GarminAccelerometer" = list(
      type = "dk.cachet.carp.garminalllogsdata",
      fun = ingest_garmin_accelerometer
    ),
    "GarminActigraphy" = list(
      type = "dk.cachet.carp.garminalllogsdata",
      fun = ingest_garmin_actigraphy
    ),
    "GarminBBI" = list(
      type = "dk.cachet.carp.garminalllogsdata",
      fun = ingest_garmin_bbi
    ),
    "GarminEnhancedBBI" = list(
      type = "dk.cachet.carp.garminalllogsdata",
      fun = ingest_garmin_enhanced_bbi
    ),
    "GarminGyroscope" = list(
      type = "dk.cachet.carp.garminalllogsdata",
      fun = ingest_garmin_gyroscope
    ),
    "GarminHeartRate" = list(
      type = "dk.cachet.carp.garminalllogsdata",
      fun = ingest_garmin_heartrate
    ),
    "GarminMeta" = list(
      type = "dk.cachet.carp.garminalllogsdata",
      fun = ingest_garmin_meta
    ),
    "GarminRespiration" = list(
      type = "dk.cachet.carp.garminalllogsdata",
      fun = ingest_garmin_respiration
    ),
    "GarminSkinTemperature" = list(
      type = "dk.cachet.carp.garminalllogsdata",
      fun = ingest_garmin_skintemperature
    ),
    "GarminSPO2" = list(
      type = "dk.cachet.carp.garminalllogsdata",
      fun = ingest_garmin_spo2
    ),
    "GarminSteps" = list(
      type = "dk.cachet.carp.garminalllogsdata",
      fun = ingest_garmin_steps
    ),
    "GarminStress" = list(
      type = "dk.cachet.carp.garminalllogsdata",
      fun = ingest_garmin_stress
    ),
    "GarminWristStatus" = list(
      type = "dk.cachet.carp.garminalllogsdata",
      fun = ingest_garmin_wriststatus
    ),
    "GarminZeroCrossing" = list(
      type = "dk.cachet.carp.garminalllogsdata",
      fun = ingest_garmin_zerocrossing
    ),
    "Heartbeat" = list(type = "dk.cachet.carp.heartbeat", fun = ingest_heartbeat),
    "Light" = list(type = "dk.cachet.carp.ambientlight", fun = ingest_light),
    "Location" = list(type = "dk.cachet.carp.location", fun = ingest_location),
    "Memory" = list(type = "dk.cachet.carp.freememory", fun = ingest_memory),
    "Pedometer" = list(type = "dk.cachet.carp.stepcount", fun = ingest_pedometer),
    "Screen" = list(type = "dk.cachet.carp.screenevent", fun = ingest_screen),
    "Timezone" = list(type = "dk.cachet.carp.timezone", fun = ingest_timezone),
    "Weather" = list(type = "dk.cachet.carp.weather", fun = ingest_weather),
    "Wifi" = list(type = "dk.cachet.carp.wifi", fun = ingest_wifi)
  )
}

sensor_registry <- list(
  "5" = new_sensor_registry(),
  "6" = new_sensor_registry(),
  "default" = new_sensor_registry()
)

# Payload types that are known but deliberately not ingested. These are
# skipped silently (no warning), as they carry no data of interest.
ignored_sensor_types <- c(
  "dk.cachet.carp.triggeredtask" # executions of triggered tasks; no measurements
)

# Typed JSON schemas for the array-based sensors. The arrays are transformed
# directly to lists of STRUCTs with these schemas, which uses far less memory
# than keeping the elements as JSON values (relevant for Garmin logs, where a
# single entry can hold tens of thousands of values). Missing fields become
# NULL; extra fields are ignored.
array_schemas <- list(
  GarminAccelerometer = '[{"timestamp": "BIGINT", "xValue": "DOUBLE", "yValue": "DOUBLE", "zValue": "DOUBLE", "macAddress": "VARCHAR"}]',
  GarminActigraphy = '[{"startTimestamp": "BIGINT", "endTimestamp": "BIGINT", "instance": "VARCHAR", "totalEnergy": "DOUBLE", "zeroCrossingCount": "BIGINT", "timeAboveThreshold": "DOUBLE", "macAddress": "VARCHAR"}]',
  GarminBBI = '[{"timestamp": "BIGINT", "bbi": "BIGINT", "macAddress": "VARCHAR"}]',
  GarminEnhancedBBI = '[{"timestamp": "BIGINT", "bbi": "BIGINT", "status": "VARCHAR", "gapDuration": "BIGINT", "macAddress": "VARCHAR"}]',
  GarminGyroscope = '[{"timestamp": "BIGINT", "xValue": "DOUBLE", "yValue": "DOUBLE", "zValue": "DOUBLE", "macAddress": "VARCHAR"}]',
  GarminHeartRate = '[{"timestamp": "BIGINT", "beatsPerMinute": "BIGINT", "status": "VARCHAR", "macAddress": "VARCHAR"}]',
  GarminRespiration = '[{"timestamp": "BIGINT", "breathsPerMinute": "DOUBLE", "status": "VARCHAR", "macAddress": "VARCHAR"}]',
  GarminSkinTemperature = '[{"timestamp": "BIGINT", "temperature": "DOUBLE", "status": "VARCHAR", "macAddress": "VARCHAR"}]',
  GarminSPO2 = '[{"timestamp": "BIGINT", "spo2Reading": "BIGINT", "macAddress": "VARCHAR"}]',
  GarminSteps = '[{"startTimestamp": "BIGINT", "endTimestamp": "BIGINT", "stepCount": "BIGINT", "totalSteps": "BIGINT", "macAddress": "VARCHAR"}]',
  GarminStress = '[{"timestamp": "BIGINT", "stressScore": "BIGINT", "status": "VARCHAR", "macAddress": "VARCHAR"}]',
  GarminWristStatus = '[{"timestamp": "BIGINT", "status": "VARCHAR", "macAddress": "VARCHAR"}]',
  GarminZeroCrossing = '[{"startTimestamp": "BIGINT", "endTimestamp": "BIGINT", "totalEnergy": "DOUBLE", "zeroCrossingCount": "BIGINT", "deadband": "BIGINT", "macAddress": "VARCHAR"}]',
  Bluetooth = '[{"advertisementName": "VARCHAR", "bluetoothDeviceId": "VARCHAR", "bluetoothDeviceName": "VARCHAR", "connectable": "BOOLEAN", "rssi": "BIGINT", "txPowerLevel": "BIGINT"}]',
  BluetoothBeacon = '[{"uuid": "VARCHAR", "rssi": "BIGINT", "major": "BIGINT", "minor": "BIGINT", "accuracy": "DOUBLE", "proximity": "VARCHAR"}]'
)
