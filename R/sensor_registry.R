# Versioned sensor registry of the read_mpath_sense() pipeline.
#
# The registry is keyed by the senseVersion found in the mpathinfo entry;
# the "default" parser set is used for versions that are not (yet)
# registered. Each entry maps a sensor name to the CARP payload type it
# ingests and the ingest function, which takes the senseVersion and returns
# the SQL statement to execute.
#
# Most sensors fit one of two common statement shapes, so their ingest function
# is built by a shared helper in ingest.R from the spec written here, next to
# the sensor's payload type:
#
#   scalar_sensor()       one row per staged entry, with the values read from
#                         the payload object (ingest_scalar()).
#   garmin_array_sensor() one row per element of one array inside the
#                         garminalllogsdata payload (ingest_garmin_array()).
#
# Sensors whose statement does not fit either shape use their own ingest
# function, written out in full in ingest.R.

# A sensor whose payload is a single measurement object. `columns` maps the
# raw.<sensor> column names, in INSERT order, to value expressions over the
# staged row `s`.
scalar_sensor <- function(sensor, type, columns) {
  list(
    type = type,
    fun = ingest_scalar(sensor, type, columns)
  )
}

# A Garmin array sensor. `array` is the garmin_parsed list column it expands;
# read_mpath_sense() also uses it to skip the ingest when the array holds no
# elements in the batch. `time` is the element field holding the
# millisecond epoch, `columns` maps the raw.<sensor> column names, in INSERT
# order, to expressions over the unnested element `e`.
garmin_array_sensor <- function(sensor, array, time, columns) {
  list(
    type = "dk.cachet.carp.garminalllogsdata",
    array = array,
    fun = ingest_garmin_array(sensor, array, time, columns)
  )
}

# Build a registry of ingest functions for a given senseVersion
new_sensor_registry <- function() {
  list(
    "Accelerometer" = list(
      type = "dk.cachet.carp.accelerationfeatures",
      fun = ingest_accelerometer
    ),
    "Activity" = scalar_sensor(
      "Activity",
      "dk.cachet.carp.activity",
      c(
        confidence = "CAST(s.data->>'confidence' AS INTEGER)",
        type = "CAST(s.data->>'type' AS TEXT)"
      )
    ),
    "AppUsage" = list(type = "dk.cachet.carp.appusage", fun = ingest_appusage),
    "Battery" = scalar_sensor(
      "Battery",
      "dk.cachet.carp.batterystate",
      c(
        battery_level = "CAST(s.data->>'batteryLevel' AS INTEGER)",
        battery_status = "CAST(s.data->>'batteryStatus' AS TEXT)"
      )
    ),
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
    "Error" = scalar_sensor(
      "Error",
      "dk.cachet.carp.error",
      c(message = "CAST(s.data->>'message' AS TEXT)")
    ),
    "GarminAccelerometer" = garmin_array_sensor(
      "GarminAccelerometer",
      "accelerometer",
      "timestamp",
      c(
        x = "CAST(e.xValue AS REAL)",
        y = "CAST(e.yValue AS REAL)",
        z = "CAST(e.zValue AS REAL)",
        mac_address = "CAST(e.macAddress AS TEXT)"
      )
    ),
    "GarminActigraphy" = list(
      type = "dk.cachet.carp.garminalllogsdata",
      array = c("actigraphy1", "actigraphy2", "actigraphy3"),
      fun = ingest_garmin_actigraphy
    ),
    "GarminBBI" = garmin_array_sensor(
      "GarminBBI",
      "bbi",
      "timestamp",
      c(
        bbi = "NULLIF(TRY_CAST(e.bbi AS BIGINT), -1)",
        mac_address = "CAST(e.macAddress AS TEXT)"
      )
    ),
    "GarminEnhancedBBI" = garmin_array_sensor(
      "GarminEnhancedBBI",
      "enhancedBbi",
      "timestamp",
      c(
        bbi = "NULLIF(TRY_CAST(e.bbi AS BIGINT), -1)",
        status = "CAST(e.status AS TEXT)",
        gap_duration = "CAST(e.gapDuration AS INTEGER)",
        mac_address = "CAST(e.macAddress AS TEXT)"
      )
    ),
    "GarminGyroscope" = garmin_array_sensor(
      "GarminGyroscope",
      "gyroscope",
      "timestamp",
      c(
        x = "CAST(e.xValue AS REAL)",
        y = "CAST(e.yValue AS REAL)",
        z = "CAST(e.zValue AS REAL)",
        mac_address = "CAST(e.macAddress AS TEXT)"
      )
    ),
    "GarminHeartRate" = garmin_array_sensor(
      "GarminHeartRate",
      "heartRate",
      "timestamp",
      c(
        bpm = "NULLIF(TRY_CAST(e.beatsPerMinute AS BIGINT), -1)",
        status = "CAST(e.status AS TEXT)",
        mac_address = "CAST(e.macAddress AS TEXT)"
      )
    ),
    "GarminMeta" = list(
      type = "dk.cachet.carp.garminalllogsdata",
      fun = ingest_garmin_meta
    ),
    "GarminRespiration" = garmin_array_sensor(
      "GarminRespiration",
      "respiration",
      "timestamp",
      c(
        bpm = "CAST(e.breathsPerMinute AS REAL)",
        status = "CAST(e.status AS TEXT)",
        mac_address = "CAST(e.macAddress AS TEXT)"
      )
    ),
    "GarminSkinTemperature" = garmin_array_sensor(
      "GarminSkinTemperature",
      "skinTemperature",
      "timestamp",
      c(
        temperature = "CAST(e.temperature AS REAL)",
        status = "CAST(e.status AS TEXT)",
        mac_address = "CAST(e.macAddress AS TEXT)"
      )
    ),
    "GarminSPO2" = garmin_array_sensor(
      "GarminSPO2",
      "spo2",
      "timestamp",
      c(
        spo2 = "NULLIF(TRY_CAST(e.spo2Reading AS BIGINT), -1)",
        mac_address = "CAST(e.macAddress AS TEXT)"
      )
    ),
    "GarminSteps" = garmin_array_sensor(
      "GarminSteps",
      "steps",
      "startTimestamp",
      c(
        end_time = "to_timestamp(CAST(e.endTimestamp AS BIGINT) / 1000.0)",
        step_count = "NULLIF(TRY_CAST(e.stepCount AS BIGINT), -1)",
        total_steps = "NULLIF(TRY_CAST(e.totalSteps AS BIGINT), -1)",
        mac_address = "CAST(e.macAddress AS TEXT)"
      )
    ),
    "GarminStress" = garmin_array_sensor(
      "GarminStress",
      "stress",
      "timestamp",
      c(
        stress = "NULLIF(TRY_CAST(e.stressScore AS BIGINT), -1)",
        status = "CAST(e.status AS TEXT)",
        mac_address = "CAST(e.macAddress AS TEXT)"
      )
    ),
    "GarminWristStatus" = garmin_array_sensor(
      "GarminWristStatus",
      "wristStatus",
      "timestamp",
      c(
        status = "CAST(e.status AS TEXT)",
        mac_address = "CAST(e.macAddress AS TEXT)"
      )
    ),
    "GarminZeroCrossing" = garmin_array_sensor(
      "GarminZeroCrossing",
      "zeroCrossing",
      "startTimestamp",
      c(
        end_time = "to_timestamp(CAST(e.endTimestamp AS BIGINT) / 1000.0)",
        total_energy = "NULLIF(TRY_CAST(e.totalEnergy AS BIGINT), -1)",
        n_zero_crossing = "NULLIF(TRY_CAST(e.zeroCrossingCount AS BIGINT), -1)",
        deadband = "CAST(e.deadband AS INTEGER)",
        mac_address = "CAST(e.macAddress AS TEXT)"
      )
    ),
    "Heartbeat" = scalar_sensor(
      "Heartbeat",
      "dk.cachet.carp.heartbeat",
      c(
        period = "CAST(s.data->>'period' AS INTEGER)",
        device_type = "CAST(s.data->>'deviceType' AS TEXT)",
        device_role_name = "CAST(s.data->>'deviceRoleName' AS TEXT)"
      )
    ),
    "Light" = scalar_sensor(
      "Light",
      "dk.cachet.carp.ambientlight",
      c(
        end_time = "to_timestamp(CAST(s.sensorEndTime AS BIGINT) / 1000000.0)",
        mean_lux = "CAST(s.data->>'meanLux' AS REAL)",
        std_lux = "CAST(s.data->>'stdLux' AS REAL)",
        min_lux = "CAST(s.data->>'minLux' AS REAL)",
        max_lux = "CAST(s.data->>'maxLux' AS REAL)"
      )
    ),
    "Location" = list(
      type = "dk.cachet.carp.location",
      fun = ingest_location
    ),
    "Memory" = scalar_sensor(
      "Memory",
      "dk.cachet.carp.freememory",
      c(
        free_physical_memory = "CAST(s.data->>'freePhysicalMemory' AS BIGINT)",
        free_virtual_memory = "CAST(s.data->>'freeVirtualMemory' AS BIGINT)"
      )
    ),
    "Pedometer" = scalar_sensor(
      "Pedometer",
      "dk.cachet.carp.stepcount",
      c(step_count = "CAST(s.data->>'steps' AS INTEGER)")
    ),
    "Screen" = scalar_sensor(
      "Screen",
      "dk.cachet.carp.screenevent",
      c(screen_event = "CAST(s.data->>'screenEvent' AS TEXT)")
    ),
    "Timezone" = scalar_sensor(
      "Timezone",
      "dk.cachet.carp.timezone",
      c(timezone = "CAST(s.data->>'timezone' AS TEXT)")
    ),
    "Weather" = list(
      type = "dk.cachet.carp.weather",
      fun = ingest_weather
    ),
    "Wifi" = scalar_sensor(
      "Wifi",
      "dk.cachet.carp.wifi",
      c(
        ssid = "CAST(s.data->>'ssid' AS TEXT)",
        bssid = "CAST(s.data->>'bssid' AS TEXT)",
        ip = "CAST(s.data->>'ip' AS TEXT)"
      )
    )
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
