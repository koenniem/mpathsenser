# convenience function for create a data frame with the specified column name(s), and assigning
# the right class to it
test_alias_column_names <- function(names, sensor) {
  data <- rep(list(NA), length(names))
  names(data) <- names
  data <- tibble(!!!data, .name_repair = "minimal")

  class(data) <- c(tolower(sensor), class(data))
  res <- alias_column_names(data)
  colnames(res)
}

generic_test <- function(sensor, time = "start_time") {
  # An invalid name is returned unchanged
  expect_equal(
    test_alias_column_names("foo", sensor),
    "foo"
  )

  # Check multiple inputs
  expect_equal(
    test_alias_column_names(c("id", time), sensor),
    c("measurement_id", "time")
  )

  # A correct column name is returned unchanged
  expect_equal(
    test_alias_column_names("measurement_id", sensor),
    "measurement_id"
  )

  # Ensure participant_id is kept
  expect_equal(
    test_alias_column_names("participant_id", sensor),
    "participant_id"
  )

  # Some common changed names
  expect_equal(
    test_alias_column_names("id", sensor),
    "measurement_id"
  )
  expect_equal(
    test_alias_column_names(time, sensor),
    "time"
  )
}

# Test incorrect sensor =============
test_that("Test incorrect sensor", {
  expect_error(test_alias_column_names("foo", "bar"))
})

# Accelerometer ==========
test_that("alias_column_names.accelerometer", {
  generic_test("Accelerometer")

  # Other names
  expect_equal(
    test_alias_column_names("xm", "Accelerometer"),
    "x_mean"
  )

  # Name may not be duplicate after being changed
  expect_error(
    test_alias_column_names(c("xm", "xMean"), "Accelerometer")
  )
})

# Activity =========
test_that("alias_column_names.activity", {
  generic_test("Activity")
})

# AirQuality ==========
test_that("alias_column_names.airquality", {
  generic_test("AirQuality")

  # Other names
  expect_equal(
    test_alias_column_names("airQualityIndex", "AirQuality"),
    "air_quality_index"
  )
  expect_equal(
    test_alias_column_names("airQualityLevel", "AirQuality"),
    "air_quality_level"
  )
})

# AppUsage ==========
test_that("alias_column_names.appusage", {
  generic_test("AppUsage")

  # Other names
  expect_equal(
    test_alias_column_names("appName", "AppUsage"),
    "app"
  )
})

# Battery =========
test_that("alias_column_names.battery", {
  generic_test("Battery")

  # Other tests
  expect_equal(
    test_alias_column_names("batteryLevel", "Battery"),
    "battery_level"
  )
})

# Bluetooth =========
test_that("alias_column_names.bluetooth", {
  generic_test("Bluetooth")

  # Other tests
  expect_equal(
    test_alias_column_names("scanResult", "Bluetooth"),
    "scan_result"
  )
  expect_equal(
    test_alias_column_names("bluetoothDeviceName", "Bluetooth"),
    "bluetooth_device_name"
  )
})

# Connectivity =======
test_that("alias_column_names.connectivity", {
  generic_test("Connectivity")

  # Other tests
  expect_equal(
    test_alias_column_names("connectivityStatus", "Connectivity"),
    "connectivity_status"
  )
})

# Device =========
test_that("alias_column_names.device", {
  generic_test("Device")

  # Other tests
  expect_equal(
    test_alias_column_names("operatingSystem", "Device"),
    "operating_system"
  )
})

# Error ===========
test_that("alias_column_names.error", {
  generic_test("Error")
})

# Geofence ==========
test_that("alias_column_names.geofence", {
  generic_test("Geofence")
})

# Gyroscope ==========
test_that("alias_column_names.gyroscope", {
  generic_test("Gyroscope")
})

# Heartbeat ==========
test_that("alias_column_names.heartbeat", {
  generic_test("Heartbeat")

  # Other tests
  expect_equal(
    test_alias_column_names("deviceType", "Heartbeat"),
    "device_type"
  )
})

# Keyboard ==========
test_that("alias_column_names.keyboard", {
  generic_test("Keyboard")
})

# Light ==========
test_that("alias_column_names.light", {
  generic_test("Light")

  # Other tests
  expect_equal(
    test_alias_column_names("meanLux", "Light"),
    "mean_lux"
  )
})

# Location ==========
test_that("alias_column_names.location", {
  generic_test("Location")

  # Other tests
  expect_equal(
    test_alias_column_names("verticalAccuracy", "Location"),
    "vertical_accuracy"
  )
})

# Memory ==========
test_that("alias_column_names.memory", {
  generic_test("Memory")

  # Other tests
  expect_equal(
    test_alias_column_names("freePhysicalMemory", "Memory"),
    "free_physical_memory"
  )
})

# Noise =========
test_that("alias_column_names.noise", {
  generic_test("Noise")

  # Other tests
  expect_equal(
    test_alias_column_names("meanDecibel", "Noise"),
    "mean_decibel"
  )
})

# Pedometer =========
test_that("alias_column_names.pedometer", {
  generic_test("Pedometer")

  # Other tests
  expect_equal(
    test_alias_column_names("stepCount", "Pedometer"),
    "step_count"
  )
})

# Screen ========
test_that("alias_column_names.screen", {
  generic_test("Screen")

  # Other tests
  expect_equal(
    test_alias_column_names("screenEvent", "Screen"),
    "screen_event"
  )
})

# Timezone ========
test_that("alias_column_names.timezone", {
  generic_test("Timezone")
})

# Weather =========
test_that("alias_column_names.weather", {
  generic_test("Weather")

  # Other tests
  expect_equal(
    test_alias_column_names("weatherMain", "Weather"),
    "weather_main"
  )
})

# Wifi ============
test_that("alias_column_names.wifi", {
  generic_test("Wifi")
})

# GarminAccelerometer ==========
test_that("alias_column_names.garminaccelerometer", {
  generic_test("GarminAccelerometer", "timestamp")

  # Other names
  expect_equal(
    test_alias_column_names("xValue", "GarminAccelerometer"),
    "x"
  )
  expect_equal(
    test_alias_column_names("yValue", "GarminAccelerometer"),
    "y"
  )
  expect_equal(
    test_alias_column_names("zValue", "GarminAccelerometer"),
    "z"
  )
  expect_equal(
    test_alias_column_names("macAddress", "GarminAccelerometer"),
    "mac_address"
  )
})

# GarminActigraphy ==========
test_that("alias_column_names.garminactigraphy", {
  generic_test("GarminActigraphy", "startTimestamp")

  # Other names
  expect_equal(
    test_alias_column_names("startTimestamp", "GarminActigraphy"),
    "time"
  )
  expect_equal(
    test_alias_column_names("endTimestamp", "GarminActigraphy"),
    "end_time"
  )
  expect_equal(
    test_alias_column_names("totalEnergy", "GarminActigraphy"),
    "total_energy"
  )
  expect_equal(
    test_alias_column_names("zeroCrossingCount", "GarminActigraphy"),
    "n_zero_crossing"
  )
  expect_equal(
    test_alias_column_names("timeAboveThreshold", "GarminActigraphy"),
    "time_above_threshold"
  )
  expect_equal(
    test_alias_column_names("macAddress", "GarminActigraphy"),
    "mac_address"
  )
})

# GarminBBI ==========
test_that("alias_column_names.garminbbi", {
  generic_test("GarminBBI", "timestamp")

  # Other names
  expect_equal(
    test_alias_column_names("timestamp", "GarminBBI"),
    "time"
  )
  expect_equal(
    test_alias_column_names("macAddress", "GarminBBI"),
    "mac_address"
  )
})

# GarminEnhancedBBI ==========
test_that("alias_column_names.garminenhancedbbi", {
  generic_test("GarminEnhancedBBI", "timestamp")

  # Other names
  expect_equal(
    test_alias_column_names("timestamp", "GarminEnhancedBBI"),
    "time"
  )
  expect_equal(
    test_alias_column_names("gapDuration", "GarminEnhancedBBI"),
    "gap_duration"
  )
  expect_equal(
    test_alias_column_names("macAddress", "GarminEnhancedBBI"),
    "mac_address"
  )
})

# GarminGyroscope ==========
test_that("alias_column_names.garmingyroscope", {
  generic_test("GarminGyroscope", "timestamp")

  # Other names
  expect_equal(
    test_alias_column_names("xValue", "GarminGyroscope"),
    "x"
  )
  expect_equal(
    test_alias_column_names("yValue", "GarminGyroscope"),
    "y"
  )
  expect_equal(
    test_alias_column_names("zValue", "GarminGyroscope"),
    "z"
  )
  expect_equal(
    test_alias_column_names("macAddress", "GarminGyroscope"),
    "mac_address"
  )
})

# GarminHeartRate ==========
test_that("alias_column_names.garminheartrate", {
  generic_test("GarminHeartRate", "timestamp")

  # Other names
  expect_equal(
    test_alias_column_names("timestamp", "GarminHeartRate"),
    "time"
  )
  expect_equal(
    test_alias_column_names("beatsPerMinute", "GarminHeartRate"),
    "bpm"
  )
  expect_equal(
    test_alias_column_names("macAddress", "GarminHeartRate"),
    "mac_address"
  )
})

# GarminMeta ==========
test_that("alias_column_names.garminmeta", {
  generic_test("GarminMeta")

  # Other names
  expect_equal(
    test_alias_column_names("fromTime", "GarminMeta"),
    "time_from"
  )
  expect_equal(
    test_alias_column_names("toTime", "GarminMeta"),
    "time_to"
  )
  expect_equal(
    test_alias_column_names("accelerometer", "GarminMeta"),
    "n_accelerometer"
  )
  expect_equal(
    test_alias_column_names("actigraphy1", "GarminMeta"),
    "n_actigraphy_1"
  )
  expect_equal(
    test_alias_column_names("actigraphy2", "GarminMeta"),
    "n_actigraphy_2"
  )
  expect_equal(
    test_alias_column_names("actigraphy3", "GarminMeta"),
    "n_actigraphy_3"
  )
  expect_equal(
    test_alias_column_names("bbi", "GarminMeta"),
    "n_bbi"
  )
  expect_equal(
    test_alias_column_names("enhancedBbi", "GarminMeta"),
    "n_enhanced_bbi"
  )
  expect_equal(
    test_alias_column_names("gyroscope", "GarminMeta"),
    "n_gyroscope"
  )
  expect_equal(
    test_alias_column_names("heartRate", "GarminMeta"),
    "n_heartrate"
  )
  expect_equal(
    test_alias_column_names("respiration", "GarminMeta"),
    "n_respiration"
  )
  expect_equal(
    test_alias_column_names("skinTemperature", "GarminMeta"),
    "n_skin_temperature"
  )
  expect_equal(
    test_alias_column_names("spo2", "GarminMeta"),
    "n_spo2"
  )
  expect_equal(
    test_alias_column_names("steps", "GarminMeta"),
    "n_steps"
  )
  expect_equal(
    test_alias_column_names("stress", "GarminMeta"),
    "n_stress"
  )
})

# GarminRespiration ==========
test_that("alias_column_names.garminrespiration", {
  generic_test("GarminRespiration", "timestamp")

  # Other names
  expect_equal(
    test_alias_column_names("timestamp", "GarminRespiration"),
    "time"
  )
  expect_equal(
    test_alias_column_names("breathsPerMinute", "GarminRespiration"),
    "bpm"
  )
  expect_equal(
    test_alias_column_names("macAddress", "GarminRespiration"),
    "mac_address"
  )
})

# GarminSkinTemperature ==========
test_that("alias_column_names.garminskintemperature", {
  generic_test("GarminSkinTemperature", "timestamp")

  # Other names
  expect_equal(
    test_alias_column_names("timestamp", "GarminSkinTemperature"),
    "time"
  )
  expect_equal(
    test_alias_column_names("macAddress", "GarminSkinTemperature"),
    "mac_address"
  )
})

# GarminSPO2 ==========
test_that("alias_column_names.garminspo2", {
  generic_test("GarminSPO2", "timestamp")

  # Other names
  expect_equal(
    test_alias_column_names("timestamp", "GarminSPO2"),
    "time"
  )
  expect_equal(
    test_alias_column_names("spo2Reading", "GarminSPO2"),
    "spo2"
  )
  expect_equal(
    test_alias_column_names("macAddress", "GarminSPO2"),
    "mac_address"
  )
})

# GarminSteps ==========
test_that("alias_column_names.garminsteps", {
  generic_test("GarminSteps", "startTimestamp")

  # Other names
  expect_equal(
    test_alias_column_names("startTimestamp", "GarminSteps"),
    "time"
  )
  expect_equal(
    test_alias_column_names("endTimestamp", "GarminSteps"),
    "end_time"
  )
  expect_equal(
    test_alias_column_names("stepCount", "GarminSteps"),
    "step_count"
  )
  expect_equal(
    test_alias_column_names("totalSteps", "GarminSteps"),
    "total_steps"
  )
  expect_equal(
    test_alias_column_names("macAddress", "GarminSteps"),
    "mac_address"
  )
})

# GarminStress ==========
test_that("alias_column_names.garminstress", {
  generic_test("GarminStress", "timestamp")

  # Other names
  expect_equal(
    test_alias_column_names("timestamp", "GarminStress"),
    "time"
  )
  expect_equal(
    test_alias_column_names("stressScore", "GarminStress"),
    "stress"
  )
  expect_equal(
    test_alias_column_names("macAddress", "GarminStress"),
    "mac_address"
  )
})

# GarminWristStatus ==========
test_that("alias_column_names.garminwriststatus", {
  generic_test("GarminWristStatus", "timestamp")

  # Other names
  expect_equal(
    test_alias_column_names("timestamp", "GarminWristStatus"),
    "time"
  )
  expect_equal(
    test_alias_column_names("macAddress", "GarminWristStatus"),
    "mac_address"
  )
})

# GarminZeroCrossing ==========
test_that("alias_column_names.garminzerocrossing", {
  generic_test("GarminZeroCrossing", "startTimestamp")

  # Other names
  expect_equal(
    test_alias_column_names("startTimestamp", "GarminZeroCrossing"),
    "time"
  )
  expect_equal(
    test_alias_column_names("endTimestamp", "GarminZeroCrossing"),
    "end_time"
  )
  expect_equal(
    test_alias_column_names("zeroCrossingCount", "GarminZeroCrossing"),
    "n_zero_crossing"
  )
  expect_equal(
    test_alias_column_names("totalEnergy", "GarminZeroCrossing"),
    "total_energy"
  )
  expect_equal(
    test_alias_column_names("macAddress", "GarminZeroCrossing"),
    "mac_address"
  )
})
