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

generic_test <- function(sensor) {
  # An invalid name is returned unchanged
  expect_equal(
    test_alias_column_names("foo", sensor),
    "foo"
  )

  # Check multiple inputs
  expect_equal(
    test_alias_column_names(c("id", "start_time"), sensor),
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
    test_alias_column_names("start_time", sensor),
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
