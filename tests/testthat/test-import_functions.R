# Tests for import_functions.R

# common_data ===========
common_data <- function(sensor, ..., end_time = NULL) {
  tibble::tibble(
    study_id = "test-study",
    participant_id = "12345",
    data_format = "cams 1.0.0",
    start_time = "2021-11-14 16:40:00.123456",
    end_time = end_time,
    sensor = sensor,
    data = list(...)
  )
}

# unit_test ===========
unit_test <- function(sensor, ..., .cols = NULL, new_names = NULL, end_time = NULL) {
  # Define the input
  dat <- common_data(
    sensor,
    list(
      data = ...
    ),
    end_time = end_time
  )

  # Execute the sensor function based on its name
  class(dat) <- c(tolower(sensor), class(dat))
  res <- unpack_sensor_data(dat)

  true <- tibble(
    measurement_id = "12345a",
    participant_id = "12345",
    time = "2021-11-14 16:40:00.123456",
    end_time = end_time,
    ...
  )

  # If there is a list column containing data, unlist it first
  which_list <- colnames(true)[purrr::map_lgl(true, is.list)]
  if (length(which_list) > 0) {
    first_element <- true[[which_list]][[1]]
    if (is.list(first_element) || is.null(first_element)) {
      true <- tidyr::unnest_wider(true, all_of(which_list))
    } else {
      true <- unnest(true, all_of(which_list))
    }
  }

  # Replace different-styled column names
  if (!is.null(new_names)) {
    colnames <- colnames(true)
    colnames[colnames %in% new_names] <- names(new_names)
    colnames(true) <- colnames
  }

  # Make sure all columns are present
  if (!is.null(.cols)) {
    missing <- setdiff(.cols, colnames(true))
    if (length(missing) > 0) {
      true[, missing] <- NA
    }
  }

  # Make sure columns are in the same order
  true <- dplyr::relocate(true, any_of(colnames(res)))

  # Check that a measurement_id is present, but don't check the value
  expect_true("measurement_id" %in% colnames(res))
  expect_type(res$measurement_id, "character")

  # Test measurement_id length, but detect suffix
  if (any(grepl("_", res$measurement_id))) {
    expect_equal(unique(nchar(res$measurement_id)), 38) # 38 characters
  } else {
    expect_equal(unique(nchar(res$measurement_id)), 36) # 36 characters
  }

  res$measurement_id <- "12345a" # Hardcode the value to avoid checking it

  true <- as.data.frame(true)
  expect_equal(res, true)
}

# rand ===========
test_that("rand works", {
  # Return a string of correct length
  expect_equal(nchar(rand(10)), 10)
  expect_equal(nchar(rand(5)), 5)

  # rand handles combination of characters and numbers correctly
  # Assuming the default behaviour includes both letters and numbers
  sample <- rand(100)
  expect_true(any(grepl("[a-z]", sample)) & any(grepl("[0-9]", sample)))

  # rand returns only uppercase when uppercase=TRUE
  sample <- rand(10, uppercase = TRUE)
  expect_true(all(grepl("[A-Z0-9]", sample)))

  # rand aborts when both chars and numbers are FALSE
  expect_error(rand(10, chars = FALSE, numbers = FALSE))
})

# gen_id ==============
test_that("gen_id works", {
  # Return a string of correct format and length
  id <- gen_id()
  expect_equal(nchar(id), 36) # Format: 8-4-4-4-12 = 32 chars + 4 hyphens
  expect_match(id, "^([a-z0-9]{8}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{12})$")

  # gen_id returns a string with uppercase characters when uppercase=TRUE"
  id_upper <- gen_id(uppercase = TRUE)
  expect_match(id_upper, "^([A-Z0-9]{8}-[A-Z0-9]{4}-[A-Z0-9]{4}-[A-Z0-9]{4}-[A-Z0-9]{12})$")
})

# safe_data_frame  ===========
test_that("safe_data_frame", {
  dat <- data.frame(a = 1, b = NA)
  res <- safe_data_frame(a = dat$a, b = dat$b, c = dat$c)
  true <- data.frame(a = 1, b = NA, c = NA)
  expect_equal(res, true)
})

# safe_tibble ===========
test_that("safe_tibble", {
  dat <- tibble::tibble(a = 1, b = NA, c = vector("list", 1))
  dat2 <- tibble::tibble(a = 1, b = NA, c = vector("list", 0))
  res <- safe_tibble(a = dat$a, b = dat$b, c = dat$c, d = dat$d)
  res2 <- safe_tibble(a = dat$a, b = dat$b, c = dat$c, d = dat$d)
  true <- tibble::tibble(a = 1, b = NA, c = NA, d = NA)
  expect_equal(res, true)
  expect_equal(res2, true)
})

# Accelerometer =========
test_that("accelerometer", {
  col_names <- c(
    "n",
    "x_mean",
    "y_mean",
    "z_mean",
    "x_median",
    "y_median",
    "z_median",
    "x_std",
    "y_std",
    "z_std",
    "x_aad",
    "y_aad",
    "z_aad",
    "x_min",
    "y_min",
    "z_min",
    "x_max",
    "y_max",
    "z_max",
    "x_max_min_diff",
    "y_max_min_diff",
    "z_max_min_diff",
    "x_mad",
    "y_mad",
    "z_mad",
    "x_iqr",
    "y_iqr",
    "z_iqr",
    "x_neg_n",
    "y_neg_n",
    "z_neg_n",
    "x_pos_n",
    "y_pos_n",
    "z_pos_n",
    "x_above_mean",
    "y_above_mean",
    "z_above_mean",
    "x_energy",
    "y_energy",
    "z_energy",
    "avg_res_acc",
    "sma"
  )

  unit_test(
    "accelerometer",
    end_time = "2021-11-14 16:40:05.123456",
    .cols = col_names,
    new_names = c(n = "count"),
    count = 100
  )

  unit_test(
    "accelerometer",
    .cols = col_names,
    end_time = "2021-11-14 16:40:05.123456",
    n = 100,
    x_mean = 0.1,
    y_mean = 0.2,
    z_mean = 0.3,
    x_median = 0.1,
    y_median = 0.2,
    z_median = 0.3,
    x_std = 0.1,
    y_std = 0.2,
    z_std = 0.3,
    x_aad = 0.1,
    y_aad = 0.2,
    z_aad = 0.3,
    x_min = 0.1,
    y_min = 0.2,
    z_min = 0.3,
    x_max = 0.1,
    y_max = 0.2,
    z_max = 0.3,
    x_max_min_diff = 0.1,
    y_max_min_diff = 0.2,
    z_max_min_diff = 0.3,
    x_mad = 0.1,
    y_mad = 0.2,
    z_mad = 0.3,
    x_iqr = 0.1,
    y_iqr = 0.2,
    z_iqr = 0.3,
    x_neg_n = 1,
    y_neg_n = 2,
    z_neg_n = 3,
    x_pos_n = 1,
    y_pos_n = 2,
    z_pos_n = 3,
    x_above_mean = 1,
    y_above_mean = 2,
    z_above_mean = 3,
    x_energy = 0.1,
    y_energy = 0.2,
    z_energy = 0.3,
    avg_res_acc = 0.1,
    sma = 0.1
  )
  unit_test(
    "accelerometer",
    end_time = NA,
    .cols = col_names
  )

  # Test non-existing column
  expect_error(
    unit_test(
      "accelerometer",
      end_time = NA,
      foo = 1,
      .cols = col_names
    )
  )
})

# Activity ===========
test_that("activity", {
  col_names <- c("confidence", "type")

  unit_test(
    "activity",
    .cols = col_names,
    confidence = 80,
    type = "WALKING"
  )
  unit_test(
    "activity",
    .cols = col_names
  )
})

# Air Quality ===========
test_that("air_quality", {
  col_names <- c(
    "air_quality_index",
    "air_quality_level",
    "source",
    "place",
    "latitude",
    "longitude"
  )

  new_names <- c(
    air_quality_index = "airQualityIndex",
    air_quality_level = "airQualityLevel"
  )

  unit_test(
    "airquality",
    .cols = col_names,
    air_quality_index = 30,
    air_quality_level = "GOOD",
    source = "IRCEL-CELINE - Belgian Interregional Environment Agency",
    place = "Aarschot, Belgium",
    latitude = 50.12345678901234,
    longitude = 4.12345678901234
  )
  unit_test(
    "airquality",
    .cols = col_names,
    airQualityIndex = 30,
    airQualityLevel = "GOOD",
    source = "IRCEL-CELINE - Belgian Interregional Environment Agency",
    place = "Aarschot, Belgium",
    latitude = 50.12345678901234,
    longitude = 4.12345678901234,
    new_names = new_names
  )
  unit_test(
    "airquality",
    .cols = col_names,
    airQualityIndex = NA,
    airQualityLevel = NA,
    new_names = new_names
  )
})

# AppUsage ==========
test_that("appusage", {
  # Test with empty usage
  dat <- common_data(
    sensor = "appusage",
    end_time = "2024-01-24 20:46:40.434183",
    list(
      start = "2024-01-24 20:16:40.434183",
      end = "2024-01-24 20:46:40.434183",
      usage = list()
    ),
    list(
      start = "2024-01-24 20:16:41.434183",
      end = "2024-01-24 20:46:41.434183",
      usage = list()
    )
  )

  # Execute the sensor function based on its name
  class(dat) <- c(tolower("appusage"), class(dat))
  res <- unpack_sensor_data(dat)

  true <- tibble(
    measurement_id = "12345a",
    participant_id = "12345",
    time = "2021-11-14 16:40:00.123456",
    end_time = "2024-01-24 20:46:40.434183",
    start = c("2024-01-24 20:16:40.434183", "2024-01-24 20:16:41.434183"),
    end = c("2024-01-24 20:46:40.434183", "2024-01-24 20:46:41.434183"),
    usage = c(NA, NA),
    app = c(NA, NA),
    package_name = c(NA, NA),
    last_foreground = c(NA, NA)
  )

  # Check that a measurement_id is present, but don't check the value
  expect_true("measurement_id" %in% colnames(res))
  expect_type(res$measurement_id, "character")
  expect_match(res$measurement_id, ".{36}") # 36 characters
  res$measurement_id <- "12345a" # Hardcode the value to avoid checking it

  true <- as.data.frame(true)
  expect_equal(res, true)

  dat <- common_data(
    sensor = "appusage",
    end_time = "2024-01-24 20:46:40.434183",
    list(
      start = "2024-01-24 20:16:40.434183",
      end = "2024-01-24 20:46:40.434183",
      usage = list(
        com.google.android.maps = list(
          packageName = "com.google.android.maps",
          appName = "Google Maps",
          usage = 100,
          startDate = "2021-11-14 16:40:05.123456",
          endDate = "2021-11-14 16:40:05.123456",
          lastForeground = "2021-11-14 16:40:05.123456"
        ),
        com.google.android.youtube = list(
          packageName = "com.google.android.youtube",
          appName = "YouTube",
          usage = 100,
          startDate = "2021-11-14 16:40:05.123456",
          endDate = "2021-11-14 16:40:05.123456",
          lastForeground = "2021-11-14 16:40:05.123456"
        )
      )
    )
  )

  # Execute the sensor function based on its name
  class(dat) <- c(tolower("appusage"), class(dat))
  res <- unpack_sensor_data(dat)

  true <- tibble(
    measurement_id = "12345a",
    participant_id = "12345",
    time = "2021-11-14 16:40:00.123456",
    end_time = "2024-01-24 20:46:40.434183",
    start = c("2021-11-14 16:40:05.123456", "2021-11-14 16:40:05.123456"),
    end = c("2021-11-14 16:40:05.123456", "2021-11-14 16:40:05.123456"),
    usage = c(100, 100),
    app = c("Google Maps", "YouTube"),
    package_name = c("com.google.android.maps", "com.google.android.youtube"),
    last_foreground = c("2021-11-14 16:40:05.123456", "2021-11-14 16:40:05.123456")
  )

  # Check that a measurement_id is present, but don't check the value
  expect_true("measurement_id" %in% colnames(res))
  expect_type(res$measurement_id, "character")
  expect_equal(unique(nchar(res$measurement_id)), 38) # 38 characters, because of suffix
  res$measurement_id <- "12345a" # Hardcode the value to avoid checking it

  true <- as.data.frame(true)
  expect_equal(res, true)

  # Repeat but with different column names
  dat2 <- common_data(
    sensor = "appusage",
    end_time = "2024-01-24 20:46:40.434183",
    list(
      start = "2024-01-24 20:16:40.434183",
      end = "2024-01-24 20:46:40.434183",
      usage = list(
        com.google.android.maps = list(
          package_name = "com.google.android.maps",
          app_name = "Google Maps",
          usage = 100,
          start_date = "2021-11-14 16:40:05.123456",
          end_date = "2021-11-14 16:40:05.123456",
          last_foreground = "2021-11-14 16:40:05.123456"
        ),
        com.google.android.youtube = list(
          package_name = "com.google.android.youtube",
          app_name = "YouTube",
          usage = 100,
          start_date = "2021-11-14 16:40:05.123456",
          end_date = "2021-11-14 16:40:05.123456",
          last_foreground = "2021-11-14 16:40:05.123456"
        )
      )
    )
  )

  # Execute the sensor function based on its name
  class(dat2) <- c(tolower("appusage"), class(dat2))
  res2 <- unpack_sensor_data(dat2)

  # Check that a measurement_id is present, but don't check the value
  expect_true("measurement_id" %in% colnames(res2))
  expect_type(res2$measurement_id, "character")
  expect_equal(unique(nchar(res2$measurement_id)), 38) # 38 characters, because of suffix
  res2$measurement_id <- "12345a" # Hardcode the value to avoid checking it

  true <- as.data.frame(true)
  expect_equal(res2, true)
  expect_identical(res, res2)
})

# Battery ===========
test_that("battery", {
  .cols <- c("battery_level", "battery_status")
  new_names <- c(
    battery_level = "batteryLevel",
    battery_status = "batteryStatus"
  )

  unit_test(
    "battery",
    .cols = .cols
  )

  unit_test(
    "battery",
    .cols = .cols,
    battery_level = 80,
    battery_status = "CHARGING"
  )

  unit_test(
    "battery",
    .cols = .cols,
    batteryLevel = 80,
    batteryStatus = "CHARGING",
    new_names = new_names
  )
})

# Bluetooth ===============
test_that("bluetooth", {
  .cols <- c(
    "start_scan",
    "end_scan",
    "advertisement_name",
    "bluetooth_device_id",
    "bluetooth_device_name",
    "bluetooth_device_type",
    "connectable",
    "rssi",
    "tx_power_level"
  )
  new_names <- c(
    start_scan = "startScan",
    end_scan = "endScan",
    advertisement_name = "advertisementName",
    bluetooth_device_id = "bluetoothDeviceId",
    bluetooth_device_name = "bluetoothDeviceName",
    bluetooth_device_type = "bluetoothDeviceType",
    connectable = "connectable",
    rssi = "rssi",
    tx_power_level = "txPowerLevel"
  )

  unit_test(
    "bluetooth",
    .cols = .cols
  )

  unit_test(
    "bluetooth",
    .cols = .cols,
    scan_result = list(NULL)
  )

  unit_test(
    "bluetooth",
    .cols = .cols,
    scan_result = list(
      list(
        start_scan = "2021-11-14 16:40:05.123456",
        end_scan = "2021-11-14 16:40:05.123456",
        advertisement_name = "Samsung TV",
        bluetooth_device_id = "00:11:22:33:44:55",
        bluetooth_device_name = "Samsung TV",
        bluetooth_device_type = "BLE",
        connectable = TRUE,
        rssi = -50,
        tx_power_level = -50
      )
    )
  )

  unit_test(
    "bluetooth",
    .cols = .cols,
    scan_result = list(
      list(
        startScan = "2021-11-14 16:40:05.123456",
        endScan = "2021-11-14 16:40:05.123456",
        advertisementName = "Samsung TV",
        bluetoothDeviceId = "00:11:22:33:44:55",
        bluetoothDeviceName = "Samsung TV",
        bluetoothDeviceType = "BLE",
        connectable = TRUE,
        rssi = -50,
        txPowerLevel = -50
      )
    ),
    new_names = new_names
  )
})

# Connectivity ============
test_that("connectivity", {
  unit_test(
    "connectivity",
    .cols = "connectivity_status"
  )
  unit_test(
    "connectivity",
    .cols = "connectivity_status",
    connectivity_status = "Mobile"
  )
  unit_test(
    "connectivity",
    .cols = "connectivity_status",
    new_names = c(connectivity_status = "connectivityStatus"),
    connectivityStatus = "Mobile"
  )

  # Test with multiple entries for connectivityStatus
  unit_test(
    "connectivity",
    .cols = "connectivity_status",
    connectivity_status = list("Mobile", "WiFi")
  )
})

# Device ===========
test_that("device", {
  .cols <- c(
    "device_id",
    "hardware",
    "device_name",
    "device_manufacturer",
    "device_model",
    "operating_system",
    "platform",
    "operating_system_version",
    "sdk"
  )
  new_names <- c(
    device_id = "deviceId",
    hardware = "hardware",
    device_name = "deviceName",
    device_manufacturer = "deviceManufacturer",
    device_model = "deviceModel",
    operating_system = "operatingSystem",
    platform = "platform",
    operating_system_version = "operatingSystemVersion",
    sdk = "sdk"
  )

  unit_test(
    "device",
    .cols = .cols
  )

  unit_test(
    "device",
    .cols = .cols,
    device_id = "1234567890",
    hardware = "iPhone",
    device_name = "iPhone 12",
    device_manufacturer = "Apple",
    device_model = "iPhone 12",
    operating_system = "iOS",
    platform = "iOS",
    operating_system_version = "15.0",
    sdk = "1.0.0"
  )

  # iOS
  # TODO: Add deviceData
  unit_test(
    "device",
    .cols = .cols,
    deviceId = "1234567890",
    hardware = "iPhone",
    deviceName = "iPhone 12",
    deviceManufacturer = "Apple",
    deviceModel = "iPhone 12",
    operatingSystem = "iOS",
    platform = "iOS",
    operatingSystemVersion = "15.0",
    # deviceData = list(
    #   utsName = list(
    #     sysname = "Darwin",
    #     nodename = "iPhone",
    #     release = "15.0",
    #     version = "15.0.0",
    #     machine = "iPhone12,1"
    #   ),
    #   systemVersion = "15.0"
    # ),
    sdk = "1.0.0",
    new_names = new_names
  )
})

# Error ========
test_that("error", {
  unit_test(
    "error",
    .cols = "message"
  )
  unit_test(
    "error",
    .cols = "message",
    message = "Some error message"
  )
})

# Geofence ==========
test_that("geofence", {
  .cols <- c("center", "dwell", "name", "radius", "state")

  unit_test(
    "geofence",
    .cols = .cols
  )

  unit_test(
    "geofence",
    .cols = .cols,
    center = 50.12345678901234,
    dwell = 100,
    name = "Home",
    radius = 100,
    state = "inside"
  )
})

# Gyroscope ============
test_that("gyroscope", {
  .cols <- c("x", "y", "z")

  unit_test(
    "gyroscope",
    .cols = .cols
  )

  unit_test(
    "gyroscope",
    .cols = .cols,
    x = 0.1,
    y = 0.2,
    z = 0.3
  )
})

# Heartbeat ===========
test_that("hearbeat", {
  col_names <- c("period", "device_type", "device_role_name")
  new_names <- c(
    device_type = "deviceType",
    device_role_name = "deviceRoleName"
  )

  unit_test(
    "heartbeat",
    .cols = col_names
  )

  unit_test(
    "heartbeat",
    .cols = col_names,
    period = 5,
    device_type = "Smartphone",
    device_role_name = "Masterphone"
  )

  unit_test(
    "heartbeat",
    .cols = col_names,
    new_names = new_names,
    period = 5,
    deviceType = "Smartphone",
    deviceRoleName = "Masterphone"
  )
})

# Keyboard ============
test_that("keyboard", {
  data <- tibble()
  class(data) <- c("keyboard", class(data))
  expect_warning({
    res <- unpack_sensor_data(data)
  })
  expect_null(res)
})

# Light ========
test_that("light", {
  .cols <- c("mean_lux", "std_lux", "min_lux", "max_lux")
  new_names <- c(
    mean_lux = "meanLux",
    std_lux = "stdLux",
    min_lux = "minLux",
    max_lux = "maxLux"
  )

  unit_test(
    "light",
    .cols = .cols,
    end_time = NA
  )
  unit_test(
    "light",
    .cols = .cols,
    end_time = "2021-11-14 16:40:05.123456",
    mean_lux = 50,
    std_lux = 10,
    min_lux = 40,
    max_lux = 60
  )
  unit_test(
    "light",
    .cols = .cols,
    new_names = new_names,
    end_time = "2021-11-14 16:40:05.123456",
    meanLux = 50,
    stdLux = 10,
    minLux = 40,
    maxLux = 60
  )
})

# Location ===============
test_that("location", {
  .cols <- c(
    "latitude",
    "longitude",
    "altitude",
    "accuracy",
    "vertical_accuracy",
    "speed",
    "speed_accuracy",
    "heading",
    "heading_accuracy",
    "is_mock"
  )
  new_names <- c(
    vertical_accuracy = "verticalAccuracy",
    speed_accuracy = "speedAccuracy",
    heading_accuracy = "headingAccuracy",
    is_mock = "isMock"
  )

  unit_test(
    "location",
    .cols = .cols
  )

  unit_test(
    "location",
    .cols = .cols,
    latitude = 50.12345678901234,
    longitude = 4.12345678901234,
    altitude = 100,
    accuracy = 10,
    vertical_accuracy = 5,
    speed = 10,
    speed_accuracy = 5,
    heading = 90,
    heading_accuracy = 5,
    is_mock = FALSE
  )

  unit_test(
    "location",
    .cols = .cols,
    latitude = 50.12345678901234,
    longitude = 4.12345678901234,
    altitude = 100,
    accuracy = 10,
    verticalAccuracy = 5,
    speed = 10,
    speedAccuracy = 5,
    heading = 90,
    headingAccuracy = 5,
    isMock = FALSE,
    new_names = new_names
  )
})

# Memory ========
test_that("memory", {
  .cols <- c("free_physical_memory", "free_virtual_memory")
  new_names <- c(
    free_physical_memory = "freePhysicalMemory",
    free_virtual_memory = "freeVirtualMemory"
  )

  unit_test(
    "memory",
    .cols = .cols
  )
  unit_test(
    "memory",
    .cols = .cols,
    free_physical_memory = 100,
    free_virtual_memory = 200
  )
  unit_test(
    "memory",
    .cols = .cols,
    new_names = new_names,
    freePhysicalMemory = 100,
    freeVirtualMemory = 200
  )
})

# Noise ========
test_that("noise", {
  .cols <- c("mean_decibel", "std_decibel", "min_decibel", "max_decibel")
  new_names <- c(
    mean_decibel = "meanDecibel",
    std_decibel = "stdDecibel",
    min_decibel = "minDecibel",
    max_decibel = "maxDecibel"
  )

  unit_test(
    "noise",
    .cols = .cols,
    end_time = NA
  )
  unit_test(
    "noise",
    .cols = .cols,
    end_time = "2021-11-14 16:40:05.123456",
    mean_decibel = 50,
    std_decibel = 10,
    min_decibel = 40,
    max_decibel = 60
  )
  unit_test(
    "noise",
    .cols = .cols,
    end_time = "2021-11-14 16:40:05.123456",
    new_names = new_names,
    meanDecibel = 50,
    stdDecibel = 10,
    minDecibel = 40,
    maxDecibel = 60
  )
})

# Pedometer ========
test_that("pedometer", {
  unit_test(
    "pedometer",
    .cols = "step_count"
  )
  unit_test(
    "pedometer",
    .cols = "step_count",
    step_count = 1
  )
  unit_test(
    "pedometer",
    .cols = "step_count",
    new_names = c(step_count = "stepCount"),
    stepCount = 1
  )
})

# Screen ========
test_that("screen", {
  unit_test(
    "screen",
    .cols = "screen_event"
  )
  unit_test(
    "screen",
    .cols = "screen_event",
    screen_event = "SCREEN_ON"
  )
})

# Timezone ========
test_that("timezone", {
  unit_test(
    "timezone",
    .cols = "timezone"
  )
  unit_test(
    "timezone",
    .cols = "timezone",
    timezone = "Europe/Brussels"
  )
})

# Weather ===============
test_that("weather", {
  col_names <- c(
    "country",
    "area_name",
    "weather_main",
    "weather_description",
    "sunrise",
    "sunset",
    "latitude",
    "longitude",
    "pressure",
    "wind_speed",
    "wind_degree",
    "humidity",
    "cloudiness",
    "rain_last_hour",
    "rain_last_3hours",
    "snow_last_hour",
    "snow_last_3hours",
    "temperature",
    "temp_min",
    "temp_max"
  )

  new_names <- c(
    area_name = "areaName",
    weather_main = "weatherMain",
    weather_description = "weatherDescription",
    wind_speed = "windSpeed",
    wind_degree = "windDegree",
    rain_last_hour = "rainLastHour",
    rain_last_3hours = "rainLast3Hours",
    snow_last_hour = "snowLastHour",
    snow_last_3hours = "snowLast3Hours",
    temp_min = "tempMin",
    temp_max = "tempMax"
  )

  unit_test(
    "weather",
    areaName = "Aarschot",
    weatherMain = "Clouds",
    weatherDescription = "scattered clouds",
    windSpeed = 5,
    windDegree = 180,
    rainLastHour = 0,
    rainLast3Hours = 0,
    snowLastHour = 0,
    snowLast3Hours = 0,
    tempMin = 5,
    tempMax = 10,
    .cols = col_names,
    new_names = new_names
  )

  unit_test(
    "weather",
    .cols = col_names,
    country = "BE",
    area_name = "Aarschot",
    weather_main = "Clouds",
    weather_description = "scattered clouds",
    sunrise = "2021-11-14 07:00:00",
    sunset = "2021-11-14 17:00:00",
    latitude = 50.12345678901234,
    longitude = 4.12345678901234,
    pressure = 1000,
    wind_speed = 5,
    wind_degree = 180,
    humidity = 80,
    cloudiness = 20,
    rain_last_hour = 0,
    rain_last_3hours = 0,
    snow_last_hour = 0,
    snow_last_3hours = 0,
    temperature = 10,
    temp_min = 5,
    temp_max = 15
  )
  unit_test(
    "weather",
    .cols = col_names
  )
})

# Wifi ============
test_that("wifi", {
  col_names <- c("ssid", "bssid", "ip")

  unit_test(
    "wifi",
    .cols = col_names,
    ssid = "MyWifi",
    bssid = "00:11:22:33:44:55",
    ip = "192.168.0.1"
  )
  unit_test(
    "wifi",
    .cols = col_names
  )
})
