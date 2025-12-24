# Tests for new_tests.json (spoofed realistic data)
# Import only new_tests.json and validate exact values per sensor.

db_test_new <- function(sensor, assert_fun) {
  path <- system.file("testdata", package = "mpathsenser")
  db <- create_db(NULL, ":memory:")

  # Only read in new_tests.json so mark all other files as processed
  files <- list.files(path, recursive = FALSE)
  files <- setdiff(files, "new_tests.json")
  add_study(db, study_id = "foo", data_format = "foo")
  add_participant(db, "12345", "foo")
  add_processed_files(
    db,
    file_name = files,
    study_id = rep("foo", length(files)),
    participant_id = rep("12345", length(files))
  )
  suppressMessages(
    import(path, db = db, sensors = sensor, batch_size = 1, recursive = FALSE)
  )

  data <- get_data(db, sensor, "12345", "2025-12-16", "2025-12-17") |>
    collect()

  assert_fun(data, db)

  close_db(db)
}


### Accelerometer ====
test_that("Accelerometer (new_tests)", {
  db_test_new("Accelerometer", function(d, db) {
    expect_equal(nrow(d), 1)
    expect_equal(d$participant_id, "12345")
    expect_equal(d$date, "2025-12-16")
    expect_equal(d$time, "12:37:32.322187")
    expect_true(is.na(d$measurement_id))
    expect_equal(d$end_time, "2025-12-16 12:39:47.319006")
    expect_equal(d$n, 673)
    expect_equal(d$x_mean, -0.1393, tolerance = 1e-4)
    expect_equal(d$y_mean, -0.4194, tolerance = 1e-4)
    expect_equal(d$z_mean, -0.0719, tolerance = 1e-4)
    expect_equal(d$x_std, 1.2418, tolerance = 1e-4)
    expect_equal(d$y_std, 1.88, tolerance = 1e-4)
    expect_equal(d$z_std, 2.4653, tolerance = 1e-4)
    expect_equal(d$x_aad, 1.0247, tolerance = 1e-4)
    expect_equal(d$y_aad, 1.6331, tolerance = 1e-4)
    expect_equal(d$z_aad, 1.9447, tolerance = 1e-4)
    expect_equal(d$x_min, -3.0428, tolerance = 1e-4)
    expect_equal(d$y_min, -6.9259, tolerance = 1e-4)
    expect_equal(d$z_min, -6.4376, tolerance = 1e-4)
    expect_equal(d$x_max, 2.9984, tolerance = 1e-4)
    expect_equal(d$y_max, 4.8714, tolerance = 1e-4)
    expect_equal(d$z_max, 4.84, tolerance = 1e-4)
    expect_equal(d$x_max_min_diff, 6.477, tolerance = 1e-3)
    expect_equal(d$y_max_min_diff, 11.0295, tolerance = 1e-3)
    expect_equal(d$z_max_min_diff, 10.7313, tolerance = 1e-3)
    expect_equal(d$x_median, -0.0023, tolerance = 1e-4)
    expect_equal(d$y_median, -0.0135, tolerance = 1e-4)
    expect_equal(d$z_median, -0.2482, tolerance = 1e-4)
    expect_equal(d$x_mad, 0.8556, tolerance = 1e-4)
    expect_equal(d$y_mad, 1.2119, tolerance = 1e-4)
    expect_equal(d$z_mad, 1.8867, tolerance = 1e-4)
    expect_equal(d$x_iqr, 1.6344, tolerance = 1e-4)
    expect_equal(d$y_iqr, 2.2751, tolerance = 1e-4)
    expect_equal(d$z_iqr, 3.9234, tolerance = 1e-4)
    expect_equal(d$x_neg_n, 334)
    expect_equal(d$y_neg_n, 339)
    expect_equal(d$z_neg_n, 365)
    expect_equal(d$x_pos_n, 327)
    expect_equal(d$y_pos_n, 319)
    expect_equal(d$z_pos_n, 301)
    expect_equal(d$x_above_mean, 357)
    expect_equal(d$y_above_mean, 382)
    expect_equal(d$z_above_mean, 322)
    expect_equal(d$x_energy, 1.3283, tolerance = 1e-4)
    expect_equal(d$y_energy, 4.0747, tolerance = 1e-4)
    expect_equal(d$z_energy, 5.9975, tolerance = 1e-4)
    expect_equal(d$avg_res_acc, 3.0844, tolerance = 1e-4)
    expect_equal(d$sma, 4.5653, tolerance = 1e-4)
    expect_equal(nrow(d), 1)
  })
})

### Activity ====
test_that("Activity (new_tests)", {
  db_test_new("Activity", function(d, db) {
    expect_equal(nrow(d), 1)
    expect_equal(d$participant_id, "12345")
    expect_equal(d$date, "2025-12-16")
    expect_equal(d$time, "12:41:50.031904")
    expect_true(is.na(d$measurement_id))
    expect_equal(d$confidence, 70L)
    expect_equal(d$type, "WALKING")
  })
})

### AppUsage ====
test_that("AppUsage (new_tests)", {
  db_test_new("AppUsage", function(d, db) {
    expect_equal(nrow(d), 3)
    expect_true(all(d$participant_id == "12345"))
    expect_true(all(d$date == "2025-12-16"))
    expect_true(all(d$time == "12:37:32.32328"))
    expect_true(all(is.na(d$measurement_id)))
    expect_true(all(d$end_time == "2025-12-16 12:45:55.888309"))
    expect_true(all(d$app %in% c("kuleuven", "music", "whatsapp")))

    expect_equal(d$usage[d$app == "kuleuven"], 55000000L)
    expect_equal(d$start[d$app == "kuleuven"], "2025-12-16T01:40:33")
    expect_equal(d$end[d$app == "kuleuven"], "2025-12-16T16:50:14")
    expect_equal(d$package_name[d$app == "kuleuven"], "io.m_Path_Sense.kuleuven")
    expect_equal(d$last_foreground[d$app == "whatsapp"], "2025-12-16T13:30:59")

    expect_equal(d$usage[d$app == "music"], 11000000L)
    expect_equal(d$start[d$app == "music"], "2025-12-16T01:40:33")
    expect_equal(d$end[d$app == "music"], "2025-12-16T16:50:53")
    expect_equal(d$package_name[d$app == "music"], "com.spotify.music")
    expect_equal(d$last_foreground[d$app == "music"], "2025-12-16T16:50:53")

    expect_equal(d$usage[d$app == "whatsapp"], 139000000L)
    expect_equal(d$start[d$app == "whatsapp"], "2025-12-16T01:40:33")
    expect_equal(d$end[d$app == "whatsapp"], "2025-12-16T16:53:42")
    expect_equal(d$package_name[d$app == "whatsapp"], "com.whatsapp")
    expect_equal(d$last_foreground[d$app == "kuleuven"], "2025-12-16T16:31:04")
  })
})

### Battery ====
test_that("Battery (new_tests)", {
  db_test_new("Battery", function(d, db) {
    expect_equal(nrow(d), 1)
    expect_equal(d$participant_id, "12345")
    expect_equal(d$date, "2025-12-16")
    expect_equal(d$time, "12:38:34.94352")
    expect_true(is.na(d$measurement_id))
    expect_equal(d$battery_level, 25L)
    expect_equal(d$battery_status, "charging")
  })
})

### Bluetooth ====
test_that("Bluetooth (new_tests)", {
  db_test_new("Bluetooth", function(d, db) {
    expect_equal(nrow(d), 1)
    expect_equal(d$participant_id, "12345")
    expect_equal(d$date, "2025-12-16")
    expect_equal(d$time, "19:23:33.695276")
    expect_true(is.na(d$measurement_id))
    expect_equal(d$start_scan, "2025-12-16T20:23:22.458165")
    expect_equal(d$end_scan, "2025-12-16T20:23:32.460703")
    expect_equal(d$advertisement_name, "TestB1")
    expect_equal(d$bluetooth_device_id, "00")
    expect_equal(d$bluetooth_device_name, "TestB1")
    expect_true(is.na(d$bluetooth_device_type))
    expect_equal(d$connectable, 1L)
    expect_equal(d$tx_power_level, -4L)
    expect_equal(d$rssi, -72L)
  })
})

### Connectivity ====
test_that("Connectivity (new_tests)", {
  db_test_new("Connectivity", function(d, db) {
    expect_equal(nrow(d), 1)
    expect_equal(d$participant_id, "12345")
    expect_equal(d$date, "2025-12-16")
    expect_equal(d$time, "12:35:53.152757")
    expect_true(is.na(d$measurement_id))
    expect_equal(d$connectivity_status, "wifi")
  })
})

### Device ====
test_that("Device (new_tests)", {
  db_test_new("Device", function(d, db) {
    expect_equal(nrow(d), 1)
    expect_equal(d$participant_id, "12345")
    expect_equal(d$date, "2025-12-16")
    expect_equal(d$time, "12:30:00.755631")
    expect_true(is.na(d$measurement_id))
    expect_equal(d$device_id, "UKQ1.231108.001")
    expect_equal(d$hardware, "xchip")
    expect_equal(d$device_name, "XPHONE-DEV")
    expect_equal(d$device_manufacturer, "XPhone")
    expect_equal(d$device_model, "XP1234")
    expect_equal(d$operating_system, "REL")
    expect_equal(d$platform, "Android")
    expect_equal(d$operating_system_version, "16")
    expect_equal(d$sdk, "34")
  })
})

### GarminAccelerometer ====
test_that("GarminAccelerometer (new_tests)", {
  db_test_new("GarminAccelerometer", function(d, db) {
    expect_equal(nrow(d), 2)
    expect_true(all(d$participant_id == "12345"))
    expect_true(all(d$date == "2025-12-16"))
    expect_true(all(d$mac_address == "00"))
    expect_equal(d$time[1], "16:30:23.559000")
    expect_equal(d$time[2], "16:30:23.598999")
    expect_equal(d$x[1], 113.3744, tolerance = 1e-4)
    expect_equal(d$y[1], 437.6353, tolerance = 1e-4)
    expect_equal(d$z[1], -906.8707, tolerance = 1e-4)
    expect_equal(d$x[2], 118.0369, tolerance = 1e-4)
    expect_equal(d$y[2], 440.0922, tolerance = 1e-4)
    expect_equal(d$z[2], -873.2643, tolerance = 1e-4)
  })
})

### GarminActigraphy ====
test_that("GarminActigraphy (new_tests)", {
  db_test_new("GarminActigraphy", function(d, db) {
    expect_equal(nrow(d), 6)
    expect_true(all(d$participant_id == "12345"))
    expect_true(all(d$date == "2025-12-16"))
    expect_true(all(d$mac_address == "00"))
    expect_true(all(d$instance %in% c("ACTIGRAPHY_1", "ACTIGRAPHY_2", "ACTIGRAPHY_3")))
    expect_equal(sum(d$instance == "ACTIGRAPHY_1"), 2)
    expect_equal(sum(d$instance == "ACTIGRAPHY_2"), 2)
    expect_equal(sum(d$instance == "ACTIGRAPHY_3"), 2)
    expect_equal(
      d$total_energy,
      c(1488289.81, 1465987.1425, 1616289.0334, 1591059.3688, 1567243.3409, 1501789.1451)
    )
    expect_equal(d$n_zero_crossing, rep(0, 6))
    expect_equal(d$time_above_threshold, c(58.1096, 59.9254, 58.7958, 59.7624, 60.2099, 60.7411))
  })
})

### GarminBBI ====
test_that("GarminBBI (new_tests)", {
  db_test_new("GarminBBI", function(d, db) {
    expect_equal(nrow(d), 2)
    expect_true(all(d$participant_id == "12345"))
    expect_true(all(d$date == "2025-12-16"))
    expect_true(all(d$mac_address == "00"))
    expect_equal(d$time[1], "16:30:22.680000")
    expect_equal(d$time[2], "16:30:23.454999")
    expect_equal(d$bbi[1], 763L)
    expect_equal(d$bbi[2], 775L)
  })
})

### GarminEnhancedBBI ====
test_that("GarminEnhancedBBI (new_tests)", {
  db_test_new("GarminEnhancedBBI", function(d, db) {
    expect_equal(nrow(d), 2)
    expect_true(all(d$participant_id == "12345"))
    expect_true(all(d$date == "2025-12-16"))
    expect_true(all(d$mac_address == "00"))
    expect_true(all(d$status == "lowConfidence"))
    expect_equal(d$time[1], "16:30:22.598000")
    expect_equal(d$time[2], "16:30:23.457000")
    expect_equal(d$bbi[1], 992L)
    expect_equal(d$bbi[2], 859L)
    expect_true(all(d$gap_duration == 0))
  })
})

### GarminGyroscope ====
test_that("GarminGyroscope (new_tests)", {
  db_test_new("GarminGyroscope", function(d, db) {
    # Should be 0 rows as gyroscope count is 0 in entryCounts
    expect_equal(nrow(d), 0)
  })
})

### GarminHeartRate ====
test_that("GarminHeartRate (new_tests)", {
  db_test_new("GarminHeartRate", function(d, db) {
    expect_equal(nrow(d), 2)
    expect_true(all(d$participant_id == "12345"))
    expect_true(all(d$date == "2025-12-16"))
    expect_true(all(d$mac_address == "00"))
    expect_true(all(d$status == "locked"))
    expect_equal(d$time[1], "16:30:25.566999")
    expect_equal(d$time[2], "16:30:30.566999")
    expect_equal(d$bpm[1], 76L)
    expect_equal(d$bpm[2], 74L)
  })
})

### GarminMeta ====
test_that("GarminMeta (new_tests)", {
  db_test_new("GarminMeta", function(d, db) {
    expect_equal(nrow(d), 1)
    expect_equal(d$participant_id, "12345")
    expect_equal(d$date, "2025-12-16")
    expect_equal(d$n_heartrate, 26L)
    expect_equal(d$n_stress, 2L)
    expect_equal(d$n_steps, 5L)
    expect_equal(d$n_bbi, 162L)
    expect_equal(d$n_enhanced_bbi, 160L)
    expect_equal(d$n_respiration, 0L)
    expect_equal(d$n_spo2, 0L)
    expect_equal(d$n_accelerometer, 2200L)
    expect_equal(d$n_gyroscope, 0L)
    expect_equal(d$n_skin_temperature, 0L)
    expect_equal(d$n_actigraphy_1, 2L)
    expect_equal(d$n_actigraphy_2, 2L)
    expect_equal(d$n_actigraphy_3, 2L)
  })
})

### GarminRespiration ====
test_that("GarminRespiration (new_tests)", {
  db_test_new("GarminRespiration", function(d, db) {
    # Should be 0 rows as respiration count is 0 in entryCounts
    expect_equal(nrow(d), 0)
  })
})

### GarminSkinTemperature ====
test_that("GarminSkinTemperature (new_tests)", {
  db_test_new("GarminSkinTemperature", function(d, db) {
    # Should be 0 rows as skinTemperature count is 0 in entryCounts
    expect_equal(nrow(d), 0)
  })
})

### GarminSPO2 ====
test_that("GarminSPO2 (new_tests)", {
  db_test_new("GarminSPO2", function(d, db) {
    # Should be 0 rows as spo2 count is 0 in entryCounts
    expect_equal(nrow(d), 0)
  })
})

### GarminSteps ====
test_that("GarminSteps (new_tests)", {
  db_test_new("GarminSteps", function(d, db) {
    expect_equal(nrow(d), 2)
    expect_true(all(d$participant_id == "12345"))
    expect_true(all(d$date == "2025-12-16"))
    expect_true(all(d$mac_address == "00"))
    expect_equal(d$time[1], "16:31:44.566999")
    expect_equal(d$end_time[1], 1765902712)
    expect_equal(d$time[2], "16:31:54.566999")
    expect_equal(d$end_time[2], 1765902722)
    expect_equal(d$step_count[1], -1L)
    expect_equal(d$step_count[2], 3L)
    expect_equal(d$total_steps[1], 122L)
    expect_equal(d$total_steps[2], 119L)
  })
})

### GarminStress ====
test_that("GarminStress (new_tests)", {
  db_test_new("GarminStress", function(d, db) {
    expect_equal(nrow(d), 2)
    expect_true(all(d$participant_id == "12345"))
    expect_true(all(d$date == "2025-12-16"))
    expect_true(all(d$mac_address == "00"))
    expect_true(all(d$status == "valid"))
    expect_equal(d$time[1], "16:30:51.566999")
    expect_equal(d$time[2], "16:31:51.566999")
    expect_equal(d$stress[1], 40L)
    expect_equal(d$stress[2], 23L)
  })
})

### GarminWristStatus ====
test_that("GarminWristStatus (new_tests)", {
  db_test_new("GarminWristStatus", function(d, db) {
    expect_equal(nrow(d), 1)
    expect_equal(d$participant_id, "12345")
    expect_equal(d$date, "2025-12-16")
    expect_equal(d$mac_address, "00")
    expect_equal(d$status, "ON_WRIST")
    expect_equal(d$time, "16:53:18.566999")
  })
})

### GarminZeroCrossing ====
test_that("GarminZeroCrossing (new_tests)", {
  db_test_new("GarminZeroCrossing", function(d, db) {
    # Should be 0 rows as zeroCrossing count is 0 in entryCounts
    expect_equal(nrow(d), 0)
  })
})

### Heartbeat ====
test_that("Heartbeat (new_tests)", {
  db_test_new("Heartbeat", function(d, db) {
    expect_equal(nrow(d), 1)
    expect_equal(d$participant_id, "12345")
    expect_equal(d$date, "2025-12-16")
    expect_equal(d$time, "12:45:55.597422")
    expect_true(is.na(d$measurement_id))
    expect_equal(d$period, 1L)
    expect_equal(d$device_type, "dk.cachet.carp.common.application.devices.GarminDevice")
    expect_equal(d$device_role_name, "garminSmartwatch")
  })
})

### Light ====
test_that("Light (new_tests)", {
  db_test_new("Light", function(d, db) {
    expect_equal(nrow(d), 1)
    expect_equal(d$participant_id, "12345")
    expect_equal(d$date, "2025-12-16")
    expect_equal(d$time, "12:39:53.632197")
    expect_equal(d$end_time, "2025-12-16 12:40:04.382454")
    expect_true(is.na(d$measurement_id))
    expect_equal(d$mean_lux, 42)
    expect_equal(d$std_lux, 13)
    expect_equal(d$min_lux, 112)
    expect_equal(d$max_lux, 1)
  })
})

### Location ====
test_that("Location (new_tests)", {
  # Note: verticalAccuracy, elapsedRealtimeNanos, and ElapsedRealtimeUncertantyNanos are not
  # implemented yet.
  db_test_new("Location", function(d, db) {
    expect_equal(nrow(d), 1)
    expect_equal(d$participant_id, "12345")
    expect_equal(d$date, "2025-12-16")
    expect_equal(d$time, "12:38:28.814321")
    expect_true(is.na(d$measurement_id))
    expect_equal(d$latitude, 50.8771, tolerance = 1e-6)
    expect_equal(d$longitude, 4.7071, tolerance = 1e-6)
    expect_equal(d$altitude, 76.9304, tolerance = 1e-3)
    expect_equal(d$accuracy, 87.8121, tolerance = 1e-3)
    expect_equal(d$vertical_accuracy, 1.2556, tolerance = 1e-3)
    expect_equal(d$speed, 0.0023, tolerance = 1e-4)
    expect_equal(d$speed_accuracy, 1.5604, tolerance = 1e-3)
    expect_equal(d$heading, -0.0065, tolerance = 1e-4)
    expect_equal(d$heading_accuracy, 45.5136, tolerance = 1e-3)
    expect_equal(d$is_mock, 0L)
  })
})

### Memory ====
test_that("Memory (new_tests)", {
  db_test_new("Memory", function(d, db) {
    expect_equal(nrow(d), 1)
    expect_equal(d$participant_id, "12345")
    expect_equal(d$date, "2025-12-16")
    expect_equal(d$time, "12:39:53.725243")
    expect_true(is.na(d$measurement_id))
    expect_equal(d$free_physical_memory, 459444220L)
    expect_equal(d$free_virtual_memory, 3978772477)
  })
})

### mpathinfo ===
test_that("mpathinfo (new_tests)", {
  db_test_new("Activity", function(d, db) {
    expect_true(!all(is.na(d$participant_id)))
    expect_equal(get_participants(db)$participant_id, "12345")
    expect_equal(get_studies(db)$study_id, c("foo", "DemoStudy"))
    expect_true("new_tests.json" %in% get_processed_files(db)$file_name)
  })
})

### Pedometer ====
test_that("Pedometer (new_tests)", {
  db_test_new("Pedometer", function(d, db) {
    expect_equal(nrow(d), 1)
    expect_equal(d$participant_id, "12345")
    expect_equal(d$date, "2025-12-16")
    expect_equal(d$time, "12:37:52.413745")
    expect_true(is.na(d$measurement_id))
    expect_equal(d$step_count, 18987L)
  })
})

### Screen ====
test_that("Screen (new_tests)", {
  db_test_new("Screen", function(d, db) {
    expect_equal(nrow(d), 1)
    expect_equal(d$participant_id, "12345")
    expect_equal(d$date, "2025-12-16")
    expect_equal(d$time, "12:42:38.964548")
    expect_true(is.na(d$measurement_id))
    expect_equal(d$screen_event, "SCREEN_OFF")
  })
})

### Timezone ====
test_that("Timezone (new_tests)", {
  db_test_new("Timezone", function(d, db) {
    expect_equal(nrow(d), 1)
    expect_equal(d$participant_id, "12345")
    expect_equal(d$date, "2025-12-16")
    expect_equal(d$time, "12:39:53.629864")
    expect_true(is.na(d$measurement_id))
    expect_equal(d$timezone, "Europe/Brussels")
  })
})

### Weather ====
test_that("Weather (new_tests)", {
  db_test_new("Weather", function(d, db) {
    expect_equal(nrow(d), 1)
    expect_equal(d$participant_id, "12345")
    expect_equal(d$date, "2025-12-16")
    expect_equal(d$time, "12:10:07.102776")
    expect_true(is.na(d$measurement_id))
    expect_equal(d$country, "BE")
    expect_equal(d$area_name, "Arrondissement Leuven")
    expect_equal(d$weather_main, "Clouds")
    expect_equal(d$weather_description, "broken clouds")
    expect_equal(d$sunrise, "2025-12-16T11:49:12")
    expect_equal(d$sunset, "2025-12-16T19:46:33")
    expect_equal(d$latitude, 50.875, tolerance = 1e-3)
    expect_equal(d$longitude, 4.7051, tolerance = 1e-4)
    expect_equal(d$pressure, 938.0049, tolerance = 1e-3)
    expect_equal(d$wind_speed, 4.9169, tolerance = 1e-3)
    expect_equal(d$wind_degree, 173.3675, tolerance = 1e-3)
    expect_equal(d$humidity, 59.9144, tolerance = 1e-3)
    expect_equal(d$cloudiness, 73.6322, tolerance = 1e-3)
    expect_equal(d$temperature, 12.4227, tolerance = 1e-3)
    expect_equal(d$temp_min, 11.8243, tolerance = 1e-3)
    expect_equal(d$temp_max, 14.1581, tolerance = 1e-3)
  })
})

### Wifi ====
test_that("Wifi (new_tests)", {
  db_test_new("Wifi", function(d, db) {
    expect_equal(nrow(d), 1)
    expect_equal(d$participant_id, "12345")
    expect_equal(d$date, "2025-12-16")
    expect_equal(d$time, "12:39:53.74816")
    expect_true(is.na(d$measurement_id))
    expect_equal(d$ip, "10.12.34.56")
    # Absent columns should be NA
    expect_true(is.na(d$ssid))
    expect_true(is.na(d$bssid))
  })
})
