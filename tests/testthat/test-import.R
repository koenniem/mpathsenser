# Tests for functions.R

test_that("import", {
  # Path to test files
  path <- system.file("testdata", package = "mpathsenser")

  # Create db
  filename <- tempfile("test", fileext = ".db")
  db <- create_db(NULL, filename)

  # Import the data
  expect_message(
    import(
      path = path,
      db = db,
      recursive = FALSE,
      .progress = FALSE
    ),
    "All files were successfully written to the database."
  )

  # Test whether no new files need to be processed
  expect_message(
    import(
      path = path,
      db = db,
      recursive = FALSE,
      .progress = FALSE
    ),
    "No new files to process."
  )

  # Test non-existing path
  expect_error(
    import(path = tempfile(), db = db),
    "Directory .+ does not exist."
  )
  temp <- tempfile()
  dir.create(temp)
  expect_error(
    import(db = db, path = temp),
    "^.*Can't find any JSON files in .+$"
  )
  unlink(temp, recursive = TRUE)
  dbDisconnect(db)
  file.remove(filename)

  # Test deprecated arguments
  filename <- tempfile("test2", fileext = ".db")
  db2 <- create_db(NULL, filename)
  warnings_log <- capture_warnings(import(
    path = path,
    db = db2,
    .progress = FALSE,
    recursive = TRUE # Includes broken files
  ))
  expect_match(
    warnings_log,
    "Invalid JSON format in file .*broken\\d\\.json",
    all = FALSE
  )
  expect_match(
    warnings_log,
    "Some files could not be written to the database.",
    all = FALSE
  )
  dbDisconnect(db2)
  file.remove(filename)

  # Test again with debug, just to see that it works
  # Path to test files
  path <- system.file("testdata", package = "mpathsenser")

  # Create db
  filename <- tempfile("test", fileext = ".db")
  db <- create_db(NULL, filename)

  # Import the data
  debug_msgs <- capture_messages(
    import(
      path = path,
      db = db,
      recursive = FALSE,
      debug = TRUE,
      .progress = FALSE
    )
  )
  expect_match(
    debug_msgs,
    "^.*Found \\d files to process..*$",
    all = FALSE
  )
  expect_match(
    debug_msgs,
    "^.*Found 0 duplicate files\\. Continuing with \\d files\\..$",
    all = FALSE
  )
  expect_match(
    debug_msgs,
    "^.*Read \\d JSON files\\..$",
    all = FALSE
  )
  expect_match(
    debug_msgs,
    "^.*Cleaned \\d files\\..$",
    all = FALSE
  )
  expect_match(
    debug_msgs,
    "^.*Extracted sensor data from \\d files\\..$",
    all = FALSE
  )
  expect_match(
    debug_msgs,
    "^.*Writing \\d* tables\\..$",
    all = FALSE
  )
  expect_match(
    debug_msgs,
    "All files were successfully written to the database.",
    all = FALSE
  )
  dbDisconnect(db)
  file.remove(filename)
})

test_that(".import_read_json", {
  path <- system.file("testdata", package = "mpathsenser")

  expect_type(
    .import_read_json(path, "test.json"),
    "list"
  )
  expect_type(
    .import_read_json(NULL, file.path(path, "test.json")),
    "list"
  )

  expect_warning(
    .import_read_json(path, "foo"),
    "foo does not exist."
  )
  expect_equal(
    suppressWarnings(.import_read_json(path, "foo")),
    NA
  )

  # Empty file
  expect_equal(
    suppressWarnings(.import_read_json(path, "empty.json")),
    NA
  )
  tempfile <- tempfile(fileext = ".json")
  file.create(tempfile)
  expect_equal(.import_read_json(NULL, tempfile), NA)
  unlink(tempfile)

  path <- system.file("testdata", "broken", package = "mpathsenser")
  expect_warning(
    .import_read_json(path, "broken1.json"),
    paste0("Invalid JSON format in file .*broken1.json.")
  )
  expect_equal(
    suppressWarnings(.import_read_json(path, "broken1.json")),
    NULL
  )
})

test_that("safe_extract", {
  data <- list(list(list(a = "a", b = "b", c = NULL, d = NA)))
  expect_equal(safe_extract(data, "a"), "a")
  expect_equal(safe_extract(data, "b"), "b")
  expect_equal(safe_extract(data, "c"), NA)
  expect_equal(safe_extract(data, "d"), NA)
  expect_equal(safe_extract(data, "e"), NA)

  data <- list(
    list(
      list(a = "a", b = "b", c = "c", d = NA, e = NULL, f = NULL)
    ),
    list(
      list(a = "a", b = NA, c = NULL, d = NA, e = NA, f = NULL)
    )
  )
  expect_equal(safe_extract(data, "a"), c("a", "a"))
  expect_equal(safe_extract(data, "b"), c("b", NA))
  expect_equal(safe_extract(data, "c"), c("c", NA))
  expect_equal(safe_extract(data, "d"), c(NA, NA))
  expect_equal(safe_extract(data, "e"), c(NA, NA))
  expect_equal(safe_extract(data, "f"), c(NA, NA))
  expect_equal(safe_extract(data, "g"), c(NA, NA))
})

test_that(".import_clean", {
  data <- list(
    list(
      header = list(
        study_id = "test-study",
        device_role_name = "masterphone",
        trigger_id = "1",
        user_id = "12345",
        start_time = "2021-11-14T14:01:00.000000Z",
        time_zone_name = "CET",
        data_format = list(
          namespace = "dk.cachet.carp",
          name = "accelerometer"
        )
      ),
      body = list()
    ),
    list(
      header = list(
        study_id = "test-study",
        device_role_name = "masterphone",
        trigger_id = "1",
        user_id = "12345",
        start_time = "2021-11-14T14:01:00.000000Z",
        time_zone_name = "CET",
        data_format = list(
          namespace = "dk.cachet.carp",
          name = "accelerometer"
        )
      ),
      body = list()
    )
  )

  expect_no_error(.import_clean(data, "accelerometer"))
  expect_equal(nrow(.import_clean(data, "accelerometer")), 2)

  # Set the first instance of study_id to NULL
  data[[1]][[1]]$study_id <- NULL
  expect_no_error(.import_clean(data, "accelerometer"))
  expect_equal(nrow(.import_clean(data, "accelerometer")), 2)
  # Interesting bug when using unlist in safe_extract: NULLs are implicitly dropped, so if only one
  # value is left, it is recycled in the rest of the data frame. Hence doing this test in two steps.
  expect_equal(
    .import_clean(data, "accelerometer")$study_id,
    c(NA, "test-study")
  )
  data[[2]][[1]]$study_id <- NULL
  expect_no_error(.import_clean(data, "accelerometer"))
  expect_equal(nrow(.import_clean(data, "accelerometer")), 2)
  expect_equal(.import_clean(data, "accelerometer")$study_id, c(NA, NA))

  data[[1]][[1]]$user_id <- NULL
  expect_no_error(.import_clean(data, "accelerometer"))
  expect_equal(nrow(.import_clean(data, "accelerometer")), 1)
  data[[2]][[1]]$user_id <- NULL
  expect_error(.import_clean(data, "accelerometer"), NA)
  expect_equal(nrow(.import_clean(data, "accelerometer")), 0)
})

test_that(".import_clean_new", {
  data <- list(
    list(
      sensorStartTime = 1.705944e+15,
      data = list(
        `__type` = "dk.cachet.carp.wifi",
        ip = "192.168.1"
      )
    ),
    list(
      sensorStartTime = 1.705945e+15,
      sensorEndTime = 1.705945e+15,
      data = list(
        `__type` = "dk.cachet.carp.ambientLight",
        meanLux = 123
      )
    )
  )

  file_name <- "123_study_456_m_Path_sense_2021-11-14_14:01:00.000000.json"

  true <- tibble(
    study_id = "study",
    participant_id = "456",
    data_format = "cams 1.0.0",
    start_time = as.character(
      as.POSIXct(
        c(1.705944e+15, 1.705945e+15) / 1e6,
        tz = "UTC",
        origin = "1970-01-01"
      )
    ),
    end_time = as.character(
      as.POSIXct(c(NA, 1.705945e+15) / 1e6, tz = "UTC", origin = "1970-01-01")
    ),
    sensor = c("wifi", "ambientLight"),
    data = list(
      list(
        ip = "192.168.1"
      ),
      list(
        meanLux = 123
      )
    )
  )

  expect_equal(.import_clean_new(data, file_name), true)
})

test_that(".import_map_sensor_names", {
  expect_equal(
    .import_map_sensor_names("accelerationfeatures"),
    "Accelerometer"
  )

  # Non-existing sensor names are unchanged
  expect_equal(
    .import_map_sensor_names("Foo"),
    "Foo"
  )
})

test_that(".import_is_duplicate", {
  db <- create_db(NULL, tempfile())

  data <- data.frame(
    study_id = "test_study",
    data_format = "carp",
    participant_id = c("12345", "12345", "23456", "23456"),
    file_name = c(
      "12345/test1.json",
      "12345/test2.json",
      "23456/test1.json",
      "23456/test2.json"
    )
  )
  add_study(db, study_id = data$study_id, data_format = data$data_format)
  add_participant(
    db,
    participant_id = data$participant_id,
    study_id = data$study_id
  )
  add_processed_files(
    db,
    file_name = data$file_name,
    study_id = data$study_id,
    participant_id = data$participant_id
  )

  expect_equal(.import_is_duplicate(db, data), rep(TRUE, 4))

  data2 <- data.frame(
    study_id = c("test_study", "test_study", "foo-study", "foo-study"),
    data_format = c("carp", "carp", "bar", "bar"),
    participant_id = c("12345", "23456", "34567", "34567"),
    file_name = c(
      "12345/test3.json",
      "23456/test3.json",
      "34567/test1.json",
      "34567/test2.json"
    )
  )
  data2 <- rbind(data[c(1, 2), ], data2)

  expect_equal(
    .import_is_duplicate(db, data2),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )

  expect_equal(.import_is_duplicate(db, data.frame()), NA)
  expect_equal(.import_is_duplicate(db, list()), NA)
  expect_equal(.import_is_duplicate(db, NULL), NA)

  # Clean up
  dbDisconnect(db)
  unlink(db@dbname)
})

test_that(".import_extract_sensor_data", {
  data <- tibble::tibble(
    body = list(
      list(
        body = list(
          id = "5d0ac8d0-777c-11eb-bf47-ed3b61db1e5e",
          timestamp = "2021-02-25T15:15:58.557282Z",
          data = list(
            list(
              timestamp = "2021-02-25T15:15:58.557282Z",
              xm = NA,
              ym = NA,
              zm = NA,
              xms = NA,
              yms = NA,
              zms = NA,
              n = NA
            ),
            list(
              timestamp = "2021-02-25T15:15:58.557282Z",
              xm = NA,
              ym = NA,
              zm = NA,
              xms = NA,
              yms = NA,
              zms = NA,
              n = NA
            )
          )
        )
      ),
      list(
        body = list(
          id = "5d0ac8d0-777c-11eb-bf47-ed3b61db1e5f",
          timestamp = "2021-02-25T15:15:58.557282Z",
          data = list(
            list(
              timestamp = "2021-02-25T15:15:58.557282Z",
              xm = NA,
              ym = NA,
              zm = NA,
              xms = NA,
              yms = NA,
              zms = NA,
              n = 10
            ),
            list(
              timestamp = "2021-02-25T15:15:58.557282Z",
              xm = NA,
              ym = NA,
              zm = NA,
              xms = NA,
              yms = NA,
              zms = NA,
              n = NA
            )
          )
        )
      )
    ),
    study_id = "test-study",
    participant_id = "12345",
    start_time = "2021-02-25T15:15:58.557282Z",
    data_format = "carp",
    sensor = "accelerometer"
  )

  expect_equal(
    .import_extract_sensor_data(data)$Accelerometer$measurement_id,
    c(
      "5d0ac8d0-777c-11eb-bf47-ed3b61db1e5e_1",
      "5d0ac8d0-777c-11eb-bf47-ed3b61db1e5e_2",
      "5d0ac8d0-777c-11eb-bf47-ed3b61db1e5f_1",
      "5d0ac8d0-777c-11eb-bf47-ed3b61db1e5f_2"
    )
  )

  # Test sensor function that provides a warning
  data$sensor <- c("Accelerometer", "Keyboard")
  expect_warning(
    .import_extract_sensor_data(data),
    "Function for implementing keyboard data currently not implemented."
  )

  expect_equal(
    suppressWarnings(.import_extract_sensor_data(data)),
    list(
      Accelerometer = data.frame(
        measurement_id = c(
          "5d0ac8d0-777c-11eb-bf47-ed3b61db1e5e_1",
          "5d0ac8d0-777c-11eb-bf47-ed3b61db1e5e_2"
        ),
        participant_id = "12345",
        date = "2021-02-25",
        time = "15:15:58.557",
        n = NA,
        x_mean = NA,
        y_mean = NA,
        z_mean = NA,
        x_energy = NA,
        y_energy = NA,
        z_energy = NA
      ),
      Keyboard = NULL
    )
  )

  # Test function that provides an error
  data$sensor <- "Accelerometer"
  data2 <- data
  data2$body <- list(list(foo = "bar"), list(foo = "bar"))
  expect_equal(
    .import_extract_sensor_data(data2),
    NA
  )

  data$sensor[1] <- "unknown"
  expect_equal(
    .import_extract_sensor_data(data)$Accelerometer$measurement_id,
    c(
      "5d0ac8d0-777c-11eb-bf47-ed3b61db1e5f_1",
      "5d0ac8d0-777c-11eb-bf47-ed3b61db1e5f_2"
    )
  )

  data$sensor[1] <- "Foo"
  expect_warning(
    .import_extract_sensor_data(data),
    "Sensor 'Foo' is not supported by this package."
  )
  expect_equal(
    names(suppressWarnings(.import_extract_sensor_data(data))),
    "Accelerometer"
  )

  data$sensor[1] <- "Gyroscope"
  expect_equal(
    names(.import_extract_sensor_data(data, sensors = "Accelerometer")),
    "Accelerometer"
  )
  expect_equal(
    names(.import_extract_sensor_data(data, sensors = "Gyroscope")),
    "Gyroscope"
  )

  data$sensor <- c(NA, NA)
  expect_equal(
    .import_extract_sensor_data(data),
    structure(list(), names = character(0))
  )

  data$sensor <- "Accelerometer"
  data$body <- list(list(), list())
  expect_equal(
    .import_extract_sensor_data(data),
    NA
  )
})

test_that(".import_write_to_db", {
  db <- create_db(NULL, tempfile())

  data <- list(
    Pedometer = tibble::tibble(
      measurement_id = "5d0ac8d0-777c-11eb-bf47-ed3b61db1e5e_1",
      participant_id = "12345",
      date = "2021-02-25",
      time = "15:15:58.557",
      step_count = 1
    )
  )
  meta_data <- data.frame(
    participant_id = "12345",
    study_id = "test-study",
    data_format = "carp",
    file_name = "12345/test1.json"
  )

  expect_equal(.import_write_to_db(db, meta_data, data), 1)
  expect_equal(.import_write_to_db(db, meta_data, data), 0)

  # Test that transactions are rolled back if an error occurs
  data$Pedometer <- rbind(data$Pedometer, data$Pedometer)
  data$Pedometer$measurement_id[[1]] <- "5d0ac8d0-777c-11eb-bf47-ed3b61db1e5e_2"
  data$Pedometer$participant_id[[2]] <- NA
  expect_error(
    .import_write_to_db(db, meta_data, data),
    "NOT NULL constraint failed: Pedometer.participant_id"
  )
  expect_false(
    "5d0ac8d0-777c-11eb-bf47-ed3b61db1e5e_2" %in%
      DBI::dbGetQuery(db, "SELECT measurement_id FROM Accelerometer")[[1]]
  )
  expect_equal(nrow(DBI::dbGetQuery(db, "SELECT * FROM  Pedometer")), 1)

  # Clean up
  dbDisconnect(db)
  unlink(db@dbname)
})

# Old test for save2db, adapted for .import_write_to_db()
test_that(".import_write_to_db writes data correctly", {
  # Create db
  filename <- tempfile("foo", fileext = ".db")
  db <- create_db(NULL, filename)

  db_size <- file.size(filename)

  meta_data <- data.frame(
    study_id = "12345",
    data_format = "mpathsenser",
    participant_id = "12345",
    file_name = "foo.json"
  )

  # Define the data
  data <- data.frame(
    measurement_id = paste0("12345_", 1:1000),
    participant_id = "12345",
    date = "2021-11-14",
    time = paste0("16:40:01.", 1:1000),
    step_count = 1
  )

  # Write to db
  expect_no_error(
    .import_write_to_db(db, meta_data = meta_data, sensor_data = list(Pedometer = data))
  )

  # Check if the file size increased
  db_size2 <- file.size(filename)
  expect_gt(db_size2, db_size)

  # Check the data output
  expect_equal(
    DBI::dbGetQuery(db, "SELECT * FROM Pedometer"),
    data
  )
  expect_equal(
    DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Pedometer")[[1]],
    1000
  )

  # Entry with the same participant_id, date, and time should simply be skipped and give no error
  expect_no_error(
    .import_write_to_db(db, meta_data = meta_data, sensor_data = list(Pedometer = data))
  )
  expect_equal(
    DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Pedometer")[[1]],
    1000
  )

  DBI::dbExecute(db, "VACUUM") # A vacuum to clear the tiny increase by replacement :)
  db_size3 <- file.size(filename)
  expect_equal(db_size2, db_size3)
  expect_equal(
    DBI::dbGetQuery(db, "SELECT * FROM Pedometer"),
    data
  )

  # Now try with part of the data being replicated
  data <- rbind(
    data,
    data.frame(
      measurement_id = paste0("12345_", 500:1500),
      participant_id = "12345",
      date = "2021-11-14",
      time = paste0("16:40:01.", 500:1500),
      step_count = 1
    )
  )

  expect_no_error(
    .import_write_to_db(db = db, meta_data = meta_data, sensor_data = list(Pedometer = data))
  )
  db_size4 <- file.size(filename)
  expect_gt(db_size4, db_size3)
  expect_equal(
    DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Pedometer")[[1]],
    1500L
  )
  expect_equal(
    DBI::dbGetQuery(db, "SELECT * FROM Pedometer"),
    distinct(data)
  )

  # Cleanup
  dbDisconnect(db)
  file.remove(filename)
})

test_that(".import_meta_data_from_mpathinfo", {
  test_data <- list(
    list(
      sensorStartTime = 1765889440388567,
      data = list(
        `__type` = "dk.cachet.carp.accelerometer",
        x = 0.0,
        y = 0.0,
        z = 9.81
      )
    ),
    list(
      sensorStartTime = 1765889440388567,
      data = list(
        `__type` = "dk.cachet.carp.activity",
        activityType = "walking",
        confidence = 0.9
      )
    ),
    list(
      sensorStartTime = 1765889440388567,
      data = list(
        `__type` = "dk.cachet.carp.mpathinfo",
        connectionId = 23456,
        accountCode = "abcd1",
        studyName = "DemoStudy",
        senseVersion = 6
      )
    )
  )

  res <- .import_meta_data_from_mpathinfo(test_data, file_name = "foo.json")
  expect_s3_class(res, "tbl_df")
  expect_equal(ncol(res), 3)
  expect_equal(
    res,
    tibble::tibble(
      study_id = "DemoStudy",
      participant_id = "23456",
      file_name = "foo.json"
    )
  )

  # Check double entry warning
  test_data <- append(test_data, test_data[3])
  expect_warning(
    res <- .import_meta_data_from_mpathinfo(test_data, file_name = "foo.json"),
    "^.*Multiple `mpathinfo` entries found in file:.+$"
  )

  # Test fallback
  test_data[[4]] <- NULL
  test_data[[3]] <- NULL

  res <- .import_meta_data_from_mpathinfo(test_data, file_name = "foo.json")
  expect_equal(
    res,
    tibble::tibble(
      study_id = "-1",
      participant_id = "N/A",
      file_name = "foo.json"
    )
  )
})

test_that(".import_meta_data_from_file_name correctly extracts metadata", {
  file_names <- c(
    "1234_studyA_participant1_m_Path_sense_2023-01-01_12-34-56.json",
    "5678_studyB_participant2_m_Path_sense_2023-01-02_01-23-45.json",
    "9012_studyC_participant3_m_Path_sense_2023-01-03_23-59-59.json"
  )

  result <- .import_meta_data_from_file_name(file_names)

  # Check structure and values
  expect_s3_class(result, "tbl_df")
  expect_equal(ncol(result), 3)
  expect_named(result, c("study_id", "participant_id", "file_name"))

  # Check extracted metadata
  expect_equal(result$study_id, c("studyA", "studyB", "studyC"))
  expect_equal(
    result$participant_id,
    c("participant1", "participant2", "participant3")
  )
  expect_equal(result$file_name, file_names)
})

test_that(".import_meta_data_from_file_name handles missing participant ID or study ID", {
  file_names <- c(
    "1234__participant1_m_Path_sense_2023-01-01_12-34-56.json",
    "5678_studyB__m_Path_sense_2023-01-02_01-23-45.json"
  )

  result <- .import_meta_data_from_file_name(file_names)

  # Check structure and values
  expect_s3_class(result, "tbl_df")
  expect_equal(ncol(result), 3)
  expect_named(result, c("study_id", "participant_id", "file_name"))

  # Check missing metadata handling
  expect_equal(result$study_id, c("", "studyB"))
  expect_equal(result$participant_id, c("participant1", ""))
  expect_equal(result$file_name, file_names)
})

test_that(".import_meta_data_from_file_name handles completely incorrect file names", {
  file_names <- c(NA, "foo", "foo_bar")

  res <- .import_meta_data_from_file_name(file_names)

  expect_s3_class(res, "tbl_df")
  expect_equal(ncol(res), 3)
  expect_named(res, c("study_id", "participant_id", "file_name"))

  expect_equal(res$study_id, rep("-1", 3))
  expect_equal(res$participant_id, rep("N/A", 3))
  expect_equal(res$file_name, c(NA, "foo", "foo_bar"))
})

# Tests for .import_extract_garmin_logs

test_that(".import_extract_garmin_logs extracts all Garmin sensor types", {
  # Create mock data with garminalllogsdata
  mock_data <- tibble(
    participant_id = "12345",
    study_id = "foo",
    date = "2025-12-16",
    time = "16:30:00.000000",
    start_time = "2025-12-16T16:30:00Z",
    sensor = c("Device", "garminalllogsdata", "Activity"),
    data = list(
      list(name = "Device Data"),
      list(
        accelerometer = list(x = 1, y = 2, z = 3),
        bbi = list(value = 763),
        enhancedBbi = list(status = "ok"),
        gyroscope = list(x = 0.1, y = 0.2),
        heartRate = list(bpm = 76),
        actigraphy1 = list(energy = 100),
        actigraphy2 = list(energy = 110),
        actigraphy3 = list(energy = 120),
        respiration = list(rate = 15),
        skinTemperature = list(temp = 37.5),
        spo2 = list(value = 98),
        steps = list(count = 42),
        stress = list(level = 40),
        wristStatus = list(status = "ON_WRIST"),
        zeroCrossing = list(value = 5),
        fromTime = 1765902600000,
        toTime = 1765902700000,
        entryCounts = list(
          heartRate = 26,
          stress = 2,
          steps = 5,
          bbi = 162,
          enhancedBbi = 160,
          respiration = 0,
          spo2 = 0,
          accelerometer = 2200,
          gyroscope = 0,
          skinTemperature = 0,
          actigraphy1 = 2,
          actigraphy2 = 2,
          actigraphy3 = 2
        )
      ),
      list(name = "Activity Data")
    )
  )

  result <- .import_extract_garmin_logs(mock_data)

  # Check that result is a tibble
  expect_s3_class(result, "tbl_df")

  # Check that non-Garmin sensors are preserved
  expect_true(any(result$sensor == "Device"))
  expect_true(any(result$sensor == "Activity"))

  # Check that all Garmin sensor types are present
  garmin_sensors <- c(
    "GarminAccelerometer",
    "GarminBBI",
    "GarminEnhancedBBI",
    "GarminGyroscope",
    "GarminHeartRate",
    "GarminActigraphy",
    "GarminRespiration",
    "GarminSkinTemperature",
    "GarminSPO2",
    "GarminSteps",
    "GarminStress",
    "GarminWristStatus",
    "GarminZeroCrossing",
    "GarminMeta"
  )

  for (sensor in garmin_sensors) {
    expect_true(
      any(result$sensor == sensor),
      label = paste("Garmin sensor", sensor, "should be present")
    )
  }
})

test_that(".import_extract_garmin_logs handles missing Garmin data gracefully", {
  # Create mock data with garminalllogsdata but some null values
  mock_data <- tibble(
    participant_id = "12345",
    study_id = "foo",
    date = "2025-12-16",
    time = "16:30:00.000000",
    start_time = "2025-12-16T16:30:00Z",
    sensor = c("Device", "garminalllogsdata"),
    data = list(
      list(name = "Device Data"),
      list(
        accelerometer = NULL,
        bbi = list(value = 763),
        enhancedBbi = NULL,
        gyroscope = NULL,
        heartRate = list(bpm = 76),
        actigraphy1 = NULL,
        actigraphy2 = NULL,
        actigraphy3 = NULL,
        respiration = NULL,
        skinTemperature = NULL,
        spo2 = NULL,
        steps = list(count = 42),
        stress = NULL,
        wristStatus = list(status = "ON_WRIST"),
        zeroCrossing = NULL,
        fromTime = 1765902600000,
        toTime = 1765902700000,
        entryCounts = list(
          heartRate = 26,
          stress = 0,
          steps = 1,
          bbi = 162,
          enhancedBbi = 0,
          respiration = 0,
          spo2 = 0,
          accelerometer = 0,
          gyroscope = 0,
          skinTemperature = 0,
          actigraphy1 = 0,
          actigraphy2 = 0,
          actigraphy3 = 0
        )
      )
    )
  )

  result <- .import_extract_garmin_logs(mock_data)

  # Non-Garmin sensors should be preserved
  expect_true(any(result$sensor == "Device"))

  # Garmin sensors with data should be present
  expect_true(any(result$sensor == "GarminBBI"))
  expect_true(any(result$sensor == "GarminHeartRate"))
  expect_true(any(result$sensor == "GarminSteps"))
  expect_true(any(result$sensor == "GarminWristStatus"))
  expect_true(any(result$sensor == "GarminMeta"))

  # Garmin sensors with NULL data should not be present
  expect_false(any(result$sensor == "GarminAccelerometer"))
  expect_false(any(result$sensor == "GarminEnhancedBBI"))
  expect_false(any(result$sensor == "GarminGyroscope"))
  expect_false(any(result$sensor == "GarminRespiration"))
  expect_false(any(result$sensor == "GarminSkinTemperature"))
  expect_false(any(result$sensor == "GarminSPO2"))
  expect_false(any(result$sensor == "GarminZeroCrossing"))
})

test_that(".import_extract_garmin_logs removes garminalllogsdata sensor", {
  # Create mock data with garminalllogsdata
  mock_data <- tibble(
    participant_id = "12345",
    study_id = "foo",
    date = "2025-12-16",
    time = "16:30:00.000000",
    start_time = "2025-12-16T16:30:00Z",
    sensor = c("Device", "garminalllogsdata"),
    data = list(
      list(name = "Device Data"),
      list(
        accelerometer = list(x = 1),
        bbi = NULL,
        enhancedBbi = NULL,
        gyroscope = NULL,
        heartRate = NULL,
        actigraphy1 = NULL,
        actigraphy2 = NULL,
        actigraphy3 = NULL,
        respiration = NULL,
        skinTemperature = NULL,
        spo2 = NULL,
        steps = NULL,
        stress = NULL,
        wristStatus = NULL,
        zeroCrossing = NULL,
        fromTime = 1765902600000,
        toTime = 1765902700000,
        entryCounts = list(accelerometer = 10)
      )
    )
  )

  result <- .import_extract_garmin_logs(mock_data)

  # The garminalllogsdata sensor should be removed
  expect_false(any(result$sensor == "garminalllogsdata"))

  # Other sensors should remain
  expect_true(any(result$sensor == "Device"))
})

test_that(".import_extract_garmin_logs unpacks actigraphy correctly", {
  # Create mock data with actigraphy data
  mock_data <- tibble(
    participant_id = "12345",
    study_id = "foo",
    date = "2025-12-16",
    time = "16:30:00.000000",
    start_time = "2025-12-16T16:30:00Z",
    sensor = "garminalllogsdata",
    data = list(
      list(
        accelerometer = NULL,
        bbi = NULL,
        enhancedBbi = NULL,
        gyroscope = NULL,
        heartRate = NULL,
        actigraphy1 = list(
          instance = "ACTIGRAPHY_1",
          energy = 100,
          threshold = 50
        ),
        actigraphy2 = list(
          instance = "ACTIGRAPHY_2",
          energy = 110,
          threshold = 55
        ),
        actigraphy3 = list(
          instance = "ACTIGRAPHY_3",
          energy = 120,
          threshold = 60
        ),
        respiration = NULL,
        skinTemperature = NULL,
        spo2 = NULL,
        steps = NULL,
        stress = NULL,
        wristStatus = NULL,
        zeroCrossing = NULL,
        fromTime = 1765902600000,
        toTime = 1765902700000,
        entryCounts = list(
          actigraphy1 = 2,
          actigraphy2 = 2,
          actigraphy3 = 2
        )
      )
    )
  )

  result <- .import_extract_garmin_logs(mock_data)

  # Check that actigraphy data is present and unnested
  actigraphy_rows <- result |>
    filter(.data$sensor == "GarminActigraphy")

  # After unnesting, should have multiple rows for actigraphy
  expect_true(nrow(actigraphy_rows) > 0)

  # Check that the data column is no longer a list after unnesting
  expect_false(inherits(actigraphy_rows$data[[1]], "list"))
})

test_that(".import_extract_garmin_logs preserves participant metadata", {
  # Create mock data
  mock_data <- tibble(
    participant_id = "test_participant",
    study_id = "test_study",
    date = "2025-12-16",
    time = "16:30:00.000000",
    start_time = "2025-12-16T16:30:00Z",
    sensor = "garminalllogsdata",
    data = list(
      list(
        accelerometer = list(x = 1),
        bbi = NULL,
        enhancedBbi = NULL,
        gyroscope = NULL,
        heartRate = NULL,
        actigraphy1 = NULL,
        actigraphy2 = NULL,
        actigraphy3 = NULL,
        respiration = NULL,
        skinTemperature = NULL,
        spo2 = NULL,
        steps = NULL,
        stress = NULL,
        wristStatus = NULL,
        zeroCrossing = NULL,
        fromTime = 1765902600000,
        toTime = 1765902700000,
        entryCounts = list(accelerometer = 10)
      )
    )
  )

  result <- .import_extract_garmin_logs(mock_data)

  # Check that metadata is preserved for all Garmin sensors
  garmin_rows <- result |>
    filter(grepl("^Garmin", .data$sensor))

  expect_true(all(garmin_rows$participant_id == "test_participant"))
  expect_true(all(garmin_rows$study_id == "test_study"))
  expect_true(all(garmin_rows$date == "2025-12-16"))
})

test_that(".import_extract_garmin_logs handles multiple rows correctly", {
  # Create mock data with multiple rows
  mock_data <- tibble(
    participant_id = c("user1", "user2"),
    study_id = c("study1", "study2"),
    date = c("2025-12-16", "2025-12-17"),
    time = c("16:30:00.000000", "17:30:00.000000"),
    start_time = c("2025-12-16T16:30:00Z", "2025-12-17T17:30:00Z"),
    sensor = c("garminalllogsdata", "garminalllogsdata"),
    data = list(
      list(
        accelerometer = list(x = 1, y = 2),
        bbi = NULL,
        enhancedBbi = NULL,
        gyroscope = NULL,
        heartRate = list(bpm = 70),
        actigraphy1 = NULL,
        actigraphy2 = NULL,
        actigraphy3 = NULL,
        respiration = NULL,
        skinTemperature = NULL,
        spo2 = NULL,
        steps = NULL,
        stress = NULL,
        wristStatus = NULL,
        zeroCrossing = NULL,
        fromTime = 1765902600000,
        toTime = 1765902700000,
        entryCounts = list(
          accelerometer = 100,
          heartRate = 10
        )
      ),
      list(
        accelerometer = list(x = 3, y = 4),
        bbi = NULL,
        enhancedBbi = NULL,
        gyroscope = NULL,
        heartRate = list(bpm = 75),
        actigraphy1 = NULL,
        actigraphy2 = NULL,
        actigraphy3 = NULL,
        respiration = NULL,
        skinTemperature = NULL,
        spo2 = NULL,
        steps = NULL,
        stress = NULL,
        wristStatus = NULL,
        zeroCrossing = NULL,
        fromTime = 1765989000000,
        toTime = 1765989100000,
        entryCounts = list(
          accelerometer = 100,
          heartRate = 10
        )
      )
    )
  )

  result <- .import_extract_garmin_logs(mock_data)

  # Check that both users' data are present
  expect_true(any(result$participant_id == "user1"))
  expect_true(any(result$participant_id == "user2"))

  # Check that each user has their respective data
  user1_data <- result |> filter(.data$participant_id == "user1")
  user2_data <- result |> filter(.data$participant_id == "user2")

  expect_true(any(user1_data$sensor == "GarminAccelerometer"))
  expect_true(any(user2_data$sensor == "GarminAccelerometer"))
})

test_that(".import_extract_garmin_logs removes unsupported sensor types with warning", {
  # Create mock data with unsupported Garmin sensor type
  mock_data <- tibble(
    participant_id = "12345",
    study_id = "foo",
    date = "2025-12-16",
    time = "16:30:00.000000",
    start_time = "2025-12-16T16:30:00Z",
    sensor = "garminalllogsdata",
    data = list(
      list(
        accelerometer = list(x = 1),
        bbi = NULL,
        enhancedBbi = NULL,
        gyroscope = NULL,
        heartRate = NULL,
        actigraphy1 = NULL,
        actigraphy2 = NULL,
        actigraphy3 = NULL,
        respiration = NULL,
        skinTemperature = NULL,
        spo2 = NULL,
        steps = NULL,
        stress = NULL,
        wristStatus = NULL,
        zeroCrossing = NULL,
        unsupported_sensor = list(value = 999),
        fromTime = 1765902600000,
        toTime = 1765902700000,
        entryCounts = list(accelerometer = 10)
      )
    )
  )

  expect_warning(
    .import_extract_garmin_logs(mock_data),
    "Garmin data type.*is not supported"
  )
})

test_that(".import_extract_garmin_logs preserves non-garminalllogsdata rows", {
  # Create mock data with multiple sensor types
  mock_data <- tibble(
    participant_id = c("12345", "12345", "12345"),
    study_id = c("foo", "foo", "foo"),
    date = c("2025-12-16", "2025-12-16", "2025-12-16"),
    time = c("16:30:00.000000", "16:31:00.000000", "16:32:00.000000"),
    start_time = c("2025-12-16T16:30:00Z", "2025-12-16T16:31:00Z", "2025-12-16T16:32:00Z"),
    sensor = c("Device", "garminalllogsdata", "Activity"),
    data = list(
      list(manufacturer = "Apple"),
      list(
        accelerometer = list(x = 1),
        bbi = NULL,
        enhancedBbi = NULL,
        gyroscope = NULL,
        heartRate = NULL,
        actigraphy1 = NULL,
        actigraphy2 = NULL,
        actigraphy3 = NULL,
        respiration = NULL,
        skinTemperature = NULL,
        spo2 = NULL,
        steps = NULL,
        stress = NULL,
        wristStatus = NULL,
        zeroCrossing = NULL,
        fromTime = 1765902600000,
        toTime = 1765902700000,
        entryCounts = list(accelerometer = 10)
      ),
      list(type = "WALKING")
    )
  )

  result <- .import_extract_garmin_logs(mock_data)

  # Non-Garmin sensors should be preserved exactly
  device_row <- result |> filter(.data$sensor == "Device")
  activity_row <- result |> filter(.data$sensor == "Activity")

  expect_equal(nrow(device_row), 1)
  expect_equal(nrow(activity_row), 1)
  expect_equal(device_row$time, "16:30:00.000000")
  expect_equal(activity_row$time, "16:32:00.000000")
})
