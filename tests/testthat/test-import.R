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
      recursive = FALSE
    ),
    "All files were successfully written to the database."
  )

  # Test whether no new files need to be processed
  expect_message(
    import(
      path = path,
      db = db,
      recursive = FALSE
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
    "Can't find JSON files in .+\\."
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
    recursive = TRUE # Includes broken files
  ))
  expect_match(
    warnings_log,
    "Invalid JSON format in file broken\\/broken\\d\\.json",
    all = FALSE
  )
  expect_match(
    warnings_log,
    "Some files could not be written to the database.",
    all = FALSE
  )
  dbDisconnect(db2)
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
    "Invalid JSON format in file broken1.json"
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

  db_path <- db@driver@dbdir
  expect_equal(.import_is_duplicate(db_path, data), rep(TRUE, 4))

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
    .import_is_duplicate(db_path, data2),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )

  expect_equal(.import_is_duplicate(db_path, data.frame()), NA)
  expect_equal(.import_is_duplicate(db_path, list()), NA)
  expect_equal(.import_is_duplicate(db_path, NULL), NA)

  # Clean up
  dbDisconnect(db)
  unlink(db_path)
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
        time = c(
          "2021-02-25T15:15:58.557282Z",
          "2021-02-25T15:15:58.557282Z"
        ),
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
      time = "2021-02-25T15:15:58.557Z",
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
  data$Pedometer$measurement_id[[2]] <- NA
  expect_error(
    .import_write_to_db(db, meta_data, data),
    "NOT NULL constraint failed"
  )
  expect_false(
    "5d0ac8d0-777c-11eb-bf47-ed3b61db1e5e_2" %in%
      DBI::dbGetQuery(db, "SELECT measurement_id FROM Accelerometer")[[1]]
  )
  expect_equal(nrow(DBI::dbGetQuery(db, "SELECT * FROM  Pedometer")), 1)

  # Clean up
  db_path <- db@driver@dbdir
  dbDisconnect(db)
  unlink(db_path)
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
