# Tests for database.R

test_that("sensors-vec", {
  expect_vector(sensors, character(), size = 27)
})

test_that("create_db", {
  filename <- tempfile("create", fileext = ".db")
  db <- create_db(path = NULL, filename)
  dbDisconnect(db)
  expect_true(file.exists(filename))

  # Test merging path and filename
  temp_file <- basename(tempfile())
  expect_error(
    {
      db <- create_db(path = tempdir(), db_name = temp_file)
      dbDisconnect(db)
    },
    NA
  )

  # Test overwrite argument
  expect_error(
    {
      db <- create_db(path = NULL, filename, overwrite = TRUE)
      dbDisconnect(db)
    },
    NA
  )

  expect_error(
    {
      db <- create_db(path = NULL, filename, overwrite = FALSE)
      dbDisconnect(db)
    },
    "Database .+?(?=\\.db)\\.db already exists\\.",
    perl = TRUE
  )

  # Test non-existing path
  expect_error(create_db("foo", "bar"), "Directory .*?(?=foo)foo does not exist\\.", perl = TRUE)

  file.remove(filename)
})

test_that("open_db", {
  fake_db <- tempfile("foo", fileext = ".db")
  expect_error(open_db(NULL, fake_db), "There is no such file")

  # Create a new (non-mpathsenser db)
  db <- dbConnect(duckdb::duckdb(), fake_db)
  dbExecute(db, "CREATE TABLE foo(bar INTEGER, PRIMARY KEY(bar));")
  dbDisconnect(db)
  gc() # Force garbage collection to ensure file handles are released
  expect_error(open_db(NULL, fake_db), "Sorry, this does not appear to be a mpathsenser database.")
  file.remove(fake_db)

  # Test with a fresh test database
  db <- create_test_db()
  db_path <- db@driver@dbdir
  dbDisconnect(db)
  gc() # Force garbage collection to ensure file handles are released

  db <- open_db(NULL, db_path)
  expect_true(dbIsValid(db))
  dbDisconnect(db)
  file.remove(db_path)
})

test_that("copy_db", {
  # Create a test database
  db <- create_test_db()

  filename <- tempfile("copy", fileext = ".db")
  new_db <- create_db(NULL, filename)

  # Invalid sensor
  expect_error(
    copy_db(db, new_db, sensor = "foo"),
    "Sensor\\(s\\) foo not found."
  )

  new_db <- copy_db(db, new_db, sensor = "All")
  expect_equal(get_nrows(db), get_nrows(new_db))
  close_db(new_db)
  file.remove(filename)

  # Create new db and copy to it
  new_db <- create_db(NULL, filename)
  new_db <- copy_db(db, new_db, sensor = "Accelerometer")
  true <- c(2L, rep(0L, 26))
  names(true) <- sensors
  expect_equal(get_nrows(new_db), true)

  cleanup_test_db(db)
  dbDisconnect(new_db)
  file.remove(filename)
})

test_that("close_db", {
  db <- create_test_db()
  expect_error(close_db(db), NA)
  expect_false(dbIsValid(db))
  expect_error(close_db(db), NA) # Invalid db
  rm(db)
  expect_error(close_db(db), NA) # db does not exist
  db <- NULL
  expect_error(close_db(db), NA) # NULL db
})

test_that("index_db", {
  # Create db
  filename <- tempfile("foo", fileext = ".db")
  db <- create_db(NULL, filename)

  expect_error(index_db(db), NA)
  expect_error(index_db(db), "already exists")

  # Cleanup
  dbDisconnect(db)
  file.remove(filename)

  expect_error(
    index_db(db),
    "Database connection `db` is not valid."
  )
})

test_that("vacuum_db", {
  # Create db
  filename <- tempfile("foo", fileext = ".db")
  db <- create_db(NULL, filename)
  expect_error(vacuum_db(db), NA)

  # Cleanup
  dbDisconnect(db)
  file.remove(filename)
})

test_that("add_study", {
  # Create db
  filename <- tempfile("foo", fileext = ".db")
  db <- create_db(NULL, filename)

  data <- data.frame(study_id = "12345", data_format = "mpathsenser")
  expect_equal(add_study(db, data$study_id, data$data_format), 1)

  studies <- DBI::dbGetQuery(db, "SELECT * FROM Study")
  expect_equal(studies, data)
  expect_equal(add_study(db, data$study_id, data$data_format), 0)
  expect_equal(add_study(db, NULL, NULL), 0)

  # Cleanup
  dbDisconnect(db)
  file.remove(filename)
})

test_that("add_participant", {
  # Create db
  filename <- tempfile("foo", fileext = ".db")
  db <- create_db(NULL, filename)

  data <- data.frame(participant_id = "12345", study_id = "12345")
  dbExecute(db, "INSERT INTO Study VALUES('12345', 'mpathsenser')")
  expect_equal(add_participant(db, data$participant_id, data$study_id), 1)
  participants <- DBI::dbGetQuery(db, "SELECT * FROM Participant")
  expect_equal(participants, data)
  expect_equal(add_participant(db, data$participant_id, data$study_id), 0)
  expect_equal(add_participant(db, NULL, NULL), 0)

  # Cleanup
  dbDisconnect(db)
  file.remove(filename)
})

test_that("add_processed_file", {
  # Create db
  filename <- tempfile("foo", fileext = ".db")
  db <- create_db(NULL, filename)

  data <- data.frame(file_name = "12345.json", participant_id = "12345", study_id = "12345")
  dbExecute(db, "INSERT INTO Study VALUES('12345', 'mpathsenser')")
  dbExecute(db, "INSERT INTO Participant VALUES('12345', '12345')")
  expect_equal(add_processed_files(db, data$file_name, data$study_id, data$participant_id), 1)
  files <- DBI::dbGetQuery(db, "SELECT * FROM ProcessedFiles")
  expect_equal(files, data)
  expect_equal(add_processed_files(db, data$file_name, data$study_id, data$participant_id), 0)
  expect_equal(add_processed_files(db, NULL, NULL, NULL), 0)

  # Cleanup
  dbDisconnect(db)
  file.remove(filename)
})

test_that("clear_db", {
  path <- system.file("testdata", package = "mpathsenser")

  # Create db
  filename <- tempfile("foo", fileext = ".db")
  db <- create_db(NULL, filename)

  suppressMessages(import(path, db = db, recursive = FALSE))
  original <- sum(get_nrows(db))
  original <- original + nrow(get_participants(db))
  original <- original + nrow(get_studies(db))
  original <- original + nrow(get_processed_files(db))
  res <- clear_db(db)
  expect_type(res, "double")
  expect_length(res, length(sensors) + 3)
  expect_equal(Reduce(`+`, res), original)
  expect_equal(sum(get_nrows(db)), 0L)

  # Cleanup
  dbDisconnect(db)
  file.remove(filename)
})

test_that("get_processed_files", {
  db <- create_test_db()
  res <- get_processed_files(db)
  true <- data.frame(
    file_name = c("test.json", "empty.json", "new_tests.json"),
    participant_id = c("12345", "N/A", "N/A"),
    study_id = c("test-study", "-1", "-1")
  )
  expect_equal(res, true)
  cleanup_test_db(db)
})

test_that("get_participants", {
  db <- create_test_db()
  res <- get_participants(db)
  res_lazy <- get_participants(db, lazy = TRUE)
  true <- data.frame(
    participant_id = c("12345", "N/A"),
    study_id = c("test-study", "-1")
  )
  expect_equal(res, true)
  expect_s3_class(res_lazy, "tbl_duckdb_connection")
  cleanup_test_db(db)
})

test_that("get_study", {
  db <- create_test_db()
  res <- get_studies(db)
  res_lazy <- get_studies(db, lazy = TRUE)
  true <- data.frame(
    study_id = c("test-study", "-1"),
    data_format = c("carp", NA)
  )
  expect_equal(res, true)
  expect_s3_class(res_lazy, "tbl_duckdb_connection")
  cleanup_test_db(db)
})

test_that("get_nrows", {
  db <- create_test_db()
  expect_vector(get_nrows(db), numeric(), length(sensors))
  cleanup_test_db(db)
})
