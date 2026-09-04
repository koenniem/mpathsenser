# Tests for database.R

test_that("sensors-vec", {
  expect_vector(sensors, character(), size = 32)
})

test_that("create_db", {
  filename <- tempfile("create", fileext = ".db")
  db <- create_db(path = NULL, filename)
  dbDisconnect(db)
  expect_true(file.exists(filename))

  # Test merging path and filename
  temp_file <- basename(tempfile())
  expect_no_error(
    {
      db <- create_db(path = tempdir(), db_name = temp_file)
      dbDisconnect(db)
    }
  )

  # Test overwrite argument
  expect_no_error(
    {
      db <- create_db(path = NULL, filename, overwrite = TRUE)
      dbDisconnect(db)
    }
  )

  expect_error(
    {
      db <- create_db(path = NULL, filename, overwrite = FALSE)
      dbDisconnect(db)
    },
    NULL
  )

  # Test non-existing path
  expect_error(create_db("foo", "bar"), NULL)

  file.remove(file.path(tempdir(), temp_file))
  file.remove(filename)
})

test_that("open_db", {
  fake_db <- tempfile("foo", fileext = ".db")
  expect_error(open_db(fake_db), "There is no database at")

  # A directory is not a database file
  dir_d <- tempfile("open_db_dir")
  dir.create(dir_d)
  expect_error(open_db(dir_d), "is a directory, not a database file")
  file.create(file.path(dir_d, "mydb.duckdb"))
  expect_error(open_db(dir_d), "Did you mean")

  # The path can be given as a directory plus a file name, like create_db()
  dir_d2 <- tempfile("open_db_dir2")
  dir.create(dir_d2)
  db0 <- create_db(dir_d2, "mydb.duckdb")
  close_db(db0)
  db0 <- open_db(dir_d2, "mydb.duckdb")
  expect_true(dbIsValid(db0))
  close_db(db0)
  unlink(dir_d2, recursive = TRUE)
  unlink(dir_d, recursive = TRUE)

  # Create a new (non-mpathsenser db)
  db <- dbConnect(duckdb::duckdb(), fake_db)
  dbExecute(db, "CREATE TABLE foo(bar INTEGER, PRIMARY KEY(bar));")
  dbDisconnect(db)
  gc() # Force garbage collection to ensure file handles are released
  expect_error(open_db(fake_db), "does not appear to be an mpathsenser database")
  file.remove(fake_db)

  # Test with a fresh test database
  db <- create_test_db(path = tempfile())
  db_path <- db@driver@dbdir
  dbDisconnect(db)
  closeAllConnections()

  db <- open_db(db_path)
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
    "Sensor `foo` could not be found."
  )

  new_db <- copy_db(db, new_db, sensor = NULL)
  expect_equal(get_nrows(db), get_nrows(new_db))
  close_db(new_db)
  file.remove(filename)

  # Create new db and copy to it
  new_db <- create_db(NULL, filename)
  new_db <- copy_db(db, new_db, sensor = "Accelerometer")
  true <- c(0L, rep(0L, 31))
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
  expect_no_error(close_db(db)) # Invalid db
  rm(db)
  expect_no_error(close_db(db)) # db does not exist
  db <- NULL
  expect_no_error(close_db(db)) # NULL db
})

test_that("optimize_db", {
  # Create db
  filename <- tempfile("foo", fileext = ".db")
  db <- create_test_db(path = filename)

  expect_error(optimize_db(db, sensors = "Activity", .progress = FALSE), NA)
  expect_error(optimize_db(db, sensors = "Error", .progress = FALSE), NA)
  expect_error(optimise_db(db, sensors = "Activity", .progress = FALSE), NA)
  expect_equal(
    DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Activity")[[1]],
    1
  )

  # The rewrite must preserve the schema of the original table: the NOT NULL
  # constraints of the sensor tables survive the reordering
  nullable <- DBI::dbGetQuery(
    db,
    "SELECT is_nullable FROM information_schema.columns
     WHERE table_schema = 'main' AND table_name = 'Activity'
       AND column_name = 'participant_id'"
  )[[1]]
  expect_equal(nullable, "NO")

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

  data <- data.frame(participant_id = 12345, study_id = "12345")
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

  dbExecute(db, "INSERT INTO Study VALUES('12345', 'mpathsenser')")
  dbExecute(db, "INSERT INTO Participant VALUES('12345', '12345')")
  expect_equal(
    add_processed_files(
      db,
      file_name = "12345.json",
      participant_id = "12345",
      file_size_bytes = 10,
      modified_at = as.POSIXct("2025-01-01", tz = "UTC")
    ),
    1
  )
  files <- DBI::dbGetQuery(db, "SELECT file_name, participant_id FROM ProcessedFiles")
  expect_equal(
    files,
    data.frame(file_name = "12345.json", participant_id = 12345)
  )
  # The same name, size, and modification time is a duplicate
  expect_equal(
    add_processed_files(
      db,
      file_name = "12345.json",
      participant_id = "12345",
      file_size_bytes = 10,
      modified_at = as.POSIXct("2025-01-01", tz = "UTC")
    ),
    0
  )
  # A different modification time is a new record
  expect_equal(
    add_processed_files(
      db,
      file_name = "12345.json",
      participant_id = "12345",
      file_size_bytes = 10,
      modified_at = as.POSIXct("2025-01-02", tz = "UTC")
    ),
    1
  )
  expect_equal(
    add_processed_files(
      db,
      file_name = NULL,
      participant_id = NULL
    ),
    0
  )

  # Cleanup
  dbDisconnect(db)
  file.remove(filename)
})

test_that("clear_db", {
  # Create db
  filename <- tempfile("foo", fileext = ".db")
  db <- create_test_db(path = filename)

  original <- sum(get_nrows(db))
  original <- original + nrow(get_participants(db))
  original <- original + nrow(get_studies(db))
  original <- original + nrow(get_processed_files(db))
  res <- clear_db(db)
  expect_type(res, "double")
  expect_length(res, length(sensors) + 3)
  expect_equal(Reduce(`+`, res), original)
  expect_equal(sum(get_nrows(db)), 0L)

  # The file_id sequence is reset after clearing
  expect_equal(
    DBI::dbGetQuery(db, "SELECT nextval('processed_files_seq')")[[1]],
    1
  )

  # Cleanup
  dbDisconnect(db)
  file.remove(filename)
})

test_that("get_processed_files", {
  db <- create_test_db()
  res <- get_processed_files(db)
  # file_id assignment order is not guaranteed, so compare on file name
  res <- res[order(res$file_name), ]
  expect_equal(res$file_name, c("test1.json", "test2.json"))
  expect_equal(res$participant_id, c(12345, 12345))
  expect_equal(res$sense_version, c(5L, 5L))
  expect_false("file_hash" %in% colnames(res))
  expect_s3_class(res$modified_at, "POSIXct")
  expect_s3_class(res$processed_at, "POSIXct")
  cleanup_test_db(db)
})

test_that("get_participants", {
  db <- create_test_db()
  res <- get_participants(db)
  res_lazy <- get_participants(db, lazy = TRUE)
  true <- data.frame(
    participant_id = 12345,
    study_id = "test_study"
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
    study_id = "test_study",
    data_format = "CARP JSON"
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
