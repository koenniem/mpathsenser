# Tests for database.R

test_that("sensors-vec", {
  expect_vector(sensors, character(), size = 32)
})

test_that("create_db", {
  filename <- tempfile("create", fileext = ".db")
  db <- create_db(path = NULL, filename, shared_home = FALSE)
  dbDisconnect(db)
  expect_true(file.exists(filename))

  # Test merging path and filename
  temp_file <- basename(tempfile())
  expect_no_error(
    {
      db <- create_db(path = tempdir(), db_name = temp_file, shared_home = FALSE)
      dbDisconnect(db)
    }
  )

  # Test overwrite argument
  expect_no_error(
    {
      db <- create_db(path = NULL, filename, overwrite = TRUE, shared_home = FALSE)
      dbDisconnect(db)
    }
  )

  expect_error(
    {
      db <- create_db(path = NULL, filename, overwrite = FALSE, shared_home = FALSE)
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
  db0 <- create_db(dir_d2, "mydb.duckdb", shared_home = FALSE)
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

  # A raw table without its main view (e.g. views dropped) is rejected too:
  # the schema check requires both layers.
  db1 <- create_db(NULL, tempfile("schemacheck", fileext = ".db"), shared_home = FALSE)
  p1 <- db1@driver@dbdir
  DBI::dbExecute(db1, "DROP VIEW main.Accelerometer")
  dbDisconnect(db1)
  gc()
  expect_error(open_db(p1), "does not appear to be an mpathsenser database")
  file.remove(p1)

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

test_that("open_db loads required DuckDB extensions without autoloading", {
  db <- create_test_db(path = tempfile())
  db_path <- db@driver@dbdir
  close_db(db)
  gc() # Force garbage collection to ensure file handles are released

  # A fresh DuckDB version has no extensions installed yet, and autoloading
  # fails when it cannot install icu from scratch. With autoloading disabled,
  # open_db() must install and load icu and json itself.
  db <- open_db(db_path, config = list(autoload_known_extensions = "false"))
  loaded <- dbGetQuery(
    db,
    "SELECT extension_name FROM duckdb_extensions() WHERE loaded"
  )$extension_name
  expect_true(all(c("icu", "json") %in% loaded))
  close_db(db)
  gc()

  # Read-only connections cannot write to the database, but must still install
  # and load the extensions before the session settings run.
  db <- open_db(
    db_path,
    read_only = TRUE,
    config = list(autoload_known_extensions = "false")
  )
  on.exit(close_db(db), add = TRUE)
  loaded <- dbGetQuery(
    db,
    "SELECT extension_name FROM duckdb_extensions() WHERE loaded"
  )$extension_name
  expect_true(all(c("icu", "json") %in% loaded))

  close_db(db)
  file.remove(db_path)
})

test_that("copy_db", {
  # Create a test database
  db <- create_test_db()

  filename <- tempfile("copy", fileext = ".db")
  new_db <- create_db(NULL, filename, shared_home = FALSE)

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
  new_db <- create_db(NULL, filename, shared_home = FALSE)
  new_db <- copy_db(db, new_db, sensor = "Accelerometer")
  true <- c(0L, rep(0L, 31))
  names(true) <- sensors
  expect_equal(get_nrows(new_db), true)

  cleanup_test_db(db)
  dbDisconnect(new_db)
  file.remove(filename)
})

test_that("copy_db handles quoted file paths and DBI path fallback", {
  dir <- tempfile("copy_db_", tmpdir = testthat::test_path())
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  source_db <- create_test_db()
  on.exit(cleanup_test_db(source_db), add = TRUE)
  target_path <- file.path(dir, "copy's.db")
  target_db <- create_db(NULL, target_path, shared_home = FALSE)
  on.exit(cleanup_test_db(target_db), add = TRUE)
  testthat::local_mocked_bindings(
    dbGetInfo = function(dbObj, ...) list(dbname = NULL),
    .package = "DBI"
  )

  target_db <- copy_db(source_db, target_db)
  expect_equal(get_nrows(source_db), get_nrows(target_db))
})

test_that("copy_db rejects in-memory targets without disconnecting them", {
  source_db <- create_test_db()
  on.exit(cleanup_test_db(source_db), add = TRUE)
  target_db <- create_db(NULL, ":memory:", shared_home = FALSE)
  on.exit(close_db(target_db), add = TRUE)

  expect_snapshot(error = TRUE, copy_db(source_db, target_db))
  expect_true(DBI::dbIsValid(target_db))
})

test_that("copy_db detaches its target after a copy error", {
  dir <- tempfile("copy_db_", tmpdir = testthat::test_path())
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  source_db <- create_test_db()
  on.exit(cleanup_test_db(source_db), add = TRUE)
  target_db <- create_db(NULL, file.path(dir, "target.db"), shared_home = FALSE)
  on.exit(cleanup_test_db(target_db), add = TRUE)
  testthat::local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      if (grepl("INSERT INTO new_db.Study", statement, fixed = TRUE)) {
        stop("forced copy failure")
      }
      DBI::dbExecute(conn, statement, ...)
    },
    .package = "mpathsenser"
  )

  expect_snapshot(error = TRUE, copy_db(source_db, target_db))
  databases <- DBI::dbGetQuery(
    source_db,
    "SELECT database_name FROM duckdb_databases()"
  )$database_name
  expect_false("new_db" %in% databases)
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
  # constraints of the sensor tables survive the reordering (checked on the
  # physical raw table; the main.Activity view exposes the same columns minus
  # the provenance ids)
  nullable <- DBI::dbGetQuery(
    db,
    "SELECT is_nullable FROM information_schema.columns
     WHERE table_schema = 'raw' AND table_name = 'Activity'
       AND column_name = 'participant_id'"
  )[[1]]
  expect_equal(nullable, "NO")

  # The main.Activity view keeps working after the raw-table rewrite
  expect_equal(
    DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Activity")[[1]],
    1
  )
  expect_equal(
    DBI::dbGetQuery(db, "SELECT COUNT(*) FROM raw.Activity")[[1]],
    1
  )

  # Cleanup
  dbDisconnect(db)
  file.remove(filename)
})

test_that("optimize_db rewrites unsorted sensor tables", {
  db <- create_db(NULL, ":memory:", shared_home = FALSE)

  # 200 rows in reverse time order, so the physical order violates
  # (participant_id, time) and the table must be rewritten
  DBI::dbExecute(
    db,
    "INSERT INTO raw.Pedometer
       (participant_id, time, step_count, timezone, source_file_id, source_row_id,
        source_measurement_id)
     SELECT (i % 5)::UINTEGER,
            TIMESTAMPTZ '2020-01-01' + (interval (200 - i) millisecond),
            (i * 10)::UINTEGER,
            'Europe/Brussels',
            1::UINTEGER, i::UINTEGER, 1::UINTEGER
     FROM range(1, 201) t(i)"
  )
  before <- DBI::dbGetQuery(db, "SELECT * FROM Pedometer ORDER BY participant_id, time")
  expect_equal(nrow(before), 200)

  expect_no_error(optimize_db(db, sensors = "Pedometer", .progress = FALSE))

  # The rewrite only reorders: the rows are unchanged
  after <- DBI::dbGetQuery(db, "SELECT * FROM Pedometer ORDER BY participant_id, time")
  expect_identical(after, before)

  # The physical order now follows (participant_id, time)
  violations <- DBI::dbGetQuery(
    db,
    "SELECT COUNT(*) AS n FROM (
       SELECT participant_id, time,
              LAG(participant_id) OVER (ORDER BY rowid) AS previous_participant_id,
              LAG(time) OVER (ORDER BY rowid) AS previous_time
       FROM raw.Pedometer
     ) q
     WHERE previous_participant_id IS NOT NULL
       AND (participant_id < previous_participant_id
         OR (participant_id = previous_participant_id AND time < previous_time))"
  )$n[[1]]
  expect_equal(violations, 0)

  # The NOT NULL constraints and the full raw schema survive the rewrite
  not_null <- DBI::dbGetQuery(
    db,
    "SELECT column_name FROM information_schema.columns
     WHERE table_schema = 'raw' AND table_name = 'Pedometer' AND is_nullable = 'NO'"
  )$column_name
  expect_setequal(
    not_null,
    c("participant_id", "time", "source_file_id", "source_row_id", "source_measurement_id")
  )
  expect_setequal(
    names(DBI::dbGetQuery(db, "SELECT * FROM raw.Pedometer LIMIT 0")),
    c(
      "participant_id",
      "time",
      "step_count",
      "timezone",
      "source_file_id",
      "source_row_id",
      "source_measurement_id"
    )
  )

  # No temporary table is left behind and the views keep working
  expect_false("Pedometer_optimize_tmp" %in% DBI::dbListTables(db, schema = "raw"))
  expect_equal(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Pedometer")[[1]], 200)

  cleanup_test_db(db)
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
