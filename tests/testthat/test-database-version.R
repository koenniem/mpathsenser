# Tests for database versioning (R/database_version.R and its wiring)

test_that("create_db records one DatabaseInfo row with the current version", {
  db <- fresh_test_db()
  on.exit(cleanup_test_db(db), add = TRUE)

  info <- DBI::dbGetQuery(
    db,
    "SELECT database_version, created_with, created_at FROM DatabaseInfo"
  )
  expect_equal(nrow(info), 1L)
  expect_identical(info$database_version, .current_database_version())
  expect_identical(info$created_with, as.character(utils::packageVersion("mpathsenser")))
  expect_false(is.na(info$created_at))
})

test_that("database version map is internally consistent", {
  versions <- .supported_database_versions()
  expect_type(versions, "integer")
  expect_false(anyDuplicated(versions) > 0)
  expect_identical(.current_database_version(), max(versions))

  for (entry in .database_versions) {
    expect_type(entry$version, "integer")
    expect_type(entry$sense_versions, "integer")
    expect_true(length(entry$sense_versions) > 0)
    expect_match(entry$introduced_in, "^[0-9]+(\\.[0-9]+)+$")
  }
})

set_database_version <- function(db, version) {
  dbExecute(
    db,
    sprintf("UPDATE DatabaseInfo SET database_version = %d", as.integer(version))
  )
}

test_that("open_db rejects an unsupported older database version", {
  path <- tempfile("old-version", fileext = ".duckdb")
  db <- create_db(NULL, path, shared_home = FALSE)
  set_database_version(db, min(.supported_database_versions()) - 1L)
  close_db(db)

  expect_error(open_db(path), "too old")
  expect_error(open_db(path), "create_db")
  # The file must be deletable afterwards: a connection leaked on the error
  # path would keep the lock (visible on Windows CI).
  expect_true(file.remove(path))
})

test_that("open_db rejects a database from a newer mpathsenser", {
  path <- tempfile("new-version", fileext = ".duckdb")
  db <- create_db(NULL, path, shared_home = FALSE)
  set_database_version(db, max(.supported_database_versions()) + 1L)
  close_db(db)

  expect_error(open_db(path), "newer")
  expect_error(open_db(path), "create_db")
  expect_true(file.remove(path))
})

test_that("check_db rejects an unsupported database version", {
  db <- fresh_test_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  set_database_version(db, max(.supported_database_versions()) + 1L)

  expect_error(get_data(db, "Activity"), "newer")
})

test_that("the database version check cannot be skipped", {
  db <- fresh_test_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  set_database_version(db, max(.supported_database_versions()) + 1L)
  old <- options(mpathsenser.check_missing_sensors = FALSE)
  on.exit(options(old), add = TRUE)

  expect_error(check_db(db), "newer")
})

drop_database_version <- function(path) {
  db <- create_db(NULL, path, shared_home = FALSE)
  dbExecute(db, "DROP TABLE DatabaseInfo")
  close_db(db)
  path
}

test_that("open_db adopts an unversioned database with a warning", {
  path <- drop_database_version(tempfile("unversioned", fileext = ".duckdb"))

  expect_warning(db <- open_db(path), "no version information")
  on.exit(cleanup_test_db(db), add = TRUE)

  info <- DBI::dbGetQuery(db, "SELECT database_version FROM DatabaseInfo")
  expect_equal(nrow(info), 1L)
  expect_identical(info$database_version, .current_database_version())

  # The fallback table must match the schema dbdef.sql creates.
  fresh <- create_db(NULL, tempfile(fileext = ".duckdb"), shared_home = FALSE)
  on.exit(cleanup_test_db(fresh), add = TRUE)
  columns <- function(conn) {
    DBI::dbGetQuery(
      conn,
      "SELECT column_name, data_type, is_nullable
       FROM information_schema.columns
       WHERE table_schema = 'main' AND table_name = 'DatabaseInfo'
       ORDER BY ordinal_position"
    )
  }
  expect_identical(columns(db), columns(fresh))
})

test_that("open_db does not write to an unversioned read-only database", {
  path <- drop_database_version(tempfile("unversioned-ro", fileext = ".duckdb"))

  expect_warning(db <- open_db(path, read_only = TRUE), "no version information")
  on.exit(close_db(db), add = TRUE)

  expect_equal(
    DBI::dbGetQuery(
      db,
      "SELECT COUNT(*) AS n FROM information_schema.tables
       WHERE table_schema = 'main' AND table_name = 'DatabaseInfo'"
    )$n,
    0L
  )
  close_db(db)
  file.remove(path)
})

test_that("open_db adopts an empty DatabaseInfo table", {
  path <- tempfile("empty-info", fileext = ".duckdb")
  db <- create_db(NULL, path, shared_home = FALSE)
  dbExecute(db, "DELETE FROM DatabaseInfo")
  close_db(db)

  expect_warning(db <- open_db(path), "no version information")
  on.exit(cleanup_test_db(db), add = TRUE)
  expect_identical(
    dbGetQuery(db, "SELECT database_version FROM DatabaseInfo")$database_version,
    .current_database_version()
  )
})

test_that("copy_db leaves exactly one current-version row in the target", {
  source_db <- create_test_db()
  on.exit(cleanup_test_db(source_db), add = TRUE)
  target_db <- create_db(NULL, tempfile(fileext = ".duckdb"), shared_home = FALSE)
  target_db <- copy_db(source_db, target_db)
  on.exit(cleanup_test_db(target_db), add = TRUE)

  info <- DBI::dbGetQuery(target_db, "SELECT database_version FROM DatabaseInfo")
  expect_equal(nrow(info), 1L)
  expect_identical(info$database_version, .current_database_version())
})

test_that("create_db(overwrite = TRUE) records exactly one current-version row", {
  path <- tempfile("overwrite-version", fileext = ".duckdb")
  db <- create_db(NULL, path, shared_home = FALSE)
  close_db(db)
  db <- create_db(NULL, path, overwrite = TRUE, shared_home = FALSE)
  on.exit(cleanup_test_db(db), add = TRUE)

  expect_equal(dbGetQuery(db, "SELECT COUNT(*) AS n FROM DatabaseInfo")$n, 1L)
  expect_identical(
    dbGetQuery(db, "SELECT database_version FROM DatabaseInfo")$database_version,
    .current_database_version()
  )
})

test_that("example_db records the current version exactly once", {
  db <- example_db()
  on.exit(close_db(db), add = TRUE)

  info <- DBI::dbGetQuery(db, "SELECT database_version FROM DatabaseInfo")
  expect_equal(nrow(info), 1L)
  expect_identical(info$database_version, .current_database_version())
})

test_that("read_mpath_sense refuses an unsupported database before importing", {
  db <- fresh_test_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  set_database_version(db, max(.supported_database_versions()) + 1L)
  before <- dbGetQuery(db, "SELECT COUNT(*) AS n FROM ProcessedFiles")$n

  dir <- tempfile("mpathsenser_version_test")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  write_test_json_files(dir)

  expect_error(
    suppressMessages(read_mpath_sense(dir, db, recursive = FALSE, .progress = FALSE)),
    "newer"
  )
  expect_identical(dbGetQuery(db, "SELECT COUNT(*) AS n FROM ProcessedFiles")$n, before)
})

test_that("open_db rejects a malformed version entry and releases the file", {
  path <- tempfile("malformed-version", fileext = ".duckdb")
  db <- create_db(NULL, path, shared_home = FALSE)
  dbExecute(db, "DROP TABLE DatabaseInfo")
  dbExecute(db, "CREATE TABLE DatabaseInfo (database_version TEXT)")
  dbExecute(db, "INSERT INTO DatabaseInfo VALUES ('not-a-version')")
  close_db(db)

  expect_error(open_db(path), "corrupt")
  # A leaked connection would make this second open fail on its file lock.
  expect_error(open_db(path), "corrupt")
  expect_true(file.remove(path))
})

test_that("open_db rejects a NULL version entry", {
  path <- tempfile("null-version", fileext = ".duckdb")
  db <- create_db(NULL, path, shared_home = FALSE)
  dbExecute(db, "DROP TABLE DatabaseInfo")
  dbExecute(db, "CREATE TABLE DatabaseInfo (database_version INTEGER)")
  dbExecute(db, "INSERT INTO DatabaseInfo VALUES (NULL)")
  close_db(db)

  expect_error(open_db(path), "corrupt")
  expect_true(file.remove(path))
})

test_that("open_db releases the file after rejecting duplicate version rows", {
  path <- tempfile("duplicate-version", fileext = ".duckdb")
  db <- create_db(NULL, path, shared_home = FALSE)
  dbExecute(db, "INSERT INTO DatabaseInfo (database_version) VALUES (1)")
  close_db(db)

  expect_error(open_db(path), "more than one entry")
  # The failed open must not keep the file lock.
  expect_error(open_db(path), "more than one entry")
  expect_true(file.remove(path))
})

test_that("check_db rejects a malformed version entry", {
  db <- fresh_test_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  dbExecute(db, "DROP TABLE DatabaseInfo")
  dbExecute(db, "CREATE TABLE DatabaseInfo (database_version TEXT)")
  dbExecute(db, "INSERT INTO DatabaseInfo VALUES ('not-a-version')")

  expect_error(get_data(db, "Activity"), "corrupt")
})
