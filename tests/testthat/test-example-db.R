test_that("example_db() opens the shipped example capture", {
  db <- example_db()
  on.exit(cleanup_test_db(db), add = TRUE)

  expect_true(inherits(db, "duckdb_connection"))
  expect_equal(get_participants(db)$participant_id, 372780)
  expect_equal(nrow(dplyr::collect(get_data(db, "Pedometer"))), 8002)
  expect_equal(sum(get_nrows(db)), 16457)
})

test_that("example_db() connections are independent", {
  first <- example_db()
  second <- example_db()
  on.exit(cleanup_test_db(first), add = TRUE)
  on.exit(cleanup_test_db(second), add = TRUE)

  close_db(first)
  expect_equal(nrow(dplyr::collect(get_data(second, "Battery"))), 242)
})

test_that("the parquet snapshot matches the current schema", {
  db <- example_db()
  on.exit(cleanup_test_db(db), add = TRUE)

  empty <- create_db(NULL, ":memory:", shared_home = FALSE)
  on.exit(cleanup_test_db(empty), add = TRUE)

  snapshot <- system.file("extdata", "example-db", package = "mpathsenser")
  files <- list.files(snapshot, pattern = "^raw_.*\\.parquet$", full.names = TRUE)

  for (file in files) {
    table <- sub("^raw_(.*)\\.parquet$", "\\1", basename(file))
    parquet_cols <- dbGetQuery(
      db,
      sprintf("DESCRIBE SELECT * FROM read_parquet(%s)", DBI::dbQuoteString(db, file))
    )$column_name
    schema_cols <- dbGetQuery(
      empty,
      sprintf(
        "SELECT column_name FROM information_schema.columns
         WHERE table_schema = 'raw' AND table_name = %s
         ORDER BY ordinal_position",
        DBI::dbQuoteString(empty, table)
      )
    )$column_name
    expect_equal(parquet_cols, schema_cols, info = table)
  }
})

test_that("the shipped zips still import to the same rows", {
  zip_dir <- system.file("extdata", "example", package = "mpathsenser")
  json_dir <- file.path(tempdir(), "mpathsenser-example-parity")
  unzip_data(zip_dir, to = json_dir, overwrite = TRUE, .progress = FALSE)
  on.exit(unlink(json_dir, recursive = TRUE), add = TRUE)

  imported <- create_db(NULL, ":memory:", shared_home = FALSE)
  on.exit(cleanup_test_db(imported), add = TRUE)
  read_mpath_sense(json_dir, imported, .progress = FALSE)

  example <- example_db()
  on.exit(cleanup_test_db(example), add = TRUE)

  expect_equal(get_participants(example)$participant_id, 372780)
  expect_equal(get_nrows(example), get_nrows(imported))
})
