# Tests for functions.R

test_that("import", {
  path <- system.file("testdata", package = "mpathsenser")
  # Create db
  filename <- tempfile("test", fileext = ".db")
  db <- create_db(NULL, filename)

  expect_message(import(
    path = path,
    db = db,
    recursive = FALSE
  ), "All files were successfully written to the database.")
  dbDisconnect(db)
  file.remove(filename)


  # Create db
  filename <- tempfile("test2", fileext = ".db")
  db2 <- create_db(NULL, filename)
  warnings_log <- capture_warnings(import(
    path = path,
    db = db2,
    recursive = TRUE # Includes broken files
  ))
  expect_match(warnings_log, "Invalid JSON in file broken\\/broken\\d\\.json", all = FALSE)
  expect_match(warnings_log, "Some files could not be written to the database.", all = FALSE)
  dbDisconnect(db2)
  file.remove(filename)
})
