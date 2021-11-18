# Tests for functions.R

test_that("import", {
  path <- system.file("testdata", package = "CARP")
  db <- create_db(path, "test.db", overwrite = TRUE)

  expect_message(import(
    path = path,
    db = db,
    recursive = FALSE
  ), "All files were successfully written to the database.")

  if (DBI::dbIsValid(db)) {
    DBI::dbDisconnect(db)
  }
})

test_that("coverage", {
	if (file.exists(system.file("testdata", "test.db", package = "CARP"))) {
	  path <- system.file("testdata", package = "CARP")
	  db <- open_db(path, db_name = "test.db")
	  expect_s3_class(coverage(db, "27624"), "ggplot")
	  RSQLite::dbDisconnect(db)
	}
})

test_that("freq", {
  expect_vector(freq, ptype = numeric(), 11)
})
