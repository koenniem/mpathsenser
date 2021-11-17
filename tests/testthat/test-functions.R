
test_that("import", {
  path <- system.file("testdata", package = "CARP")
  db <- create_db(path, "test.db", overwrite = TRUE)

  expect_message(import(
    path = path,
    db = db,
    recursive = FALSE
  ))

  if (DBI::dbIsValid(db)) {
    DBI::dbDisconnect(db)
  }
})

# test_that("coverage", {
# 	expect_true(file.exists("test.db"))
#   import(test_path(), dbname = "test.db", recursive = FALSE, overwrite_db = TRUE)
#   db <- open_db(".", db_name = "test.db")
#   expect_s3_class(coverage(db, "27624"), "ggplot")
#   RSQLite::dbDisconnect(db)
# })
