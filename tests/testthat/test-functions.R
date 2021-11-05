
test_that("import", {
  expect_message(import(
    path = ".",
    dbname = "test.db",
    recursive = FALSE,
    overwrite_db = TRUE
  ))
})

# test_that("coverage", {
# 	expect_true(file.exists("test.db"))
#   import(test_path(), dbname = "test.db", recursive = FALSE, overwrite_db = TRUE)
#   db <- open_db(".", db_name = "test.db")
#   expect_s3_class(coverage(db, "27624"), "ggplot")
#   RSQLite::dbDisconnect(db)
# })
