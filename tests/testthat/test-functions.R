test_that("import", {
	db <- create_db("test.db", overwrite = TRUE)
  expect_message(import(path = ".", db = db))
})

test_that("coverage", {
	db <- open_db("test.db")
	expect_s3_class(coverage(db, "27624"), "ggplot")
})
