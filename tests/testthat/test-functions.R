test_that("import", {
	db <- create_db("test.db", overwrite = TRUE)
  # expect_message(import(path = ".", db = db))
	expect_message(test_jsons("."))
})
