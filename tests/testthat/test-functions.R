test_that("import", {
	db <- create_db(".", "test.db", overwrite = TRUE)
  expect_message(import(path = ".", db = db))
  RSQLite::dbDisconnect(db)
})

test_that("coverage", {
	db <- open_db("test.db")
	expect_s3_class(coverage(db, "27624"), "ggplot")
	RSQLite::dbDisconnect(db)
})

test_that("no lint errors", {
	lintr::expect_lint_free(
		linters = lintr::with_defaults(
			no_tab_linter = NULL,
			commented_code_linter = NULL,
			line_length_linter = NULL,
			cyclocomp_linter = NULL,
			lintr::line_length_linter(100)
		),
		cache = TRUE
	)
})
