test_that("test_jsons", {
  expect_message(test_jsons("."), "No issues found.")
	suppressWarnings(expect_vector(test_jsons("./broken/"), "character"))
})

test_that("fix_jsons", {
		dir.create("broken/tmp")
		files <- list.files("broken", "*.json$")
		invisible(do.call(
			file.copy,
			list(from = file.path("broken/", files),
					 to = "broken/tmp",
					 overwrite = FALSE
			)))
		expect_message(fix_jsons("./broken/"), "Fixed 7 files")
		invisible(do.call(
			file.copy,
			list(from = file.path("broken/tmp", files),
					 to = "broken",
					 overwrite = TRUE
			)))
		unlink("broken/tmp", TRUE, TRUE)
})

test_that("ccopy", {
	expect_message(ccopy(".", "."), "No files left to copy")
	suppressMessages(expect_invisible(ccopy(".", "./broken")))
	invisible(suppressWarnings(file.remove("./broken/test.zip")))
})

test_that("unzip_carp", {
	expect_message(unzip_carp(".", overwrite = TRUE), "Unzipped \\d files.")
	expect_message(unzip_carp(".", overwrite = FALSE), "No files found to unzip.")
	suppressMessages(expect_warning(unzip_carp(".")))
})
