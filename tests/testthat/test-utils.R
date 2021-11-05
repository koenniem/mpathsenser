test_that("test_jsons", {
  expect_message(test_jsons(".", recursive = FALSE), "No issues found.")
  suppressWarnings(expect_vector(test_jsons("./broken/"), ptype = character()))
})

test_that("fix_jsons", {
  expect_message(fix_jsons("./broken/", recursive = FALSE), "Fixed 9 files")
  invisible(do.call(
    file.copy,
    list(
      from = file.path("./broken/backup", list.files("./broken/backup/")),
      to = "./broken",
      overwrite = TRUE
    )
  ))
})

test_that("ccopy", {
  expect_message(ccopy(".", "."), "No files left to copy")
  suppressMessages(expect_invisible(ccopy(".", "./broken")))
  invisible(suppressWarnings(file.remove("./broken/test.zip")))
})

test_that("unzip_carp", {
  expect_message(unzip_carp(".", recursive = FALSE, overwrite = TRUE), "Unzipped \\d files.")
  expect_message(unzip_carp(".", recursive = FALSE, overwrite = FALSE), "No files found to unzip.")
  expect_error(unzip_carp(1), "path must be a character string")
})
