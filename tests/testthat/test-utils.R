test_that("test_jsons", {
  path <- system.file("testdata", package = "CARP")
  expect_message(test_jsons(path, recursive = FALSE), "No issues found.")
  suppressWarnings(expect_vector(test_jsons(paste0(path, "/broken/")), ptype = character()))
})

test_that("fix_jsons", {
  path <- system.file("testdata", "broken/", package = "CARP")
  expect_message(fix_jsons(path, recursive = FALSE), "Fixed 9 files")
  invisible(do.call(
    file.copy,
    list(
      from = file.path(paste0(path, "/backup/"), list.files(paste0(path, "/backup/"))),
      to = path,
      overwrite = TRUE
    )
  ))
})

test_that("ccopy", {
  path <- system.file("testdata", package = "CARP")
  expect_message(ccopy(path, path), "No files left to copy")
  suppressMessages(expect_invisible(ccopy(path, paste0(path, "/broken/"))))
  invisible(suppressWarnings(file.remove(paste0(path, "/broken/test.zip"))))
})

test_that("unzip_carp", {
  path <- system.file("testdata", package = "CARP")
  expect_message(unzip_carp(path, recursive = FALSE, overwrite = TRUE), "Unzipped \\d files.")
  expect_message(unzip_carp(path, recursive = FALSE, overwrite = FALSE), "No files found to unzip.")
  expect_error(unzip_carp(1), "path must be a character string")
})
