test_that("test_jsons", {
  path <- system.file("testdata", package = "mpathsenser")
  expect_message(test_jsons(path, recursive = FALSE), "No issues found.")
  suppressWarnings(expect_vector(test_jsons(paste0(path, "/broken/")), ptype = character()))
})

test_that("fix_jsons", {
  path <- system.file("testdata", "broken/", package = "mpathsenser")
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
  path <- system.file("testdata", package = "mpathsenser")
  broken_path <- system.file("testdata", "broken", package = "mpathsenser")
  utils::zip(paste0(path, "/test.zip"), system.file("testdata", "test.json",
                                                    package = "mpathsenser"))
  expect_message(ccopy(path, path), "No files left to copy")
  expect_message(ccopy(path, broken_path), "Copying 1 files\\.")
  file.remove(paste0(path, "/broken/test.zip"))
  file.remove(system.file("testdata", "test.zip", package = "mpathsenser"))
})

test_that("unzip_data", {
  path <- system.file("testdata", package = "mpathsenser")
  utils::zip(paste0(path, "/test.zip"), system.file("testdata", "test.json",
                                                    package = "mpathsenser"))
  expect_message(unzip_data(path, recursive = FALSE, overwrite = TRUE), "Unzipped 1 files.")
  expect_message(unzip_data(path, recursive = TRUE, overwrite = FALSE), "No files found to unzip.")
  expect_error(unzip_data(1), "path must be a character string")
  file.remove(system.file("testdata", "test.zip", package = "mpathsenser"))
})
