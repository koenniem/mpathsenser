test_that("test_jsons", {
  path <- system.file("testdata", package = "mpathsenser")
  expect_message(test_jsons(path, recursive = FALSE), "No issues found.")
  suppressWarnings(expect_vector(test_jsons(paste0(path, "/broken/")), ptype = character()))
})

test_that("fix_jsons", {
  path <- system.file("testdata", "broken/", package = "mpathsenser")
  files <- list.files(path, pattern = ".json")
  invisible(do.call(
    file.copy,
    list(
      from = file.path(path, files),
      to = tempdir(),
      overwrite = TRUE,
      copy.mode = FALSE
    )
  ))

  expect_message(fix_jsons(
    files = file.path(tempdir(), files),
    recursive = FALSE
  ), "Fixed 9 files")

  file.remove(file.path(tempdir(), files))
})

test_that("ccopy", {
  # Create directory for zip in tempdir
  zip_dir <- file.path(tempdir(), "mpathsenser_zip")
  dir.create(zip_dir)

  # Define the path for the zip
  zipfile <- tempfile(file.path("mpathsenser_zip", "test"), fileext = ".zip")

  # Zip in the new temp directory
  utils::zip(zipfile, system.file("testdata", "test.json", package = "mpathsenser"))

  # Test input
  expect_error(ccopy(from = 1, to = zip_dir), "from must be a character")
  expect_error(ccopy(from = zip_dir, to = 1), "to must be a character")
  expect_error(
    ccopy(from = zip_dir, to = zip_dir, recursive = "Yes"),
    "recursive must be a logical"
  )

  expect_message(ccopy(zip_dir, zip_dir), "No files left to copy")
  expect_message(ccopy(zip_dir, tempdir()), "Copying 1 files\\.")

  # Get the correct file name in the temp directory and remove
  zipfile <- list.files(path = zip_dir)[1]
  unlink(zip_dir, recursive = TRUE)
  file.remove(file.path(tempdir(), zipfile))
})

test_that("unzip_data", {
  # Create directory for zip in tempdir
  zip_dir <- file.path(tempdir(), "mpathsenser_zip")
  dir.create(zip_dir)

  # Define the path for the zip
  zipfile <- tempfile(file.path("mpathsenser_zip", "test"), fileext = ".zip")

  # Zip in the new temp directory
  utils::zip(zipfile, system.file("testdata", "test.json", package = "mpathsenser"))

  expect_message(unzip_data(zip_dir, recursive = FALSE, overwrite = TRUE), "Unzipped 1 files.")
  expect_message(unzip_data(zip_dir, recursive = TRUE, overwrite = FALSE), "No files found to unzip.")
  expect_message(unzip_data(zip_dir, recursive = FALSE, overwrite = TRUE), "Unzipped 1 files.")
  expect_error(unzip_data(1), "path must be a character string")

  # Get the correct file name in the temp directory and remove
  unlink(zip_dir, recursive = TRUE)
})
