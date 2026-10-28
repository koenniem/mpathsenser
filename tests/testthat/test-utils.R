test_that("unzip_data", {
  skip_if_not(nzchar(Sys.which("zip")), "zip binary not available")
  # Create directory for zip in tempdir
  zip_dir <- tempfile()
  dir.create(zip_dir)

  # Define the path for the zip
  zipfile <- file.path(zip_dir, "test.zip")

  # Zip in the new temp directory
  utils::zip(
    zipfile,
    system.file("testdata", "tests.json", package = "mpathsenser"),
    flags = "-rjq9X"
  )

  expect_message(
    unzip_data(zip_dir, recursive = FALSE, overwrite = TRUE),
    "Unzipped 1 file."
  )

  expect_message(
    unzip_data(zip_dir, recursive = TRUE, overwrite = FALSE),
    "No files were unzipped."
  )

  expect_message(
    unzip_data(zip_dir, recursive = FALSE, overwrite = TRUE),
    "Unzipped 1 file."
  )

  # Try a mixture of zip and json files
  file.copy(
    from = system.file("testdata", "tests.json", package = "mpathsenser"),
    to = file.path(zip_dir, "test2.json"),
    overwrite = TRUE
  )
  expect_message(
    unzip_data(zip_dir, recursive = FALSE, overwrite = TRUE),
    "Unzipped 1 file."
  )

  # Get the correct file name in the temp directory and remove
  unlink(zip_dir, recursive = TRUE)
})
