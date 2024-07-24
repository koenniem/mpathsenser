test_that("ccopy", {
  # Create directory for zip in tempdir
  zip_dir <- file.path(tempdir(), "mpathsenser_zip")
  dir.create(zip_dir)

  # Define the path for the zip
  zipfile <-
    tempfile(file.path("mpathsenser_zip", "test"), fileext = ".zip")

  # Zip in the new temp directory
  utils::zip(zipfile,
    system.file("testdata", "test.json", package = "mpathsenser"),
    flags = "-q"
  )

  expect_message(ccopy(zip_dir, zip_dir), "No files left to copy")
  expect_message(ccopy(zip_dir, tempdir()), "Copying 1 files\\.")

  # Get the correct file name in the temp directory and remove
  zipfile <- list.files(path = zip_dir)[1]
  unlink(zip_dir, recursive = TRUE)
  file.remove(file.path(tempdir(), zipfile))
})

test_that("fix_jsons", {
  # Path to the test data folder
  path <- system.file("testdata", "broken/", package = "mpathsenser")

  # Path to the etst files
  files <- list.files(path, pattern = ".json")

  # Set up temporary directory for copying the broken files to
  tempdir <- tempfile()
  dir.create(tempdir)

  # copy the data from the test folder to the temp folder
  invisible(do.call(
    file.copy,
    list(
      from = file.path(path, files),
      to = tempdir,
      overwrite = TRUE,
      copy.mode = FALSE
    )
  ))

  # Test arguments
  expect_error(
    fix_jsons(path = NULL, files = NULL),
    "`path` and `files` cannot be NULL at the same time."
  )

  # With path argument
  expect_message(
    fix_jsons(path = tempdir, recursive = FALSE),
    "Fixed 13 files"
  )

  # copy again after fixing
  invisible(do.call(
    file.copy,
    list(
      from = file.path(path, files),
      to = tempdir,
      overwrite = TRUE,
      copy.mode = FALSE
    )
  ))

  # With files argument
  expect_message(
    fix_jsons(
      path = NULL,
      files = file.path(tempdir, files),
      recursive = FALSE
    ),
    "Fixed 13 files"
  )

  # copy again after fixing
  invisible(do.call(
    file.copy,
    list(
      from = file.path(path, files),
      to = tempdir,
      overwrite = TRUE,
      copy.mode = FALSE
    )
  ))

  # With both path and files arguments
  expect_message(
    fix_jsons(
      path = tempdir,
      files = files,
      recursive = FALSE
    ),
    "Fixed 13 files"
  )

  file.remove(file.path(tempdir, files))

  expect_error(
    fix_jsons(path = tempdir),
    "No JSON files found."
  )

  invisible(do.call(
    file.copy,
    list(
      from = file.path(path, files),
      to = tempdir,
      overwrite = TRUE,
      copy.mode = FALSE
    )
  ))

  unlink(tempdir, recursive = TRUE)
})

test_that("test_jsons", {
  path <- system.file("testdata", package = "mpathsenser")
  broken_path <-
    system.file("testdata", "broken/", package = "mpathsenser")
  files <- list.files(path, pattern = "*.json", full.names = TRUE)

  # Test with path argument
  expect_message(
    test_jsons(path, recursive = FALSE),
    "No issues found."
  )

  # Test with files argument
  expect_message(
    test_jsons(path = NULL, files = files),
    "No issues found."
  )

  # Test with both path and files argument
  expect_message(
    test_jsons(
      path = path,
      files = list.files(path, pattern = "*.json")
    ),
    "No issues found."
  )

  # Test arguments
  expect_error(
    test_jsons(path = NULL, files = NULL),
    "`path` and `files` cannot be NULL at the same time."
  )

  # Test output type if errors are found
  suppressWarnings(expect_vector(test_jsons(broken_path),
    ptype = character()
  ))

  # Test empty file
  empty <- tempfile(fileext = ".json")
  file.create(empty)
  expect_message(
    test_jsons(path = NULL, files = empty),
    "No issues found."
  )

  # Test with db
  temp_path <- tempfile()
  db <- create_db(NULL, temp_path)
  expect_message(
    test_jsons(path, db = db, recursive = FALSE),
    "No issues found."
  )
  dbDisconnect(db)
  unlink(temp_path)
})

test_that("unzip_data", {
  # Create directory for zip in tempdir
  zip_dir <- file.path(tempdir(), "mpathsenser_zip")
  dir.create(zip_dir)

  # Define the path for the zip
  zipfile <- tempfile(file.path("mpathsenser_zip", "test"), fileext = ".zip")

  # Zip in the new temp directory
  utils::zip(zipfile,
    system.file("testdata", "test.json", package = "mpathsenser"),
    flags = "-q"
  )

  expect_message(
    unzip_data(zip_dir, recursive = FALSE, overwrite = TRUE),
    "Unzipped 1 files."
  )

  expect_message(
    unzip_data(zip_dir, recursive = TRUE, overwrite = FALSE),
    "No files found to unzip."
  )

  expect_message(
    unzip_data(zip_dir, recursive = FALSE, overwrite = TRUE),
    "Unzipped 1 files."
  )

  # Try a mixture of zip and json files
  file.copy(
    from = system.file("testdata", "test.json", package = "mpathsenser"),
    to = file.path(zip_dir, "test.json"),
    overwrite = TRUE
  )
  expect_message(
    unzip_data(zip_dir, recursive = FALSE, overwrite = TRUE),
    "Unzipped 1 files."
  )

  # Get the correct file name in the temp directory and remove
  unlink(zip_dir, recursive = TRUE)
})
