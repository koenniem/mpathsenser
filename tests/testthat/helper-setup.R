# Helper functions for setting up test databases

# Create a fresh test database with sample data for testing
# This replaces the need for a pre-existing test.db file
create_test_db <- function() {

  # Create a temporary database
  filename <- tempfile("test", fileext = ".db")
  db <- create_db(NULL, filename)

  # Import test data from JSON files
  # Try installed package location first, then fall back to local inst folder

  path <- system.file("testdata", package = "mpathsenser")
  if (path == "") {
    # When running tests during development, use local path
    path <- file.path(testthat::test_path(), "..", "..", "inst", "testdata")
  }
  suppressMessages(suppressWarnings(import(
    path = path,
    db = db,
    recursive = FALSE
  )))

  db
}

# Clean up a test database
cleanup_test_db <- function(db) {
  if (is.null(db) || !inherits(db, "duckdb_connection")) {
    return(invisible(NULL))
  }
  
  db_path <- tryCatch(
    db@driver@dbdir,
    error = function(e) NULL
  )
  
  if (dbIsValid(db)) {
    dbDisconnect(db)
  }
  
  if (!is.null(db_path) && file.exists(db_path)) {
    file.remove(db_path)
  }
  
  invisible(NULL)
}
