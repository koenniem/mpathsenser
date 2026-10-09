#' Open the built-in example database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Opens a ready-to-use, in-memory DuckDB database with the example capture
#' that ships with the package: 27 zipped files from one Android participant,
#' covering the common sensors. Because the database lives in memory, any
#' changes are discarded when the connection is closed.
#'
#' @returns A DuckDB connection. Close it with [close_db()] when you are done.
#' @export
#'
#' @seealso [read_mpath_sense()] to import your own files, [unzip_data()] to
#'   extract the example zips yourself.
#'
#' @examples
#' db <- example_db()
#' get_participants(db)
#' get_nrows(db)
#' close_db(db)
example_db <- function() {
  snapshot_dir <- .example_db_snapshot_dir()
  db <- create_db(NULL, ":memory:")

  tryCatch(
    .load_example_snapshot(db, snapshot_dir),
    error = function(cnd) {
      DBI::dbDisconnect(db, shutdown = TRUE)
      cli_abort(c(
        "Could not load the example database.",
        x = conditionMessage(cnd)
      ))
    }
  )

  db
}

# Find the snapshot that example_db() loads. Prefer the repository copy during
# development, matching how create_db() resolves dbdef.sql.
.example_db_snapshot_dir <- function() {
  dir <- file.path("inst", "extdata", "example-db")
  if (!dir.exists(dir)) {
    dir <- system.file("extdata", "example-db", package = "mpathsenser")
  }

  if (!dir.exists(dir)) {
    cli_abort(c(
      "Could not find the example database snapshot.",
      i = "The package installation may be incomplete."
    ))
  }
  if (length(list.files(dir, pattern = "\\.parquet$")) == 0) {
    cli_abort(c(
      "The example database snapshot is empty.",
      i = "The package installation may be incomplete."
    ))
  }

  dir
}

# Metadata first: Participant and ProcessedFiles reference Study, and the raw
# tables reference Participant.
.load_example_snapshot <- function(db, dir) {
  for (table in c("Study", "Participant", "ProcessedFiles")) {
    .load_example_table(db, "main", table, dir)
  }

  raw_tables <- dbGetQuery(
    db,
    "SELECT table_name FROM duckdb_tables() WHERE schema_name = 'raw'"
  )$table_name
  for (table in raw_tables) {
    .load_example_table(db, "raw", table, dir)
  }

  invisible(TRUE)
}

# Snapshot files are named <schema>_<Table>.parquet and use the DuckDB table
# names verbatim. Tables without a file stay empty.
.load_example_table <- function(db, schema, table, dir) {
  file <- file.path(dir, paste0(schema, "_", table, ".parquet"))
  if (!file.exists(file)) {
    return(invisible(FALSE))
  }

  dbExecute(
    db,
    sprintf(
      "INSERT INTO %s BY NAME SELECT * FROM read_parquet(%s)",
      dbQuoteIdentifier(db, Id(schema = schema, table = table)),
      as.character(DBI::dbQuoteString(db, file))
    )
  )
  invisible(TRUE)
}
