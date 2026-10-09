# Build the derived parquet snapshot that example_db() loads.
#
# The shipped zips in inst/extdata/example/ are the ground truth; this script
# unzips them, imports them with the package's own importer, and writes one
# parquet file per table to inst/extdata/example-db/. Run from the package
# root:
#
#   Rscript data-raw/build_example_db.R
#
# The snapshot is committed, so re-run this script whenever the corpus or the
# database schema changes.

if (!"mpathsenser" %in% loadedNamespaces()) {
  pkgload::load_all(".", quiet = TRUE)
}

example_dir <- "inst/extdata/example"
snapshot_dir <- "inst/extdata/example-db"

# Unzip the shipped transfer -----------------------------------------------

json_dir <- file.path(tempdir(), "mpathsenser-example-json")
unlink(json_dir, recursive = TRUE)
unzip_data(example_dir, to = json_dir, overwrite = TRUE, .progress = FALSE)

files <- list.files(json_dir, pattern = "\\.json$", full.names = TRUE)
if (length(files) != 27) {
  stop("Expected 27 extracted JSON files, found ", length(files), ".")
}

# The zip names carry the export time. Pin it as the file mtime before
# importing, so ProcessedFiles.modified_at does not depend on when the files
# happened to be extracted and rebuilds stay byte-identical.
stamp <- sub(
  ".*m_Path_sense_([0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-)([0-9]{2})([0-9]{6}).*",
  "\\1\\2.\\3",
  basename(files)
)
Sys.setFileTime(
  files,
  as.POSIXct(stamp, format = "%Y-%m-%d_%H-%M-%OS", tz = "UTC")
)

# Import --------------------------------------------------------------------

db <- create_db(NULL, ":memory:")
failed <- read_mpath_sense(json_dir, db, .progress = FALSE)
if (!identical(failed, "")) {
  stop("Some example files failed to import: ", paste(failed, collapse = ", "))
}

# processed_at defaults to the import time. Pin it to the file's own export
# time, so the snapshot contains no build-machine timestamps and rebuilds
# stay byte-identical.
DBI::dbExecute(db, "UPDATE ProcessedFiles SET processed_at = modified_at")

# Export --------------------------------------------------------------------

unlink(snapshot_dir, recursive = TRUE)
dir.create(snapshot_dir, recursive = TRUE)

copy_table <- function(schema, table) {
  path <- file.path(snapshot_dir, paste0(schema, "_", table, ".parquet"))
  DBI::dbExecute(
    db,
    sprintf(
      "COPY (SELECT * FROM %s ORDER BY ALL) TO %s (FORMAT PARQUET)",
      DBI::dbQuoteIdentifier(db, DBI::Id(schema = schema, table = table)),
      as.character(DBI::dbQuoteString(db, path))
    )
  )
}

main_tables <- c("Study", "Participant", "ProcessedFiles")
for (table in main_tables) {
  copy_table("main", table)
}

# Empty tables come back from create_db() when the snapshot is loaded, so only
# tables with measurements need a parquet file.
raw_tables <- DBI::dbGetQuery(
  db,
  "SELECT table_name FROM duckdb_tables()
   WHERE schema_name = 'raw' AND estimated_size > 0
   ORDER BY table_name"
)$table_name
for (table in raw_tables) {
  copy_table("raw", table)
}

# Summary -------------------------------------------------------------------

snapshot_files <- list.files(snapshot_dir, pattern = "\\.parquet$")
raw_rows <- DBI::dbGetQuery(
  db,
  "SELECT sum(estimated_size) AS n FROM duckdb_tables() WHERE schema_name = 'raw'"
)$n[[1]]

cat(
  "Wrote",
  length(snapshot_files),
  "parquet files",
  "(",
  length(main_tables),
  "main +",
  length(raw_tables),
  "raw ).",
  "\n"
)
cat("Raw rows:", format(raw_rows, big.mark = ","), "\n")
cat(
  "Snapshot bytes:",
  format(sum(file.info(file.path(snapshot_dir, snapshot_files))$size), big.mark = ","),
  "\n"
)

DBI::dbDisconnect(db, shutdown = TRUE)
