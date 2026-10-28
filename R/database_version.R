# Database layout versioning. The database stores its version in the single-row
# DatabaseInfo table; this file holds the matching policy: which database
# versions the package supports and which m-Path Sense formats each version can
# store. Update the map by hand when the layout changes.

# One entry per database version this package can open. Add an entry when a new
# layout starts being written; remove one when the package drops support for
# that layout (users recreate with create_db()). `sense_versions` is consumed
# by the importer; this file only records it.
.database_versions <- list(
  list(
    version = 1L,
    introduced_in = "2.0.0",
    sense_versions = c(5L, 6L)
  )
)

.supported_database_versions <- function() {
  vapply(.database_versions, function(entry) entry$version, integer(1))
}

.current_database_version <- function() {
  max(.supported_database_versions())
}

.write_database_version <- function(db) {
  # Keep this shape in sync with DatabaseInfo in inst/extdata/dbdef.sql.
  dbExecute(
    db,
    "CREATE TABLE IF NOT EXISTS DatabaseInfo(
      database_version INTEGER NOT NULL,
      created_with TEXT,
      created_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp
    )"
  )
  dbExecute(
    db,
    "INSERT INTO DatabaseInfo (database_version, created_with) VALUES ($1, $2)",
    params = list(
      .current_database_version(),
      as.character(utils::packageVersion("mpathsenser"))
    )
  )
  invisible(TRUE)
}

# The single version row, or NA when the table or row is missing. More than one
# row means the database is corrupt and cannot be trusted.
.read_database_version <- function(db) {
  exists <- dbGetQuery(
    db,
    "SELECT COUNT(*) AS n FROM information_schema.tables
     WHERE table_schema = 'main' AND table_name = 'DatabaseInfo'
       AND table_type = 'BASE TABLE'"
  )$n[[1]]
  if (exists == 0) {
    return(NA_integer_)
  }

  versions <- dbGetQuery(db, "SELECT database_version FROM DatabaseInfo")$database_version
  if (length(versions) == 0) {
    return(NA_integer_)
  }
  if (length(versions) > 1) {
    cli_abort(c(
      "The database has more than one entry in {.code DatabaseInfo}.",
      i = "The database is corrupt; recreate it with {.help create_db}."
    ))
  }

  # Reserve NA for a missing table or row: a present but malformed value is
  # corruption, not a database created before versioning.
  version <- suppressWarnings(as.integer(versions))
  if (is.na(version) || !isTRUE(all.equal(version, versions))) {
    cli_abort(c(
      "The database has an invalid entry in {.code DatabaseInfo}.",
      i = "The database is corrupt; recreate it with {.help create_db}."
    ))
  }
  version
}

# Missing versions are supported for now: open_db() adopts databases created
# before versioning as the current layout. Remove this fallback (and make
# check_db() reject NA) once those databases no longer need support.
.database_version_supported <- function(version) {
  is.na(version) || version %in% .supported_database_versions()
}

.abort_unsupported_database_version <- function(version, call = caller_env()) {
  supported <- .supported_database_versions()
  pkg_version <- as.character(utils::packageVersion("mpathsenser"))

  if (version < min(supported)) {
    cli_abort(
      c(
        "Database version {.val {version}} is too old for {.pkg mpathsenser} {.val {pkg_version}}.",
        i = "This version supports {length(supported)} database version{?s}: {.val {supported}}.",
        i = "Recreate the database with {.help create_db} and re-import the source files."
      ),
      call = call
    )
  }

  cli_abort(
    c(
      "Database version {.val {version}} is newer than {.pkg mpathsenser} {.val {pkg_version}} supports.",
      i = "This version supports {length(supported)} database version{?s}: {.val {supported}}.",
      i = "Update {.pkg mpathsenser}; if that does not help, recreate the database with {.help create_db}."
    ),
    call = call
  )
}
