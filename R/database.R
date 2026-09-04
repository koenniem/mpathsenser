#' Available Sensors
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' A list containing all available sensors in this package you can work with. This variable was
#' created so it is easier to use in your own functions, e.g. to loop over sensors.
#'
#' @returns A character vector containing all sensor names supported by `mpathsenser`.
#' @examples
#' sensors
#' @export sensors
sensors <- c(
  "Accelerometer",
  "Activity",
  "AppUsage",
  "Battery",
  "Bluetooth",
  "BluetoothBeacon",
  "Connectivity",
  "Device",
  "Error",
  "GarminAccelerometer",
  "GarminActigraphy",
  "GarminBBI",
  "GarminEnhancedBBI",
  "GarminGyroscope",
  "GarminHeartRate",
  "GarminMeta",
  "GarminRespiration",
  "GarminSkinTemperature",
  "GarminSPO2",
  "GarminSteps",
  "GarminStress",
  "GarminWristStatus",
  "GarminZeroCrossing",
  "Heartbeat",
  "Light",
  "Location",
  "Memory",
  "Pedometer",
  "Screen",
  "Timezone",
  "Weather",
  "Wifi"
)


# Configure connection defaults for DuckDB. Session-level settings, so they
# apply to the connection only.
.configure_duckdb <- function(
  db,
  threads = NULL,
  memory_limit = NULL,
  temp_directory = NULL
) {
  # Canonical instant handling: always display UTC by default
  DBI::dbExecute(db, "SET timezone = 'UTC'")

  # DuckDB's own query progress bar clashes with the cli progress bars of this
  # package, so it is disabled on all connections
  DBI::dbExecute(db, "PRAGMA disable_progress_bar")

  # Parallel pipelines (e.g. staging large JSON files) use considerably less
  # memory when DuckDB does not need to preserve insertion order. Query results
  # without an explicit ORDER BY are unordered anyway.
  DBI::dbExecute(db, "SET preserve_insertion_order = false")

  if (!is.null(threads)) {
    DBI::dbExecute(db, sprintf("SET threads = %d", as.integer(threads)))
  }
  if (!is.null(memory_limit)) {
    DBI::dbExecute(db, sprintf("SET memory_limit = '%s'", memory_limit))
  }
  if (!is.null(temp_directory)) {
    temp_directory <- gsub("'", "''", temp_directory)
    DBI::dbExecute(db, sprintf("SET temp_directory = '%s'", temp_directory))
  }

  # Cache object scans (e.g. when reading parquet files)
  DBI::dbExecute(db, "SET enable_object_cache = true")

  invisible(TRUE)
}

# Define the to_local_time() macro and the per-sensor _local/_with_local views
# from the static SQL file. Views live in SQL, not in R, so they are generated
# once against the canonical schema and stay in sync with it. Creating them is
# idempotent (CREATE OR REPLACE), but is skipped when they already exist, which
# lets a read-only connection reopen a database without attempting to write.
.create_local_views <- function(db) {
  already <- DBI::dbGetQuery(
    db,
    "SELECT COUNT(*) AS n FROM duckdb_functions()
     WHERE function_name = 'to_local_time' AND function_type = 'macro'"
  )$n >
    0
  if (isTRUE(already)) {
    return(invisible(TRUE))
  }

  fn <- file.path("inst", "extdata", "views.sql")
  if (!file.exists(fn)) {
    fn <- system.file("extdata", "views.sql", package = "mpathsenser")
  }
  script <- paste0(readLines(fn, warn = FALSE), collapse = "\n")
  dbExecute(db, script)
  invisible(TRUE)
}

#' Create a new mpathsenser database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param path The path to the database file, or the directory in which the
#'   database is created when `db_name` is given. Use `NULL` for an in-memory
#'   database.
#' @param db_name Optional name of the database file. When given, `path` is
#'   treated as the directory in which the database is created.
#' @param ... Additional arguments passed to [DBI::dbConnect()].
#' @param overwrite In case a database already exists, indicate whether it
#'   should be overwritten or not. Otherwise, this option is ignored.
#' @param threads The number of threads DuckDB may use. Defaults to `NULL`,
#'   which lets DuckDB use all available cores. Lower this on shared servers.
#' @param memory_limit Memory limit for the DuckDB instance (e.g. `"4GB"`).
#'   Defaults to `NULL`, which uses DuckDB's default of roughly 80% of RAM.
#' @param temp_directory Directory for temporary files (spill-to-disk). Use a
#'   local disk with sufficient space; do not point this at a synced directory
#'   such as OneDrive.
#'
#' @returns A connection using prepared database schemas.
#' @export
#'
#' @examples
#' # Create a new database in a temporary directory
#' db <- create_db(tempdir(), "mydb.db")
#'
#' # You can also create an in-memory database
#' db2 <- create_db(path = NULL, ":memory:")
#'
#' # Cleanup
#' close_db(db)
#' close_db(db2)
#' file.remove(file.path(tempdir(), "mydb.db"))
create_db <- function(
  path = getwd(),
  db_name = NULL,
  ...,
  overwrite = FALSE,
  threads = NULL,
  memory_limit = NULL,
  temp_directory = NULL
) {
  check_arg(path, "character", n = 1, allow_null = TRUE)
  check_arg(db_name, "character", n = 1, allow_null = TRUE)
  check_arg(overwrite, "logical", n = 1)

  # Resolve the database file path. When db_name is given, path is treated as
  # the directory containing the database (or NULL for an in-memory database).
  if (!is.null(db_name)) {
    if (is.null(path) || identical(db_name, ":memory:")) {
      path <- db_name
    } else {
      path <- file.path(path, db_name)
    }
  }

  # If db already exists, remove it or throw an error
  if (!is.null(path) && file.exists(path)) {
    if (overwrite) {
      tryCatch(
        file.remove(path),
        warning = function(e) cli_abort(conditionMessage(e)),
        error = function(e) cli_abort(conditionMessage(e))
      )
    } else {
      cli_abort(c(
        "Database {.path {path}} already exists.",
        i = "Use {.code overwrite = TRUE} to overwrite."
      ))
    }
  }

  # Check if the parent directory exists
  if (!is.null(path) && !dir.exists(dirname(path))) {
    cli_abort("Directory {.path {dirname(path)}} does not exist.")
  }

  # Create a new db instance
  tryCatch(
    {
      db <- dbConnect(duckdb::duckdb(allow_extensions = TRUE), dbdir = path, ...)
    },
    error = function(e) {
      cli_abort("Could not create a database at {.path {path}}.") # nocov
    }
  )

  # json and icu are bundled with DuckDB >= 1.0; older versions need to
  # install them from the network (which fails on offline machines).
  if (utils::packageVersion("duckdb") < "1.0") {
    dbExecute(db, "INSTALL json; INSTALL icu;")
  }

  # Populate the db with empty tables
  tryCatch(
    {
      # Prefer the repository schema during development so pkgload::load_all()
      # does not silently use a stale installed copy of the package.
      fn <- file.path("inst", "extdata", "dbdef.sql")
      if (!file.exists(fn)) {
        fn <- system.file("extdata", "dbdef.sql", package = "mpathsenser")
      }
      script <- paste0(readLines(fn, warn = FALSE), collapse = "\n")
      dbExecute(db, script)
    },
    error = function(e) {
      # nocov start
      dbDisconnect(db)
      cli_abort(c(
        "Database definition file not found. The package is probably corrupted.",
        i = "Please reinstall {.pkg mpathsenser} using {.code install.packages(\"mpathsenser\")}"
      )) # nocov end
    }
  )

  # Configure connection defaults and define the derived local-time views
  .configure_duckdb(
    db,
    threads = threads,
    memory_limit = memory_limit,
    temp_directory = temp_directory
  )
  .create_local_views(db)

  return(db)
}

#' Open an mpathsenser database.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param path The path to the database file, or the directory in which the
#'   database resides when `db_name` is given.
#' @param db_name Optional name of the database file. When given, `path` is
#'   treated as the directory containing the database, just like
#'   [create_db()].
#' @param ... Additional arguments passed to [DBI::dbConnect()].
#' @param read_only Open the database in read-only mode. Recommended for
#'   connections used by dashboards, so that no accidental writes are possible.
#' @param threads The number of threads DuckDB may use. Defaults to `NULL`,
#'   which lets DuckDB use all available cores.
#' @param memory_limit Memory limit for the DuckDB instance (e.g. `"2GB"`).
#'   Defaults to `NULL`, which uses DuckDB's default of roughly 80% of RAM.
#' @param temp_directory Directory for temporary files (spill-to-disk). Use a
#'   local disk with sufficient space; do not point this at a synced directory
#'   such as OneDrive.
#'
#' @details On Windows, a database file is locked while any R process has it
#'   open. Close connections with [close_db()] when you are done. A database
#'   that is open in another process cannot be opened for writing; use
#'   `read_only = TRUE` to query it anyway (DuckDB allows multiple readers).
#'
#' @seealso [close_db()] for closing a database; [copy_db()] for copying (part of) a database;
#'   [optimize_db()] for re-ordering a database; [get_data()] for extracting data from a database.
#'
#' @returns A connection to an mpathsenser database.
#' @export
#'
#' @examples
#' # First create a database in a temporary directory
#' db <- create_db(file.path(tempdir(), "mydb.db"))
#' close_db(db)
#' DBI::dbIsValid(db) # db is closed
#'
#' # Then re-open it
#' db2 <- open_db(file.path(tempdir(), "mydb.db"))
#' DBI::dbIsValid(db2) # db is opened
#'
#' # The path can also be given as a directory plus a file name
#' db3 <- open_db(tempdir(), "mydb.db")
#' DBI::dbIsValid(db3)
#'
#' # Cleanup
#' close_db(db2)
#' close_db(db3)
#' file.remove(file.path(tempdir(), "mydb.db"))
open_db <- function(
  path,
  db_name = NULL,
  ...,
  read_only = FALSE,
  threads = NULL,
  memory_limit = NULL,
  temp_directory = NULL
) {
  check_arg(path, "character", n = 1, allow_null = TRUE)
  check_arg(db_name, "character", n = 1, allow_null = TRUE)
  check_arg(read_only, "logical", n = 1)

  # Like create_db(), the path can be given as a directory plus a file name.
  # An in-memory database makes no sense for open_db().
  if (!is.null(db_name)) {
    if (is.null(path)) {
      cli_abort("Cannot open an in-memory database with {.code open_db()}.")
    }
    path <- file.path(path, db_name)
  }

  if (is.null(path)) {
    cli_abort("Argument {.arg path} must point to a database file.")
  }

  # A directory is not a database file; point the user to the file inside it.
  # Without this check, DuckDB fails with a cryptic "Access is denied" error.
  if (dir.exists(path)) {
    dbs <- list.files(path, pattern = "\\.(db|duckdb)$", full.names = TRUE)
    dbs <- dbs[!dir.exists(dbs)]
    if (length(dbs) == 0) {
      cli_abort(c(
        "{.path {path}} is a directory, not a database file.",
        i = "No database file ({.code *.db} or {.code *.duckdb}) was found in this directory."
      ))
    }
    cli_abort(c(
      "{.path {path}} is a directory, not a database file.",
      i = if (length(dbs) == 1) {
        "Did you mean {.path {dbs}}?"
      } else {
        "Found {length(dbs)} database file{?s} in this directory: {.path {dbs}}."
      }
    ))
  }

  if (!file.exists(path)) {
    cli_abort("There is no database at {.path {path}}.")
  }

  db <- tryCatch(
    dbConnect(duckdb::duckdb(), dbdir = path, read_only = read_only, ...),
    error = function(e) {
      # Distinguish the most common cause: the file is open in another process
      # (which still allows read-only connections) from other failures.
      locked <- tryCatch(
        {
          db2 <- dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
          dbDisconnect(db2)
          TRUE
        },
        error = function(e2) FALSE
      )
      if (locked) {
        cli_abort(c(
          "Could not open the database at {.path {path}} because it is locked by another process.",
          x = conditionMessage(e),
          i = "Close the other connection first, or use {.code read_only = TRUE} to open a read-only connection."
        ))
      }
      cli_abort(c(
        "Could not open the database at {.path {path}}.",
        x = conditionMessage(e),
        i = "On Windows, the file may be temporarily locked by a syncing tool or antivirus scanner; wait a moment and try again."
      ))
    }
  )

  if (
    !DBI::dbExistsTable(db, "Participant") ||
      !DBI::dbExistsTable(db, "Activity", schema = "main")
  ) {
    dbDisconnect(db)
    cli_abort("The file {.path {path}} does not appear to be an {.pkg mpathsenser} database.")
  }

  .configure_duckdb(
    db,
    threads = threads,
    memory_limit = memory_limit,
    temp_directory = temp_directory
  )

  .create_local_views(db)

  return(db)
}

#' Close a database connection
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This is a convenience function that is simply a wrapper around [DBI::dbDisconnect()].
#'
#' @inheritParams get_data
#'
#' @seealso [open_db()] for opening an mpathsenser database.
#'
#' @returns Returns invisibly regardless of whether the database is active, valid,
#' or even exists.
#' @export
#'
#' @examples
#' # First create a database in a temporary directory
#' db <- create_db(tempdir(), "mydb.db")
#'
#' # Then close it
#' close_db(db)
#'
#' # You can even try to close a database that is already closed. This will not trigger an error.
#' close_db(db)
#'
#' # Cleanup
#' file.remove(file.path(tempdir(), "mydb.db"))
close_db <- function(db) {
  exists <- try(db, silent = TRUE)
  if (inherits(exists, "duckdb_connection") && !is.null(db)) {
    if (dbIsValid(db)) {
      dbDisconnect(db)
    }
  }
}

#' Copy (a subset of) a database to another database
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param source_db A mpathsenser database connection from where the data will be transferred.
#' @param target_db A mpathsenser database connection where the data will be transferred to.
#'   [create_db()] to create a new database.
#' @param sensor A character vector containing one or multiple sensors. See
#'   \code{\link[mpathsenser]{sensors}} for a list of available sensors. Defaults to `NULL`, which
#'   means all available sensors.
#'
#' @returns Returns a connection to `target_db`. Note that this is not the same connection as the
#' input `target_db`.
#' @export
#'
#' @examples
#' # First create two databases in a temporary directory
#' db1 <- create_db(tempdir(), "mydb1.db")
#' db2 <- create_db(tempdir(), "mydb2.db")
#'
#' # Populate the first database with some data
#' DBI::dbExecute(db1, "INSERT INTO Study VALUES ('study_1', 'default')")
#' DBI::dbExecute(db1, "INSERT INTO Participant VALUES (1, 'study_1')")
#' DBI::dbExecute(db1, "INSERT INTO ProcessedFiles(file_name, participant_id) VALUES ('f1', 1)")
#' DBI::dbExecute(db1, "INSERT INTO Activity(participant_id, time, confidence, type, source_file_id) VALUES(
#'                1, '2024-01-01 08:00:00', 100, 'WALKING', 1)")
#'
#' # Then copy the first database to the second database
#' db2 <- copy_db(db1, db2)
#'
#' # Check that the second database has the same data as the first database
#' get_data(db2, "Activity")
#'
#' # Cleanup
#' close_db(db1)
#' close_db(db2)
#' file.remove(file.path(tempdir(), "mydb1.db"))
#' file.remove(file.path(tempdir(), "mydb2.db"))
copy_db <- function(
  source_db,
  target_db,
  sensor = NULL
) {
  check_db(source_db, arg = "source_db")
  check_db(target_db, arg = "target_db")
  check_sensors(sensor, allow_null = TRUE, arg = "sensor")

  if (is.null(sensor)) {
    sensor <- sensors
  }
  sensor <- .physical_sensor(sensor)

  # Get target database path - for duckdb, we access the path via the driver
  DBI::dbDisconnect(target_db) # Disconnect to avoid locking issues
  target_path <- target_db@driver@dbdir

  # Attach new database to old database (DuckDB syntax)
  dbExecute(source_db, paste0("ATTACH '", target_path, "' AS new_db"))

  # Copy participants, studies, processed_files (using ON CONFLICT DO NOTHING for DuckDB)
  dbExecute(source_db, "INSERT INTO new_db.Study SELECT * FROM Study ON CONFLICT DO NOTHING")
  dbExecute(
    source_db,
    "INSERT INTO new_db.Participant SELECT * FROM Participant ON CONFLICT DO NOTHING"
  )
  dbExecute(
    source_db,
    "INSERT INTO new_db.ProcessedFiles SELECT * FROM ProcessedFiles ON CONFLICT DO NOTHING"
  )

  # Copy all specified sensors. The sensor tables have no unique constraints,
  # so ON CONFLICT cannot be used here.
  for (i in seq_along(sensor)) {
    dbExecute(
      source_db,
      paste0(
        "INSERT INTO new_db.",
        sensor[i],
        " SELECT * FROM ",
        sensor[i]
      )
    )
  }

  # Re-sync the file_id sequence of the target database: copying the rows does
  # not advance the sequence, so without this the next import would collide
  # with the copied file_id values.
  DBI::dbExecute(source_db, "ALTER TABLE new_db.ProcessedFiles ALTER file_id DROP DEFAULT")
  DBI::dbExecute(source_db, "DROP SEQUENCE IF EXISTS new_db.processed_files_seq")
  max_id <- DBI::dbGetQuery(
    source_db,
    "SELECT COALESCE(MAX(file_id), 0) + 1 AS m FROM new_db.ProcessedFiles"
  )[[1]]
  DBI::dbExecute(source_db, sprintf("CREATE SEQUENCE new_db.processed_files_seq START %d", max_id))
  # Use the unqualified sequence name in the default: the expression is stored
  # as-is, and 'new_db.' would not resolve after the database is reopened.
  DBI::dbExecute(
    source_db,
    "ALTER TABLE new_db.ProcessedFiles ALTER file_id SET DEFAULT nextval('processed_files_seq')"
  )

  # Detach
  dbExecute(source_db, "DETACH new_db")

  # Reopen the target_db
  target_db <- dbConnect(duckdb::duckdb(), target_path)

  target_db
}

#' @noRd
add_study <- function(db, study_id, data_format) {
  check_db(db)

  # Filter out NULL values in vectorized inputs
  valid <- !is.na(study_id) & !is.null(study_id)
  if (!any(valid)) {
    return(0)
  }

  study_id <- study_id[valid]
  data_format <- data_format[valid]

  dbExecute(
    db,
    paste(
      "INSERT INTO Study(study_id, data_format)",
      "VALUES($1, $2)",
      "ON CONFLICT DO NOTHING;"
    ),
    list(study_id, data_format)
  )
}

#' @noRd
add_participant <- function(db, participant_id, study_id) {
  check_db(db)

  # Filter out NULL values in vectorized inputs
  valid <- !is.na(participant_id) & !is.null(participant_id)
  if (!any(valid)) {
    return(0)
  }

  participant_id <- participant_id[valid]
  study_id <- study_id[valid]

  dbExecute(
    db,
    paste(
      "INSERT INTO Participant(participant_id, study_id)",
      "VALUES($1, $2)",
      "ON CONFLICT DO NOTHING;"
    ),
    list(participant_id, study_id)
  )
}

#' @noRd
add_processed_files <- function(
  db,
  file_name,
  participant_id,
  sense_version = NULL,
  file_size_bytes = NULL,
  modified_at = NULL
) {
  check_db(db)

  # Filter out NULL values in vectorized inputs
  valid <- !is.na(file_name) & !is.null(file_name)
  if (!any(valid)) {
    return(0)
  }

  file_name <- file_name[valid]
  participant_id <- participant_id[valid]

  # NULL parameters are bound as NA so that DBI can bind all values
  n <- length(file_name)
  sense_version <- sense_version %||% rep(NA_integer_, n)
  file_size_bytes <- file_size_bytes %||% rep(NA_real_, n)
  modified_at <- modified_at %||% as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")

  dbExecute(
    db,
    paste(
      "INSERT INTO ProcessedFiles(file_name, participant_id, sense_version, file_size_bytes, modified_at)",
      "VALUES($1, $2, $3, $4, $5)",
      "ON CONFLICT DO NOTHING;"
    ),
    list(
      file_name,
      participant_id,
      sense_version,
      file_size_bytes,
      modified_at
    )
  )
}

#' @noRd
clear_db <- function(db) {
  check_db(db)
  tables <- c(sensors, "ProcessedFiles", "Participant", "Study")
  res <- vapply(
    tables,
    \(x) {
      dbExecute(
        db,
        paste0("DELETE FROM ", if (x %in% sensors) "" else "", x, " WHERE 1;")
      )
    },
    numeric(1)
  )
  names(res) <- tables

  # Reset the file_id sequence so a cleared database starts at 1 again
  DBI::dbExecute(db, "ALTER TABLE ProcessedFiles ALTER file_id DROP DEFAULT")
  DBI::dbExecute(db, "DROP SEQUENCE IF EXISTS processed_files_seq")
  DBI::dbExecute(db, "CREATE SEQUENCE processed_files_seq START 1")
  DBI::dbExecute(
    db,
    "ALTER TABLE ProcessedFiles ALTER file_id SET DEFAULT nextval('processed_files_seq')"
  )

  res
}

#' Re-order the data in a database for faster processing
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Rewrites the sensor tables ordered by `participant_id` and `time`. DuckDB
#'   stores columnar data and maintains per-block zonemaps; ordering the data
#'   this way improves zonemap pruning (e.g. for time range queries) and
#'   compression. Run this after large imports for best query performance.
#'
#' @param db A database connection, as created by [create_db()].
#' @param sensors A character vector of one or multiple sensors. Use
#'   `sensors = NULL` for all sensors. See \link[mpathsenser]{sensors} for a
#'   list of all available sensors.
#' @param .progress Logical; whether to show a progress bar. Defaults to `TRUE`.
#'
#' @returns Invisibly returns `TRUE`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Open a database connection
#' db <- open_db("path/to/db")
#'
#' # Re-order all sensor tables
#' optimize_db(db)
#'
#' # Or only the Accelerometer table
#' optimize_db(db, sensors = "Accelerometer")
#'
#' # Remember to close the connection
#' close_db(db)
#' }
optimize_db <- function(db, sensors = NULL, .progress = TRUE) {
  check_db(db)
  check_sensors(sensors, allow_null = TRUE)
  check_arg(.progress, "logical", n = 1)

  if (is.null(sensors)) {
    sensors <- mpathsenser::sensors
  }
  sensors <- setdiff(.physical_sensor(sensors), "Timezone")

  quote_raw <- function(name) {
    as.character(DBI::dbQuoteIdentifier(db, DBI::Id(schema = "main", table = name)))
  }
  quote_table <- function(name) {
    as.character(DBI::dbQuoteIdentifier(db, name))
  }
  is_sorted <- function(sensor) {
    DBI::dbGetQuery(
      db,
      sprintf(
        "SELECT COUNT(*) AS n FROM (
           SELECT participant_id, time,
                  LAG(participant_id) OVER (ORDER BY rowid) AS previous_participant_id,
                  LAG(time) OVER (ORDER BY rowid) AS previous_time
           FROM %s
         ) q
         WHERE previous_participant_id IS NOT NULL
           AND (participant_id < previous_participant_id
             OR (participant_id = previous_participant_id AND time < previous_time))",
        quote_raw(sensor)
      )
    )$n[[1]] ==
      0
  }

  if (.progress && length(sensors) > 0) {
    cli::cli_progress_bar("Optimizing sensor tables...", total = length(sensors))
    on.exit(cli::cli_progress_done(), add = TRUE)
  }

  DBI::dbWithTransaction(db, {
    for (sensor in sensors) {
      if (is_sorted(sensor)) {
        if (.progress) {
          cli::cli_progress_update()
        }
        next
      }

      source <- quote_raw(sensor)
      source_table <- quote_table(sensor)
      temporary_id <- quote_raw(paste0(sensor, "_optimize_tmp"))

      DBI::dbExecute(db, sprintf("DROP TABLE IF EXISTS %s", temporary_id))
      DBI::dbExecute(
        db,
        sprintf("CREATE TABLE %s AS SELECT * FROM %s WHERE FALSE", temporary_id, source)
      )

      not_null <- DBI::dbGetQuery(
        db,
        "SELECT column_name FROM information_schema.columns
         WHERE table_schema = 'main' AND table_name = ? AND is_nullable = 'NO'
         ORDER BY ordinal_position",
        params = list(sensor)
      )$column_name
      for (column in not_null) {
        DBI::dbExecute(
          db,
          sprintf(
            "ALTER TABLE %s ALTER COLUMN %s SET NOT NULL",
            temporary_id,
            as.character(DBI::dbQuoteIdentifier(db, column))
          )
        )
      }

      DBI::dbExecute(
        db,
        sprintf(
          "INSERT INTO %s SELECT * FROM %s ORDER BY participant_id, time",
          temporary_id,
          source
        )
      )
      DBI::dbExecute(db, sprintf("DROP TABLE %s", source))
      DBI::dbExecute(
        db,
        sprintf("ALTER TABLE %s RENAME TO %s", temporary_id, source_table)
      )
      if (.progress) cli::cli_progress_update()
    }
  })

  invisible(TRUE)
}

#' @rdname optimize_db
optimise_db <- function(db, sensors = NULL, .progress = TRUE) {
  optimize_db(db, sensors, .progress = .progress)
}

#' Deduplicate measurements in a database
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#'   Removes duplicate measurements from the sensor tables. Per measurement
#'   key (participant and time, plus sensor-specific extras such as the app
#'   for AppUsage), the most recent row is kept: the row of the newest source
#'   file, and within that file the last recorded row. This is an upsert
#'   (INSERT OR REPLACE) semantics, matching the historical behaviour of the
#'   SQLite-based importer — a later measurement overwrites an earlier one
#'   with the same key, whether the duplicate came from the same file or a
#'   different one. Rows of older files with the same key are removed, rows
#'   whose key occurs only once are never touched. This runs automatically at
#'   the end of [read_mpath_sense()]; run it manually to clean up duplicates
#'   left behind by an interrupted import. Unlike the automatic pass, which
#'   only examines the key groups of the rows that were just imported, this
#'   function scans the entire sensor tables, so it also resolves duplicates
#'   that predate the current import (e.g. left behind by an interrupted run).
#'
#' @param db A database connection, as created by [create_db()].
#' @param sensors A character vector of one or multiple sensors. Use
#'   `sensors = NULL` for all sensors. See \link[mpathsenser]{sensors} for a
#'   list of all available sensors.
#' @param debug Whether to print a message per sensor table.
#'
#' @returns A named integer vector with the number of duplicate rows removed
#'   per sensor.
#' @export
#'
#' @examples
#' \dontrun{
#' # Open a database connection
#' db <- open_db("path/to/db")
#'
#' # Remove duplicate measurements from all sensors
#' deduplicate_db(db)
#'
#' # Or only from the Accelerometer table
#' deduplicate_db(db, sensors = "Accelerometer")
#'
#' # Remember to close the connection
#' close_db(db)
#' }
deduplicate_db <- function(db, sensors = NULL, debug = FALSE) {
  check_db(db)
  check_sensors(sensors, allow_null = TRUE)
  check_arg(debug, "logical", n = 1)

  if (is.null(sensors)) {
    sensors <- mpathsenser::sensors
  }
  sensors <- .physical_sensor(sensors)

  .read_dedup(db, sensors, debug = debug)
}

### ----------- Getters ---------------

#' Get all processed files from a database
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param db A database connection, as created by [create_db()].
#'
#' @returns A data frame containing the `file_id`, `file_name`, `participant_id`,
#'   `sense_version`, `file_size_bytes`, and `modified_at` of the processed
#'   files.
#' @export
#'
#' @examples
#' # Create a database
#' db <- create_db(tempdir(), "mydb.db")
#'
#' # Add some processed files
#' DBI::dbExecute(db, "INSERT INTO Study VALUES('study1', 'data_format1')")
#' DBI::dbExecute(db, "INSERT INTO Participant VALUES(1, 'study1')")
#' DBI::dbExecute(db, "INSERT INTO ProcessedFiles(file_name, participant_id) VALUES('file1', 1)")
#'
#' # Get the processed files
#' get_processed_files(db)
#'
#' # Cleanup
#' close_db(db)
#' file.remove(file.path(tempdir(), "mydb.db"))
get_processed_files <- function(db) {
  check_db(db)

  DBI::dbReadTable(db, "ProcessedFiles")
}

#' Get all participants
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param db db A database connection, as created by [create_db()].
#' @param lazy Whether to evaluate lazily using \link[dbplyr]{dbplyr}.
#'
#' @returns A data frame containing all `participant_id` and `study_id`.
#' @export
#'
#' @examples
#' # Create a database
#' db <- create_db(tempdir(), "mydb.db")
#'
#' # Add some participants
#' DBI::dbExecute(db, "INSERT INTO Study VALUES('study1', 'data_format1')")
#' DBI::dbExecute(db, "INSERT INTO Participant VALUES(1, 'study1')")
#'
#' # Get the participants
#' get_participants(db)
#'
#' # Cleanup
#' close_db(db)
#' file.remove(file.path(tempdir(), "mydb.db"))
get_participants <- function(db, lazy = FALSE) {
  check_db(db)
  check_arg(lazy, "logical", n = 1)

  if (lazy) {
    dplyr::tbl(db, "Participant")
  } else {
    DBI::dbReadTable(db, "Participant")
  }
}

#' Get all studies
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param db db A database connection, as created by [create_db()].
#' @param lazy Whether to evaluate lazily using \link[dbplyr]{dbplyr}.
#'
#' @returns A data frame containing all studies.
#' @export
#'
#' @examples
#' # Create a database
#' db <- create_db(tempdir(), "mydb.db")
#'
#' # Add some studies
#' DBI::dbExecute(db, "INSERT INTO Study VALUES('study1', 'data_format1')")
#'
#' # Get the studies
#' get_studies(db)
#'
#' # Cleanup
#' close_db(db)
#' file.remove(file.path(tempdir(), "mydb.db"))
get_studies <- function(db, lazy = FALSE) {
  check_db(db)
  check_arg(lazy, "logical", n = 1)

  if (lazy) {
    dplyr::tbl(db, "Study")
  } else {
    DBI::dbReadTable(db, "Study")
  }
}

#' Get the number of rows per sensor in a mpathsenser database
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param db db A database connection, as created by [create_db()].
#' @param sensor A character vector of one or multiple vectors. Use `sensor = NULL` for all
#'   sensors. See \link[mpathsenser]{sensors} for a list of all available sensors.
#' @param participant_id A single participant identifier (stored as an unsigned integer; an
#'   integer, numeric, or character value is accepted). Use
#'   [get_participants()] to retrieve all participants from the database. Leave empty to get data
#'   for all participants.
#' @param start_date Optional search window specifying date where to begin search. Must be
#'   convertible to date using [base::as.Date()]. Use [first_date()] to find the date of the first
#'   entry for a participant.
#' @param end_date Optional search window specifying date where to end search. Must be convertible
#'   to date using [base::as.Date()]. Use [last_date()] to find the date of the last entry for a
#'   participant.
#'
#' @returns A named vector containing the number of rows for each sensor.
#' @export
#'
#' @examples
#' \dontrun{
#' # Open a database connection
#' db <- open_db("path/to/db")
#'
#' # Get the number of rows for all sensors
#' get_nrows(db, sensor = NULL)
#'
#' # Get the number of rows for the Accelerometer and Gyroscope sensors
#' get_nrows(db, c("Accelerometer", "Gyroscope"))
#'
#' # Remember to close the connection
#' close_db(db)
#' }
get_nrows <- function(
  db,
  sensor = NULL,
  participant_id = NULL,
  start_date = NULL,
  end_date = NULL
) {
  check_db(db)
  check_sensors(sensor, allow_null = TRUE)

  if (is.null(sensor)) {
    sensor <- sensors
  }

  vapply(
    sensor,
    function(x) {
      get_data(db, x, participant_id, start_date, end_date) |>
        dplyr::count() |>
        pull(n)
    },
    numeric(1)
  )
}
