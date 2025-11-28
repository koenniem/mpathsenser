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
  "AirQuality",
  "AppUsage",
  "Battery",
  "Bluetooth",
  "Calendar",
  "Connectivity",
  "Device",
  "Error",
  "Geofence",
  "Gyroscope",
  "Heartbeat",
  "InstalledApps",
  "Keyboard",
  "Light",
  "Location",
  "Memory",
  "Mobility",
  "Noise",
  "Pedometer",
  "PhoneLog",
  "Screen",
  "TextMessage",
  "Timezone",
  "Weather",
  "Wifi"
)

#' Create a new mpathsenser database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param path The path to the database.
#' @param db_name The name of the database.
#' @param overwrite In case a database with `db_name` already exists, indicate whether it should
#' be overwritten or not. Otherwise, this option is ignored.
#'
#' @returns A database connection using prepared database schemas.
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
create_db <- function(path = getwd(), db_name = "sense.db", overwrite = FALSE) {
  check_arg(path, "character", n = 1, allow_null = TRUE)
  check_arg(db_name, "character", n = 1)
  check_arg(overwrite, "logical", n = 1)

  # Merge path and file name
  if (!is.null(path)) {
    db_name <- normalizePath(file.path(path, db_name), mustWork = FALSE)
  }

  # If db already exists, remove it or throw an error
  if (file.exists(db_name)) {
    if (overwrite) {
      tryCatch(
        file.remove(db_name),
        warning = function(e) abort(as.character(e)),
        error = function(e) abort(as.character(e))
      )
    } else {
      abort(c(
        paste("Database", db_name, "already exists."),
        i = " Use overwrite = TRUE to overwrite."
      ))
    }
  }

  # Check if path exists
  if (!dir.exists(dirname(db_name))) {
    abort(paste0("Directory ", dirname(db_name), " does not exist."))
  }

  # Create a new db instance
  tryCatch(
    {
      db <- dbConnect(duckdb::duckdb(), db_name)
    },
    error = function(e) {
      abort(paste0("Could not create a database in ", db_name)) # nocov
    }
  )

  # Populate the db with empty tables
  tryCatch(
    {
      fn <- system.file("extdata", "dbdef.sql", package = "mpathsenser")
      if (fn == "") {
        # Fall back to local inst folder when package isn't installed
        fn <- file.path(getwd(), "inst", "extdata", "dbdef.sql")
      }
      script <- strsplit(paste0(readLines(fn, warn = FALSE), collapse = "\n"), "\n\n")[[1]]
      for (statement in script) {
        dbExecute(db, statement)
      }
    },
    error = function(e) {
      # nocov start
      dbDisconnect(db, shutdown = TRUE)
      abort(c(
        "Database definition file not found. The package is probably corrupted.",
        i = "Please reinstall mpathsenser using `install.packages(\"mpathsenser\")`"
      )) # nocov end
    }
  )

  return(db)
}

#' Open an mpathsenser database.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param path The path to the database. Use NULL to use the full path name in db_name.
#' @param db_name The name of the database.
#'
#' @seealso [close_db()] for closing a database; [copy_db()] for copying (part of) a database;
#'   [index_db()] for indexing a database; [get_data()] for extracting data from a database.
#'
#' @returns A connection to an mpathsenser database.
#' @export
#'
#' @examples
#' # First create a database in a temporary directory
#' db <- create_db(tempdir(), "mydb.db")
#' close_db(db)
#' DBI::dbIsValid(db) # db is closed
#'
#' # Then re-open it
#' db2 <- open_db(tempdir(), "mydb.db")
#' DBI::dbIsValid(db2) # db is opened
#'
#' # Cleanup
#' close_db(db2)
#' file.remove(file.path(tempdir(), "mydb.db"))
open_db <- function(path = getwd(), db_name = "sense.db") {
  check_arg(path, "character", n = 1, allow_null = TRUE)
  check_arg(db_name, c("character", "integerish"), n = 1)

  # Merge path and file name
  if (!is.null(path)) {
    db_name <- suppressWarnings(normalizePath(file.path(path, db_name)))
  }

  if (!file.exists(db_name)) {
    abort("There is no such file")
  }
  db <- dbConnect(duckdb::duckdb(), db_name, read_only = TRUE)
  if (!DBI::dbExistsTable(db, "Participant")) {
    dbDisconnect(db, shutdown = TRUE)
    abort("Sorry, this does not appear to be a mpathsenser database.")
  }
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
      dbDisconnect(db, shutdown = TRUE)
    }
  }
}

#' Create indexes for an mpathsenser database
#'
#' @description `r lifecycle::badge("stable")`
#'
#'   Create indexes for an mpathsenser database on the `participant_id`, `date`, and a combination
#'   of these variable for all the tables in the database. This will speed up queries that use these
#'   variables in the `WHERE` clause.
#'
#' @inheritParams get_data
#'
#' @returns Returns `TRUE` invisibly, called for side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' # First create a database in a temporary directory
#' db <- create_db(tempdir(), "mydb.db")
#'
#' # Import some files
#' import(path = "path/to/jsons", db = db)
#'
#' # Then index it to speed up the database
#' index_db(db)
#' }
index_db <- function(db) {
  check_db(db)

  tryCatch(
    {
      fn <- system.file("extdata", "indexes.sql", package = "mpathsenser")
      if (fn == "") {
        # Fall back to local inst folder when package isn't installed
        fn <- file.path(getwd(), "inst", "extdata", "indexes.sql")
      }
      script <- strsplit(paste0(readLines(fn, warn = FALSE), collapse = "\n"), "\n\n")[[1]]
      for (statement in script) {
        dbExecute(db, statement)
      }
    },
    error = function(e) {
      abort(as.character(e))
    }
  )

  invisible(TRUE)
}

#' Vacuum a database
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is a convenience function that calls the `VACUUM` command on a database. This command will
#' rebuild the database file, repacking it into a minimal amount of disk space.
#'
#' @inheritParams get_data
#'
#' @return  a scalar numeric that specifies the number of rows affected by the vacuum.
#' @export
#'
#' @examples
#' # Create a database in a temporary directory
#' db <- create_db(tempdir(), "mydb.db")
#'
#' # Assuming that we have imported some data into the database, we can vacuum it
#' vacuum_db(db)
#'
#' # Cleanup
#' close_db(db)
#' file.remove(file.path(tempdir(), "mydb.db"))
vacuum_db <- function(db) {
  check_db(db)
  dbExecute(db, "VACUUM")
}

#' Copy (a subset of) a database to another database
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param source_db A mpathsenser database connection from where the data will be transferred.
#' @param target_db A mpathsenser database connection where the data will be transferred to.
#'   [create_db()] to create a new database.
#' @param sensor A character vector containing one or multiple sensors. See
#'   \code{\link[mpathsenser]{sensors}} for a list of available sensors. Use "All" for all available
#'   sensors.
#'
#' @returns Returns `TRUE` invisibly, called for side effects.
#' @export
#'
#' @examples
#' # First create two databases in a temporary directory
#' db1 <- create_db(tempdir(), "mydb1.db")
#' db2 <- create_db(tempdir(), "mydb2.db")
#'
#' # Populate the first database with some data
#' DBI::dbExecute(db1, "INSERT INTO Study VALUES ('study_1', 'default')")
#' DBI::dbExecute(db1, "INSERT INTO Participant VALUES ('1', 'study_1')")
#' DBI::dbExecute(db1, "INSERT INTO Activity VALUES(
#'                '123', '1', '2024-01-01', '08:00:00', '100', 'WALKING')")
#'
#' # Then copy the first database to the second database
#' copy_db(db1, db2)
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
  sensor = "All"
) {
  check_db(source_db, arg = "source_db")
  check_db(target_db, arg = "target_db")
  check_arg(sensor, "character")

  # Check sensors
  if (length(sensor) == 1 && sensor == "All") {
    sensor <- sensors
  } else {
    missing <- sensor[!(sensor %in% sensors)]
    if (length(missing) != 0) {
      abort(paste0("Sensor(s) ", paste0(missing, collapse = ", "), " not found."))
    }
  }

  # Get target database path - for duckdb, we access the path via the driver
  target_path <- target_db@driver@dbdir

  # Attach new database to old database (DuckDB syntax)
  dbExecute(source_db, paste0("ATTACH '", target_path, "' AS new_db"))

  # Copy participants, studies, processed_files (using ON CONFLICT DO NOTHING for DuckDB)
  dbExecute(source_db, "INSERT INTO new_db.Study SELECT * FROM Study ON CONFLICT DO NOTHING")
  dbExecute(source_db, "INSERT INTO new_db.Participant SELECT * FROM Participant ON CONFLICT DO NOTHING")
  dbExecute(
    source_db,
    "INSERT INTO new_db.ProcessedFiles SELECT * FROM ProcessedFiles ON CONFLICT DO NOTHING"
  )

  # Copy all specified sensors
  for (i in seq_along(sensor)) {
    dbExecute(
      source_db,
      paste0(
        "INSERT INTO new_db.",
        sensor[i],
        " SELECT * FROM ",
        sensor[i],
        " ON CONFLICT DO NOTHING"
      )
    )
  }

  # Detach
  dbExecute(source_db, "DETACH new_db")

  return(invisible(TRUE))
}

#' @noRd
add_study <- function(db, study_id, data_format) {
  check_db(db)

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
add_processed_files <- function(db, file_name, study_id, participant_id) {
  check_db(db)

  dbExecute(
    db,
    paste(
      "INSERT INTO ProcessedFiles(file_name, study_id, participant_id)",
      "VALUES($1, $2, $3)",
      "ON CONFLICT DO NOTHING;"
    ),
    list(
      file_name,
      study_id,
      participant_id
    )
  )
}

#' @noRd
clear_db <- function(db) {
  check_db(db)
  tables <- c("Study", "Participant", "ProcessedFiles", sensors)
  res <- vapply(
    tables,
    \(x) dbExecute(db, paste0("DELETE FROM ", x, " WHERE 1;")),
    numeric(1)
  )
  names(res) <- tables
  res
}

### ----------- Getters ---------------

#' Get all processed files from a database
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param db A database connection, as created by [create_db()].
#'
#' @returns A data frame containing the `file_name`, `participant_id`, and `study_id` of the
#'   processed files.
#' @export
#'
#' @examples
#' # Create a database
#' db <- create_db(tempdir(), "mydb.db")
#'
#' # Add some processed files
#' DBI::dbExecute(db, "INSERT INTO Study VALUES('study1', 'data_format1')")
#' DBI::dbExecute(db, "INSERT INTO Participant VALUES('participant1', 'study1')")
#' DBI::dbExecute(db, "INSERT INTO ProcessedFiles VALUES('file1', 'participant1', 'study1')")
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
#' DBI::dbExecute(db, "INSERT INTO Participant VALUES('participant1', 'study1')")
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
#' @param sensor A character vector of one or multiple vectors. Use `sensor = "All"` for all
#'   sensors. See \link[mpathsenser]{sensors} for a list of all available sensors.
#' @param participant_id A character string identifying a single participant. Use
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
  sensor = "All",
  participant_id = NULL,
  start_date = NULL,
  end_date = NULL
) {
  check_db(db)
  check_arg(sensor, "character", allow_null = TRUE)

  if (is.null(sensor) || sensor[[1]] == "All") {
    sensor <- sensors
  }

  vapply(
    sensor,
    function(x) {
      get_data(db, x, participant_id, start_date, end_date) |>
        dplyr::count() |>
        pull(n)
    },
    integer(1)
  )
}
