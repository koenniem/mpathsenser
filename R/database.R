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
  "Accelerometer", "AirQuality", "Activity", "AppUsage", "Battery", "Bluetooth", "Calendar",
  "Connectivity", "Device", "Error", "Geofence", "Gyroscope", "Heartbeat", "InstalledApps",
  "Keyboard", "Light", "Location", "Memory", "Mobility", "Noise", "Pedometer", "PhoneLog",
  "Screen", "TextMessage", "Timezone", "Weather", "Wifi"
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
      tryCatch(file.remove(db_name),
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
      db <- dbConnect(RSQLite::SQLite(), db_name, cache_size = 8192)
    },
    error = function(e) {
      abort(paste0("Could not create a database in ", db_name)) # nocov
    }
  )


  # Populate the db with empty tables
  tryCatch(
    {
      fn <- system.file("extdata", "dbdef.sql", package = "mpathsenser")
      script <- strsplit(paste0(readLines(fn, warn = FALSE), collapse = "\n"), "\n\n")[[1]]
      for (statement in script) {
        dbExecute(db, statement)
      }
    },
    error = function(e) { # nocov start
      dbDisconnect(db)
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
  db <- dbConnect(RSQLite::SQLite(), db_name, cache_size = 8192)
  if (!DBI::dbExistsTable(db, "Participant")) {
    dbDisconnect(db)
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
close_db <- function(db) {
  exists <- try(db, silent = TRUE)
  if (inherits(exists, "SQLiteConnection") && !is.null(db)) {
    if (dbIsValid(db)) {
      dbDisconnect(db)
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
#' @returns No return value, called for side effects.
#' @export
index_db <- function(db) {
  check_db(db)

  tryCatch(
    {
      fn <- system.file("extdata", "indexes.sql", package = "mpathsenser")
      script <- strsplit(paste0(readLines(fn, warn = FALSE), collapse = "\n"), "\n\n")[[1]]
      for (statement in script) {
        dbExecute(db, statement)
      }
    },
    error = function(e) {
      abort(as.character(e))
    }
  )
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
#' @returns No return value, called for side effects.
#' @export
copy_db <- function(source_db,
                    target_db,
                    sensor = "All") {

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

  # Attach new database to old database
  dbExecute(source_db, paste0("ATTACH DATABASE '", target_db@dbname, "' AS new_db"))

  # Copy participants, studies, processed_files
  dbExecute(source_db, "INSERT OR IGNORE INTO new_db.Study SELECT * FROM Study")
  dbExecute(source_db, "INSERT OR IGNORE INTO new_db.Participant SELECT * FROM Participant")
  dbExecute(
    source_db,
    "INSERT OR IGNORE INTO new_db.ProcessedFiles SELECT * FROM ProcessedFiles"
  )


  # Copy all specified sensors
  for (i in seq_along(sensor)) {
    dbExecute(source_db, paste0(
      "INSERT OR IGNORE INTO new_db.", sensor[i],
      " SELECT * FROM ", sensor[i]
    ))
  }

  # Detach
  dbExecute(source_db, "DETACH DATABASE new_db")

  return(invisible(TRUE))
}

#' @noRd
add_study <- function(db, study_id, data_format) {
  check_db(db)

  dbExecute(
    db,
    paste(
      "INSERT OR IGNORE INTO Study(study_id, data_format)",
      "VALUES(:study_id, :data_format);"
    ),
    list(study_id = study_id, data_format = data_format)
  )
}

#' @noRd
add_participant <- function(db, participant_id, study_id) {
  check_db(db)

  dbExecute(
    db,
    paste(
      "INSERT OR IGNORE INTO Participant(participant_id, study_id)",
      "VALUES(:participant_id, :study_id);"
    ),
    list(participant_id = participant_id, study_id = study_id)
  )
}

#' @noRd
add_processed_files <- function(db, file_name, study_id, participant_id) {
  check_db(db)

  dbExecute(
    db,
    paste(
      "INSERT OR IGNORE INTO ProcessedFiles(file_name, study_id, participant_id)",
      "VALUES(:file_name, :study_id, :participant_id);"
    ),
    list(
      file_name = file_name,
      study_id = study_id,
      participant_id = participant_id
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
get_nrows <- function(
    db,
    sensor = "All",
    participant_id = NULL,
    start_date = NULL,
    end_date = NULL) {
  check_db(db)
  check_arg(sensor, "character", allow_null = TRUE)

  if (is.null(sensor) || sensor[[1]] == "All") {
    sensor <- sensors
  }

  vapply(sensor, function(x) {
    get_data(db, x, participant_id, start_date, end_date) |>
      dplyr::count() |>
      pull(n)
  }, integer(1))
}
