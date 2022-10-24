#' Available Sensors
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' A list containing all available sensors in this package you can work with. This variable was
#' created so it is easier to use in your own functions, e.g. to loop over sensors.
#'
#' @return A character vector containing all sensor names supported by \code{mpathsenser}.
#' @examples
#' sensors
#' @export sensors
sensors <- c(
  "Accelerometer", "AirQuality", "Activity", "AppUsage", "Battery", "Bluetooth",
  "Calendar", "Connectivity", "Device", "Error", "Geofence", "Gyroscope",
  "InstalledApps", "Keyboard", "Light", "Location", "Memory", "Mobility", "Noise",
  "Pedometer", "PhoneLog", "Screen", "TextMessage", "Weather", "Wifi"
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
#' @return A database connection using prepared database schemas.
#' @export
create_db <- function(path = getwd(), db_name = "sense.db", overwrite = FALSE) {
  if (!is.character(db_name)) abort("Argument db_name must be a filename")
  if (!(is.null(path) | is.character(path))) abort("Argument path must be a character string")

  # Merge path and file name
  if (!is.null(path)) {
    db_name <- suppressWarnings(normalizePath(file.path(path, db_name)))
  }

  # If db already exists, remove it or throw an error
  if (file.exists(db_name)) {
    if (overwrite) {
      tryCatch(file.remove(db_name),
        warning = function(e) abort(as.character(e)),
        error = function(e) abort(as.character(e))
      )
    } else {
      abort(paste("Database", db_name, "already exists. Use overwrite = TRUE to overwrite."))
    }
  }

  # Create a new db instance
  db <- dbConnect(RSQLite::SQLite(), db_name, cache_size = 8192)

  # Populate the db with empty tables
  tryCatch(
    {
      fn <- system.file("extdata", "dbdef.sql", package = "mpathsenser")
      script <- strsplit(paste0(readLines(fn, warn = FALSE), collapse = "\n"), "\n\n")[[1]]
      for (statement in script) {
        dbExecute(db, statement)
      }
    },
    error = function(e) {
      dbDisconnect(db)
      abort(as.character(e))
    }
  )

  return(db)
}

#' Open an mpathsenser database.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param path The path to the database. Use NULL to use the full path name in db_name.
#' @param db_name The name of the database.
#'
#' @seealso \code{\link[mpathsenser]{close_db}} for closing a database;
#' \code{\link[mpathsenser]{copy_db}} for copying (part of) a database;
#' \code{\link[mpathsenser]{index_db}} for indexing a database;
#' \code{\link[mpathsenser]{get_data}} for extracting data from a database.
#'
#' @return A connection to an mpathsenser database.
#' @export
open_db <- function(path = getwd(), db_name = "sense.db") {
  if (!is.character(db_name)) abort("Argument db_name must be a filename")
  if (!(is.null(path) | is.character(path))) abort("Argument path must be a character string")

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
#' This is a convenience function that is simply a wrapper around \link[DBI]{dbDisconnect}.
#'
#' @inheritParams get_data
#'
#' @seealso \code{\link[mpathsenser]{open_db}} for opening an mpathsenser database.
#'
#' @return \code{close_db} returns invisibly regardless of whether the database is active, valid,
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

#' Create indexes for a mpathsenser database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @inheritParams get_data
#'
#' @return No return value, called for side effects.
#' @export
index_db <- function(db) {
  if (is.null(db) || !dbIsValid(db)) abort("Database connection is not valid")

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

vacuum_db <- function(db) {
  if (is.null(db) || !dbIsValid(db)) abort("Database connection is not valid")
  dbExecute(db, "VACUUM")
}

#' Copy (a subset of) a database to another database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param source_db A mpathsenser database connection from where the data will be transferred.
#' @param target_db A mpathsenser database connection where the data will be transferred to.
#' \link[mpathsenser]{create_db} to create a new database.
#' @param sensor A character vector containing one or multiple sensors. See
#' \code{\link[mpathsenser]{sensors}} for a list of available sensors. Use "All" for all available
#' sensors.
#' @param path  `r lifecycle::badge("deprecated")`: This argument was used when database
#'   creation in \code{copy_db} was still supported. As this functionality is deprecated,
#'   \code{overwrite_db} is now ignored and will be removed in future versions.
#' @param db_name  `r lifecycle::badge("deprecated")`: Creating new databases on the fly has been
#'   deprecated as it is better to separate the two functions. You must now create a new database
#'   using \code{\link[mpathsenser]{create_db}} or reuse an existing one.
#'
#' @return No return value, called for side effects.
#' @export
copy_db <- function(
    source_db,
    target_db,
    sensor = "All",
    path = deprecated(),
    db_name = deprecated()
) {

  # Handle old database creation functionality
  if (lifecycle::is_present(path)) {
    lifecycle::deprecate_warn(when = "1.1.1",
                              what = "copy_db(path)",
                              details = c(
                                i = "Please create a database using `create_db()` first."
                              ))
  }
  if (lifecycle::is_present(db_name)) {
    lifecycle::deprecate_stop(when = "1.1.1",
                              what = "copy_db(db_name)",
                              details = c(
                                i = "Please create a database using `create_db()` first."
                              ))
  }

  if (!dbIsValid(source_db)) abort("The connection to source_db is not valid.")
  if (!dbIsValid(target_db)) abort("The connection to target_db is not valid.")
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

add_study <- function(db, data) {
  dbExecute(
    db,
    "INSERT OR IGNORE INTO Study(study_id, data_format)
  VALUES(:study_id, :data_format);",
    list(study_id = data$study_id, data_format = data$data_format)
  )
}

add_participant <- function(db, data) {
  dbExecute(
    db,
    "INSERT OR IGNORE INTO Participant(participant_id, study_id)
  VALUES(:participant_id, :study_id);",
    list(participant_id = data$participant_id, study_id = data$study_id)
  )
}

add_processed_files <- function(db, data) {
  dbExecute(
    db,
    "INSERT OR IGNORE INTO ProcessedFiles(file_name, study_id, participant_id)
  VALUES(:file_name, :study_id, :participant_id);",
    list(
      file_name = data$file_name,
      study_id = data$study_id,
      participant_id = data$participant_id
    )
  )
}

clear_sensors_db <- function(db) {
  res <- lapply(sensors, function(x) dbExecute(db, paste0("DELETE FROM ", x, " WHERE 1;")))
  names(res) <- sensors
  res
}

### ----------- Getters ---------------


#' Get all processed files from a database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param db A database connection, as created by \link[mpathsenser]{create_db}.
#'
#' @return A data frame containing the \code{file_name}, \code{participant_id}, and \code{study_id}
#' of the processed files.
#' @export
get_processed_files <- function(db) {
  if (!dbIsValid(db)) abort("Database connection is not valid")
  DBI::dbReadTable(db, "ProcessedFiles")
}

#' Get all participants
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param db db A database connection, as created by \link[mpathsenser]{create_db}.
#' @param lazy Whether to evaluate lazily using \link[dbplyr]{dbplyr}.
#'
#' @return A data frame containing all \code{participant_id} and \code{study_id}.
#' @export
get_participants <- function(db, lazy = FALSE) {
  if (!dbIsValid(db)) abort("Database connection is not valid")
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
#' @param db db A database connection, as created by \link[mpathsenser]{create_db}.
#' @param lazy Whether to evaluate lazily using \link[dbplyr]{dbplyr}.
#'
#' @return A data frame containing all studies.
#' @export
get_studies <- function(db, lazy = FALSE) {
  if (!dbIsValid(db)) abort("Database connection is not valid")
  if (lazy) {
    dplyr::tbl(db, "Study")
  } else {
    DBI::dbReadTable(db, "Study")
  }
}

#' Get the number of rows per sensor in a mpathsenser database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param db db A database connection, as created by \link[mpathsenser]{create_db}.
#' @param sensor A character vector of one or multiple vectors. Use "All" for all sensors. See
#' \link[mpathsenser]{sensors} for a list of all available sensors.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[mpathsenser]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#' @param start_date Optional search window specifying date where to begin search. Must be
#' convertible to date using \link[base]{as.Date}. Use \link[mpathsenser]{first_date} to find the
#' date of the first entry for a participant.
#' @param end_date Optional search window specifying date where to end search. Must be convertible
#' to date using \link[base]{as.Date}. Use \link[mpathsenser]{last_date} to find the date of the
#' last entry for a participant.
#'
#' @return A named vector containing the number of rows for each sensor.
#' @export
get_nrows <- function(db, sensor = "All", participant_id = NULL, start_date = NULL,
                      end_date = NULL) {
  if (!dbIsValid(db)) abort("Database connection is not valid")

  if (sensor[[1]] == "All") {
    sensor <- sensors
  }

  vapply(sensor, function(x) {
    get_data(db, x, participant_id, start_date, end_date) %>%
      dplyr::count() %>%
      pull(n)
  }, integer(1))
}
