#' Available Sensors
#'
#' A list containing all available sensors in this package you can work with. This variable was
#' created so it is easier to use in your own functions, e.g. to loop over sensors.
#'
#' @return A character vector containing all sensor names supported by \code{mpathsenser}.
#' @examples
#' sensors
#' @export sensors
sensors <- c("Accelerometer", "AirQuality", "Activity", "AppUsage", "Battery", "Bluetooth",
             "Calendar", "Connectivity", "Device", "Error", "Geofence", "Gyroscope",
             "InstalledApps", "Keyboard", "Light", "Location", "Memory", "Mobility", "Noise",
             "Pedometer", "PhoneLog", "Screen", "TextMessage", "Weather", "Wifi")

#' Create a new mpathsenser database
#'
#' @param path The path to the database.
#' @param db_name The name of the database.
#' @param overwrite In case a database with `db_name` already exists, indicate whether it should
#' be overwritten or not. Otherwise, this option is ignored.
#'
#' @return A database connection using prepared database schemas.
#' @export
create_db <- function(path = getwd(), db_name = "sense.db", overwrite = FALSE) {
  if (!is.character(db_name)) stop("Argument db_name must be a filename")
  if (!is.character(path)) stop("Argument path must be a character string")

  # Merge path and file name
  db_name <- suppressWarnings(normalizePath(paste0(path, "/", db_name)))

  # If db already exists, remove it or throw an error
  if (file.exists(db_name)) {
    if (overwrite) {
      tryCatch(file.remove(db_name),
               warning = function(e) stop(warningCondition(e)),
               error = function(e) stop(errorCondition(e)))
    } else {
      stop(paste("Database", db_name, "already exists. Use overwrite = TRUE to overwrite."))
    }
  }

  # Create a new db instance
  db <- DBI::dbConnect(RSQLite::SQLite(), db_name, cache_size = 8192)

  # Populate the db with empty tables
  tryCatch({
    fn <- system.file("extdata", "dbdef.sql", package = "mpathsenser")
    script <- strsplit(paste0(readLines(fn, warn = FALSE), collapse = "\n"),  "\n\n")[[1]]
    for (statement in script) {
      DBI::dbExecute(db, statement)
    }
  }, error = function(e) {
    DBI::dbDisconnect(db)
    stop(e)
  })

  return(db)
}

#' Open an mpathsenser database
#'
#' @param path The path to the database. Use NULL to use the full path name in db_name.
#' @inheritParams get_data
#'
#' @return A connection to an mpathsenser database.
#' @export
open_db <- function(path = getwd(), db_name = "sense.db") {
  if (!is.character(db_name)) stop("Argument db_name must be a filename")
  if (!(is.null(path) | is.character(path))) stop("Argument path must be a character string")

  # Merge path and file name
  if (!is.null(path)) {
    db_name <- suppressWarnings(normalizePath(paste0(path, "/", db_name)))
  }

  if (!file.exists(db_name))
    stop("There is no such file")
  db <- DBI::dbConnect(RSQLite::SQLite(), db_name, cache_size = 8192)
  if (!DBI::dbExistsTable(db, "Participant")) {
    DBI::dbDisconnect(db)
    stop("Sorry, this does not appear to be a mpathsenser database.")
  }
  return(db)
}

#' Close a database connection
#'
#' This is a convenience function that is simply a wrapper around \link[DBI]{dbDisconnect}.
#'
#' @inheritParams get_data
#'
#' @return \code{cose_db} returns invisibly regardless of whether the database is active, valid,
#' or even exists.
#' @export
close_db <- function(db) {
  exists <- try(db, silent = TRUE)
  if (inherits(exists, "SQLiteConnection") && !is.null(db)) {
    if (DBI::dbIsValid(db)) {
      DBI::dbDisconnect(db)
    }
  }
}

#' Create indexes for a mpathsenser database
#'
#' @inheritParams get_data
#'
#' @return No return value, called for side effects.
#' @export
index_db <- function(db) {
  if (is.null(db) || !DBI::dbIsValid(db)) stop("Database connection is not valid")

  tryCatch({
    fn <- system.file("extdata", "indexes.sql", package = "mpathsenser")
    script <- strsplit(paste0(readLines(fn, warn = FALSE), collapse = "\n"),  "\n\n")[[1]]
    for (statement in script) {
      DBI::dbExecute(db, statement)
    }
  }, error = function(e) {
    stop(e)
  })
}

vacuum_db <- function(db) {
  if (is.null(db) || !DBI::dbIsValid(db)) stop("Database connection is not valid")
  DBI::dbExecute(db, "VACUUM")
}

#' Copy (a subset of) a database to another database
#'
#' @param from_db A mpathsenser database connection from where the data will be transferred.
#' @param to_db A mpathsenser database connection where the data will be transferred to. If no
#' new_db is specified, a path (and possibly a db_name) must be specified for
#' \link[mpathsenser]{create_db} to create a new database.
#' @param sensor A character vector containing one or multiple sensors. See
#' \code{\link[mpathsenser]{sensors}} for a list of available sensors. Use "All" for all available
#' sensors.
#' @param path The path to the database. Use NULL to use the full path name in db_name.
#' @param db_name The name of the database.
#'
#' @return No return value, called for side effects.
#' @export
copy_db <- function(from_db, to_db = NULL, sensor = "All", path = getwd(), db_name = "sense.db") {
  if (is.null(from_db) || !DBI::dbIsValid(from_db)) stop("Database connection is not valid")
  # Check sensors
  if (length(sensor) == 1 && sensor == "All") {
    sensor <- sensors
  } else {
    missing <- sensor[!(sensor %in% sensors)]
    if (length(missing) != 0) {
      stop(paste0("Sensor(s) ", paste0(missing, collapse = ", "), " not found."))
    }
  }

  # If no new database is specified, create a new one
  no_db_specified <- FALSE
  if (is.null(to_db)) {
    no_db_specified <- TRUE

    if (!file.exists(file.path(path, db_name))) {
      to_db <- create_db(path, db_name)
      message(paste0("New database created in ", path))
    } else {
      stop(paste0("A file in ", path, " with the name ", db_name, " already exists. Please choose ",
                  "a different name or path or remove the file."))
    }
  }

  # Attach new database to old database
  DBI::dbExecute(from_db, paste0("ATTACH DATABASE '", to_db@dbname, "' AS new_db"))

  # Copy participants, studies, processed_files
  DBI::dbExecute(from_db, "INSERT OR IGNORE INTO new_db.Study SELECT * FROM Study")
  DBI::dbExecute(from_db, "INSERT OR IGNORE INTO new_db.Participant SELECT * FROM Participant")
  DBI::dbExecute(from_db,
                 "INSERT OR IGNORE INTO new_db.ProcessedFiles SELECT * FROM ProcessedFiles")


  # Copy all specified sensors
  for (i in seq_along(sensor)) {
    DBI::dbExecute(from_db, paste0("INSERT OR IGNORE INTO new_db.", sensor[i],
                                   " SELECT * FROM ", sensor[i]))
  }

  # Detach
  DBI::dbExecute(from_db, "DETACH DATABASE new_db")

  if (no_db_specified) {
    close_db(to_db)
  }

  return(invisible(TRUE))
}

add_study <- function(db, data) {
  DBI::dbExecute(db,
                 "INSERT OR IGNORE INTO Study(study_id, data_format)
  VALUES(:study_id, :data_format);",
                 list(study_id = data$study_id, data_format = data$data_format))
}

add_participant <- function(db, data) {
  DBI::dbExecute(db,
                 "INSERT OR IGNORE INTO Participant(participant_id, study_id)
  VALUES(:participant_id, :study_id);",
                 list(participant_id = data$participant_id, study_id = data$study_id))
}

add_processed_files <- function(db, data) {
  DBI::dbExecute(db,
                 "INSERT OR IGNORE INTO ProcessedFiles(file_name, study_id, participant_id)
  VALUES(:file_name, :study_id, :participant_id);",
                 list(file_name = data$file_name,
                      study_id = data$study_id,
                      participant_id = data$participant_id))
}

clear_sensors_db <- function(db) {
  res <- lapply(sensors, function(x) DBI::dbExecute(db, paste0("DELETE FROM ", x, " WHERE 1;")))
  names(res) <- sensors
  res
}

### ----------- Getters ---------------


#' Get all processed files from a database
#'
#' @param db A database connection, as created by \link[mpathsenser]{create_db}.
#'
#' @return A data frame containing the \code{file_name}, \code{participant_id}, and \code{study_id}
#' of the processed files.
#' @export
get_processed_files <- function(db) {
  if (!DBI::dbIsValid(db)) stop("Database connection is not valid")
  DBI::dbReadTable(db, "ProcessedFiles")
}

#' Get all participants
#'
#' @param db db A database connection, as created by \link[mpathsenser]{create_db}.
#' @param lazy Whether to evaluate lazily using \link[dbplyr]{dbplyr}.
#'
#' @return A data frame containing all \code{participant_id} and \code{study_id}.
#' @export
get_participants <- function(db, lazy = FALSE) {
  if (!DBI::dbIsValid(db)) stop("Database connection is not valid")
  if (lazy) {
    dplyr::tbl(db, "Participant")
  } else {
    DBI::dbReadTable(db, "Participant")
  }
}

#' Get all studies
#'
#' @param db db A database connection, as created by \link[mpathsenser]{create_db}.
#' @param lazy Whether to evaluate lazily using \link[dbplyr]{dbplyr}.
#'
#' @return A data frame containing all studies.
#' @export
get_studies <- function(db, lazy = FALSE) {
  if (!DBI::dbIsValid(db)) stop("Database connection is not valid")
  if (lazy) {
    dplyr::tbl(db, "Study")
  } else {
    DBI::dbReadTable(db, "Study")
  }
}

#' Get the number of rows sensors in a mpathsenser database
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
  if (!DBI::dbIsValid(db)) stop("Database connection is not valid")

  if (sensor[[1]] == "All") {
    sensor <- sensors
  }

  sapply(sensor, function(x) {
    get_data(db, x, participant_id, start_date, end_date) %>%
      dplyr::count() %>%
      dplyr::pull(n)
  })
}
