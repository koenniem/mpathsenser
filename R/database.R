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
  "accelerometer", "airquality", "activity", "appusage", "battery", "bluetooth",
  "calendar", "connectivity", "device", "error", "geofence", "gyroscope",
  "installedapps", "keyboard", "light", "location", "memory", "mobility", "noise",
  "pedometer", "phonelog", "screen", "textmessage", "weather", "wifi"
)

#' Create a new mpathsenser database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param path The path to the database.
#' @param db_name The name of the database.
#' @param backend The name of the backend to be used. Supports SQLite, MySQL, and Postgresql.
#' @param overwrite In case a database with `db_name` already exists, indicate whether it should
#' be overwritten or not. Otherwise, this option is ignored.
#' @param ... Arguments passed on to [DBI::dbConnect()], such as `uid` and `pwd`, if necessary.
#'
#' @returns A database connection using prepared database schemas.
#' @export
create_db <- function(path = NULL,
                      db_name = "mpathsenser",
                      backend = "SQLite",
                      host = NULL,
                      port = NULL,
                      user = NULL,
                      password = NULL,
                      overwrite = FALSE,
                      ...) {

  check_arg(db_name, "character", n = 1)
  check_arg(overwrite, "logical", n = 1)

  if (any(grepl(c("sqlite"), tolower(backend), ignore.case = TRUE))) {
    db <- create_db.sqlite(path = path,
                           db_name = db_name,
                           overwrite = overwrite,
                           ...,
                           caller_env = environment())
  } else if (any(grep(c("mysql"), tolower(backend), ignore.case = TRUE))) {
    db <- create_db.mysql(db_name = db_name,
                          host = host,
                          port = port,
                          user = user,
                          password = password,
                          overwrite = overwrite,
                          ...,
                          caller_env = environment())
  } else if (any(grep(c("postgres"), tolower(backend), ignore.case = TRUE))) {
    db <- create_db.postgresql(db_name = db_name,
                               host = host,
                               port = port,
                               user = user,
                               password = password,
                               overwrite = overwrite,
                               ...,
                               caller_env = environment())
  } else {
    abort(paste("Backend", backend, "is not supported."))
  }

  # Populate the db with empty tables
  tryCatch(
    {
      dbdef <- system.file("extdata", "dbdef.sql", package = "mpathsenser")
      script <- strsplit(paste0(readLines(dbdef, warn = FALSE), collapse = "\n"), "\n\n")[[1]]

      if (any(grep(c("mysql"), tolower(backend), ignore.case = TRUE))) {
        script <- lapply(script, function(x) gsub('"', "`", x))
      }

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

create_db.sqlite <- function(path, db_name, overwrite, ..., caller_env) {
  ensure_suggested_package("RSQLite")
  check_arg(path, "character", n = 1, allow_null = TRUE, call = caller_env)

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
      ), call = caller_env)
    }
  }

  # Check if path exists
  if (!dir.exists(dirname(db_name))) {
    abort(paste0("Directory ", dirname(db_name), " does not exist."))
  }

  # Create a new db instance
  tryCatch(
    {
      db <- dbConnect(drv = RSQLite::SQLite(),
                      dbname = db_name,
                      cache_size = 8192,
                      ...)
    },
    error = function(e) {
      abort(paste0("Could not create a database in ", db_name)) # nocov
    }
  )

  # Write some PRAGMAs
  DBI::dbExecute(db, "PRAGMA foreign_keys = 1;")
  DBI::dbExecute(db, "PRAGMA main.synchronous = 1;")
  DBI::dbExecute(db, "PRAGMA busy_timeout = 3000;")
  DBI::dbExecute(db, "PRAGMA page_size = 8192;")

  db
}

create_db.mysql <- function(db_name, host, port, user, password, overwrite, ..., caller_env) {
  ensure_suggested_package("RMySQL")
  check_arg(host, "character", n = 1, call = caller_env)
  check_arg(port, "integerish", n = 1, call = caller_env)
  check_arg(user, "character", n = 1, call = caller_env)
  check_arg(password, "character", n = 1, call = caller_env)

  # Open a connection to the database
  db <- dbConnect(drv = RMySQL::MySQL(),
                  host = host,
                  port = port,
                  user = user,
                  password = password,
                  ...)

  if (db_name %in% dbGetQuery(db, "SHOW DATABASES")[, 1]) {
    if (overwrite) {
      dbExecute(db, paste0("DROP DATABASE ", db_name, ";"))
    } else {
      abort(c(
        paste0("Database '", db_name, "' already exists."),
        i = " Use `overwrite = TRUE` to overwrite."
      ), call = caller_env)
    }
  }

  # Create and use a new schema
  DBI::dbExecute(db, paste0("CREATE DATABASE ", db_name, ";"))
  DBI::dbExecute(db, paste0("USE ", db_name, ";"))

  db
}

create_db.postgresql <- function(db_name, host, port, user, password, overwrite, ..., caller_env) {
  ensure_suggested_package("RPostgres")
  check_arg(db_name, "character", n = 1, call = caller_env)
  check_arg(host, "character", n = 1, call = caller_env)
  check_arg(port, c("character", "integerish"), n = 1, call = caller_env)
  check_arg(user, "character", n = 1, call = caller_env)
  check_arg(password, "character", n = 1, call = caller_env)

  # Create a new db instance
  tryCatch(
    {
      db <- dbConnect(drv = RPostgres::Postgres(),
                      dbname = db_name,
                      host = host,
                      port = port,
                      user = user,
                      password = password,
                      ...)
    },
    error = function(e) {
      abort(paste0("Could not create a database in ", db_name)) # nocov
    }
  )

  # Create and use a new schema
  # DBI::dbExecute(db, paste0("CREATE DATABASE ", db_name, ";"))
  # DBI::dbExecute(db, paste0("USE ", db_name, ";"))

  db
}

#' Open an mpathsenser database.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams create_db
#'
#' @seealso [close_db()] for closing a database;
#' [copy_db()] for copying (part of) a database;
#' [index_db()] for indexing a database;
#' [get_data()] for extracting data from a database.
#'
#' @returns A connection to an `mpathsenser` database.
#' @export
open_db <- function(path = NULL,
                    db_name = "mpathsenser",
                    backend = "SQLite",
                    host = NULL,
                    port = NULL,
                    user = NULL,
                    password = NULL,
                    ...) {

  check_arg(db_name, c("character", "integerish"), n = 1)

  if (any(grepl(c("sqlite"), tolower(backend), ignore.case = TRUE))) {
    db <- open_db.sqlite(path = path,
                           db_name = db_name,
                           ...,
                           caller_env = environment())
  } else if (any(grep(c("mysql"), tolower(backend), ignore.case = TRUE))) {
    db <- open_db.mysql(db_name = db_name,
                          host = host,
                          port = port,
                          user = user,
                          password = password,
                          ...,
                          caller_env = environment())
  } else if (any(grep(c("postgres"), tolower(backend), ignore.case = TRUE))) {
    db <- open_db.postgresql(db_name = db_name,
                             host = host,
                             port = port,
                             user = user,
                             password = password,
                             ...,
                             caller_env = environment())
  } else {
    abort(paste("Backend", backend, "is not supported."))
  }

  if (!("participant" %in% tolower(DBI::dbListTables(db)))) {
    dbDisconnect(db)
    abort("Sorry, this does not appear to be a valid mpathsenser database.")
  }

  return(db)
}

open_db.sqlite <- function(path, db_name, ..., caller_env) {
  ensure_suggested_package("RSQLite")
  check_arg(path, "character", n = 1, allow_null = TRUE)

  # Merge path and file name
  if (!is.null(path)) {
    db_name <- suppressWarnings(normalizePath(file.path(path, db_name)))
  }

  if (!file.exists(db_name)) {
    abort(paste0("Could not open a database connection to ", db_name, "."), call = caller_env)
  }
  db <- dbConnect(drv = RSQLite::SQLite(),
                  dbname = db_name,
                  cache_size = 8192,
                  ...)

  db
}

open_db.mysql <- function(db_name, host, port, user, password, ..., caller_env) {
  ensure_suggested_package("RMySQL")
  check_arg(host, "character", n = 1, call = caller_env)
  check_arg(port, "integerish", n = 1, call = caller_env)
  check_arg(user, "character", n = 1, call = caller_env)
  check_arg(password, "character", n = 1, call = caller_env)

  # Open a connection to the database
  db <- dbConnect(drv = RMySQL::MySQL(),
                  host = host,
                  port = port,
                  user = user,
                  password = password,
                  ...)

  if (!(db_name %in% DBI::dbGetQuery(db, "SHOW DATABASES")[, 1])) {
      abort(c(
        paste("Database", db_name, "does not yet exist."),
        i = "Please create a database first using `create_db()`."
      ), call = call)
  }

  # Make this schema the default
  DBI::dbExecute(db, paste0("USE ", db_name, ";"))

  db
}

open_db.postgresql <- function(db_name, host, port, user, password, ..., caller_env) {
  ensure_suggested_package("RPostgres")
  check_arg(host, "character", n = 1, call = caller_env)
  check_arg(port, "integerish", n = 1, call = caller_env)
  check_arg(user, "character", n = 1, call = caller_env)
  check_arg(password, "character", n = 1, call = caller_env)

  # Open a connection to the database
  db <- dbConnect(drv = RPostgres::Postgres(),
                  dbname = db_name,
                  host = host,
                  port = port,
                  user = user,
                  password = password,
                  ...)
#
#   if (!(db_name %in% DBI::dbGetQuery(db, "SHOW DATABASES")[, 1])) {
#     abort(c(
#       paste("Database", db_name, "does not yet exist."),
#       i = "Please create a database first using `create_db()`."
#     ), call = call)
#   }

  db
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
<<<<<<< HEAD
#' @returns Returns invisibly regardless of whether the database is active, valid,
#' or even exists.
=======
#' @returns \code{close_db} Returns a logical value invisible, indicating whether the connection has
#' been closed.
>>>>>>> c5bcd06 (create_db, open_db, and vacuum_db for database extensions MySQL and PostgreSQL)
#' @export
close_db <- function(db) {
  out <- suppressWarnings(try(dbDisconnect(db), silent = TRUE))
  return(invisible(!inherits(out, "try-error")))
}

#' Create indexes for an mpathsenser database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @inheritParams get_data
#'
#' @returns Returns `TRUE`, invisibly.
#' @export
index_db <- function(db) {
  check_db(db)

  fn <- system.file("extdata", "indexes.sql", package = "mpathsenser")
  script <- strsplit(paste0(readLines(fn, warn = FALSE), collapse = "\n"), "\n\n")[[1]]
  for (statement in script) {
    dbExecute(db, statement)
  }
  return(invisible(TRUE))
}

#' Vacuum a database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' The SQL `VACUUM` command reclaims storage occupied by dead rows. This is mostly useful for
#' SQLite databases, as the `VACUUM` command rebuilds the database file, repacking it into a minimal
#' amount of disk space. Note that MySQL databases need not (nor can) be vacuumed.
#'
#' @param db A database connection to an m-Path Sense database.
#' @param ... Currently not used.
#'
#' @returns An integer indicating the number of rows that were vacuumed.
#' @export
vacuum_db <- function(db, ...) {
  check_db(db)
  UseMethod("vacuum_db", db)
}

#' @export
#' @keywords internal
vacuum_db.MySQLConnection <- function(db, ...) {
  warn("MySQL databases do not need to be vacuumed.")
  return(0L)
}

#' @export
#' @keywords internal
vacuum_db.default <- function(db, ...) {
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
#' @param path  `r lifecycle::badge("deprecated")`: This argument was used when database creation in
#'   [copy_db()] was still supported. As this functionality is deprecated, `overwrite_db` is now
#'   ignored and will be removed in future versions.
#' @param db_name  `r lifecycle::badge("deprecated")`: Creating new databases on the fly has been
#'   deprecated as it is better to separate the two functions. You must now create a new database
#'   using [create_db()] or reuse an existing one.
#'
#' @returns No return value, called for side effects.
#' @export
copy_db <- function(source_db,
                    target_db,
                    sensor = "All",
                    path = deprecated(),
                    db_name = deprecated()) {
  # Handle old database creation functionality
  if (lifecycle::is_present(path)) {
    lifecycle::deprecate_warn(
      when = "1.1.1",
      what = "copy_db(path)",
      details = c(
        i = "Please create a database using `create_db()` first."
      )
    )
  }
  if (lifecycle::is_present(db_name)) {
    lifecycle::deprecate_stop(
      when = "1.1.1",
      what = "copy_db(db_name)",
      details = c(
        i = "Please create a database using `create_db()` first."
      )
    )
  }

  check_db(source_db, arg = "source_db")
  check_db(target_db, arg = "target_db")
  check_arg(sensor, "character")

  if (!(inherits(source_db, "SQLiteConnection") && inherits(target_db, "SQLiteConnection"))) {
    abort(c("Both `source_db` and `target_db` must be SQLite databases.",
          i = "To copy to/from an SQLite database, use a database dump.",
          i = "To copy other database types, use dedicated programs."))
  }

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

add_study <- function(db, study_id, data_format, ...) {
  check_db(db)
  UseMethod("add_study", db)
}

add_study.SQLiteConnection <- function(db, study_id, data_format, ...) {
  dbExecute(
    db,
    paste(
      "INSERT OR IGNORE INTO Study(study_id, data_format)",
      "VALUES(:study_id, :data_format);"
    ),
    list(study_id = study_id, data_format = data_format)
  )
}

add_participant <- function(db, participant_id, study_id, ...) {
  check_db(db)
  UseMethod("add_participant", db)
}

add_participant.SQLiteConnection <- function(db, participant_id, study_id, ...) {
  dbExecute(
    db,
    paste(
      "INSERT OR IGNORE INTO Participant(participant_id, study_id)",
      "VALUES(:participant_id, :study_id);"
    ),
    list(participant_id = participant_id, study_id = study_id)
  )
}

add_processed_files <- function(db, file_name, study_id, participant_id) {
  check_db(db)
  UseMethod("add_processed_files", db)
}

add_processed_files.SQLiteConnection <- function(db, file_name, study_id, participant_id) {
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

clear_sensors_db <- function(db) {
  check_db(db)
  res <- lapply(sensors, function(x) dbExecute(db, paste0("DELETE FROM ", x, " WHERE 1;")))
  names(res) <- sensors
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
get_nrows <- function(db,
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
    get_data(db, x, participant_id, start_date, end_date) %>%
      dplyr::select(1) %>%
      dplyr::count() %>%
      pull(n)
  }, double(1))
}
