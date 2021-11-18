#' Available Sensors
#'
#' A list containing all available sensors in this package you can work with. This variable was
#' created so it is easier to use in your own functions, e.g. to loop over sensors.
#'
#' @examples
#' sensors
#' @export sensors
sensors <- c("Accelerometer", "AirQuality", "Activity", "AppUsage", "Battery", "Bluetooth",
						 "Calendar", "Connectivity", "Device", "Geofence", "Gyroscope", "InstalledApps",
						 "Keyboard", "Light", "Location", "Memory", "Mobility", "Noise",
						 "Pedometer", "PhoneLog", "Screen", "TextMessage", "Weather", "Wifi")

#' Create a new CARP database
#'
#' @param path The path to the database.
#' @param db_name The name of the database.
#' @param overwrite In case a database with `db_name` already exists, indicate whether it should
#' be overwritten or not. Otherwise, this option is ignored.
#'
#' @return A database connection using prepared database schemas.
#' @export
create_db <- function(path = getwd(), db_name = "carp.db", overwrite = FALSE) {
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
	db <- RSQLite::dbConnect(RSQLite::SQLite(), db_name, winslash = "/")

	# Populate the db with empty tables
	tryCatch({
		fn <- system.file("extdata", "dbdef.sql", package = "CARP")
		script <- strsplit(paste0(readLines(fn, warn = FALSE), collapse = "\n"),	"\n\n")[[1]]
		for (statement in script) {
			RSQLite::dbExecute(db, statement)
		}
	}, error = function(e) {
		RSQLite::dbDisconnect(db)
		stop(e)
	})

	return(db)
}

#' Open a CARP database
#'
#' @param path The path to the database. Use NULL to use the full path name in db_name.
#' @param db_name The name of the database.
#'
#' @return A database connection.
#' @export
open_db <- function(path = getwd(), db_name = "carp.db") {
	if (!is.character(db_name)) stop("Argument db_name must be a filename")
	if (!(is.null(path) | is.character(path))) stop("Argument path must be a character string")

	# Merge path and file name
	if (!is.null(path)) {
		db_name <- suppressWarnings(normalizePath(paste0(path, "/", db_name)))
	}

	if (!file.exists(db_name))
		stop("There is no such file")
	db <- DBI::dbConnect(RSQLite::SQLite(), db_name)
	if (!RSQLite::dbExistsTable(db, "Participant")) {
		RSQLite::dbDisconnect(db)
		stop("Sorry, this does not appear to be a CARP database.")
	}
	return(db)
}


#' Close a database connection
#'
#' This is a convenience function that is simply a wrapper around \link[DBI]{dbDisconnect}.
#'
#' This function returns invisibly regardless of whether the database is active, valid, or even
#' exists.
#'
#' @param db A database connection
#' @export
close_db <- function(db) {
	exists <- try(db, silent = TRUE)
	if (inherits(exists, "SQLiteConnection") && !is.null(db)) {
		if (DBI::dbIsValid(db)) {
			DBI::dbDisconnect(db)
		}
	}
}

#' Create indexes for a CARP database
#'
#' @param db A database connection
#'
#' @return
#' @export
index_db <- function(db) {
	if (is.null(db) || !DBI::dbIsValid(db)) stop("Database connection is not valid")

	tryCatch({
		fn <- system.file("extdata", "indexes.sql", package = "CARP")
		script <- strsplit(paste0(readLines(fn, warn = FALSE), collapse = "\n"),	"\n\n")[[1]]
		for (statement in script) {
			DBI::dbExecute(db, statement)
		}
	}, error = function(e) {
		stop(e)
	})
}

add_study <- function(db, data) {
	RSQLite::dbExecute(db,
	"INSERT INTO Study(study_id, data_format)
	VALUES(:study_id, :data_format)
	ON CONFLICT DO NOTHING;",
	list(study_id = data$study_id, data_format = data$data_format))
}

add_participant <- function(db, data) {
	RSQLite::dbExecute(db,
	"INSERT INTO Participant(participant_id, study_id)
	VALUES(:participant_id, :study_id)
	ON CONFLICT DO NOTHING;",
	list(participant_id = data$participant_id, study_id = data$study_id))
}

add_processed_files <- function(db, data) {
	RSQLite::dbExecute(db,
	"INSERT INTO ProcessedFiles(file_name, study_id, participant_id)
	VALUES(:file_name, :study_id, :participant_id)
	ON CONFLICT DO NOTHING;",
	list(file_name = data$file_name, study_id = data$study_id, participant_id = data$participant_id))
}

clear_sensors_db <- function(db) {
	res <- lapply(sensors, function(x) RSQLite::dbExecute(db, paste0("DELETE FROM ", x, ";")))
	names(res) <- sensors
	res
}

### ----------- Getters ---------------


#' Get all processed files from a database
#'
#' @param db A database connection, as created by \link[CARP]{create_db}.
#'
#' @return A data frame contain processed file for each participant and study.
#' @export
get_processed_files <- function(db) {
	if (!RSQLite::dbIsValid(db)) stop("Database connection is not valid")
	RSQLite::dbReadTable(db, "ProcessedFiles")
}

#' Get all participants
#'
#' @param db db A database connection, as created by \link[CARP]{create_db}.
#' @param lazy Whether to evaluate lazily using \link[dbplyr]{dbplyr}.
#'
#' @return A data frame containing all participants.
#' @export
get_participants <- function(db, lazy = FALSE) {
	if (!RSQLite::dbIsValid(db)) stop("Database connection is not valid")
	if (lazy) {
		dplyr::tbl(db, "Participant")
	} else {
		RSQLite::dbReadTable(db, "Participant")
	}
}

#' Get all studies
#'
#' @param db db A database connection, as created by \link[CARP]{create_db}.
#' @param lazy Whether to evaluate lazily using \link[dbplyr]{dbplyr}.
#'
#' @return A data frame containing all studies.
#' @export
get_studies <- function(db, lazy = FALSE) {
	if (!RSQLite::dbIsValid(db)) stop("Database connection is not valid")
	if (lazy) {
		dplyr::tbl(db, "Study")
	} else {
		RSQLite::dbReadTable(db, "Study")
	}
}

#' Get the number of rows sensors in a CARP database
#'
#' @param db db A database connection, as created by \link[CARP]{create_db}.
#' @param sensor A character vector of one or multiple vectors. Use "All" for all sensors. See
#' \link[CARP]{sensors} for a list of all available sensors.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[CARP]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#' @param start_date Optional search window specifying date where to begin search. Must be
#' convertible to date using \link[base]{as.Date}. Use \link[CARP]{first_date} to find the date of
#' the first entry for a participant.
#' @param end_date Optional search window specifying date where to end search. Must be convertible
#' to date using \link[base]{as.Date}. Use \link[CARP]{last_date} to find the date of the last
#' entry for a participant.
#'
#' @return A named vector containing the number of rows for each sensor.
#' @export
get_nrows <- function(db, sensor = "All", participant_id = NULL, start_date = NULL,
											end_date = NULL) {
	if (!RSQLite::dbIsValid(db)) stop("Database connection is not valid")

	if (sensor[[1]] == "All") {
		sensor <- sensors
	}

	# sapply(sensor, function(x) RSQLite::dbGetQuery(db, paste0("SELECT COUNT(*) FROM ", x))[[1]])
	sapply(sensor, function(x) {
		get_data(db, x, participant_id, start_date, end_date) %>%
			dplyr::count() %>%
			dplyr::pull(n)
	})
}
