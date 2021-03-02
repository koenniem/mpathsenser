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
#' @param db_name The name of the database.
#' @param overwrite In case a database with `db_name` already exists, indicate whether it should
#' be overwritten or not. Otherwise, this option is ignored.
#'
#' @return A database connection using prepared database schemas.
#' @export
create_db <- function(db_name = "carp.db", overwrite = FALSE) {
	if(is.character(db_name)) {
		if(file.exists(db_name)) {
			if(overwrite) {
				tryCatch(file.remove(db_name),
								 warning = function(e) stop(warningCondition(e)),
								 error = function(e) stop(errorCondition(e)))
			} else {
				stop("Database db_name already exists. Use overwrite = TRUE to overwrite.")
			}
		}
		db <- RSQLite::dbConnect(RSQLite::SQLite(), db_name)
	} else {
		stop("Argument db_name must be a filename.")
	}

	tryCatch({
		script <- strsplit(paste0(readLines("dbdef.sql", warn = FALSE), collapse = "\n"),	"\n\n")[[1]]
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
#' @param db_name The name of the database.
#'
#' @return A database connection.
#' @export
open_db <- function(db_name = "carp.db") {
	if (!file.exists(db_name))
		stop("There is no such file")
	db <- DBI::dbConnect(RSQLite::SQLite(), db_name)
	if (!RSQLite::dbExistsTable(db, "Participant")) {
		RSQLite::dbDisconnect(db)
		stop("Sorry, this does not appear to be a CARP database.")
	}
	return(db)
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

add_processed_file <- function(db, data) {
	RSQLite::dbExecute(db,
	"INSERT INTO ProcessedFiles(file_name, study_id, participant_id)
	VALUES(:file_name, :study_id, :participant_id);",
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
#' @param db A database connection, as created by \link[CARP]{create_db()}.
#'
#' @return A data frame contain processed file for each participant and study.
#' @export
get_processed_files <- function(db) {
	if(!RSQLite::dbIsValid(db)) stop("Database connection is not valid")
	RSQLite::dbReadTable(db, "ProcessedFiles")
}

#' Get all participants
#'
#' @param db db A database connection, as created by \link[CARP]{create_db()}.
#' @param lazy Whether to evaluate lazily using \link[dbplyr]{dbplyr}.
#'
#' @return A data frame containing all participants.
#' @export
get_participants <- function(db, lazy = FALSE) {
	if(!RSQLite::dbIsValid(db)) stop("Database connection is not valid")
	if(lazy) {
		dplyr::tbl(db, "Participant")
	} else {
		RSQLite::dbReadTable(db, "Participant")
	}
}

#' Get all studies
#'
#' @param db db A database connection, as created by \link[CARP]{create_db()}.
#' @param lazy Whether to evaluate lazily using \link[dbplyr]{dbplyr}.
#'
#' @return A data frame containing all studies.
#' @export
get_studies <- function(db, lazy = FALSE) {
	if(!RSQLite::dbIsValid(db)) stop("Database connection is not valid")
	if(lazy) {
		dplyr::tbl(db, "Study")
	} else {
		RSQLite::dbReadTable(db, "Study")
	}
}

#' Get the number of rows sensors in a CARP database
#'
#' @param db db A database connection, as created by \link[CARP]{create_db()}.
#' @param sensor A character vector of one or multiple vectors. Use "All" for all sensors. See
#' \link[CARP]{sensors} for a list of all available sensors.
#'
#' @return A named vector containing the number of rows for each sensor.
#' @export
get_nrows <- function(db, sensor = "All") {
	if(!RSQLite::dbIsValid(db)) stop("Database connection is not valid")

	if(sensor == "All") {
		sensor <- sensors
	}

	sapply(sensor, function(x) RSQLite::dbGetQuery(db, paste0("SELECT COUNT(*) FROM ", x))[[1]])
}
