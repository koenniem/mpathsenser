
#' Copy CARP zip files to a new location
#'
#' Copy zip files from a source destination to an origin destination where they do not yet exist.
#' That is, it only updates the origin folder from the source folder.
#'
#'
#' @param from A path to copy files from.
#' @param to A path to copy files to.
#'
#' @return Invisible result. Messages indicating how many files were copied.
#' @export
#'
#' @examples
#' library(CARP)
#' ccopy("K:/data/myproject/", "~/myproject")
ccopy <- function(from, to = getwd()) {
	# from <- "K:/GHUM-PPW-MICAS-OKPIV-PHD_KOEN-0800-A/emosense/"
	from_list <- dir(path = from, pattern = "*.zip$")
	to_list <- dir(path = to, pattern = "*.zip$")
	copy <- from_list[!(from_list %in% to_list)]
	if(length(copy) == 0) {
		message("No files left to copy")
		return(invisible(""))
	}
	message(paste0("Copying ", length(copy), " files."))
	copy <- paste0(from, copy)
	invisible(do.call(file.copy, list(from = copy, to = to, overwrite = FALSE)))
}

#' Clean up direct CARP output
#'
#' When copying data directly coming from CARP, JSON files are often corrupt due to the
#' app not properly closing them. This function (hopefully) fixes that. Additionally,
#' it can also unzip all the output in the directory.
#'
#' Note: Be careful when running this function multiple times. In the case there are
#' many zips and no JSON files, it will be fast. However, when re-running the function,
#' every previously unzipped JSON file will be checked for errors.
#'
#' @param path The path to the directory containing the JSON and/or zip files
#' @param zipped Logical value indicating whether you want to unzip the files as well.
#' @param fix Logical value indicating whether to fix incorrectly formatted JSON files in path.
#' @param overwrite Defaults to true.
#'
#' @return Invisible result. Side effect is corrupted JSON files fixed and/or data unzipped.
#' @export
clean <- function(path = getwd(), zipped = TRUE, fix = FALSE, overwrite = FALSE) {
	# 1. Get list of JSONs

	# Find all JSON files that are _not_ zipped
	# Thus, make sure you didn't unzip them yet, otherwise this may take a long time
	jsonfiles <- dir(path = path, pattern = "*.json$", TRUE)
	tag.json <- substr(jsonfiles, 11, nchar(jsonfiles) - 5)

	# 2. Fix JSONs
	if(fix) {
		if(length(jsonfiles > 0)) {
			lines <- lapply(jsonfiles, readLines)
			eof <- lapply(lines, function(x) x[length(x)])

			for(i in 1:length(jsonfiles)) {
				file <- paste0(path, "/", jsonfiles[i])
				if(eof[i] == ",") {
					write("{}]", file, append = TRUE)
				}
				if(eof[i] == "[") {
					write("]", file, append = TRUE)
				}
				if(nchar(eof[i]) > 3 && substr(eof[i], nchar(eof[i])-1, nchar(eof[i])) == "}}") {
					write("]", file, append = TRUE)
				}
			}
		} else{
			message("No JSON files found. Checking if there are zips...")
		}
	}

	# 3. Unzip
	# Get all zipfiles in the path
	zipfiles <- dir(path = path, pattern = "*.zip$")
	tag.zip <- sapply(strsplit(zipfiles, "carp-data-"), function(x) x[2])
	tag.zip <- substr(tag.zip, 1, nchar(tag.zip) - 4)

	# Do not unzip files that already exist as JSON file
	if(!overwrite) {
		# zipfiles <- setdiff(zipfiles, jsonfiles)
		zipfiles <- zipfiles[!(tag.zip %in% tag.json)]
	}

	if(length(zipfiles) > 0) {
		message(paste0("Unzipping ", length(zipfiles), " files."))
		# TODO: implement error handling in case unzipping fails
		# (e.g. unexpected end of data)
		invisible(lapply(paste0(path, "/", zipfiles), unzip, exdir = path))
	} else {
		message("No files found to unzip.")
	}

	if(zipfiles == 0 && jsonfiles == 0) {
		warning("No files were found to be processed.")
	}
}


#' Import CARP files into a database (CARP data scheme)
#'
#' Currently, only SQLite is supported as a backend. Due to its concurrency restrcition, the
#' `parallel` is disabled.
#'
#' @param path The path to the file directory
#' @param db Valid database connection.
#' @param dbname If no database is provided, a new database dbname is created.
#' @param backend Name of the database backend that is used. Currently, only RSQLite is supported.
#' @param progress Logical value to show a progress bar or not.
#' @param parallel Logical value which indicates whether to do reading in and processing in parallel.
#'
#' @return Invisible. Imported database can be reopened using \link[CARP]{open_db}.
#' @export
import <- function(path = getwd(), db = NULL, dbname = "carp.db", backend = "RSQLite", progress = TRUE, parallel = False) {

	# Retrieve all JSON files
	files <- dir(path = path, pattern = "*.json$")

	if(length(files) == 0) {
		stop("No JSON files found")
	}

	# Check backend and parallel constraint
	if(backend == "RSQLite" & parallel) {
		warning("Parallel cannot be used when RSQLite is provided as a backend
						due to concurrency constraint. Setting parallel to false.")
		parallel <- FALSE
	}

	# Set up database if not provided
	if(is.null(db)) {
		db <- create_db(db_name = dbname)
	}

	# If there are no duplicate files
	# Proceed with the unsafe (but fast) check
	# to prevent duplicate insertion into db
	if(!anyDuplicated(files)) {
		processedFiles <- get_processed_files(db)
		# Keep files _not_ already registered in db
		files <- files[!(files %in% processedFiles$file_name)]

		if(length(files) == 0) {
			return("No new files to process.")
		}
	}

	# Set up parallel backend
	if(parallel) {
		doFuture::registerDoFuture()
		future::plan(future::multisession)
	} else {
		foreach::registerDoSEQ()
	}

	# Call on implementation with or without progress bar
	if(progress) {
		progressr::with_progress({
			res <- import_impl(path, files, db@dbname)
		})
	} else{
		res <- import_impl(path, files, db@dbname)
	}

	# Return to sequential processing
	if(parallel) {
		future::plan(future::sequential)
	}

	dbDisconnect(db)
}

import_impl <- function(path, files, db_name) {
	p <- progressr::progressor(length(files))
	foreach::`%dopar%`(foreach::foreach(i = 1:length(files)), {

		# Update progress bar
		p(sprintf("x=%g", i))
	# for(i in 1:length(files)) {

		data <- rjson::fromJSON(file = paste0(path, "/", files[i]), simplify = FALSE)

		# Check if it is not an empty file
		# Return NULL is true
		if(length(data) == 0) {
			return(NULL)
		}

		# Clean-up and extract the header and body
		data <- tibble::tibble(header = lapply(data, function(x) x[1]),
													 body = lapply(data, function(x) x[2]))
		# Extract columns
		data$study_id <- lapply(data$header, function(x) x$header$study_id)
		data$participant_id <- lapply(data$header, function(x) x$header$user_id)
		data$start_time <- lapply(data$header, function(x) x$header$start_time)
		data$data_format <- lapply(data$header, function(x) x$header$data_format$namespace)
		data$sensor <- lapply(data$header, function(x) x$header$data_format$name)
		data$header <- NULL
		data <- tidyr::unnest(data, c(study_id:sensor))

		# Open db
		tmp_db <- open_db(db_name)

		#Safe duplicate check before insertion
		# Check if file is already registered as processed
		# Now using the participant_id and study_id as
		this_file <- data.frame(
			file_name = files[i],
			participant_id = unique(data$participant_id),
			study_id = unique(data$study_id)
		)
		processedFiles <- get_processed_files(tmp_db)
		matches <- inner_join(this_file,
													processedFiles,
													by = c("file_name", "participant_id", "study_id"))
		if(nrow(matches) > 0) {
			return(NULL) # File was already processed
		}

		# Populate study specifics to db
		add_study(db = tmp_db, data = distinct(data, study_id, data_format))

		# Add participants
		add_participant(db = tmp_db, data = distinct(data, participant_id, study_id))

		# Divide et impera
		data <- split(data, as.factor(data$sensor), drop = TRUE)

		# Drop useless data
		data[["error"]] <- NULL
		data[["unknown"]] <- NULL

		# Call function for each sensor
		tryCatch({
			dbWithTransaction(tmp_db, {
				for(j in 1:length(data)) {
					# Get sensor name
					sensor <- names(data)[[j]]
					tmp <- data[[sensor]]

					which_sensor(tmp_db, tmp, sensor)
				}
			})

			# Add file to list of processed files
			add_processed_file(tmp_db, this_file)
		}, error = function(e) {}) # Empty for now

		# Close db connection of worker
		dbDisconnect(tmp_db)
	})
}

acceleration_vector <- function(data, x = x, y = y, z = z, gravity = 9.810467) {
	x <- data$x
	y <- data$y
	z <- data$z
	data$acceleration <- sqrt((x)^2 + (y)^2 + (z - gravity)^2)
}

# Per hour
freq <- c(
	Accelerometer = 3600, # Once per second
	AirQuality = 1,
	AppUsage = 360,
	Bluetooth = 60,  # Once per minute
	Light = 360, # Once per 10 seconds
	Location = 120, # Once per 30 seconds
	Memory = 60, # Once per minute
	Noise = 120,
	Weather = 1,
	Wifi = 60 # once per minute
)


#' Create a coverage chart showing sampling rate
#'
#' Only applicable to non-reactive sensors with "continuous" sampling
#'
#' @param data A valid database connection. Schema must be that as it is created by
#' \link[CARP]{open_db}.
#' @param frequency A named numeric vector with sensors as names
#' and the number of expected samples per hour
#'
#' @return
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#' setwd("~/data")
#' clean()
#' import()
#' freq <- c(
#'   accelerometer = 3600, # Once per second
#'   air_quality = 1,
#'   app_usage = 360, # Once per 10 seconds
#'   bluetooth = 60,  # Once per minute
#'   light = 360, # Once per 10 seconds
#'   location = 120, # Once per 30 seconds
#'   memory = 60, # Once per minute
#'   noise = 120,
#'   weather = 1,
#'   wifi = 60 # once per minute
#' )
#' coverage(db, freq)
coverage <- function(db, participant_id, sensor = "All", frequency, relative = TRUE) {
	# Check db
	if(!inherits(db, "DBIConnection")) {
		stop("Argument db is not a database connection.")
	}

	if(!dbIsValid(db)) {
		stop("Database is invalid.")
	}

	# Check sensors
	if(length(sensor) == 1 && sensor == "All") {
		sensor <- sensors
	} else {
		missing <- sensor[!(sensor %in% sensors)]
		if(length(missing) != 0) {
			stop(paste0("Sensor(s) ", paste0(missing, collapse = ", "), " not found."))
		}
	}

	# Check participants
	if(length(participant_id) > 1) {
		stop("Only 1 participant per coverage chart allowed")
	}

	if(is.character(participant_id)) {
		if(!(participant_id %in% get_participants(db)$participant_id))
			stop("Participant_id not known.")
	} else {
		stop("participant_id must be a character string")
	}

	# Check frequency
	if(!is.numeric(frequency) || is.null(names(frequency))) {
		stop("Frequency is must be a named numeric vector")
	}

	# Retain only frequencies that appear in the sensor list
	frequency <- frequency[names(frequency) %in% sensor]

	# Interesting bug/feature in dbplyr: If participant_id is used in the query,
	# the index of the table is not used. Hence, we rename participant_id to p_id
	p_id <- as.character(participant_id)

	# Get Data
	data <- lapply(sensor, function(x) {
		tmp <- tbl(db, x) %>%
			filter(participant_id == p_id) %>%
			select(measurement_id, time)

		# From the last hour (unless otherwise specified)
		max_date <- tmp %>%
			summarise(max = datetime(max(time, na.rm = T), '-1 days')) %>%
			pull(max) %>%
			lubridate::as_datetime() %>%
			lubridate::format_ISO8601()

		tmp <- tmp %>%
			filter(time > max_date)

		tmp
	})
	names(data) <- sensor

	# Get the number of observations per hour
	data <- lapply(data, function(x) {
		x %>%
			# TODO: Something to filter out duplicate IDs with _
			mutate(Hour = strftime("%H", time)) %>%
			mutate(Date = date(time)) %>%
			count(Date, Hour) %>%
			group_by(Hour) %>%
			summarise(Coverage = sum(n, na.rm = TRUE) / n())
	})

	# Calculate its relative target frequency ratio
	if(relative) {
		data <- mapply(
			FUN = function(.x, .y) mutate(.x, Coverage = round(Coverage / .y, 2)),
			.x = data,
			.y = frequency,
			SIMPLIFY = F)
	}
	# Pour into ggplot format
	data <- mapply(
		FUN = function(.x, .y) dplyr::mutate(.x, measure = .y),
		.x = data,
		.y = names(data),
		SIMPLIFY = FALSE)

	# Collect into local tibble
	data <- lapply(data, collect)

	# Force correct column types
	# In case one sensor comes back empty, columns are logical by default
	data <- lapply(data,
								 function(x) mutate(x,
								 									 Hour = as.numeric(Hour),
								 									 Coverage = as.numeric(Coverage),
								 									 measure = as.character(measure)))

	# Complete missing hours with 0
	data <- mapply(
		FUN = function(.x, .y) complete(.x, Hour = 0:23, measure = .y, fill = list(Coverage = 0)),
		.x = data,
		.y = names(data),
		SIMPLIFY = FALSE
	)

	data <- bind_rows(data)
	data$measure <- factor(data$measure)
	data$measure <- factor(data$measure, levels = rev(levels(data$measure)))

	# Plot
	ggplot(data = data, mapping = aes(x = Hour, y = measure, fill = Coverage)) +
		geom_tile() +
		geom_text(mapping = aes(label = Coverage), colour = "white") +
		scale_x_continuous(breaks = 0:23) +
		scale_fill_gradientn(colours = c("#d70525", "#645a6c", "#3F7F93"),
																	breaks = c(0, 0.5, 1),
																	labels = c(0, 0.5, 1),
																	limits = c(0,1)) +
		theme_minimal() +
		ggtitle(paste0("Coverage for participant ", participant_id))
}
