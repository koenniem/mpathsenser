#' Clean up direct CARP output
#'
#' When copying data directly coming from CARP, JSON files are often corrupt due to the
#' app not properly closing them. This function (hopefully) fixes that. Additionally,
#' it can also unzip all the output in the directory.
#'
#' @param path The path to the directory containing the JSON and/or zip files
#' @param zipped Logical value indiciating whether you want to unzip the files as well.
#'
#' @return Invisible result. Side effect is corrupted JSON files fixed and/or data unzipped.
#' @export
clean <- function(path = getwd(), zipped = TRUE) {
	# 1. Get list of JSONs

	# Find all JSON files that are _not_ zipped
	# Thus, make sure you didn't unzip them yet, otherwise this may take a long time
	files <- dir(path = path, pattern = "*.json$")

	# 2. Fix JSONs
	if(length(files > 0)) {
		lines <- lapply(files, readLines)
		eof <- lapply(lines, function(x) x[length(x)])

		for(i in 1:length(files)) {
			file <- paste0(path, "/", files[i])
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

	# 3. Unzip
	zipfiles <- dir(pattern = "*.zip$", full.names = TRUE)
	if(length(zipfiles) > 0) {
		message(paste0("Unzipping ", length(zipfiles), " files."))
		invisible(lapply(zipfiles, unzip))
	} else {
		message("No files found to unzip.")
	}

	if(zipfiles == 0 && files == 0) {
		warning("No files were found to be processed.")
	}
}


#' Import CARP files into R (CARP data scheme)
#'
#' @param path The path to the file directory
#' @param progress Logical value to show a progress bar or not.
#' @param parallel Logical value which indicates whether to do reading in and processing in parallel.
#'
#' @return An S3 object containing a tibble for each sensor in the data.
#' @export
import <- function(path = getwd(), progress = TRUE, parallel = TRUE) {

	# Retrieve all JSON files
	files <- dir(path = path, pattern = "*.json$")

	if(length(files) == 0) {
		stop("No JSON files found")
	}

	# Set up parallel backend
	if(parallel) {
		doFuture::registerDoFuture()
		future::plan(future::multisession)
	} else {
		foreach::registerDoSEQ()
	}

	if(progress) {
		progressr::with_progress({
			res <- import_impl(path, files)
		})
	} else{
		res <- import_impl(path, files)
	}

	# Return to sequential processing
	if(parallel) {
		future::plan(future::sequential)
	}

	# TODO: # Replace with non-purrr version
	res <- purrr::flatten(res)
	res <- tibble::enframe(res)

	# Split by sensor
	res <- split(res, res$name)

	res <- lapply(res, function(x) tidyr::unnest(x, value))

	# Only keep elements that have rows (i.e. not 0)
	res <- res[sapply(res, function(x) nrow(x) != 0)]

	# ISO8601 to R data format
	for(i in 1:length(res)) {
		if("timestamp" %in% colnames(res[[i]])) {
			res[[i]][["timestamp"]] <- lubridate::as_datetime(res[[i]][["timestamp"]])
			res[[i]][["timestamp"]] <- lubridate::force_tz(res[[i]][["timestamp"]], "Europe/Brussels")
		}
	}

	res
}

import_impl <- function(path, files) {
	p <- progressr::progressor(length(files))
	res <- foreach::`%dopar%`(foreach::foreach(i = 1:length(files)), {

		# Update progress bar
		p(sprintf("x=%g", i))

		data <- rjson::fromJSON(file = paste0(path, "/", files[i]), simplify = FALSE)

		# Check if it is not an empty file
		if(length(data) == 0) {
			# next
			return(NULL)
		}

		# Clean-up and extract the header and body
		data <- tibble::tibble(header = lapply(data, function(x) x[1]),
													 body = lapply(data, function(x) x[2]))
		data$header <- lapply(data$header, as.data.frame)
		data <- tidyr::unnest(data, header)
		colnames(data) <- c("study_id", "user_id", "start_time", "data_namespace", "sensor", "body")

		# Check if it has any rows
		if(nrow(data) == 0) {
			return(NULL)
		}

		# Convert to POSIX time
		data$start_time <- as.POSIXct(data$start_time, "%Y-%m-%dT%H:%M:%S", tz="Europe/Brussels")

		# Divide et impera
		data <- split(data, as.factor(data$sensor), drop = TRUE)

		# Drop useless data
		data[["error"]] <- NULL
		data[["unknown"]] <- NULL

		out <- vector("list", length(data))
		names(out) <- names(data)

		# Call function for each list according to their name plus _fun
		for(i in 1:length(data)) {
			# Get sensor name
			sensor <- names(data)[[i]]
			tmp <- data[[sensor]]

			# Make the package for the decision function explicition
			# otherwise globals won't know to export it
			out[[i]] <- which_sensor(tmp, sensor)
		}
		out
	})
}

acceleration <- function(data, x = x, y = y, z = z) {
	x <- data$x
	y <- data$y
	z <- data$z
	data$acceleration <- sqrt((x)^2 + (y)^2 + (z - 9.810467)^2)
}

freq <- c(
	accelerometer = 3600, # Once per second
	bluetooth = 60,  # Once per minute
	light = 360, # Once per 10 seconds
	location = 120, # Once per 30 seconds
	memory = 60, # Once per minute
	wifi = 60 # once per minute
)


#' Create a coverage chart showing sampling rate
#'
#' Only applicable to non-reactive sensors with "continuous" sampling
#'
#' @param data A list of data frames containing data per sensor
#' as output by \link[CARP]{import}
#' @param frequency A named numeric vector with sensors as names
#' and the number of expected samples per hour
#'
#' @return
#' @export
#'
#' @examples
coverage <- function(data, frequency) {

	uid <- unique(unlist(lapply(data, function(x) unique(x$user_id))))
	if(length(uid) > 1) stop("Only 1 participant per coverage chart allowed")

	if(!is.list(data)) stop("Data is expected to be a list of data frames")

	if(!is.numeric(frequency) || !is.null(names(frequency))) stop("Frequency is supposed to be a named vector")

	# Device information
	device <- paste(
		unique(data$device$platform),
		unique(data$device$device_model),
		lubridate::date(data$device$start_time[1])
	)

	x <- data %>%
		map(~ {
			.x %>%
				distinct(start_time) %>%
				mutate(Hour = lubridate::hour(start_time)) %>%
				mutate(Date = lubridate::date(start_time)) %>%
				count(Date, Hour) %>%
				group_by(Hour) %>%
				summarise(Coverage = sum(n) / n(), .groups = "drop")
		})

	x <- x[names(x) %in% names(frequency)]
	x <- map2(x, frequency, ~mutate(.x, Coverage = round(Coverage / .y, 2)))
	x <- tibble::enframe(x, name = "measure")
	x <- unnest(x, value)
	x <- mutate(x, measure = factor(measure),
							measure = factor(measure, levels = rev(levels(measure))))

	# Plot
	ggplot(x, aes(x = Hour, y = measure, fill = Coverage)) +
		geom_raster() +
		geom_text(aes(label = Coverage), colour = "white") +
		scale_x_continuous(breaks = 0:23) +
		scale_fill_gradient2(midpoint = 0.5, high = "#3F7F93", low = "#d70525") +
		theme_minimal() +
		ggtitle(device)
}

