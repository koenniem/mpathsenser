
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
	zipfiles <- dir(pattern = "*.zip$")
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
		invisible(lapply(zipfiles, unzip))
	} else {
		message("No files found to unzip.")
	}

	if(zipfiles == 0 && jsonfiles == 0) {
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
		for(j in 1:length(data)) {
			# Get sensor name
			sensor <- names(data)[[j]]
			tmp <- data[[sensor]]

			# Make the package for the decision function explicit
			# otherwise globals won't know to export it
			out[[j]] <- which_sensor(tmp, sensor)
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

# Per hour
freq <- c(
	accelerometer = 3600, # Once per second
	air_quality = 1,
	app_usage = 360,
	bluetooth = 60,  # Once per minute
	light = 360, # Once per 10 seconds
	location = 120, # Once per 30 seconds
	memory = 60, # Once per minute
	noise = 120,
	weather = 1,
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
#' @importFrom magrittr "%>%"
#'
#' @examples
#' setwd("~/data")
#' clean()
#' data <- import()
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
#' coverage(data, freq)
coverage <- function(data, frequency) {

	uid <- unique(unlist(lapply(data, function(x) unique(x$user_id))))
	if(length(uid) > 1) stop("Only 1 participant per coverage chart allowed")

	if(!is.list(data)) stop("Data is expected to be a list of data frames")

	if(!is.numeric(frequency) || is.null(names(frequency)))
		stop("Frequency is supposed to be a named numeric vector")

	# Retain only frequencies that appear in the data
	frequency <- frequency[names(frequency) %in% names(data)]

	# Retain only variables that appear in the frequency
	data <- data[names(data) %in% names(frequency)]

	# Get device information
	device <- paste(
		unique(data$device$platform),
		unique(data$device$device_model),
		as.Date(data$device$start_time[1])
	)

	# Get the number of observations per hour
	data <- lapply(data, function(.x) {
		.x %>%
			dplyr::distinct(start_time) %>%
			dplyr::mutate(Hour = lubridate::hour(start_time)) %>%
			dplyr::mutate(Date = lubridate::date(start_time)) %>%
			dplyr::count(Date, Hour) %>%
			dplyr::group_by(Hour) %>%
			dplyr::summarise(Coverage = sum(n) / dplyr::n(), .groups = "drop")
	})

	# Calculate its target frequency ratio
	data <- mapply(
		FUN = function(.x, .y) dplyr::mutate(.x, Coverage = round(Coverage / .y, 2)),
		.x = data,
		.y = frequency,
		SIMPLIFY = F)

	# Pour into ggplot format
	data <- mapply(
		FUN = function(.x, .y) dplyr::mutate(.x, measure = .y),
		.x = data,
		.y = names(data),
		SIMPLIFY = FALSE)
	data <- do.call("rbind", data)
	data$measure <- factor(data$measure)
	data$measure <- factor(data$measure, levels = rev(levels(data$measure)))
	#
	# data <- tibble::enframe(data, name = "measure")
	# data <- tidyr::unnest(data, value)
	# data <- dplyr::mutate(data, measure = factor(measure),
	# 						measure = factor(measure, levels = rev(levels(measure))))

	# Plot
	ggplot2::ggplot(data = data,
									mapping = ggplot2::aes(x = Hour, y = measure, fill = Coverage)) +
		ggplot2::geom_tile() +
		ggplot2::geom_text(mapping = ggplot2::aes(label = Coverage),
											 colour = "white") +
		ggplot2::scale_x_continuous(breaks = 0:23) +
		ggplot2::scale_fill_gradientn(colours = c("#d70525", "#FFFFFF", "#3F7F93"),
																	breaks = c(0, 0.5, 1),
																	labels = c(0, 0.5, 1),
																	limits = c(0,1)) +
		ggplot2::theme_minimal() +
		ggplot2::ggtitle(device)
}

