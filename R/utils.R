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
#' \dontrun{
#' ccopy("K:/data/myproject/", "~/myproject")
#' }
ccopy <- function(from, to = getwd()) {
	# from <- "K:/GHUM-PPW-MICAS-OKPIV-PHD_KOEN-0800-A/emosense/"
	from_list <- dir(path = from, pattern = "*.zip$")
	to_list <- dir(path = to, pattern = "*.zip$")
	copy <- from_list[!(from_list %in% to_list)]
	if (length(copy) == 0) {
		return(message("No files left to copy"))
	}
	message(paste0("Copying ", length(copy), " files."))
	copy <- normalizePath(paste0(from, "/", copy))
	invisible(do.call(file.copy, list(from = copy, to = to, overwrite = FALSE)))
}

#' Fix JSON files
#'
#' When copying data directly coming from CARP, JSON files are often corrupt due to the
#' app not properly closing them. This function attempts to fix the most common
#' problems associated with improper file closure by CARP.
#'
#' Note: Be careful when running this function multiple times. In the case there are
#' many zips and no JSON files, it will be fast. However, when re-running the function,
#' every previously unzipped JSON file will be checked for errors.
#'
#' @param path The pathname of the JSON files.
#' @param files Alternatively, a character list of the input files
#' @param parallel A logical value whether you want to check in parallel. Useful for a lot of files.
#'
#' @return A message indicating whether any files need to be fixed.
#' @export
#' @examples
#' \dontrun{
#' future::plan(future::multisession)
#' files <- test_jsons()
#' fix_jsons(files = files)
#' }
fix_jsons <- function(path = getwd(), files = NULL, parallel = FALSE) {
	# Find all JSON files that are _not_ zipped
	# Thus, make sure you didn't unzip them yet, otherwise this may take a long time
	if(is.null(files)) {
		jsonfiles <- dir(path = path, pattern = "*.json$", all.files = TRUE)
	} else {
		jsonfiles <- files
	}

	if(parallel) {
		future::plan(future::multisession)
	}

	if (length(jsonfiles > 0)) {
		progressr::with_progress({
			n_fixed <- fix_jsons_impl(path, jsonfiles)
		})
	} else {
		return(message("No JSON files found."))
	}

	if(parallel) {
		future::plan(future::sequential)
	}

	return(message("Fixed ", sum(n_fixed), " files"))
}

fix_jsons_impl <- function(path, jsonfiles) {
	p <- progressr::progressor(steps = length(jsonfiles))
	furrr::future_map_int(jsonfiles, ~{
		p()
		file <- normalizePath(paste0(path, "/", path.expand(.x)))
		lines <- readLines(file)
		if(length(lines) > 2) {
			eof <- lines[(length(lines) - 2):length(lines)]
		} else {
			eof <- lines
		}

		# Cases where it can go wrong
		if (eof[1] == "[") {
			write("]", file, append = TRUE)
		} else if (eof[2] == "," & eof[3] == "{}]") {
			return(0L)
		} else if (eof[1] == "{}]" & eof[2] == "]" & eof[3] == "]") {
			write(lines[1:(length(lines) - 2)], file, append = FALSE)
		} else if (eof[2] == "]" & eof[3] == "]") {
			write(lines[1:(length(lines) - 1)], file, append = FALSE)
		} else if (eof[1] == "{}]" & eof[2] == "{}]" & eof[3] == "{}]") {
			write(lines[1:(length(lines) - 2)], file, append = FALSE)
		} else if (eof[2] == "{}]" & eof[3] == "{}]") {
			write(lines[1:(length(lines) - 1)], file, append = FALSE)
		} else if (eof[2] == "," & eof[3] == "]") {
			write(lines[1:(length(lines) - 2)], file, append = FALSE)
			write("]", file, append = TRUE)
		} else if (eof[3] == ",") {
			write("{}]", file, append = TRUE)
		} else if (eof[3] == "[") {
			write("]", file, append = TRUE)
		} else if (nchar(eof[3]) > 3 && substr(eof[3], nchar(eof[3]) - 1, nchar(eof[3])) == "}}") {
			write("]", file, append = TRUE)
		} else {
			return(0L)
		}
		return(1L)
	})
}

#' Test JSON files for being in the correct format.
#'
#' @param path The pathname of the JSON files.
#' @param db A CARP database connection (optional). If provided, will be used to check which files are already in the database and check only those JSON files which are not.
#' @param parallel A logical value whether you want to check in parallel. Useful when there are a lot of files. If you have already used \code{future::plan}, you can leave this parameter to \code{FALSE}.
#'
#' @return A message indicating whether there were any issues and a character vector of the file names that need to be fixed. If there were no issues, no result is returned.
#' @export
test_jsons <- function(path = getwd(), db = NULL, parallel = FALSE) {
	jsonfiles <- dir(path = path, pattern = "*.json$", all.files = TRUE)
	if(!is.null(db)) {
		processed_files <- get_processed_files(db)
		jsonfiles <- jsonfiles[!(jsonfiles %in% processed_files$file_name)]
	}

	if(parallel) {
		future::plan(future::multisession)
	}

	progressr::with_progress({
		p <- progressr::progressor(steps = length(jsonfiles))
		missing <- furrr::future_map_lgl(jsonfiles, ~{
			p()
			tryCatch({
				rjson::fromJSON(file = normalizePath(paste0(path, "/", .x)), simplify = FALSE)
				return(FALSE)
			}, error = function(e) return(TRUE))
		})
	})

	if(parallel) {
		future::plan(future::sequential)
	}

	jsonfiles <- jsonfiles[missing]
	if(length(jsonfiles) == 0) {
		message("No issues found.")
		return(invisible(""))
	} else {
		warning("There were issues in some files")
		return(jsonfiles)
	}
}

#' Unzip CARP output
#'
#' Similar to \link[utils]{unzip}, but makes it easier to unzip all files in a given path
#' with one function call.
#'
#' @param path The path to the directory containing the zip files.
#' @param overwrite Logical value whether you want to overwrite already existing zip files.
#'
#' @export
unzip_carp <- function(path = getwd(), overwrite = FALSE) {
	# Get all json and zipfiles in the path
	jsonfiles <- dir(path = path, pattern = "*.json$", all.files = TRUE)
	tag.json <- substr(jsonfiles, 11, nchar(jsonfiles) - 5)
	zipfiles <- dir(path = path, pattern = "*.zip$")
	tag.zip <- sapply(strsplit(zipfiles, "carp-data-"), function(x) x[2])
	tag.zip <- substr(tag.zip, 1, nchar(tag.zip) - 9)

	# Do not unzip files that already exist as JSON file
	if(!overwrite) {
		zipfiles <- zipfiles[!(tag.zip %in% tag.json)]
	}


	if (length(zipfiles) > 0) {
		message(paste0("Unzipping ", length(zipfiles), " files."))
		# TODO: implement error handling in case unzipping fails
		# (e.g. unexpected end of data)
		invisible(lapply(paste0(path, "/", zipfiles),
										 utils::unzip,
										 exdir = path,
										 overwrite = overwrite
		))
	} else {
		message("No files found to unzip.")
	}
}
