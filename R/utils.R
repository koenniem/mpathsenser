#' Copy CARP zip files to a new location
#'
#' Copy zip files from a source destination to an origin destination where they do not yet exist.
#' That is, it only updates the origin folder from the source folder.
#'
#'
#' @param from A path to copy files from.
#' @param to A path to copy files to.
#' @param recursive Should files from subdirectories be copied?
#'
#' @return A message indicating how many files were copied.
#' @export
#'
#' @examples
#' \dontrun{
#' ccopy("K:/data/myproject/", "~/myproject")
#' }
ccopy <- function(from, to = getwd(), recursive = TRUE) {
	# from <- "K:/GHUM-PPW-MICAS-OKPIV-PHD_KOEN-0800-A/emosense/"
	from_list <- dir(path = from, pattern = "*.zip$", recursive = recursive)
	to_list <- dir(path = to, pattern = "*.zip$", recursive = recursive)
	copy <- from_list[!(from_list %in% to_list)]
	if (length(copy) == 0) {
		return(message("No files left to copy"))
	}
	message(paste0("Copying ", length(copy), " files."))
	copy <- normalizePath(paste0(from, "/", copy))
	invisible(do.call(file.copy, list(from = copy, to = to, overwrite = FALSE)))
}

#' Fix the end of JSON files
#'
#' When copying data directly coming from CARP, JSON files are sometimes corrupted due to the
#' app not properly closing them. This function attempts to fix the most common
#' problems associated with improper file closure by CARP.
#'
#' @param path The path name of the JSON files.
#' @param files Alternatively, a character list of the input files
#' @param recursive Should the listing recurse into directories?
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
fix_jsons <- function(path = getwd(), files = NULL, recursive = TRUE, parallel = FALSE) {

	if (is.null(path) || !is.character(path))
		stop("path must be a character string of the path name")
	if (!is.null(files) && !is.character(files))
		stop("files must be NULL or a character vector of file names")

	# Find all JSON files that are _not_ zipped
	# Thus, make sure you didn't unzip them yet, otherwise this may take a long time
	if (is.null(files)) {
		jsonfiles <- dir(path = path, pattern = "*.json$", all.files = TRUE, recursive = recursive)
	} else {
		jsonfiles <- files
	}

	if (parallel) {
		future::plan(future::multisession)
	}

	if (length(jsonfiles > 0)) {
		progressr::with_progress({
			# Test if files are still corrupted
			jsonfiles <- suppressWarnings(test_jsons(path, jsonfiles))
			n_fixed <- fix_jsons_impl(path, jsonfiles)
		})
	} else {
		return(message("No JSON files found."))
	}

	if (parallel) {
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

		if (length(lines) == 0) {
			return(0L)
		} else if (length(lines) > 2) {
			eof <- lines[(length(lines) - 2):length(lines)]
		} else {
			eof <- lines
		}

		# Cases where it can go wrong
		if (length(eof) == 1 & eof[1] == "[") { #1
			write("]", file, append = TRUE)
		} else if (eof[2] == "," & eof[3] == "{}]") {
			return(0L)
		} else if (eof[1] == "{}]" & eof[2] == "]" & eof[3] == "]") { #2
			write(lines[1:(length(lines) - 2)], file, append = FALSE)
		} else if (eof[2] == "]" & eof[3] == "]") { #3
			write(lines[1:(length(lines) - 1)], file, append = FALSE)
		} else if (eof[1] == "{}]" & eof[2] == "{}]" & eof[3] == "{}]") { #4
			write(lines[1:(length(lines) - 2)], file, append = FALSE)
		} else if (eof[2] == "{}]" & eof[3] == "{}]") { #5
			write(lines[1:(length(lines) - 1)], file, append = FALSE)
		} else if (eof[2] == "," & eof[3] == "]") { #6
			write(lines[1:(length(lines) - 2)], file, append = FALSE)
			write("]", file, append = TRUE)
		} else if (eof[3] == ",") { #7
			write("{}]", file, append = TRUE)
		} else if (eof[3] == "[") { #8
			write("]", file, append = TRUE)
		} else if (nchar(eof[3]) > 3 && substr(eof[3], nchar(eof[3]) - 1, nchar(eof[3])) == "}}") { #9
			write("]", file, append = TRUE)
		} else {
			return(0L)
		}
		return(1L)
	})
}

#' Test JSON files for being in the correct format.
#'
#' @param path The path name of the JSON files.
#' @param files Alternatively, a character list of the input files.
#' @param db A CARP database connection (optional). If provided, will be used to check which files
#' are already in the database and check only those JSON files which are not.
#' @param recursive Should the listing recurse into directories?
#' @param parallel A logical value whether you want to check in parallel. Useful when there are a
#' lot of files. If you have already used \code{\link[future]{plan}}, you can leave this parameter
#' to \code{FALSE}.
#'
#' @return A message indicating whether there were any issues and a character vector of the file
#' names that need to be fixed. If there were no issues, no result is returned.
#' @export
test_jsons <- function(path = getwd(),
											 files = NULL,
											 db = NULL,
											 recursive = TRUE,
											 parallel = FALSE) {

	if (is.null(path) || !is.character(path))
		stop("path must be a character string of the path name")
	if (!is.null(files) && !is.character(files))
		stop("files must be NULL or a character vector of file names")

	if (is.null(files)) {
		jsonfiles <- dir(path = path, pattern = "*.json$", all.files = TRUE, recursive = recursive)
	} else {
		jsonfiles <- files
	}

	if (!is.null(db)) {
		processed_files <- get_processed_files(db)
		jsonfiles <- jsonfiles[!(jsonfiles %in% processed_files$file_name)]
	}

	if (parallel) {
		future::plan(future::multisession)
	}

	progressr::with_progress({
		p <- progressr::progressor(steps = length(jsonfiles))
		missing <- furrr::future_map_lgl(jsonfiles, ~{
			p()
			str <- readLines(normalizePath(paste0(path, "/", .x)), warn = FALSE)
			if (length(str) == 0) { # empty file
				return(TRUE)
			}
		  jsonlite::validate(str)
		})
	})

	if (parallel) {
		future::plan(future::sequential)
	}

	jsonfiles <- jsonfiles[!missing]
	if (length(jsonfiles) == 0) {
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
#' @param recursive Logical value indicating whether to unzip files in subdirectories as well. These
#' files will then be unzipped in their respective subdirectory.
#' @param parallel A logical value whether you want to check in parallel. Useful when there are a
#' lot of files. If you have already used \code{future::plan}, you can leave this parameter to
#' \code{FALSE}.
#'
#' @export
unzip_carp <- function(path = getwd(), overwrite = FALSE, recursive = TRUE, parallel = FALSE) {
	if (is.null(path) || !is.character(path)) stop("path must be a character string")
	if (is.null(overwrite) || !is.logical(overwrite)) stop("overwrite must be TRUE or FALSE")
	if (is.null(recursive) || !is.logical(recursive)) stop("recursive must be TRUE or FALSE")

	if (parallel) {
		future::plan(future::multisession)
	}

	unzipped_files <- 0
	if (recursive) {
		# Find all dirs
		dirs <- list.dirs(path = path, recursive = TRUE)
		dirs <- dirs[2:length(dirs)]

		progressr::with_progress({
			p <- progressr::progressor(steps = length(dirs))
			unzipped_files <- furrr::future_map_int(dirs, ~{
				p()
				unzip_impl(.x, overwrite)
			})
			unzipped_files <- sum(unzipped_files)
		})
	} else {
		unzipped_files <- unzip_impl(path, overwrite)
	}

	if (parallel) {
		future::plan(future::sequential)
	}

	if (unzipped_files > 0) {
		message(paste("Unzipped", unzipped_files, "files."))
	} 	else {
		message("No files found to unzip.")
	}
}

unzip_impl <- function(path, overwrite) {
	# Get all json and zipfiles in the path
	jsonfiles <- dir(path = path, pattern = "*.json$", all.files = TRUE)
	tag_json <- sapply(strsplit(jsonfiles, "carp-data-"), function(x) x[2])
	zipfiles <- dir(path = path, pattern = "*.zip$", all.files = TRUE)
	tag_zip <- sapply(strsplit(zipfiles, "carp-data-"), function(x) x[2])
	tag_zip <- substr(tag_zip, 1, nchar(tag_zip) - 4)

	# Do not unzip files that already exist as JSON file
	if (!overwrite) {
		zipfiles <- zipfiles[!(tag_zip %in% tag_json)]
	}


	if (length(zipfiles) > 0) {
		# TODO: implement error handling in case unzipping fails
		# (e.g. unexpected end of data)
		invisible(lapply(paste0(path, "/", zipfiles),
										 utils::unzip,
										 exdir = path,
										 overwrite = overwrite
		))
	}
	return(length(zipfiles))
}
