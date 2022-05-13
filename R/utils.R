#' Copy mpathsenser zip files to a new location
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
#' ccopy('K:/data/myproject/', '~/myproject')
#' }
ccopy <- function(from, to = getwd(), recursive = TRUE) {
  from_list <- dir(path = from, pattern = "*.zip$", recursive = recursive)
  to_list <- dir(path = to, pattern = "*.zip$", recursive = recursive)
  copy <- from_list[!(from_list %in% to_list)]
  if (length(copy) == 0) {
    return(message("No files left to copy"))
  }
  message(paste0("Copying ", length(copy), " files."))
  to_copy <- suppressWarnings(normalizePath(paste0(from, "/", copy)))
  invisible(do.call(file.copy, list(from = to_copy, to = to, overwrite = FALSE)))
}

#' Fix the end of JSON files
#'
#' When copying data directly coming from m-Path Sense, JSON files are sometimes corrupted due to
#' the app not properly closing them. This function attempts to fix the most common
#' problems associated with improper file closure by m-Path Sense.
#'
#' @inheritSection import Progress
#'
#' @param path The path name of the JSON files.
#' @param files Alternatively, a character list of the input files
#' @param recursive Should the listing recurse into directories?
#' @param parallel A logical value whether you want to check in parallel. Useful for a lot of files.
#'
#' @return A message indicating how many files were fixed.
#' @export
#' @examples
#' \dontrun{
#' future::plan(future::multisession)
#' files <- test_jsons()
#' fix_jsons(files = files)
#' }
fix_jsons <- function(path = getwd(), files = NULL, recursive = TRUE, parallel = FALSE) {

  if (!requireNamespace("vroom", quietly = TRUE)) {
    stop(paste0("package vroom is needed for this function to work. ",
                "Please install it using install.packages(\"vroom\")"),
         call. = FALSE)
  }

  if (is.null(path) || !is.character(path))
    stop("path must be a character string of the path name")
  if (!is.null(files) && !is.character(files))
    stop("files must be NULL or a character vector of file names")

  # Find all JSON files that are _not_ zipped Thus, make sure you didn't unzip them yet,
  # otherwise this may take a long time
  if (is.null(files)) {
    jsonfiles <- dir(path = path, pattern = "*.json$", all.files = TRUE, recursive = recursive)
  } else {
    jsonfiles <- files
  }

  if (parallel) {
    future::plan(future::multisession)
  }

  if (length(jsonfiles > 0)) {
    # Test if files are still corrupted
    jsonfiles <- suppressWarnings(test_jsons(path, jsonfiles))
    n_fixed <- 0L

    if (jsonfiles[1] != "") {
      n_fixed <- fix_jsons_impl(path, jsonfiles)
    }
  } else {
    return(message("No JSON files found."))
  }

  if (parallel) {
    future::plan(future::sequential)
  }

  return(message("Fixed ", sum(n_fixed), " files"))
}

fix_jsons_impl <- function(path, jsonfiles) {
  if (requireNamespace("progressr", quietly = TRUE)) {
    p <- progressr::progressor(steps = length(jsonfiles))
  }

  furrr::future_map_int(jsonfiles, ~{

    if (requireNamespace("progressr", quietly = TRUE)) {
      p()
    }

    path <- suppressWarnings(normalizePath(paste0(path, "/", .x), winslash = "/"))
    lines <- suppressWarnings(vroom::vroom_lines(path, altrep = FALSE, skip_empty_rows = TRUE))
    res <- 0L

    # Are there any illegal characters in the file? If so, these prevent readLines from reading
    # further and also need to be removed before parsing
    illegal_ascii <- any(grepl("[^ -~]", lines))
    if (illegal_ascii) {
      lines <- fix_illegal_ascii(path, lines)
      res <- 1L
    }

    if (length(lines) == 0) {
      return(0L)
    } else if (length(lines) > 2) {
      eof <- lines[(length(lines) - 2):length(lines)]
    } else {
      eof <- lines
    }

    res <- res + fix_eof(path, eof, lines)
    if (res != 0) return(1L) else return(0L)
  })
}

fix_illegal_ascii <- function(path, lines) {
  # Read in the file using vroom, since this doesn't stop early when encountering illegal ASCII's
  # lines <- suppressWarnings(vroom::vroom_lines(path, altrep = FALSE, skip_empty_rows = TRUE))

  # Find which lines contain non ASCII characters
  corrupt <- which(grepl("[^ -~]", lines))

  # Also take the next line, since this is generally a comma we don't want to double
  corrupt <- union(corrupt, corrupt + 1)
  lines <- lines[-corrupt]

  # Write it to file
  vroom::vroom_write_lines(lines, path, num_threads = 1)
  lines
}

fix_eof <- function(file, eof, lines) {
  last <- eof[length(eof)]

  # Cases where it can go wrong
  if (length(eof) == 1 & eof[1] == "[") {
    # 1: Similar to 8?
    write("]", file, append = TRUE)
  } else if (eof[2] == "," & eof[3] == "{}]") {
    return(0L)
  } else if (eof[1] == "{}]" & eof[2] == "]" & eof[3] == "]") {
    # 2: Closing bracket applied thrice. Probably the result of a bad fix applied by this function
    write(lines[1:(length(lines) - 2)], file, append = FALSE)
  } else if (eof[2] == "]" & eof[3] == "]") {
    # 3: Closing bracket applied twice. Probably the result of a bad fix applied by this function
    write(lines[1:(length(lines) - 1)], file, append = FALSE)
  } else if (all(eof == "{}]")) {
    # 4: An empty object followed by a closing bracket is generally the result of a bad fix
    # applied by this function. This autocorrects it.
    write(lines[1:(length(lines) - 2)], file, append = FALSE)
  } else if (eof[2] == "{}]" & eof[3] == "{}]") {
    # 5: An empty object followed by a closing bracket is generally the result of a bad fix
    # applied by this function. This autocorrects it.
    write(lines[1:(length(lines) - 1)], file, append = FALSE)
  } else if (eof[2] == "," & eof[3] == "]") {
    # 6: If the file closed with a comma, another object is expected
    # To fix this, rewrite the entire file without the comma as deleting characters
    # is not possible
    write(lines[1:(length(lines) - 2)], file, append = FALSE)
    write("]", file, append = TRUE)
  } else if (last == ",") {
    # 7: Similar to 6, but without a closing ] for the file
    # Instead of rewriting the file, just add an empty object
    write("{}]", file, append = TRUE)
  } else if (last == "[") {
    # 8: If the last (and also only) line in the file is [ then it means the file was only
    # opened but nothing was written. So, just close it with ] to have an empty JSON file.
    write("]", file, append = TRUE)
  } else if (nchar(last) > 3 &&
             substr(last, nchar(last) - 1, nchar(last)) == "}}") {
    # 9: Is the last line long (>3) and are the last two characters "}}"? Then somehow all
    # we are missing is a closing bracket.
    write("]", file, append = TRUE)
  } else {
    # If no known pattern is detected, return without counting it as a fixed file
    return(0L)
  }

  # If some fix has been applied, the if-else sequence breaks and continues here
  # Count it as a fix
  return(1L)
}

#' Test JSON files for being in the correct format.
#'
#' @inheritSection import Progress
#'
#' @param path The path name of the JSON files.
#' @param files Alternatively, a character list of the input files.
#' @param db A mpathsenser database connection (optional). If provided, will be used to check
#' which files are already in the database and check only those JSON files which are not.
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

  if (requireNamespace("progressr", quietly = TRUE)) {
    p <- progressr::progressor(steps = length(jsonfiles))
  }

  missing <- furrr::future_map_lgl(jsonfiles, ~{

    if (requireNamespace("progressr", quietly = TRUE)) {
      p()
    }
    str <- readLines(suppressWarnings(normalizePath(paste0(path, "/", .x))),
                     warn = FALSE,
                     skipNul = TRUE)
    if (length(str) == 0) {
      # empty file
      return(TRUE)
    }
    jsonlite::validate(str)
  }, .options = furrr::furrr_options(seed = TRUE))

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

#' Unzip m-Path Sense output
#'
#' Similar to \link[utils]{unzip}, but makes it easier to unzip all files in a given path
#' with one function call.
#'
#' @inheritSection import Progress
#'
#' @param path The path to the directory containing the zip files.
#' @param overwrite Logical value whether you want to overwrite already existing zip files.
#' @param recursive Logical value indicating whether to unzip files in subdirectories as well. These
#' files will then be unzipped in their respective subdirectory.
#' @param parallel A logical value whether you want to check in parallel. Useful when there are a
#' lot of files. If you have already used \code{future::plan}, you can leave this parameter to
#' \code{FALSE}.
#'
#' @return A message indicating how many files were unzipped.
#' @export
unzip_data <- function(path = getwd(), overwrite = FALSE, recursive = TRUE, parallel = FALSE) {
  if (is.null(path) || !is.character(path))
    stop("path must be a character string")
  if (is.null(overwrite) || !is.logical(overwrite))
    stop("overwrite must be TRUE or FALSE")
  if (is.null(recursive) || !is.logical(recursive))
    stop("recursive must be TRUE or FALSE")

  if (parallel) {
    future::plan(future::multisession)
  }

  unzipped_files <- 0
  if (recursive) {
    # Find all dirs
    dirs <- list.dirs(path = path, recursive = TRUE)
    dirs <- dirs[2:length(dirs)]

    if (requireNamespace("progressr", quietly = TRUE)) {
      p <- progressr::progressor(steps = length(dirs))
    }

    unzipped_files <- furrr::future_map_int(dirs, ~{
      if (requireNamespace("progressr", quietly = TRUE)) {
        p()
      }
      unzip_impl(.x, overwrite)
    })
    unzipped_files <- sum(unzipped_files)
  } else {
    unzipped_files <- unzip_impl(path, overwrite)
  }

  if (parallel) {
    future::plan(future::sequential)
  }

  if (unzipped_files > 0) {
    message(paste("Unzipped", unzipped_files, "files."))
  } else {
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
    # TODO: implement error handling in case unzipping fails (e.g. unexpected end of data)
    lapply(zipfiles, function(x) {
      tryCatch({
        invisible(utils::unzip(zipfile = file.path(path, x),
                               overwrite = overwrite,
                               junkpaths = TRUE,
                               exdir = path))
      }, error = function(e) warning(paste0("Failed to unzip", x)))
    })
  }
  return(length(zipfiles))
}

