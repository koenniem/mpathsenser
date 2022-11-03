#' Copy mpathsenser zip files to a new location
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Copy zip files from a source destination to an origin destination where they do not yet exist.
#' That is, it only updates the target folder from the source folder.
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
ccopy <- function(from,
                  to,
                  recursive = TRUE) {
  check_arg(from, "character", n = 1)
  check_arg(to, "character", n = 1)
  check_arg(recursive, "logical", n = 1)

  from_list <- dir(path = from, pattern = "*.zip$", recursive = recursive)
  to_list <- dir(path = to, pattern = "*.zip$", recursive = recursive)
  copy <- setdiff(from_list, to_list)

  if (length(copy) == 0) {
    return(inform("No files left to copy"))
  }

  inform(paste0("Copying ", length(copy), " files."))
  to_copy <- file.path(from, copy)
  invisible(do.call(
    file.copy,
    list(from = to_copy, to = to, overwrite = FALSE, copy.mode = FALSE)
  ))
}

#' Fix the end of JSON files
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' When copying data directly coming from m-Path Sense, JSON files are sometimes corrupted due to
#' the app not properly closing them. This function attempts to fix the most common
#' problems associated with improper file closure by m-Path Sense.
#'
#' @details
#' There are two distinct problems this functions tries to tackle. First of all, there are often
#' bad file endings (e.g. no \code{]}) because the app was closed before it could properly close
#' the file. There are several cases that may be wrong (or even multiple), so it unclear what the
#' precise problems are. As this function is experimental, it may even make it worse by accidentally
#' inserting an incorrect file ending.
#'
#' Secondly, in rare scenarios there are illegal ASCII characters in the JSON files. Not often does
#' this happen, and it is likely because of an OS failure (such as a flush error), a disk failure,
#' or corrupted data during transmit. Nevertheless, these illegal characters make the file
#' completely unreadable. Fortunately, they are detected correctly by
#' \link[mpathsenser]{test_jsons}, but they cannot be imported by \link[mpathsenser]{import}. This
#' functions attempts to surgically remove lines with illegal characters, by removing that specific
#' line as well as the next line, as this is often a comma. It may therefore be too liberal in its
#' approach -- cutting away more data than necessary -- or not liberal enough when the corruption
#' has spread throughout multiple lines. Nevertheless, it is a first step in removing some
#' straightforward corruption from files so that only a small number may still need to be fixed by
#' hand.
#'
#' @inheritSection import Parallel
#'
#' @inheritSection import Progress
#'
#' @param path The path name of the JSON files.
#' @param files Alternatively, a character list of the input files
#' @param recursive Should the listing recurse into directories?
#' @param parallel A logical value whether you want to check in parallel. Useful for a lot of files.
#'
#'   `r lifecycle::badge("deprecated")` As functions should not modify the user's workspace,
#'   directly toggling parallel support has been deprecated. Please use
#'   \code{\link[future]{plan}("multisession")} before calling this function to use multiple
#'   workers.
#'
#' @return A message indicating how many files were fixed.
#' @export
#' @examples
#' \dontrun{
#' future::plan("multisession")
#' files <- test_jsons()
#' fix_jsons(files = files)
#' }
fix_jsons <- function(path = getwd(),
                      files = NULL,
                      recursive = TRUE,
                      parallel = deprecated()) {
  ensure_suggested_package("vroom")

  if (!is.null(path) && !is.character(path)) {
    abort("`path` must be a character string of the path name.")
  }
  if (!is.null(files) && !is.character(files)) {
    abort("`files` must be NULL or a character vector of file names.")
  }
  if (is.null(path) && is.null(files)) {
    abort("`path` and `files` cannot be NULL at the same time.")
  }
  check_arg(recursive, "logical", n = 1)

  # Find all JSON files that are _not_ zipped Thus, make sure you didn't unzip them yet,
  # otherwise this may take a long time
  if (!is.null(path) && is.null(files)) {
    jsonfiles <- list.files(
      path = path,
      pattern = "*.json$",
      all.files = TRUE,
      recursive = recursive,
      full.names = TRUE
    )
  } else if (!is.null(path) && !is.null(files)) {
    jsonfiles <- normalizePath(file.path(path, files), mustWork = TRUE)
  } else {
    jsonfiles <- normalizePath(file.path(files), mustWork = TRUE)
  }

  # Old parallel argument
  if (lifecycle::is_present(parallel)) {
    lifecycle::deprecate_warn(when = "1.1.1",
                              what = "fix_jsons(parallel)",
                              details = c(
                                i = "Use future::plan(\"multisession\") instead"
                              ))
  }

  if (length(jsonfiles > 0)) {
    # Test if files are still corrupted
    jsonfiles <- suppressWarnings(test_jsons(path = NULL, files = jsonfiles))
    n_fixed <- 0L

    if (jsonfiles[1] != "") {
      n_fixed <- fix_jsons_impl(jsonfiles)
    }
  } else {
    abort("No JSON files found.")
  }

  inform(paste0("Fixed ", sum(n_fixed), " files"))
  return(invisible(sum(n_fixed)))
}

fix_jsons_impl <- function(jsonfiles) {
  if (requireNamespace("progressr", quietly = TRUE)) {
    p <- progressr::progressor(steps = length(jsonfiles))
  }

  furrr::future_map_int(jsonfiles, ~ {
    if (requireNamespace("progressr", quietly = TRUE)) {
      p()
    }

    # lines <- suppressWarnings(vroom::vroom_lines(.x, altrep = FALSE, skip_empty_rows = TRUE))
    lines <- readLines(.x, warn = FALSE, skipNul = TRUE)
    res <- 0L

    # Are there any illegal characters in the file? If so, these prevent readLines from reading
    # further and also need to be removed before parsing
    illegal_ascii <- any(grepl("[^ -~]", lines))
    if (illegal_ascii) {
      lines <- fix_illegal_ascii(.x, lines)
      res <- 1L
    }

    if (length(lines) == 0) {
      return(res)
    } else if (length(lines) > 2) {
      eof <- lines[(length(lines) - 2):length(lines)]
    } else {
      eof <- character(3)
      eof[seq_along(lines)] <- lines
    }

    res <- res + fix_eof(.x, eof, lines)
    if (res != 0) {
      return(1L)
    } else {
      return(0L)
    }
  })
}

fix_illegal_ascii <- function(file, lines) {

  # Find which lines contain non ASCII characters
  corrupt <- which(grepl("[^ -~]", lines))

  # Also take the next line, since this is generally a comma we don't want to double
  corrupt <- union(corrupt, corrupt + 1)
  lines <- lines[-corrupt]

  # Write it to file
  vroom::vroom_write_lines(lines, file, num_threads = 1)
  lines
}

fix_eof <- function(file, eof, lines) {
  last <- eof[eof != ""]
  last <- last[length(last)]

  # Cases where it can go wrong
  if (eof[1] == "[" && eof[2] == "" && eof[3] == "") {
    # 1: If the last (and also only) line in the file is [ then it means the file was only
    # opened but nothing was written. So, just close it with ] to have an empty JSON file.
    write("]", file, append = TRUE)

  } else if (eof[1] == "{}]" && eof[2] == "]" && eof[3] == "]") {
    # 2: Closing bracket applied thrice. Probably the result of a bad fix applied by this function
    write(lines[1:(length(lines) - 2)], file, append = FALSE)

  } else if (eof[2] == "]" && eof[3] == "]") {
    # 3: Closing bracket applied twice. Probably the result of a bad fix applied by this function
    write(lines[1:(length(lines) - 1)], file, append = FALSE)

  } else if (all(eof == "{}]")) {
    # 4: An empty object followed by a closing bracket is generally the result of a bad fix
    # applied by this function. This autocorrects it.
    write(lines[1:(length(lines) - 2)], file, append = FALSE)

  } else if (eof[2] == "{}]" && eof[3] == "{}]") {
    # 5: An empty object followed by a closing bracket is generally the result of a bad fix
    # applied by this function. This autocorrects it.
    write(lines[1:(length(lines) - 1)], file, append = FALSE)

  } else if (eof[2] == "," && eof[3] == "]") {
    # 6: If the file closed with a comma, another object is expected
    # To fix this, rewrite the entire file without the comma as deleting characters
    # is not possible
    write(lines[1:(length(lines) - 2)], file, append = FALSE)
    write("]", file, append = TRUE)

  } else if (last == ",") {
    # 7: Similar to 6, but without a closing ] for the file
    # Instead of rewriting the file, just add an empty object
    write("{}]", file, append = TRUE)

  } else if (nchar(last) > 3 &&
    substr(last, nchar(last) - 1, nchar(last)) == "}}") {
    # 8: Is the last line long (>3) and are the last two characters "}}"? Then somehow all
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
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritSection import Parallel
#'
#' @inheritSection import Progress
#'
#' @param path The path name of the JSON files.
#' @param files Alternatively, a character list of the input files.
#' @param db A mpathsenser database connection (optional). If provided, will be used to check which
#'   files are already in the database and check only those JSON files which are not.
#' @param recursive Should the listing recurse into directories?
#' @param parallel A logical value whether you want to check in parallel. Useful when there are a
#'   lot of files. If you have already used \code{\link[future]{plan}}, you can leave this parameter
#'   to \code{FALSE}.
#'
#'   `r lifecycle::badge("deprecated")` As functions should not modify the user's workspace,
#'   directly toggling parallel support has been deprecated. Please use
#'   \code{\link[future]{plan}("multisession")} before calling this function to use multiple
#'   workers.
#'
#' @return A message indicating whether there were any issues and a character vector of
#'   the file names that need to be fixed. If there were no issues, an invisible empty string is
#'   returned.
#' @export
test_jsons <- function(path = getwd(),
                       files = NULL,
                       db = NULL,
                       recursive = TRUE,
                       parallel = deprecated()) {
  if (!is.null(path) && !is.character(path)) {
    abort("`path` must be a character string of the path name.")
  }
  if (!is.null(files) && !is.character(files)) {
    abort("`files` must be NULL or a character vector of file names.")
  }
  if (is.null(path) && is.null(files)) {
    abort("`path` and `files` cannot be NULL at the same time.")
  }
  check_arg(recursive, "logical", n = 1)

  # Find all JSON files that are _not_ zipped Thus, make sure you didn't unzip them yet,
  # otherwise this may take a long time
  if (!is.null(path) && is.null(files)) {
    jsonfiles <- list.files(
      path = path,
      pattern = "*.json$",
      all.files = TRUE,
      recursive = recursive,
      full.names = TRUE
    )
  } else if (!is.null(path) && !is.null(files)) {
    jsonfiles <- normalizePath(file.path(path, files), mustWork = TRUE)
  } else {
    jsonfiles <- normalizePath(file.path(files), mustWork = TRUE)
  }


  if (!is.null(db)) {
    processed_files <- get_processed_files(db)
    jsonfiles <- jsonfiles[!(jsonfiles %in% processed_files$file_name)]
  }

  # Old parallel argument
  if (lifecycle::is_present(parallel)) {
    lifecycle::deprecate_warn(when = "1.1.1",
                              what = "test_jsons(parallel)",
                              details = c(
                                i = "Use future::plan(\"multisession\") instead."
                              ))
  }

  if (requireNamespace("progressr", quietly = TRUE)) {
    p <- progressr::progressor(steps = length(jsonfiles))
  }

  missing <- furrr::future_map_lgl(jsonfiles, ~ {
    if (requireNamespace("progressr", quietly = TRUE)) {
      p()
    }
    str <- readLines(.x, warn = FALSE, skipNul = TRUE)
    if (length(str) == 0) {
      # empty file
      return(TRUE)
    }
    jsonlite::validate(str)
  }, .options = furrr::furrr_options(seed = TRUE))

  jsonfiles <- jsonfiles[!missing]
  if (length(jsonfiles) == 0) {
    inform("No issues found.")
    return(invisible(""))
  } else {
    warn("There were issues in some files")
    return(normalizePath(jsonfiles))
  }
}

#' Unzip m-Path Sense output
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Similar to \link[utils]{unzip}, but makes it easier to unzip all files in a given path
#' with one function call.
#'
#' @inheritSection import Parallel
#'
#' @inheritSection import Progress
#'
#' @param path The path to the directory containing the zip files.
#' @param to The output path.
#' @param overwrite Logical value whether you want to overwrite already existing zip files.
#' @param recursive Logical value indicating whether to unzip files in subdirectories as well. These
#' files will then be unzipped in their respective subdirectory.
#' @param parallel A logical value whether you want to check in parallel. Useful when there are a
#' lot of files. If you have already used \code{future::plan}, you can leave this parameter to
#' \code{FALSE}.
#'
#'   `r lifecycle::badge("deprecated")` As functions should not modify the user's workspace,
#'   directly toggling parallel support has been deprecated. Please use
#'   \code{\link[future]{plan}("multisession")} before calling this function to use multiple
#'   workers.
#'
#' @return A message indicating how many files were unzipped.
#' @export
unzip_data <- function(path = getwd(),
                       to = NULL,
                       overwrite = FALSE,
                       recursive = TRUE,
                       parallel = deprecated()) {
  check_arg(path, "character", n = 1)
  check_arg(to, "character", allow_null = TRUE, n = 1)
  check_arg(overwrite, "logical", n = 1)
  check_arg(recursive, "logical", n = 1)

  if (is.null(to)) {
    to <- path
  }

  # Old parallel argument
  if (lifecycle::is_present(parallel)) {
    lifecycle::deprecate_warn(when = "1.1.1",
                              what = "unzip_data(parallel)",
                              details = c(
                                i = "Use future::plan(\"multisession\") instead"
                              ))
  }

  unzipped_files <- 0
  if (recursive) {
    # Find all dirs
    dirs <- list.dirs(path = path, recursive = TRUE)

    if (requireNamespace("progressr", quietly = TRUE)) {
      p <- progressr::progressor(steps = length(dirs))
    }

    unzipped_files <- furrr::future_map_int(dirs, ~ {
      if (requireNamespace("progressr", quietly = TRUE)) {
        p()
      }
      unzip_impl(.x, to, overwrite)
    })
    unzipped_files <- sum(unzipped_files)
  } else {
    unzipped_files <- unzip_impl(path, to, overwrite)
  }

  if (unzipped_files > 0) {
    inform(paste("Unzipped", unzipped_files, "files."))
  } else {
    inform("No files found to unzip.")
  }
}

unzip_impl <- function(path, to, overwrite) {
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
      tryCatch(
        {
          invisible(utils::unzip(
            zipfile = file.path(path, x),
            overwrite = overwrite,
            junkpaths = TRUE,
            exdir = to
          ))
        },
        error = function(e) warn(paste0("Failed to unzip", x))
      )
    })
  }
  return(length(zipfiles))
}
