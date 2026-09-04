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
ccopy <- function(from, to, recursive = TRUE) {
  lifecycle::deprecate_stop(
    when = "2.0.0",
    what = "ccopy()",
    with = "base::file.copy()"
  )
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
#' @inheritSection read_mpath_sense Parallel
#'
#' @param path The path name of the JSON files.
#' @param files Alternatively, a character list of the input files
#' @param recursive Should the listing recurse into directories?
#' @inheritParams read_mpath_sense
#'
#' @return A message indicating how many files were fixed, and the number of fixed files invisibly.
#' @export
#' @examples
#' \dontrun{
#' future::plan("multisession")
#' files <- test_jsons()
#' fix_jsons(files = files)
#' }
fix_jsons <- function(
  path = getwd(),
  files = NULL,
  recursive = TRUE,
  .progress = TRUE
) {
  lifecycle::deprecate_stop(
    when = "2.0.0",
    what = "fix_jsons()",
    details = "This function is no longer supported and will be removed in a future version of mpathsenser."
  )
}

#' Test JSON files for being in the correct format.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritSection read_mpath_sense Parallel
#'
#' @param path The path name of the JSON files.
#' @param files Alternatively, a character list of the input files.
#' @param db A mpathsenser database connection (optional). If provided, will be used to check which
#'   files are already in the database and check only those JSON files which are not.
#' @param recursive Should the listing recurse into directories?
#' @inheritParams read_mpath_sense
#'
#' @return A message indicating whether there were any issues and a character vector of the file
#'   names that need to be fixed. If there were no issues, an invisible empty string is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' # Test all files in a directory
#' test_jsons(path = "path/to/jsons", recursive = FALSE)
#'
#' # Test all files in a directory and its subdirectories
#' test_jsons(path = "path/to/jsons", recursive = TRUE)
#'
#' # Test specific files
#' test_jsons(files = c("file1.json", "file2.json"))
#'
#' # Test files in a directory, but skip those that are already in the database
#' test_jsons(path = "path/to/jsons", db = db)
#' }
test_jsons <- function(
  path = getwd(),
  files = NULL,
  db = NULL,
  recursive = TRUE,
  .progress = TRUE
) {
  lifecycle::deprecate_stop(
    when = "2.0.0",
    what = "test_jsons()",
    with = "jsonlite::validate()",
    details = "This function is no longer supported and will be removed in a future version of mpathsenser."
  )
}

#' Unzip m-Path Sense output
#'
#' @description `r lifecycle::badge("stable")`
#'
#'   Similar to \link[utils]{unzip}, but makes it easier to unzip all files in a given path with one
#'   function call.
#'
#' @param path The path to the directory containing the zip files.
#' @param to The output path. Defaults to `path`.
#' @param overwrite Logical value whether you want to overwrite already existing zip files.
#' @param recursive  Logical value indicating whether to search subdirectories recursively.
#'   Extracted files are placed in the corresponding subdirectory of `to`.
#' @inheritParams read_mpath_sense
#'
#' @return Invisibly returns the number of ZIP files successfully unzipped.
#' @export
#'
#' @examples
#' \dontrun{
#' # Unzip all files in a directory
#' unzip_data(path = "path/to/zipfiles", to = "path/to/unzipped", recursive = FALSE)
#'
#' # Unzip files recursively
#' unzip_data(path = "path/to/zipfiles", to = "path/to/unzipped", recursive = TRUE)
#'
#' # Skip files that are already unzipped
#' unzip_data(path = "path/to/zipfiles", to = "path/to/unzipped", overwrite = FALSE)
#' }
unzip_data <- function(
  path,
  to = NULL,
  overwrite = FALSE,
  recursive = TRUE,
  .progress = TRUE
) {
  check_arg(path, "character", n = 1)
  check_arg(to, "character", allow_null = TRUE, n = 1)
  check_arg(overwrite, "logical", n = 1)
  check_arg(recursive, "logical", n = 1)
  check_arg(.progress, "logical", n = 1)

  if (!dir.exists(path)) {
    cli_abort("Directory {.path {path}} does not exist.")
  }

  to <- to %||% path

  zipfiles <- list.files(
    path = path,
    pattern = "\\.zip$",
    recursive = recursive,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(zipfiles) == 0) {
    cli_inform("No ZIP files found.")
    return(invisible(0L))
  }

  # Determine the directory in which each ZIP should be extracted.
  if (recursive) {
    relative_dirs <- dirname(sub(
      paste0("^", normalizePath(path, winslash = "/", mustWork = TRUE), "/?"),
      "",
      normalizePath(zipfiles, winslash = "/", mustWork = TRUE)
    ))
    exdirs <- file.path(to, relative_dirs)
  } else {
    exdirs <- rep(to, length(zipfiles))
  }

  if (.progress) {
    cli::cli_progress_bar(
      name = "Unzipping files",
      total = length(zipfiles)
    )
  }

  extracted_files <- 0L
  failed <- character(0)

  for (i in seq_along(zipfiles)) {
    result <- .unzip_impl(
      zipfile = zipfiles[i],
      exdir = exdirs[i],
      overwrite = overwrite
    )

    if (is.na(result)) {
      failed <- c(failed, zipfiles[i])
    } else {
      extracted_files <- extracted_files + result
    }

    if (.progress) {
      cli::cli_progress_update()
    }
  }

  if (.progress) {
    cli::cli_progress_done()
  }

  if (length(failed) > 0) {
    cli_warn(
      "{length(failed)} of {length(zipfiles)} ZIP files could not be unzipped."
    )
  }

  if (extracted_files > 0) {
    cli_inform("Unzipped {extracted_files} file{?s}.")
  } else {
    cli_inform("No files were unzipped.")
  }

  invisible(extracted_files)
}


.unzip_impl <- function(zipfile, exdir, overwrite) {
  skipped_files <- 0L

  tryCatch(
    {
      result <- withCallingHandlers(
        utils::unzip(
          zipfile = zipfile,
          overwrite = overwrite,
          junkpaths = TRUE,
          exdir = exdir
        ),
        warning = function(w) {
          msg <- trimws(conditionMessage(w))

          if (startsWith(msg, "not overwriting file ")) {
            skipped_files <<- skipped_files + 1L
            invokeRestart("muffleWarning")
          }
        }
      )

      length(result) - skipped_files
    },
    error = function(e) {
      cli_warn(
        "Failed to unzip {.file {basename(zipfile)}}: {conditionMessage(e)}"
      )
      NA_integer_
    }
  )
}
