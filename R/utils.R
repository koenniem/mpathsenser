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
    cli_progress_bar(
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
      cli_progress_update()
    }
  }

  if (.progress) {
    cli_progress_done()
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
