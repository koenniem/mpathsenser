#' Import mpathsenser files into a database (mpathsenser data scheme)
#'
#' @description `r lifecycle::badge("stable")`
#'
#'   Import JSON files from m-Path Sense into a structured database. This function is the bread and
#'   butter of this package, as it creates (or rather fills) the database that most of the other
#'   functions in this package use.
#'
#' @details \code{import} is highly customisable in the sense that you can specify which sensors to
#'   import (even though there may be more in the files) and it also allows batching for a speedier
#'   writing process. If processing in parallel is active, it is recommended that \code{batch_size}
#'   be a scalar multiple of the number of CPU cores the parallel cluster can use. If a single JSON
#'   file in the batch causes and error, the batch is terminated (but not the function) and it is up
#'   to the user to fix the file. This means that if \code{batch_size} is large, many files will not
#'   be processed. Set \code{batch_size} to 1 for sequential (one-by-one) file processing.
#'
#'   Currently, only SQLite is supported as a backend. Due to its concurrency restriction, the
#'   `parallel` option is disabled. To get an indication of the progress so far, set one of the
#'   \link[progressr]{handlers} using the \code{progressr} package, e.g.
#'   \code{progressr::handlers(global = TRUE)} and \code{progressr::handlers('progress')}.
#'
#' @section Parallel: This function supports parallel processing in the sense that it is able to
#'   distribute it's computation load among multiple workers. To make use of this functionality, run
#'   \code{\link[future]{plan}("multisession")} before calling this function.
#'
#' @section Progress: You can be updated of the progress of this function by using the
#'   \code{\link[progressr]{progress}} package. See \code{progressr}'s
#'   \href{https://cran.r-project.org/package=progressr/vignettes/progressr-intro.html}{vignette} on
#'   how to subscribe to these updates.
#'
#' @param path The path to the file directory
#' @param db Valid database connection.
#' @param sensors Select one or multiple sensors as in \code{\link[mpathsenser]{sensors}}. Leave
#'   NULL to extract all sensor data.
#' @param batch_size The number of files that are to be processed in a single batch.
#' @param backend Name of the database backend that is used. Currently, only RSQLite is supported.
#' @param recursive Should the listing recurse into directories?
#' @param dbname `r lifecycle::badge("deprecated")`: Creating new databases on the fly has been
#'   deprecated as it is better to separate the two functions. You must now create a new database
#'   using \code{\link[mpathsenser]{create_db}} or reuse an existing one.
#' @param overwrite_db `r lifecycle::badge("deprecated")`: This argument was used when database
#'   creation in \code{import} was still supported. As this functionality is deprecated,
#'   \code{overwrite_db} is now ignored and will be removed in future versions.
#' @param parallel A value that indicates whether to do reading in and processing in parallel. If
#'   this argument is a number, this indicates the number of workers that will be used.
#'
#'   `r lifecycle::badge("deprecated")`: As functions should not modify the user's workspace,
#'   directly toggling parallel support has been deprecated. Please use
#'   \code{\link[future]{plan}("multisession")} before calling this function to use multiple
#'   workers.
#'
#' @return A message indicating how many files were imported. Imported database can be reopened
#'   using \link[mpathsenser]{open_db}.
#' @export
import <- function(path = getwd(),
                   db = NULL,
                   sensors = NULL,
                   batch_size = 24,
                   backend = "RSQLite",
                   recursive = TRUE,
                   dbname = deprecated(),
                   overwrite_db = deprecated(),
                   parallel = deprecated()) {

  # Check if required packages are installed
  if (!requireNamespace("dbx", quietly = TRUE)) {
    abort(c(
      "package `dbx` must be installed for `import()` to work. ",
      i = "Please install it using install.packages(\"dbx\")."
    ))
  }
  if (!requireNamespace("rjson", quietly = TRUE)) {
    abort(c(
      "package `rjson` is needed for this function to work. ",
      i = "Please install it using install.packages(\"rjson\")"
    ))
  }

  # Handle old dbname argument
  if (lifecycle::is_present(dbname)) {
    lifecycle::deprecate_stop(when = "1.1.1",
                              what = "import(dbname)",
                              details = c(
                                i = "Please create a database using `create_db()` first."
                              ))
  }

  # Handle old overwrite argument
  if (lifecycle::is_present(overwrite_db)) {
    lifecycle::deprecate_warn(
      when = "1.1.1",
      what = "import(overwrite_db)",
      details = c("*" = "`import()` no longer supports the ability to create databases.",
                  i = "Argument `overwrite_db` is ignored.")
    )
  }

  # Handle old parallel argument
  if (lifecycle::is_present(parallel)) {
    lifecycle::deprecate_warn(when = "1.1.1",
                              what = "import(parallel)",
                              details = c(
                                i = "Use future::plan(\"multisession\") instead"
                              ))
  }

  # Check if database is valid
  if (!is.null(db)) {
    if (!dbIsValid(db)) {
      abort(c(
        "db must be a valid database connection.",
        i = "Did you forget to save the connection to the variable?"))
    }
  }

  # Retrieve all JSON files
  files <- dir(path = path, pattern = "*.json$", recursive = recursive)

  if (length(files) == 0) {
    path <- normalizePath(path, "/", mustWork = FALSE)
    if (!dir.exists("foo")) {
      abort(c(
        "Directory {path} does not exist.",
        i = "Did you make a typo in the path name?"
      ))
    }

    abort(c(
      "Can't find JSON files in {path}.",
      i = "Did you put the JSON files in the correct directory?"))
  }

  # If there are no duplicate files Proceed with the unsafe (but fast) check to prevent duplicate
  # insertion into db
  if (anyDuplicated(files) == 0) {
    processed_files <- get_processed_files(db)
    # Keep files _not_ already registered in db
    files <- files[!(files %in% processed_files$file_name)]

    if (length(files) == 0) {
      dbDisconnect(db)
      return(inform("No new files to process."))
    }
  }

  # Split the files into batches
  files <- split(files, ceiling(seq_along(files) / batch_size))

  # Display progress, if enabled
  if (requireNamespace("progressr", quietly = TRUE)) {
    p <- progressr::progressor(steps = length(files))
  }

  for (i in seq_along(files)) {

    # Get data from the files, in parallel if needed
    res <- furrr::future_map(files[[i]],
      ~ import_impl(path, .x, db@dbname, sensors),
      .options = furrr::furrr_options(seed = TRUE)
    )

    res <- purrr::transpose(res)
    res$studies <- bind_rows(res$studies)
    res$participants <- bind_rows(res$participants)
    res$file <- bind_rows(res$file)

    # Interesting feature in purrr::transpose. If the names would not be explicitly set, it would
    # only take the names of the first entry of the list. So, if some sensors would be present in
    # the first entry (e.g. low sampling sensors like Device), it would disappear from the data
    # altogether.

    # Turn data list inside out, drop NULLs and bind sensors from different files together
    res$data <- purrr::flatten(res$data)
    data <- purrr::compact(res$data)
    res$data <- NULL # memory efficiency
    data <- purrr::transpose(data, .names = sort(mpathsenser::sensors))
    data <- lapply(data, bind_rows)
    data <- purrr::compact(data)
    data <- lapply(data, distinct) # Filter out duplicate rows (for some reason)

    # Write all data as a single transaction, safely.
    try(
      expr = .import_write_to_db(db, res, data),
      silent = TRUE
    )

    # Update progress bar
    if (requireNamespace("progressr", quietly = TRUE)) {
      p(sprintf("Added %g out of %g", i * batch_size, length(files) * batch_size))
    }
  }

  processed_files <- get_processed_files(db)
  complete <- all(unlist(files, use.names = FALSE) %in% processed_files$file_name)
  if (complete) {
    inform("All files were successfully written to the database.")
  } else {
    warn("Some files could not be written to the database.")
  }
}

# Function for writing all data in a batch to the database
.import_write_to_db <- function(db, meta_data, sensor_data) {
  DBI::dbWithTransaction(db, {
    add_study(db, unique(meta_data$studies))
    add_participant(db, unique(meta_data$participants))

    for (i in seq_along(sensor_data)) {
      save2db(db, names(sensor_data)[[i]], sensor_data[[i]])
    }

    # Add files to list of processed files
    add_processed_files(db, meta_data$file)
  })
}

.import_read_json <- function(path, filename) {
  # Try to read in the file. If the file is corrupted for some reason, skip this one
  file <- normalizePath(file.path(path, filename[1]))
  file <- readLines(file, warn = FALSE, skipNul = TRUE)
  file <- paste0(file, collapse = "")
  if (file == "") {
    p_id <- sub(".*?([0-9]{5}).*", "\\1", filename[1])
    out$studies[1, ] <- c(study_id = "-1", data_format = NA)
    out$participants[1, ] <- c(study_id = "-1", participant_id = p_id)
    out$file[1, ] <- c(file_name = filename[1], study_id = "-1", participant_id = p_id)
    # next
    return(out)
  }

  if (!jsonlite::validate(file)) {
    warn(paste0("Invalid JSON in file ", filename[1]))
    return(out)
  }

  possible_error <- tryCatch(
    {
      data <- rjson::fromJSON(file, simplify = FALSE)
    },
    error = function(e) e
  )

  if (inherits(possible_error, "error")) {
    warn(paste("Could not read", filename[1]))
    return(out)
  }
}


import_impl <- function(path, filename, db_name, sensors) {
  out <- list(
    studies = data.frame(study_id = character(), data_format = character()),
    participants = data.frame(
      study_id = character(length(filename)),
      participant_id = character(length(filename))
    ),
    file = data.frame(
      file_name = character(length(filename)),
      study_id = character(length(filename)),
      participant_id = character(length(filename))
    ),
    data = vector("list", length(filename))
  )

  # Try to read in the file. If the file is corrupted for some reason, skip this one
  file <- normalizePath(file.path(path, filename[1]))
  file <- readLines(file, warn = FALSE, skipNul = TRUE)
  file <- paste0(file, collapse = "")
  if (file == "") {
    p_id <- sub(".*?([0-9]{5}).*", "\\1", filename[1])
    out$studies[1, ] <- c(study_id = "-1", data_format = NA)
    out$participants[1, ] <- c(study_id = "-1", participant_id = p_id)
    out$file[1, ] <- c(file_name = filename[1], study_id = "-1", participant_id = p_id)
    # next
    return(out)
  }

  if (!jsonlite::validate(file)) {
    warn(paste0("Invalid JSON in file ", filename[1]))
    return(out)
  }

  possible_error <- tryCatch(
    {
      data <- rjson::fromJSON(file, simplify = FALSE)
    },
    error = function(e) e
  )

  if (inherits(possible_error, "error")) {
    warn(paste("Could not read", filename[1]))
    return(out)
  }

  # Check if it is not an empty file Skip this file if empty, but add it to the list of
  # processed file to register this incident and to avoid having to do it again
  if (length(data) == 0 | identical(data, list()) | identical(data, list(list()))) {
    p_id <- sub(".*?([0-9]{5}).*", "\\1", filename[1])
    out$studies <- rbind(out$studies, data.frame(study_id = "-1", data_format = NA))
    out$participants[1, ] <- c(study_id = "-1", participant_id = p_id)
    out$file[1, ] <- c(file_name = filename[1], study_id = "-1", participant_id = p_id)
    return(out)
  }

  # Clean-up and extract the header and body
  data <- tibble::tibble(
    header = lapply(data, function(x) x[1]),
    body = lapply(data, function(x) x[2])
  )

  # Extract columns Define a safe_extract function that leaves no room for NULLs,
  # since unnest cannot handle a column full of NULLs
  safe_extract <- function(vec, var) {
    out <- lapply(vec, function(obs) {
      tmp <- obs[[1]][[var]]
      if (is.null(tmp)) {
        return(NULL)
      } else {
        return(tmp)
      }
    })
    if (all(vapply(out, is.null, logical(1), USE.NAMES = FALSE))) {
      out <- rep("N/A", length(out))
    }
    return(out)
  }
  data$study_id <- safe_extract(data$header, "study_id")
  # data$device_role_name <- safe_extract(data$header, 'device_role_name')
  # data$trigger_id <- safe_extract(data$header, 'trigger_id')
  data$device_role_name <- NULL
  data$trigger_id <- NULL
  data$participant_id <- safe_extract(data$header, "user_id")
  data$start_time <- safe_extract(data$header, "start_time")
  data$data_format <- lapply(data$header, function(x) x[[1]]["data_format"])
  data$sensor <- safe_extract(data$data_format, "name")
  data$data_format <- safe_extract(data$data_format, "namespace")
  data$header <- NULL
  data <- unnest(data, c(.data$study_id:.data$sensor))

  # Due to the hacky solution above, filter out rows where the participant_id is missing,
  # usually in the last entry of a file
  data <- data[!is.na(data$participant_id) & data$participant_id != "N/A", ]

  # Open db
  tmp_db <- open_db(NULL, db_name)

  # Safe duplicate check before insertion
  # Check if file is already registered as processed
  # Now using the participant_id and study_id
  this_file <- data.frame(
    file_name = filename[1],
    study_id = unique(data$study_id),
    participant_id = unique(data$participant_id)
  )

  matches <- DBI::dbGetQuery(
    tmp_db,
    paste0(
      "SELECT COUNT(*) AS `n` FROM `ProcessedFiles` ",
      "WHERE (`file_name` = '",
      this_file$file_name,
      "' ",
      "AND `participant_id` = '",
      this_file$participant_id,
      "' ",
      "AND `study_id` = '",
      this_file$study_id,
      "')"
    )
  )[1, 1]
  if (matches > 0) {
    dbDisconnect(tmp_db)
    # next  # File was already processed
    return(out)
  }

  # Populate study specifics to db
  study_id <- distinct(data, .data$study_id, .data$data_format)

  # Add participants
  participant_id <- distinct(data, .data$participant_id, .data$study_id)

  # Make sure top-level of data$body is called body and not carp_body as in the new version
  data <- mutate(data, body = purrr::modify(body, purrr::set_names, nm = "body"))

  # Divide et impera
  data <- split(data, as.factor(data$sensor), drop = TRUE)

  # Drop useless data
  data[["unknown"]] <- NULL

  # Set names to capitals in accordance with the table names
  names <- strsplit(names(data), "_")
  names <- lapply(names, function(x) {
    paste0(toupper(substring(x, 1, 1)), substring(x, 2), collapse = "")
  })
  names[names == "Apps"] <- "InstalledApps" # Except InstalledApps...

  # Select sensors, if not NULL
  if (!is.null(sensors)) {
    data <- data[names %in% sensors]
    names <- names[names %in% sensors]
  }

  # Call function for each sensor
  tryCatch(
    {
      data <- purrr::imap(data, ~ which_sensor(.x, .y))
      names(data) <- names

      out$studies <- rbind(out$studies, study_id)
      out$participants[1, ] <- participant_id
      out$file[1, ] <- this_file # Save to output
      out$data[[1]] <- data
    },
    error = function(e) {
      warn(paste0("processing failed for file ", filename[1]))
    }
  )

  # Close db connection of worker
  dbDisconnect(tmp_db)

  return(out)
}
