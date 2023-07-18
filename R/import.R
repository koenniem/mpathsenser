#' Import m-Path Sense files into a database
#'
#' @description `r lifecycle::badge("stable")`
#'
#'   Import JSON files from m-Path Sense into a structured database. This function is the bread and
#'   butter of this package, as it creates (or rather fills) the database that most of the other
#'   functions in this package use.
#'
#' @details `import` allows you to specify which sensors to import (even though there may be more in
#'   the files) and it also allows batching for a speedier writing process. If processing in
#'   parallel is active, it is recommended that `batch_size` be a scalar multiple of the number of
#'   CPU cores the parallel cluster can use. If a single JSON file in the batch causes and error,
#'   the batch is terminated (but not the function) and it is up to the user to fix the file. This
#'   means that if `batch_size` is large, many files will not be processed. Set `batch_size` to 1
#'   for sequential (one-by-one) file processing.
#'
#'   Currently, only SQLite is supported as a backend. Due to its concurrency restriction, the
#'   `parallel` option is disabled. To get an indication of the progress so far, set one of the
#'   [progressr::handlers()] using the `progressr` package, e.g. `progressr::handlers(global =
#'   TRUE)` and `progressr::handlers('progress')`.
#'
#' @section Parallel: This function supports parallel processing in the sense that it is able to
#'   distribute it's computation load among multiple workers. To make use of this functionality, run
#'   \href{https://rdrr.io/cran/future/man/plan.html}{\code{future::plan("multisession")}} before
#'   calling this function.
#'
#' @section Progress: You can be updated of the progress of this function by using the
#'   [progressr::progress()] package. See `progressr`'s
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
#'
#' @returns A message indicating how many files were imported. Imported database can be reopened
#'   using [open_db()].
#' @export
import <- function(path = getwd(),
                   db,
                   sensors = NULL,
                   batch_size = 24,
                   backend = "RSQLite",
                   recursive = TRUE) {

  # Check arguments
  check_arg(path, type = "character", n = 1)
  check_db(db)
  check_sensors(sensors, allow_null = TRUE)
  check_arg(batch_size, "integerish", n = 1)
  check_arg(backend, "character", n = 1)
  check_arg(recursive, "logical", n = 1)

  # Normalise path and check if directory exists
  path <- normalizePath(file.path(path), mustWork = FALSE)
  if (!dir.exists(path)) {
    abort(c(
      paste("Directory", path, "does not exist."),
      i = "Did you make a typo in the path name?"
    ))
  }

  # Retrieve all JSON files
  files <- list.files(path = path, pattern = "*.json$", recursive = recursive)

  if (length(files) == 0) {
    abort(c(
      paste("Can't find JSON files in", path, "."),
      i = "Did you put the JSON files in the correct directory?"
    ))
  }

  # If there are no duplicate files Proceed with the unsafe (but fast) check to prevent duplicate
  # insertion into db
  if (anyDuplicated(files) == 0) {
    processed_files <- get_processed_files(db)
    # Keep files _not_ already registered in db
    files <- files[!(files %in% processed_files$file_name)]

    if (length(files) == 0) {
      inform("No new files to process.")
      return(invisible(""))
    }
  }

  # Split the files into batches
  batches <- split(files, ceiling(seq_along(files) / batch_size))

  # Display progress, if enabled
  if (requireNamespace("progressr", quietly = TRUE)) {
    p <- progressr::progressor(steps = length(batches))
  }

  for (i in seq_along(batches)) {
    # All the files in the batch
    batch_files <- batches[[i]]

    # Read in all the files, in parallel
    batch_data <- furrr::future_map(
      .x = batch_files,
      .f = ~ .import_read_json(path, .x),
      .options = furrr::furrr_options(seed = TRUE)
    )
    names(batch_data) <- batch_files

    # Remove NULLs, as we want to keep these files unmarked
    # (something went wrong when reading in the data)
    batch_data <- purrr::compact(batch_data)

    # For the files marked as NA, register the study, participant_id, and the file name,
    # but keep the data empty as there was no data (empty file).
    batch_na <- batch_files[is.na(batch_data)]
    batch_data <- batch_data[!is.na(batch_data)]

    # Save the empty files in data frame to add to the meta data later
    # Meta data is what is being registered
    if (length(batch_na) > 0) {
      p_id <- purrr::map_chr(strsplit(batch_na, "_"), ~.x[3])
      if (any(is.na(p_id))) {
        p_id[is.na(p_id)] <- "N/A"
      }
      batch_na <- data.frame(
        participant_id = p_id,
        study_id = "-1",
        data_format = NA,
        file_name = batch_na
      )
    }

    # Clean the lists to be in a dataframe format
    batch_data <- furrr::future_map(
      .x = batch_data,
      .f = .import_clean,
      .options = furrr::furrr_options(seed = TRUE)
    )

    # Remove NULLs, as we want to keep these files unmarked
    # (something went wrong when reading in the data)
    batch_data <- purrr::compact(batch_data)

    # Generate the meta data, i.e. the participant_id, study_id, and file name to be written to the
    # database later.
    # Note: If a file contains multiple participant_ids, study_ids, or file names (which shouldn't
    # be possible), there will be more entries in the meta data than there are entries in the
    # batch_data. Therefore, we must use the meta_data as an index whenever combining it with
    # meta_data.
    meta_data <- purrr::pmap_dfr(
      .l = list(
        batch_data,
        names(batch_data),
        seq_along(batch_data)
      ),
      .f = ~ distinct(..1,
        participant_id,
        study_id,
        data_format,
        file_name = ..2,
        id = ..3
      )
    )
    names(batch_data) <- seq_along(batch_data)

    # From the dataframes we can get the participant_id and study_id
    # Use this information to query the database to find out whether this file has already been
    # processed. If already processed, drop it.
    duplicates <- .import_is_duplicate(
      db_name = db@dbname,
      meta_data
    )
    meta_data <- meta_data[!duplicates, ]
    batch_data <- batch_data[names(batch_data) %in% meta_data[["id"]]]

    # Extract the sensor data
    batch_data <- furrr::future_map(
      .x = batch_data,
      .f = ~ .import_extract_sensor_data(.x, sensors),
      .options = furrr::furrr_options(seed = TRUE)
    )

    # If a file failed to process, NA is returned
    batch_data <- batch_data[!is.na(batch_data)]
    meta_data <- meta_data[meta_data[["id"]] %in% names(batch_data), ]

    # If there were some empty files, add them to the meta data now
    if (is.data.frame(batch_na)) {
      meta_data <- bind_rows(meta_data, batch_na)
    }

    # Interesting feature in purrr::transpose. If the names would not be explicitly set, it would
    # only take the names of the first entry of the list. So, if some sensors would be present in
    # the first entry (e.g. low sampling sensors like Device), it would disappear from the data
    # altogether.
    # Turn data list inside out, drop NULLs and bind sensors from different files together
    batch_data <- purrr::transpose(batch_data, .names = sort(mpathsenser::sensors))
    batch_data <- lapply(batch_data, bind_rows)
    batch_data <- purrr::compact(batch_data)
    batch_data <- lapply(batch_data, distinct) # Filter out duplicate rows (for some reason)

    # Write all data as a single transaction, safely.
    try(
      expr = .import_write_to_db(db, meta_data, batch_data),
      silent = TRUE
    )

    # Update progress bar
    if (requireNamespace("progressr", quietly = TRUE)) {
      p(sprintf("Added %g out of %g", i * batch_size, length(files)))
    }
  }

  processed_files <- get_processed_files(db)
  complete <- unlist(files, use.names = FALSE) %in% processed_files$file_name
  if (all(complete)) {
    inform("All files were successfully written to the database.")
    return(invisible(""))
  } else {
    warn("Some files could not be written to the database.")
    return(files[!complete])
  }
}

# Possible outputs:
# The parsed JSON list (a list).
# NA: File was successfully read, but empty.
# NULL: Something went wrong, mark as unprocessed.
.import_read_json <- function(path, filename) {
  if (!is.null(path)) {
    full_path <- normalizePath(file.path(path, filename), mustWork = FALSE)
  } else {
    full_path <- normalizePath(file.path(filename), mustWork = FALSE)
  }

  if (!file.exists(full_path)) {
    warn(paste(filename, "does not exist."))
    return(NA)
  }

  # Try to read in the file. If the file is corrupted for some reason, skip this one
  file <- readLines(full_path, warn = FALSE, skipNul = TRUE)
  file <- paste0(file, collapse = "")

  # If it's an empty file, ...
  if (file == "" || (length(file) == 1 && nchar(trimws(file)) == 0)) {
    return(NA)
  }

  # Try to read in the JSON file
  # If it's not valid, just return an empty result
  # We don't want to make a record of having tried to process this file (but we do give a warning),
  # as we want to make sure users fix and retry the file.
  if (!jsonlite::validate(file)) {
    warn(paste0("Invalid JSON format in file ", filename[1]))
    return(NULL)
  }

  # Note: Previously, jsonlite::validate was called before parsing the JSON file. The reason was
  # that a rare errors could cause rjson::fromJSON (the previously used JSON parser) to terminate
  # the R session, thereby losing all progress and all data in the workspace. By validating the JSON
  # format first, this issue could not arise. As jsonlite::fromJSON also validates the format while
  # parsing, and now readLines is used in conjunction with skipNUL, the problem no longer seems to
  # occur.
  possible_error <- try(
    {
      data <- jsonlite::fromJSON(file, simplifyVector = FALSE)
    },
    silent = TRUE
  )

  # If reading in the file failed, provide a warning to the user and return an empty result.
  # Similar reasoning as above.
  if (inherits(possible_error, "try-error")) {
    warn(c(
      paste0("Invalid JSON format in file ", filename, "."),
      i = "Try running `fix_jsons()` to resolve issues with this file."
    ))
    return(NULL)
  }

  # Check if it is not an empty file Skip this file if empty, but add it to the list of
  # processed file to register this incident and to avoid having to do it again
  if (length(data) == 0 ||
    identical(data, list()) ||
    identical(data, list(list())) ||
    identical(data, list(structure(list(), names = character(0)))) ||
    is.null(unlist(data, use.names = FALSE))) {
    return(NA)
  }

  data
}

# Define a safe_extract function that leaves no room for NULLs,
safe_extract <- function(vec, var) {
  out <- lapply(vec, function(x) {
    tmp <- x[[1]][[var]]
    if (is.null(tmp)) {
      NA
    } else {
      tmp
    }
  })
  out <- unlist(out, use.names = FALSE)
  out
}

# Function for cleaning the raw data
# The goal is to have a list of all the sensors
.import_clean <- function(data) {
  # Sanity check if it is an mpathsenser file
  if (!any(grepl("header", names(data[[1]])))) {
    return(NULL)
  }

  # Clean-up and extract the header and body
  data <- tibble::tibble(
    header = lapply(data, function(x) x[1]),
    body = lapply(data, function(x) x[2])
  )

  # Extract columns
  # device_role_name and trigger_id are simply ignored
  data$study_id <- safe_extract(data$header, "study_id")
  data$device_role_name <- NULL
  data$trigger_id <- NULL
  data$participant_id <- safe_extract(data$header, "user_id")
  data$start_time <- safe_extract(data$header, "start_time")
  data$timezone <- safe_extract(data$header, "time_zone_name")
  data$data_format <- lapply(data$header, function(x) x[[1]]["data_format"])
  data$sensor <- safe_extract(data$data_format, "name")
  data$data_format <- safe_extract(data$data_format, "namespace")
  data$header <- NULL

  # Due to the hacky solution above, filter out rows where the participant_id is missing,
  # usually in the last entry of a file
  data <- data[!is.na(data$participant_id), ]
  data
}

# Safe duplicate check before insertion
# Check if file is already registered as processed
# Based on the ProcessedFiles in the database.
.import_is_duplicate <- function(db_name, meta_data) {
  if (!is.data.frame(meta_data) || nrow(meta_data) == 0) {
    return(NA)
  }

  # Open a database connection
  tmp_db <- open_db(NULL, db_name)

  # Find a matching query
  matches <- DBI::dbGetQuery(
    conn = tmp_db,
    statement = paste0(
      "SELECT COUNT(*) AS `n` FROM `ProcessedFiles` ",
      "WHERE (`file_name` = :file_name ",
      "AND `participant_id` = :participant_id ",
      "AND `study_id` = :study_id)"
    ),
    params = list(
      file_name = meta_data$file_name,
      participant_id = meta_data$participant_id,
      study_id = meta_data$study_id
    )
  )

  # Close db connection of worker
  dbDisconnect(tmp_db)

  # Return whether occurrence is more than 0, i.e. whether files have already been processed
  return(matches[, 1] > 0)
}

.import_extract_sensor_data <- function(data, sensors = NULL) {
  # Make sure top-level of data$body is called body and not carp_body as in the new version
  data$body <- lapply(data$body, function(x) rlang::set_names(x, "body"))

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

  # Check if all sensors exist and are supported
  if (any(!(names %in% mpathsenser::sensors))) {
    not_exist <- names[!(names %in% mpathsenser::sensors)]
    warn(c(
      paste0("Sensor '", not_exist, "' is not supported by this package."),
      i = "Data from this sensor is removed from the output."
    ))
    data <- data[names %in% mpathsenser::sensors]
    names <- names[names %in% mpathsenser::sensors]
  }

  # Call function for each sensor
  # Return result or NA
  tryCatch(
    {
      out <- purrr::imap(data, which_sensor)
      names(out) <- names
      return(out)
    },
    error = function(e) {
      return(NA)
    }
  )
}

# Function for writing all data in a batch to the database
.import_write_to_db <- function(db, meta_data, sensor_data) {
  DBI::dbWithTransaction(db, {
    add_study(
      db = db,
      study_id = meta_data$study_id,
      data_format = meta_data$data_format
    )

    add_participant(
      db = db,
      participant_id = meta_data$participant_id,
      study_id = meta_data$study_id
    )

    for (i in seq_along(sensor_data)) {
      save2db(
        db = db,
        name = names(sensor_data)[[i]],
        data = sensor_data[[i]]
      )
    }

    # Add files to list of processed files
    add_processed_files(
      db = db,
      file_name = meta_data$file_name,
      study_id = meta_data$study_id,
      participant_id = meta_data$participant_id
    )
  })
}
