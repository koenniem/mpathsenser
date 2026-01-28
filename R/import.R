#' Import m-Path Sense files into a database
#'
#' @description `r lifecycle::badge("stable")`
#'
#'   Import JSON files from m-Path Sense into a structured database. This function is the bread and
#'   butter of this package, as it populates the database with data that most of the other functions
#'   in this package use. It is recommend to first run [test_jsons()] and, if necessary,
#'   [fix_jsons()] to repair JSON files with problematic syntax.
#'
#' @details `import` allows you to specify which sensors to import (even though there may be more in
#'   the files) and it also allows batching for a speedier writing process. If processing in
#'   parallel is active, it is recommended that `batch_size` be a scalar multiple of the number of
#'   CPU cores the parallel cluster can use. If a single JSON file in the batch causes and error,
#'   the batch is terminated (but not the function) and it is up to the user to fix the file. This
#'   means that if `batch_size` is large, many files will not be processed. Set `batch_size` to 1
#'   for sequential (one-by-one) file processing.
#'
#'   Currently, only SQLite is supported as a backend. Due to its concurrency restriction, parallel
#'   processing works for cleaning the raw data, but not for importing it into the database. This is
#'   because SQLite does not allow multiple processes to write to the same database at the same
#'   time. This is a limitation of SQLite and not of this package. However, while files are
#'   processing individually (and in parallel if specified), writing to the database happens for the
#'   entire batch specified by `batch_size` at once. This means that if a single file in the batch
#'   causes an error, the entire batch is skipped. This is to ensure that the database is not left
#'   in an inconsistent state.
#'
#' @section Parallel: This function supports parallel processing in the sense that it is able to
#'   distribute it's computation load among multiple workers. To make use of this functionality, run
#'   \href{https://rdrr.io/cran/future/man/plan.html}{\code{future::plan("multisession")}} before
#'   calling this function.
#'
#'
#' @param path The path to the file directory
#' @param db Valid database connection, typically created by [create_db()].
#' @param sensors Select one or multiple sensors as in \code{\link[mpathsenser]{sensors}}. Leave
#'   NULL to extract all sensor data.
#' @param batch_size The number of files that are to be processed in a single batch.
#' @param backend Name of the database backend that is used. Currently, only RSQLite is supported.
#' @param recursive Should the listing recurse into directories?
#' @param .progress Whether to display a progress bar.
#' @param debug Whether suppressed warnings and errors should be shown for debugging purposes.
#'
#' @seealso [create_db()] for creating a database for `import()` to use, [close_db()] for closing
#'   this database; [index_db()] to create indices on the database for faster future processing, and
#'   [vacuum_db()] to shrink the database to its minimal size.
#'
#' @returns A message indicating how many files were imported. If all files were imported
#'   successfully, this functions returns an empty string invisibly. Otherwise the file names of the
#'   files that were not imported are returned visibly.
#'
#' @examples
#' \dontrun{
#' path <- "some/path"
#' # Create a database
#' db <- create_db(path = path, db_name = "my_db")
#'
#' # Import all JSON files in the current directory
#' import(path = path, db = db)
#'
#' # Import all JSON files in the current directory, but do so sequentially
#' import(path = path, db = db, batch_size = 1)
#'
#' # Import all JSON files in the current directory, but only the accelerometer data
#' import(path = path, db = db, sensors = "accelerometer")
#'
#' # Import all JSON files in the current directory, but only the accelerometer and gyroscope data
#' import(path = path, db = db, sensors = c("accelerometer", "gyroscope"))
#'
#' # Remember to close the database
#' close_db(db)
#' }
#'
#' @export
import <- function(
  path = getwd(),
  db,
  sensors = NULL,
  batch_size = 24,
  backend = "RSQLite",
  recursive = TRUE,
  .progress = TRUE,
  debug = FALSE
) {
  # Check arguments
  check_arg(path, type = "character", n = 1)
  check_db(db)
  check_sensors(sensors, allow_null = TRUE)
  check_arg(batch_size, "integerish", n = 1)
  check_arg(backend, "character", n = 1)
  check_arg(recursive, "logical", n = 1)
  check_arg(.progress, "logical", n = 1)
  check_arg(debug, "logical", n = 1)

  # Normalise path and check if directory exists
  path <- normalizePath(file.path(path), mustWork = FALSE)
  if (!dir.exists(path)) {
    cli::cli_abort(c(
      "Directory {.path {path}} does not exist.",
      i = "Did you make a typo in the path name?"
    ))
  }

  # Retrieve all JSON files
  files <- list.files(path = path, pattern = "*.json$", recursive = recursive)

  if (debug) {
    len_files <- length(files)
    cli::cli_progress_step("Found {len_files} file{?s} to process.")
  }

  if (length(files) == 0) {
    cli::cli_abort(c(
      "Can't find any JSON files in {.path {path}}.",
      i = "Did you put the JSON files in the correct directory?"
    ))
  }

  # If there are no duplicate files Proceed with the unsafe (but fast) check to prevent duplicate
  # insertion into db
  if (anyDuplicated(files) == 0) {
    processed_files <- get_processed_files(db)
    # Keep files _not_ already registered in db
    files <- files[!(files %in% processed_files$file_name)]

    if (debug) {
      len_duplicates <- len_files - length(files)
      len_files <- length(files)
      cli::cli_progress_step(
        "Found {len_duplicates} duplicate file{?s}. Continuing with {len_files} file{?s}."
      )
    }

    if (length(files) == 0) {
      cli::cli_inform("No new files to process.")
      return(invisible(""))
    }
  }

  # Split the files into batches
  batches <- split(files, ceiling(seq_along(files) / batch_size))

  # Set up a progress bar
  if (.progress) {
    pb <- cli::cli_progress_bar(
      total = length(batches),
      format = "Importing data... {cli::pb_bar} {cli::pb_current}/{cli::pb_total} batch{?es} \\
      [{cli::pb_percent}] | {cli::pb_eta_str}"
    )
    cli::cli_progress_update(inc = 0, force = TRUE)
  }

  for (i in seq_along(batches)) {
    # All the files in the batch
    batch_files <- batches[[i]]

    if (debug) {
      len_batches <- length(batches)
      cli::cli_progress_output(cli::cli_rule(
        left = "Starting work on {.field batch {i}} out of {len_batches}.",
        id = ""
      ))
    }

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

    if (debug) {
      len_batch_data <- length(batch_data)
      cli::cli_progress_output("Read {len_batch_data} JSON file{?s}.")
    }

    # Save the empty files in data frame to add to the meta data later
    # Meta data is what is being registered
    if (length(batch_na) > 0) {
      batch_na <- .import_meta_data_from_file_name(batch_na)

      batch_na <- tibble(
        batch_na,
        data_format = NA,
      )
    }

    # Clean the lists to be in a dataframe format
    batch_data <- furrr::future_map2(
      .x = batch_data,
      .y = names(batch_data),
      .f = .import_clean,
      .options = furrr::furrr_options(seed = TRUE)
    )

    # Remove NULLs, as we want to keep these files unmarked
    # (something went wrong when reading in the data)
    batch_data <- purrr::compact(batch_data)

    if (debug) {
      len_batch_data <- length(batch_data)
      cli::cli_progress_output("Cleaned {len_batch_data} file{?s}.")
    }

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
      .f = ~ distinct(
        ..1,
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
    duplicates <- .import_is_duplicate(db = db, meta_data)
    meta_data <- meta_data[!duplicates, ]
    batch_data <- batch_data[names(batch_data) %in% meta_data[["id"]]]

    # Extract the sensor data
    batch_data <- furrr::future_map(
      .x = batch_data,
      .f = ~ .import_extract_sensor_data(.x, sensors, debug = debug),
      .options = furrr::furrr_options(seed = TRUE)
    )

    # To do: Use mpathinfo to generate new metadata
    # For now, mpathinfo is already removed in .import_extract_sensor_data()

    # If a file failed to process, NA is returned
    batch_data <- batch_data[!is.na(batch_data)]
    meta_data <- meta_data[meta_data[["id"]] %in% names(batch_data), ]

    # If there were some empty files, add them to the meta data now
    if (is.data.frame(batch_na)) {
      meta_data <- bind_rows(meta_data, batch_na)
    }

    if (debug) {
      len_batch_data <- length(batch_data)
      cli::cli_progress_output("Extracted sensor data from {len_batch_data} file{?s}.")
    }

    # Interesting feature in purrr::transpose. If the names would not be explicitly set, it would
    # only take the names of the first entry of the list. So, if some sensors would be present in
    # the first entry (e.g. low sampling sensors like Device), it would disappear from the data
    # altogether.
    # Turn data list inside out, drop NULLs and bind sensors from different files together
    batch_data <- purrr::transpose(
      batch_data,
      .names = sort(mpathsenser::sensors)
    )
    batch_data <- lapply(batch_data, bind_rows)
    batch_data <- purrr::compact(batch_data)
    batch_data <- lapply(batch_data, distinct) # Filter out duplicate rows (for some reason)

    if (debug) {
      n_tables <- length(batch_data)
      write_pb <- cli::cli_progress_output("Writing {n_tables} table{?s}.")
    }
    # Write all data as a single transaction, safely.
    # Note that interrupting halfway does not rollback the transaction, in which case running
    # `import()` again will result failed writing, as another transaction is still active while
    # the previous one never completed and is thus still active.
    try(
      .import_write_to_db(db, meta_data, batch_data),
      silent = !debug
    )

    # Update progress bar
    if (.progress) {
      cli::cli_progress_update(id = pb)
    }
  }

  processed_files <- get_processed_files(db)
  complete <- unlist(files, use.names = FALSE) %in% processed_files$file_name

  # Close the progress bar
  cli::cli_progress_done()

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
    cli::cli_warn("Invalid JSON format in file {.file {full_path}}.")
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
    cli::cli_warn(c(
      paste0("Invalid JSON format in file {.file {filename}}."),
      i = "Try running {.help [fix_jsons()](mpathsenser::fix_jsons)} to \\
      resolve issues with this file."
    ))
    return(NULL)
  }

  # Check if it is not an empty file Skip this file if empty, but add it to the list of
  # processed file to register this incident and to avoid having to do it again
  # Protect against NULL, not lists, empty lists, and lists of empty lists
  # Do not use unlist() to recursively check empty lists, because this is very costly for large
  # data files.
  if (is.null(data) || !is.list(data) || length(data) == 0 || lengths(data[1]) == 0) {
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
.import_clean <- function(data, file_name) {
  # Detect new file format
  header_names <- data |>
    lapply(names) |>
    unlist(use.names = FALSE) |>
    unique()
  if (all(c("data", "sensorStartTime") %in% header_names)) {
    return(.import_clean_new(data, file_name))
  }

  # Sanity check if it is an mpathsenser file
  if (!any(grepl("header", names(data[[1]])))) {
    return(NULL)
  }

  # Clean-up and extract the header and body
  data <- tibble(
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
  data$data_format <- lapply(data$header, function(x) x[[1]]["data_format"])
  data$sensor <- safe_extract(data$data_format, "name")
  data$data_format <- safe_extract(data$data_format, "namespace")
  data$header <- NULL

  # Due to the hacky solution above, filter out rows where the participant_id is missing,
  # usually in the last entry of a file
  data <- data[!is.na(data$participant_id), ]
  data
}

# New clean function for the new file format as of CARP 1.0.0
.import_clean_new <- function(data, file_name) {
  meta <- .import_meta_data_from_mpathinfo(data, file_name)
  meta$file_name <- NULL

  data <- tibble(
    meta,
    data_format = "cams 1.0.0",
    start_time = purrr::map_dbl(data, \(x) {
      purrr::pluck(x, "sensorStartTime", .default = NA)
    }),
    end_time = purrr::map_dbl(data, \(x) {
      purrr::pluck(x, "sensorEndTime", .default = NA)
    }),
    data = purrr::map(data, \(x) purrr::pluck(x, "data", .default = NA)),
  )

  # Change the timestamps from UNIX timestamp to ISO 8601, as this is how it will be saved later
  data$start_time <- as.character(
    as.POSIXct(data$start_time / 1e6, tz = "UTC", origin = "1970-01-01")
  )
  data$end_time <- as.character(
    as.POSIXct(data$end_time / 1e6, tz = "UTC", origin = "1970-01-01")
  )

  # Extract the sensor
  data <- data |>
    tidyr::hoist(data, sensor = "__type") |>
    mutate(sensor = gsub("dk\\.cachet\\.carp\\.", "", .data$sensor))

  data
}

# Safe duplicate check before insertion
# Check if file is already registered as processed
# Based on the ProcessedFiles in the database.
.import_is_duplicate <- function(db, meta_data) {
  if (!is.data.frame(meta_data) || nrow(meta_data) == 0) {
    return(NA)
  }

  # Find a matching query
  matches <- DBI::dbGetQuery(
    conn = db,
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

  # Return whether occurrence is more than 0, i.e. whether files have already been processed
  return(matches[, 1] > 0)
}

# Function to map the sensor names to the ones used in the database
.import_map_sensor_names <- function(names) {
  names <- trimws(names)
  lower_names <- tolower(names)
  dplyr::case_match(
    lower_names,
    c("accelerometer", "accelerationfeatures", "averageaccelerometer") ~
      "Accelerometer",
    "activity" ~ "Activity",
    c("airquality", "air_quality") ~ "AirQuality",
    c("app_usage", "appusage") ~ "AppUsage",
    c("battery", "batterystate") ~ "Battery",
    "bluetooth" ~ "Bluetooth",
    "calendar" ~ "Calendar",
    "connectivity" ~ "Connectivity",
    c("device", "deviceinformation") ~ "Device",
    "error" ~ "Error",
    "geofence" ~ "Geofence",
    "gyroscope" ~ "Gyroscope",
    "heartbeat" ~ "Heartbeat",
    c("apps", "installed_apps") ~ "InstalledApps",
    "keyboard" ~ "Keyboard",
    c("light", "ambientlight") ~ "Light",
    "location" ~ "Location",
    c("memory", "freememory") ~ "Memory",
    "mobility" ~ "Mobility",
    "noise" ~ "Noise",
    c("pedometer", "stepcount") ~ "Pedometer",
    c("phone_log", "phonelog") ~ "PhoneLog",
    c("screen", "screenevent") ~ "Screen",
    c("text_message", "textmessage") ~ "TextMessage",
    "timezone" ~ "Timezone",
    "weather" ~ "Weather",
    "wifi" ~ "Wifi",
    .default = names
  )
}

.import_extract_sensor_data <- function(data, sensors = NULL, debug = FALSE) {
  # Detect if this is a file in a legacy format
  is_legacy <- "body" %in% colnames(data)

  # Make sure top-level of data$body is called body and not carp_body as in the new version (pre
  # 1.0.0)
  if (is_legacy) {
    data$body <- lapply(data$body, function(x) rlang::set_names(x, "body"))
  }

  # Special unpack process for Garmin sensing data
  has_selected_garmin_sensor <- is.null(sensors) || any(grepl("garmin", tolower(sensors)))
  if ("garminalllogsdata" %in% data$sensor && has_selected_garmin_sensor) {
    data <- .import_extract_garmin_logs(data)
  }

  # Set names in accordance with the table names
  data$sensor <- .import_map_sensor_names(data$sensor)

  # Divide et impera
  data <- split(data, as.factor(data$sensor), drop = TRUE)

  # Drop useless data
  data[["unknown"]] <- NULL
  data[["triggeredtask"]] <- NULL
  data[["mpathinfo"]] <- NULL # temporary

  # the unique sensors in this data file
  names <- names(data)

  # Select sensors, if not NULL
  if (!is.null(sensors)) {
    data <- data[tolower(names) %in% tolower(sensors)]
    names <- names[tolower(names) %in% tolower(sensors)]
  }

  # Check if all sensors exist and are supported, except 'mpathinfo', which contains metadata
  if (any(!(setdiff(names, "mpathinfo") %in% mpathsenser::sensors))) {
    not_exist <- names[!(names %in% mpathsenser::sensors)]
    not_exist <- setdiff(not_exist, "mpathinfo")
    warn(c(
      paste0("Sensor '", not_exist, "' is not supported by this package."),
      i = "Data from this sensor is removed from the output."
    ))
    # Note: the following 2 lines remove mpathinfo from the data and names
    data <- data[names %in% mpathsenser::sensors]
    names <- names[names %in% mpathsenser::sensors]
  }

  # Call function for each sensor
  # Return result or NA
  tryCatch(
    {
      if (is_legacy) {
        out <- purrr::imap(data, which_sensor)
      } else {
        data <- mapply(
          \(x, name) `class<-`(x, c(name, class(x))),
          data,
          tolower(names(data)),
          SIMPLIFY = FALSE
        )
        out <- lapply(data, unpack_sensor_data)
      }

      names(out) <- names
      return(out)
    },
    error = function(e) {
      if (debug) {
        print(e)
      }
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
      tmp_tbl <- dplyr::copy_to(
        dest = db,
        df = sensor_data[[i]],
        name = "tmp_import",
        overwrite = TRUE,
        temporary = TRUE,
        in_transaction = FALSE
      )

      dplyr::rows_upsert(
        dplyr::tbl(db, names(sensor_data)[[i]]),
        tmp_tbl,
        by = .import_get_pk(names(sensor_data)[[i]]),
        copy = FALSE,
        in_place = TRUE
      )
    }
    DBI::dbRemoveTable(db, "tmp_import", fail_if_missing = FALSE)

    # Add files to list of processed files
    add_processed_files(
      db = db,
      file_name = meta_data$file_name,
      study_id = meta_data$study_id,
      participant_id = meta_data$participant_id
    )
  })
}

.import_meta_data_from_mpathinfo <- function(data, file_name) {
  # Only keep mpathinfo entry
  has_info <- purrr::map_lgl(data, \(x) x[["data"]][["__type"]] == "dk.cachet.carp.mpathinfo")

  # No mpathinfo found
  if (!any(has_info)) {
    # Try fallback to file name
    return(.import_meta_data_from_file_name(file_name))
  }

  if (sum(has_info) > 1) {
    cli::cli_warn(c(
      "Multiple `mpathinfo` entries found in file: {.file {file_name}}.",
      i = "Using the first occurrence."
    ))
  }

  data <- data[has_info][[1]]

  # Return the same structure as .import_meta_data_from_file_name()
  # Future versions could include senseVersion and accountCode
  tibble(
    study_id = data$data[["studyName"]] %||% "-1",
    participant_id = as.character(data$data[["connectionId"]]) %||% "N/A",
    file_name = file_name
  )
}

.import_meta_data_from_file_name <- function(file_name) {
  # The file name is structured as follows:
  # therapistid_study_id_participantid_m_Path_sense_yyyy-mm-dd_HH-MM-SS%OS6.json
  # Note that the number of seconds may contain milliseconds (or smaller) as well, but that there
  # is no decimal point with the comma.
  # First, check that these are m-Path Sense files
  valid_names <- grepl("m_Path_sense", file_name)
  if (any(!valid_names)) {
    # Create a tibble with missing meta data for files with invalid names
    invalid_names <- tibble(
      study_id = "-1",
      participant_id = "N/A",
      file_name = file_name[!valid_names]
    )

    file_name <- file_name[valid_names]

    # If there are no more valid names, return the invalid names only
    if (length(file_name) == 0) {
      return(invalid_names)
    }
  }

  split_file_name <- strsplit(file_name, "_")

  # The account ID is simply the first part of the file name, and is always a number
  account_id <- purrr::map_chr(split_file_name, \(x) x[1])

  study_id <- purrr::map(split_file_name, \(x) {
    x[-c(1, seq.int(length(x) - 5, length(x)))]
  })
  study_id <- purrr::map_chr(study_id, \(x) paste0(x, collapse = "_"))

  p_id <- purrr::map_chr(split_file_name, \(x) x[length(x) - 5])

  # If the study_id is missing for some reason, indicate this with -1
  # study_id[study_id == ""] <- "-1"

  out <- tibble(
    study_id = study_id,
    participant_id = p_id,
    file_name = file_name
  )

  # Add the missing data from file name that were not valid, if any
  if (any(!valid_names)) {
    out <- bind_rows(out, invalid_names)
    out <- out[match(file_name, out$file_name), ]
  }

  out
}

.import_extract_garmin_logs <- function(.data) {
  garmin_data <- filter(.data, .data$sensor == "garminalllogsdata")

  # Extract Garmin sensor data into new columns
  garmin_data$GarminAccelerometer <- lapply(garmin_data$data, \(x) x[["accelerometer"]])
  garmin_data$GarminActigraphy <- lapply(garmin_data$data, \(x) {
    x[c("actigraphy1", "actigraphy2", "actigraphy3")]
  })
  garmin_data$GarminActigraphy <- lapply(
    garmin_data$GarminActigraphy,
    \(x) purrr::list_flatten(x, name_spec = "{inner}")
  )

  garmin_data$GarminBBI <- lapply(garmin_data$data, \(x) x[["bbi"]])
  garmin_data$GarminEnhancedBBI <- lapply(garmin_data$data, \(x) x[["enhancedBbi"]])
  garmin_data$GarminGyroscope <- lapply(garmin_data$data, \(x) x[["gyroscope"]])
  garmin_data$GarminHeartRate <- lapply(garmin_data$data, \(x) x[["heartRate"]])
  garmin_data$GarminMeta <- lapply(garmin_data$data, \(x) {
    x[c("fromTime", "toTime", "entryCounts")]
  })
  # For the meta data, flatten the entrycounts
  garmin_data$GarminMeta <- lapply(
    garmin_data$GarminMeta,
    \(x) purrr::list_flatten(x, name_spec = "{inner}")
  )
  garmin_data$GarminRespiration <- lapply(garmin_data$data, \(x) x[["respiration"]])
  garmin_data$GarminSkinTemperature <- lapply(garmin_data$data, \(x) x[["skinTemperature"]])
  garmin_data$GarminSPO2 <- lapply(garmin_data$data, \(x) x[["spo2"]])
  garmin_data$GarminSteps <- lapply(garmin_data$data, \(x) x[["steps"]])
  garmin_data$GarminStress <- lapply(garmin_data$data, \(x) x[["stress"]])
  garmin_data$GarminWristStatus <- lapply(garmin_data$data, \(x) x[["wristStatus"]])
  garmin_data$GarminZeroCrossing <- lapply(garmin_data$data, \(x) x[["zeroCrossing"]])

  # Remove the extract columns from the data column
  garmin_data$data <- lapply(garmin_data$data, \(x) {
    purrr::discard_at(
      x = x,
      at = c(
        "accelerometer",
        "actigraphy1",
        "actigraphy2",
        "actigraphy3",
        "bbi",
        "enhancedBbi",
        "gyroscope",
        "heartRate",
        "fromTime",
        "toTime",
        "entryCounts",
        "respiration",
        "skinTemperature",
        "spo2",
        "steps",
        "stress",
        "wristStatus",
        "zeroCrossing"
      )
    )
  })

  # Check if the remainder is empty
  if (any(lengths(garmin_data$data) > 0)) {
    not_exist <- lapply(garmin_data$data, names)
    not_exist <- unlist(not_exist, use.names = FALSE)
    not_exist <- unique(not_exist)
    cli::cli_warn(c(
      "Garmin data type{?s} {.var {not_exist}} is not supported by this package.",
      i = "Data is removed from the output."
    ))
  }

  # Remove the remainder
  garmin_data$data <- NULL

  # Pivot the data to get a single sensor column
  garmin_data <- garmin_data |>
    select(-"sensor") |>
    tidyr::pivot_longer(dplyr::starts_with("Garmin"), names_to = "sensor", values_to = "data")

  # Unnest the actigraphy data as there are 3 measurement at once
  actigraphy <- garmin_data |>
    filter(.data$sensor == "GarminActigraphy") |>
    unnest("data")

  garmin_data <- garmin_data |>
    filter(.data$sensor != "GarminActigraphy") |>
    bind_rows(actigraphy)

  # Remove measurements that were missing
  garmin_data <- garmin_data |>
    filter(!purrr::map_lgl(.data$data, is.null)) |>
    filter(!purrr::map_lgl(.data$data, \(x) length(x) == 0))

  # add back to the main data
  # the order of the rows doesn't matter in this case
  .data |>
    filter(.data$sensor != "garminalllogsdata") |>
    bind_rows(garmin_data)
}

# Mapping to get the primary keys for a sensor table
.import_get_pk <- function(sensor) {
  pks <- switch(
    sensor,
    AppUsage = "app",
    Bluetooth = "bluetooth_device_id",
    Calendar = c("event_id", "calendar_id", "start", "end"),
    GarminActigraphy = c("instance"),
    InstalledApps = "app",
    NA
  )

  if (!all(is.na(pks))) {
    pks <- c("participant_id", "date", "time", pks)
  } else {
    pks <- c("participant_id", "date", "time")
  }

  pks
}
