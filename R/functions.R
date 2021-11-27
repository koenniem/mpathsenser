#' Import CARP files into a database (CARP data scheme)
#'
#' Currently, only SQLite is supported as a backend. Due to its concurrency restriction, the
#' `parallel` option is disabled.
#'
#' @param path The path to the file directory
#' @param db Valid database connection.
#' @param dbname If no database is provided, a new database dbname is created.
#' @param overwrite_db If a database with the same \code{dbname}  already exists, should it be
#' overwritten?
#' @param backend Name of the database backend that is used. Currently, only RSQLite is supported.
#' @param progress Logical value to show a progress bar or not.
#' @param recursive Should the listing recurse into directories?
#' @param parallel A value that indicates whether to do reading in and processing
#' in parallel. If this argument is a number, this indicates the number of workers that will be
#' used.
#'
#' @return Invisible. Imported database can be reopened using \link[CARP]{open_db}.
#' @export
import <- function(path = getwd(),
                   db = NULL,
                   dbname = "carp.db",
                   overwrite_db = TRUE,
                   backend = "RSQLite",
                   progress = TRUE,
                   recursive = TRUE,
                   parallel = FALSE
) {

  # Retrieve all JSON files
  files <- dir(path = path, pattern = "*.json$", recursive = recursive)

  if (length(files) == 0) {
    stop("No JSON files found")
  }

  # Check back-end and parallel constraint
  # if (backend == "RSQLite" & parallel) {
  #   warning("Parallel cannot be used when RSQLite is provided as a backend due to concurrency
  #           constraint. Setting parallel to false.")
  #   parallel <- FALSE
  # }

  # Check if database is valid
  if (!is.null(db)) {
    if (!DBI::dbIsValid(db)) {
      stop("Database is not valid.")
    }
  } else {
    # Work with dbname
    if (is.null(dbname)) {
      stop("A valid database connection or path must be provided")
    }

    # Try to open the database
    tryCatch({
      db <- open_db(path, dbname)
    }, error = function(e) {
      assign("db",
             create_db(path, db_name = dbname, overwrite = overwrite_db),
             envir = parent.env(environment())
      )
    })
  }

  # If there are no duplicate files
  # Proceed with the unsafe (but fast) check
  # to prevent duplicate insertion into db
  if (anyDuplicated(files) == 0) {
    processed_files <- get_processed_files(db)
    # Keep files _not_ already registered in db
    files <- files[!(files %in% processed_files$file_name)]

    if (length(files) == 0) {
      DBI::dbDisconnect(db)
      return(message("No new files to process."))
    }
  }

  # Set up parallel back-end
  if (!is.null(parallel)) {
    if (is.numeric(parallel)) {
      future::plan(future::multisession, workers = parallel)
    } else if (parallel) {
      future::plan(future::multisession)
    }
  }

  # Call on implementation with or without progress bar
  if (progress) {
    progressr::with_progress({

      files <- split(files, ceiling(seq_along(files) / 12))
      p <- progressr::progressor(steps = length(files))

      for(i in seq_along(files)) {

        # Update progress bar
        p(sprintf("x=%g", i))

        # Get data from the files, in parallel if needed
        res <- furrr::future_map_dfr(files[[i]], ~import_impl(path, .x, db@dbname),
                                     .options = furrr::furrr_options(seed = TRUE))

        # Interesting feature in purrr::transpose. If the names would not be explicitly set, it
        # would only take the names of the first entry of the list. So, if some sensors would be
        # present in the first entry (e.g. low sampling sensors like Device), it would disappear
        # from the data altogether.

        data <- purrr::compact(res$data)
        res$data <- NULL # memory efficiency
        data <- purrr::transpose(data, .names = sort(c("Error", sensors)))
        data <- lapply(data, dplyr::bind_rows)
        data <- purrr::compact(data)

        DBI::dbWithTransaction(db, {
          add_study(db, unique(res$studies))
          add_participant(db, unique(res$participants))

          for (j in seq_along(data)) {
            save2db(db, names(data)[[j]], data[[j]])
          }
          # Add file to list of processed files
          add_processed_files(db, res$file)
        })
      }
    })
  } else {
    import_impl(path, files, db@dbname)
  }

  # Return to sequential processing
  if (!is.null(parallel)) {
    if (is.numeric(parallel)) {
      future::plan(future::sequential)
    } else if (parallel) {
      future::plan(future::sequential)
    }
  }

  processed_files <- get_processed_files(db)
  complete <- all(unlist(files, use.names = FALSE) %in% processed_files$file_name)
  if (complete) {
    message("All files were successfully written to the database.")
  } else {
    warning("Some files could not be written to the database.")
  }

  DBI::dbDisconnect(db)
}

import_impl <- function(path, files, db_name) {
  out <- list(studies = data.frame(study_id = character(),
                                 data_format = character()),
              participants = data.frame(study_id = character(length(files)),
                                        participant_id = character(length(files))),
              file = data.frame(file_name = character(length(files)),
                                study_id = character(length(files)),
                                participant_id = character(length(files))),
              data = vector("list", length(files)))

  for (i in seq_along(files)) {

    # Try to read in the file. If the file is corrupted for some reason, skip this one
    file <- normalizePath(paste0(path, "/", files[i]))
    file <- readLines(file, warn = FALSE)
    file <- paste0(file, collapse = "")
    if (!jsonlite::validate(file)) {
      print(paste0("Invalid JSON in file ", files[i]))
      next
    }

    possible_error <- tryCatch({
      data <- rjson::fromJSON(file, simplify = FALSE)
    }, error = function(e) e)

    if (inherits(possible_error, "error")) {
      print(paste("Could not read", files[i]))
      next
    }

    # Check if it is not an empty file
    # Skip this file if empty, but add it to the list of processed file to register this incident
    # and to avoid having to do it again
    if (length(data) == 0 | identical(data, list()) | identical(data, list(list()))) {
      p_id <-  sub(".*?([0-9]{5}).*", "\\1", files[i])
      out$studies[i, ] <- c(study_id = "-1", data_format = NA)
      out$participants[i, ] <- c(study_id = "-1", participant_id = p_id)
      out$file[i, ] <- c(file_name = files[i],
                         study_id = "-1",
                         participant_id = p_id)
      next
    }

    # Clean-up and extract the header and body
    data <- tibble::tibble(
      header = lapply(data, function(x) x[1]),
      body = lapply(data, function(x) x[2])
    )

    # Extract columns
    # Define a safe_extract function that leaves no room for NULLs, since unnest cannot handle
    # a column full of NULLs
    safe_extract <- function(vec, var) {
      out <- lapply(vec, function(obs) {
        tmp <- obs[[1]][[var]]
        if (is.null(tmp)) return(NULL) else return(tmp)
      })
      if (all(vapply(out, is.null, logical(1), USE.NAMES = FALSE)))
        out <- rep("N/A", length(out))
      return(out)
    }
    data$study_id <- safe_extract(data$header, "study_id")
    # data$device_role_name <- safe_extract(data$header, "device_role_name")
    # data$trigger_id <- safe_extract(data$header, "trigger_id")
    data$device_role_name <- NULL
    data$trigger_id <- NULL
    data$participant_id <- safe_extract(data$header, "user_id")
    data$start_time <- safe_extract(data$header, "start_time")
    data$data_format <- lapply(data$header, function(x) x[[1]]["data_format"])
    data$sensor <- safe_extract(data$data_format, "name")
    data$data_format <- safe_extract(data$data_format, "namespace")
    data$header <- NULL
    data <- tidyr::unnest(data, c(study_id:sensor))

    # Due to the hacky solution above, filter out rows where the participant_id is missing, usually
    # in the last entry of a file
    data <- data[!is.na(data$participant_id) & data$participant_id != "N/A", ]

    # Open db
    tmp_db <- open_db(NULL, db_name)

    # Safe duplicate check before insertion
    # Check if file is already registered as processed
    # Now using the participant_id and study_id as
    this_file <- data.frame(
      file_name = files[i],
      study_id = unique(data$study_id),
      participant_id = unique(data$participant_id)
    )

    processed_files <- get_processed_files(tmp_db)
    matches <- dplyr::inner_join(this_file,
      processed_files,
      by = c("file_name", "participant_id", "study_id")
    )
    if (nrow(matches) > 0) {
      DBI::dbDisconnect(tmp_db)
      next # File was already processed
    }

    # Populate study specifics to db
    study_id <- dplyr::distinct(data, study_id, data_format)

    # Add participants
    participant_id <- dplyr::distinct(data, participant_id, study_id)

    # Make sure top-level of data$body is called body and not carp_body as in the new version
    data <- dplyr::mutate(data, body = purrr::modify(body, purrr::set_names, nm = "body"))

    # Divide et impera
    data <- split(data, as.factor(data$sensor), drop = TRUE)

    # Drop useless data
    data[["unknown"]] <- NULL

    # Call function for each sensor
    tryCatch({
      data <- purrr::imap(data, ~which_sensor(.x, .y))

      # Set names to capitals in accordance with the table names
      names <- strsplit(names(data), "_")
      names <- lapply(names, function(x)
        paste0(toupper(substring(x, 1, 1)), substring(x, 2), collapse = ""))
      names[names=="Apps"] <- "InstalledApps" # Except InstalledApps...
      names(data) <- names

      out$studies <- rbind(out$studies, study_id)
      out$participants[i, ] <- participant_id
      out$file[i, ] <- this_file # Save to output
      out$data[[i]] <- data

    }, error = function(e) {
      print(paste0("processing failed for file ", files[i]))
    })

    # Close db connection of worker
    DBI::dbDisconnect(tmp_db)
  }

  out
}

#' Measurement frequencies per sensor
#'
#' A numeric vector containing (an example) of the measurement frequencies per sensor.
#' Such input is needed for \link[CARP]{coverage}. This vector contains the following information:
#'
#' Sensor | Frequency (per hour) | Full text
#' -------|-----------|----------
#' Accelerometer | 720 | Once per 5 seconds. Can have multiple instances.
#' AirQuality | 1 | Once per hour.
#' AppUsage | 2 | Once every 30 minutes. Can have multiple instances.
#' Bluetooth | 12 | Once every 5 minutes. Can have multiple instances.
#' Gyroscope | 720 | Once per 5 seconds. Can have multiple instances.
#' Light | 360 | Once per 10 seconds.
#' Location | 60 | Once every 60 seconds.
#' Memory | 60 | Once per minute
#' Noise | 120 | Once every 30 seconds. Microhone cannot be used in the background in Android 11.
#' Weather | 1 | Once per hour.
#' Wifi | 60 |  Once per minute.
#'
#' @export freq
freq <- c(
  Accelerometer = 720, # Once per 5 seconds. Can have multiple measurements.
  AirQuality = 1,
  AppUsage = 2, # Once every 30 minutes
  Bluetooth = 60, # Once per minute. Can have multiple measurements.
  Gyroscope = 720, # Once per 5 seconds. Can have multiple measurements.
  Light = 360, # Once per 10 seconds
  Location = 60, # Once per 60 seconds
  Memory = 60, # Once per minute
  Noise = 120,
  Weather = 1,
  Wifi = 60 # once per minute
)

#' Create a coverage chart of the sampling rate
#'
#' Only applicable to non-reactive sensors with "continuous" sampling
#'
#' @param db A valid database connection. Schema must be that as it is created by
#' \link[CARP]{open_db}.
#' @param participant_id A character string of _one_ participant ID.
#' @param sensor A character vector containing one or multiple sensors. See
#' \code{\link[CARP]{sensors}} for a list of available sensors. Use "All" for all available sensors.
#' @param frequency A named numeric vector with sensors as names and the number of expected samples
#' per hour
#' @param relative Show absolute number of measurements or relative to the expected number?
#' Logical value.
#' @param offset Currently not used.
#' @param start_date A date (or convertible to a date using \code{\link[base]{as.Date}}) indicating
#' the earliest date to show. Leave empty for all data. Must be used with \code{end_date}.
#' @param end_date A date (or convertible to a date using \code{\link[base]{as.Date}}) indicating
#' the latest date to show.Leave empty for all data. Must be used with \code{start_date}.
#' @param plot Whether to return a ggplot or its underlying data.
#'
#' @importFrom magrittr "%>%"
#'
#' @return A ggplot of the coverage results.
#' @export
#'
#'
#' @examples
#' \dontrun{
#' fix_json()
#' unzip()
#' freq <- c(
#'   Accelerometer = 720, # Once per 5 seconds. Can have multiple measurements.
#'   AirQuality = 1,
#'   AppUsage = 2, # Once every 30 minutes
#'   Bluetooth = 60, # Once per minute. Can have multiple measurements.
#'   Gyroscope = 720, # Once per 5 seconds. Can have multiple measurements.
#'   Light = 360, # Once per 10 seconds
#'   Location = 60, # Once per 60 seconds
#'   Memory = 60, # Once per minute
#'   Noise = 120,
#'   Pedometer = 1,
#'   Weather = 1,
#'   Wifi = 60 # once per minute
#' )
#' coverage(
#'   db = db,
#'   participant_id = "12345",
#'   sensor = c("Accelerometer", "Gyroscope"),
#'   frequency = CARP::freq,
#'   start_date = "2021-01-01",
#'   end_date = "2021-05-01"
#' )
#' }
coverage <- function(db,
                     participant_id,
                     sensor = "All",
                     frequency = CARP::freq,
                     relative = TRUE,
                     offset = "None",
                     start_date = NULL,
                     end_date = NULL,
                     plot = TRUE) {
  # Check db
  if (!inherits(db, "DBIConnection")) {
    stop("Argument db is not a database connection.")
  }

  if (!DBI::dbIsValid(db)) {
    stop("Database is invalid.")
  }

  # Check sensors
  if (length(sensor) == 1 && sensor == "All") {
    sensor <- sensors
  } else {
    missing <- sensor[!(sensor %in% sensors)]
    if (length(missing) != 0) {
      stop(paste0("Sensor(s) ", paste0(missing, collapse = ", "), " not found."))
    }
  }

  # Check participants
  if (length(participant_id) > 1) {
    stop("Only 1 participant per coverage chart allowed")
  }

  if (is.character(participant_id)) {
    if (!(participant_id %in% get_participants(db)$participant_id)) {
      stop("Participant_id not known.")
    }
  } else {
    stop("participant_id must be a character string")
  }

  # Check frequency
  if (!relative && !is.numeric(frequency) | is.null(names(frequency))) {
    stop("Frequency must be a named numeric vector")
  }

  # Check time subset
  if (grepl("\\d day", offset)) {
    offset <- paste0("-", offset)
  } else if (is.null(offset) || (tolower(offset) == "none")) {
    offset <- NULL
  } else {
    stop("Argument offset must be either 'None', 1 day, or 2, 3, 4, ... days.")
  }

  # Helper function for checking if a string is convertible to date
  convert2date <- function(s) {
    if (!inherits(s, "Date") & !is.character(s)) return(FALSE)
    s <- try(as.Date(s), silent = TRUE)
    if (inherits(s, "Date")) TRUE else FALSE
  }

  # Check start_date, end_date
  if ((!is.null(start_date) && !is.null(end_date)) && !is.null(offset)) {
    warning("Argument start_date/end_date and offset cannot be present at the same time. ",
						"Ignoring the offset argument.")
    offset <- NULL
  }

  else if (!(is.null(start_date) | convert2date(start_date)) ||
           !(is.null(end_date) | convert2date(end_date))) {
    stop("start_date and end_date must be NULL, a character string, or date.")
  }

  # Retain only frequencies that appear in the sensor list
  frequency <- frequency[names(frequency) %in% sensor]

  # If relative, retain only sensors that have a frequency
  if (relative) {
    sensor <- names(frequency)
  }

  # Calculate coverage from db - internal function
  data <- coverage_impl(db, participant_id, sensor, frequency, relative, start_date, end_date)

  # Bind all together and make factors
  data <- dplyr::bind_rows(data)
  data$measure <- factor(data$measure)
  data$measure <- factor(data$measure, levels = rev(levels(data$measure)))

  # Plot the result if needed
  if (plot) {
    out <- ggplot2::ggplot(data = data,
                           mapping = ggplot2::aes(x = Hour, y = measure, fill = Coverage)) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(mapping = ggplot2::aes(label = Coverage), colour = "white") +
      ggplot2::scale_x_continuous(breaks = 0:23) +
      ggplot2::scale_fill_gradientn(
        colours = c("#d70525", "#645a6c", "#3F7F93"),
        breaks = c(0, 0.5, 1),
        labels = c(0, 0.5, 1),
        limits = c(0, 1)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::ggtitle(paste0("Coverage for participant ", participant_id))
    return(out)
  } else {
    return(data)
  }
}

coverage_impl <- function(db, participant_id, sensor, frequency, relative, start_date, end_date) {
  # Interesting bug/feature in dbplyr: If participant_id is used in the query,
  # the index of the table is not used. Hence, we rename participant_id to p_id
  p_id <- as.character(participant_id)

  # Loop over each sensor and calculate the coverage rate for that sensor
  data <- furrr::future_map(.x = sensor, .f = ~ {
    tmp_db <- open_db(NULL, db@dbname)

    # Extract the data for this participant and sensor
    tmp <- dplyr::tbl(tmp_db, .x) %>%
      dplyr::filter(participant_id == p_id) %>%
      dplyr::select(measurement_id, time, date)

    # Filter by date if needed
    if (!is.null(start_date) && !is.null(end_date)) {
      tmp <- tmp %>%
        dplyr::filter(date >= start_date) %>%
        dplyr::filter(date <= end_date)
    }

    # Remove duplicate IDs with _ for certain sensors
    if (.x %in% c("Accelerometer", "AppUsage", "Bluetooth",
                  "Calendar", "Gyroscope", "TextMessage")) {
      tmp <- tmp %>%
        dplyr::mutate(measurement_id = substr(measurement_id, 1, 36)) %>%
        dplyr::distinct()
    }

    # Calculate the number of average measurements per hour
    # i.e. the sum of all measurements in that hour divided by n
    tmp <- tmp %>%
      dplyr::mutate(Hour = strftime("%H", time)) %>%
      # dplyr::mutate(Date = date(time)) %>%
      dplyr::count(date, Hour) %>%
      dplyr::group_by(Hour) %>%
      dplyr::summarise(Coverage = sum(n, na.rm = TRUE) / n())

    # Transfer the result to R's memory and ensure it's numeric
    tmp <- tmp %>%
      dplyr::collect() %>%
      dplyr::mutate(Hour = as.numeric(Hour),
                    Coverage = as.numeric(Coverage))

    # Disconnect from the temporary database connection
    DBI::dbDisconnect(tmp_db)

    # Calculate the relative target frequency ratio by dividing the average number of measurements
    # per hour by the expected number of measurements
    if (relative) {
      tmp <- tmp %>%
        dplyr::mutate(Coverage = round(Coverage / frequency[.x], 2))
    }

    tmp %>%
      # Pour into ggplot format
      dplyr::mutate(measure = .x) %>%
      # Fill in missing hours with 0
      tidyr::complete(Hour = 0:23,
                      measure = .x,
                      fill = list(Coverage = 0))
  }, .options = furrr::furrr_options(seed = TRUE))

  # Give the output list the sensor names
  names(data) <- names(sensor)

  data
}
