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
#' @param parallel Logical value which indicates whether to do reading in and processing
#' in parallel.
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
  if (backend == "RSQLite" & parallel) {
    warning("Parallel cannot be used when RSQLite is provided as a backend due to concurrency
            constraint. Setting parallel to false.")
    parallel <- FALSE
  }

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
      return(message("No new files to process."))
    }
  }

  # Set up parallel back-end
  if (parallel) {
    future::plan(future::multisession)
  }

  # Call on implementation with or without progress bar
  if (progress) {
    progressr::with_progress({
      import_impl(path, files, db@dbname)
    })
  } else {
    import_impl(path, files, db@dbname)
  }

  # Return to sequential processing
  if (parallel) {
    future::plan(future::sequential)
  }

  processed_files <- get_processed_files(db)
  complete <- all(files %in% processed_files$file_name)
  if (complete) {
    message("All files were successfully written to the database.")
  } else {
    warning("Some files could not be written to the database.")
  }

  RSQLite::dbDisconnect(db)
}

import_impl <- function(path, files, db_name) {
  p <- progressr::progressor(length(files))

  # furrr::map_walk(file, ~{})
  for (i in seq_along(files)) {

    # Update progress bar
    p(sprintf("x=%g", i))

    # Try to read in the file. If the file is corrupted for some reason, skip this one
    file <- normalizePath(paste0(path, "/", files[i]))
    if (!jsonlite::validate(readLines(file, warn = FALSE))) next

    possible_error <- tryCatch({
      data <- rjson::fromJSON(file = file, simplify = FALSE)
    }, error = function(e) e)

    if (inherits(possible_error, "error")) {
      print(paste("Could not read", files[i]))
      next
    }

    # Open db
    tmp_db <- open_db(NULL, db_name)

    # Check if it is not an empty file
    # Skip this file if empty
    if (length(data) == 0) {
      add_processed_file(tmp_db,
                         data.frame(
                           file_name = files[i],
                           # TODO: Update extraction of participant_id with new file name
                           participant_id = strsplit(files[i], "_")[[1]][2],
                           study_id = "-1"
                         )
      )
      RSQLite::dbDisconnect(tmp_db)
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
        tmp <- eval(parse(text = paste0("obs[[1]]$", var)))
        if (is.null(tmp)) return(NULL) else return(tmp)
      })
      if (all(unlist(lapply(out, is.null))))
        out <- rep("N/A", length(out))
      return(out)
    }
    data$study_id <- safe_extract(data$header, "study_id")
    data$device_role_name <- safe_extract(data$header, "device_role_name")
    data$trigger_id <- safe_extract(data$header, "trigger_id")
    data$participant_id <- safe_extract(data$header, "user_id")
    data$start_time <- safe_extract(data$header, "start_time")
    data$data_format <- safe_extract(data$header, "data_format$namespace")
    data$sensor <- safe_extract(data$header, "data_format$name")
    data$header <- NULL
    data <- tidyr::unnest(data, c(study_id:sensor))

    # Due to the hacky solution above, filter out rows where the participant_id is missing, usually
    # in the last entry of a file
    data <- data[!is.na(data$participant_id) & data$participant_id != "N/A", ]

    # Safe duplicate check before insertion
    # Check if file is already registered as processed
    # Now using the participant_id and study_id as
    this_file <- data.frame(
      file_name = files[i],
      participant_id = unique(data$participant_id),
      study_id = unique(data$study_id)
    )
    processed_files <- get_processed_files(tmp_db)
    matches <- dplyr::inner_join(this_file,
      processed_files,
      by = c("file_name", "participant_id", "study_id")
    )
    if (nrow(matches) > 0) {
      next # File was already processed
    }

    # Populate study specifics to db
    add_study(db = tmp_db, data = dplyr::distinct(data, study_id, data_format))

    # Add participants
    add_participant(db = tmp_db, data = dplyr::distinct(data, participant_id, study_id))

    # Make sure top-level of data$body is called body and not carp_body as in the new version
    data <- dplyr::mutate(data, body = purrr::modify(body, purrr::set_names, nm = "body"))

    # Divide et impera
    data <- split(data, as.factor(data$sensor), drop = TRUE)

    # Drop useless data
    data[["unknown"]] <- NULL

    # Call function for each sensor
    tryCatch({
      RSQLite::dbWithTransaction(tmp_db, {
        for (j in seq_along(data)) {
          # Get sensor name
          sensor <- names(data)[[j]]
          tmp <- data[[sensor]]
          which_sensor(tmp_db, tmp, sensor)
        }
      })

      # Add file to list of processed files
      add_processed_file(tmp_db, this_file)
    }, error = function(e) {
      print(paste0("transaction failed for file ", files[i]))
    })

    # Close db connection of worker
    RSQLite::dbDisconnect(tmp_db)
  }
}


#' Calculate total acceleration from x, y, and z coordinates
#'
#' A convenience function to calculate the total acceleration in either or a local or remote
#' tibble.
#'
#' @param data A data frame or a remote data frame connection through \link[dplyr]{tbl}.
#' @param colname The name of the newly added column containing the total acceleration.
#' @param x Acceleration along the x-axis.
#' @param y Acceleration along the y-axis.
#' @param z Acceleration along the z-axis.
#' @param gravity Gravity in meters per second. Defaults to 9.810467. Set to 0 to ignore.
#'
#' @return The input data with a column \code{colname} attached.
#' @export
#'
#' @examples
#' \dontrun{
#' db <- open_db()
#' tbl(db, "Accelerometer") %>%
#'   total_acceleration("total_acc")
#' }
total_acceleration <- function(data, colname, x = x, y = y, z = z, gravity = 9.810467) {
  data %>%
    dplyr::mutate(!!colname := sqrt((x)^2 + (y)^2 + (z - gravity)^2))
}

#' Measurement frequencies per sensor
#'
#' A numeric vector containing (an example) of the measurement frequencies per sensor.
#' Such input is needed for \link[CARP]{coverage}.
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
  Pedometer = 1,
  Weather = 1,
  Wifi = 60 # once per minute
)

#' Create a coverage chart showing sampling rate
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

  if (!RSQLite::dbIsValid(db)) {
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

  # Check start_date, end_date
  if ((!is.null(start_date) && !is.null(end_date)) && !is.null(offset)) {
    warning("Argument start_date/end_date and offset cannot be present at the same time.
						Ignoring the offset argument.")
    offset <- NULL
  }
  # Unnecesary since get_data retrieves all data anyway when no start or end date is specified
  # else if (is.null(start_date) & is.null(end_date)) {
    # start_date <- first_date(db, "Accelerometer", participant_id)
    # end_date <- last_date(db, "Accelerometer", participant_id)
  # }
  else if ((!is.null(start_date) | !is.null(end_date)) &&
           (!(inherits(start_date, "Date") | is.character(start_date)) |
           !(inherits(end_date, "Date") | is.character(end_date)))) {
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
    RSQLite::dbDisconnect(tmp_db)

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
