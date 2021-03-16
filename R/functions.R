#' Import CARP files into a database (CARP data scheme)
#'
#' Currently, only SQLite is supported as a backend. Due to its concurrency restriction, the
#' `parallel` option is disabled.
#'
#' @param path The path to the file directory
#' @param db Valid database connection.
#' @param dbname If no database is provided, a new database dbname is created.
#' @param backend Name of the database backend that is used. Currently, only RSQLite is supported.
#' @param progress Logical value to show a progress bar or not.
#' @param parallel Logical value which indicates whether to do reading in and processing in parallel.
#'
#' @return Invisible. Imported database can be reopened using \link[CARP]{open_db}.
#' @export
import <- function(path = getwd(),
                   db = NULL,
                   dbname = "carp.db",
                   backend = "RSQLite",
                   progress = TRUE,
                   parallel = FALSE
) {

  # Retrieve all JSON files
  files <- dir(path = path, pattern = "*.json$")

  if (length(files) == 0) {
    stop("No JSON files found")
  }

  # Check backend and parallel constraint
  if (backend == "RSQLite" & parallel) {
    warning("Parallel cannot be used when RSQLite is provided as a backend due to concurrency constraint. Setting parallel to false.")
    parallel <- FALSE
  }

  # Set up database if not provided
  if (is.null(db)) {
    db <- create_db(db_name = dbname)
  }

  # If there are no duplicate files
  # Proceed with the unsafe (but fast) check
  # to prevent duplicate insertion into db
  if (anyDuplicated(files) == 0) {
    processedFiles <- get_processed_files(db)
    # Keep files _not_ already registered in db
    files <- files[!(files %in% processedFiles$file_name)]

    if (length(files) == 0) {
      return("No new files to process.")
    }
  }

  # Set up parallel backend
  if (parallel) {
    doFuture::registerDoFuture()
    future::plan(future::multisession)
  } else {
    foreach::registerDoSEQ()
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

  processedFiles <- get_processed_files(db)
  complete <- all(files %in% processedFiles$file_name)
  if (complete) {
    message("All files were successfully written to the database.")
  } else {
    warning("Some files could not be written to the database.")
  }

  RSQLite::dbDisconnect(db)
}

import_impl <- function(path, files, db_name) {
  p <- progressr::progressor(length(files))
  # foreach::`%dopar%`(foreach::foreach(i = 1:length(files)), {
  for (i in 1:length(files)) {
    # Update progress bar
    p(sprintf("x=%g", i))
    # for(i in 1:length(files)) {

    data <- rjson::fromJSON(file = paste0(path, "/", files[i]), simplify = FALSE)

    # Check if it is not an empty file
    # Return NULL is true
    if (length(data) == 0) {
      return(NULL)
    }

    # Clean-up and extract the header and body
    data <- tibble::tibble(
      header = lapply(data, function(x) x[1]),
      body = lapply(data, function(x) x[2])
    )
    # Extract columns
    data$study_id <- lapply(data$header, function(x) x$header$study_id)
    data$participant_id <- lapply(data$header, function(x) x$header$user_id)
    data$start_time <- lapply(data$header, function(x) x$header$start_time)
    data$data_format <- lapply(data$header, function(x) x$header$data_format$namespace)
    data$sensor <- lapply(data$header, function(x) x$header$data_format$name)
    data$header <- NULL
    data <- tidyr::unnest(data, c(study_id:sensor))

    # Open db
    tmp_db <- open_db(db_name)

    # Safe duplicate check before insertion
    # Check if file is already registered as processed
    # Now using the participant_id and study_id as
    this_file <- data.frame(
      file_name = files[i],
      participant_id = unique(data$participant_id),
      study_id = unique(data$study_id)
    )
    processedFiles <- get_processed_files(tmp_db)
    matches <- dplyr::inner_join(this_file,
      processedFiles,
      by = c("file_name", "participant_id", "study_id")
    )
    if (nrow(matches) > 0) {
      return(NULL) # File was already processed
    }

    # Populate study specifics to db
    add_study(db = tmp_db, data = dplyr::distinct(data, study_id, data_format))

    # Add participants
    add_participant(db = tmp_db, data = dplyr::distinct(data, participant_id, study_id))

    # Divide et impera
    data <- split(data, as.factor(data$sensor), drop = TRUE)

    # Drop useless data
    data[["unknown"]] <- NULL

    # Call function for each sensor
    tryCatch({
        RSQLite::dbWithTransaction(tmp_db, {
          for(j in 1:length(data)) {
            # Get sensor name
            sensor <- names(data)[[j]]
            tmp <- data[[sensor]]
            which_sensor(tmp_db, tmp, sensor)
          }
        })

        # Add file to list of processed files
        add_processed_file(tmp_db, this_file)
      }, error = function(e) {}) # Empty for now

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
#' db <- open_db()
#' tbl(db, "Accelerometer") %>%
#'   total_acceleration("total_acc")
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
#' @param sensor A character vector containing one or multiple sensors. See \code{\link[CARP]{sensors}} for a list of available sensors. Use "All" for all available sensors.
#' @param frequency A named numeric vector with sensors as names and the number of expected samples per hour
#' @param relative Show absolute number of measurements or relative to the expected number? Logical value.
#' @param offset Currently not used.
#' @param startDate A date (or convertible to a date using \code{\link[base]{as.Date}}) indicating the earliest date to show. Leave empty for all data. Must be used with \code{endDate}.
#' @param endDate A date (or convertible to a date using \code{\link[base]{as.Date}}) indicating the latest date to show.Leave empty for all data. Must be used with \code{startDate}.
#'
#' @importFrom magrittr "%>%"
#'
#' @return A ggplot of the coverage results.
#' @export
#'
#'
#' @examples
#' setwd("~/data")
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
#'   startDate = "2021-01-01",
#'   endDate = "2021-05-01"
#' )
coverage <- function(db,
                     participant_id,
                     sensor = "All",
                     frequency = CARP::freq,
                     relative = TRUE,
                     offset = "None",
                     startDate = NULL,
                     endDate = NULL) {
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
  if (!is.numeric(frequency) || is.null(names(frequency))) {
    stop("Frequency is must be a named numeric vector")
  }

  # Check time subset
  if (grepl("\\d day", offset)) {
    offset <- paste0("-", offset)
  } else if (is.null(offset) || (tolower(offset) == "none")) {
    offset <- NULL
  } else {
    stop("Argument offset must be either 'None', 1 day, or 2, 3, 4, ... days.")
  }

  # Check startDate, endDate
  if ((!is.null(startDate) && !is.null(endDate)) && !is.null(offset)) {
    warning("Argument startDate/endDate and offset cannot be present at the same time.
						Ignoring the offset argument.")
    offset <- NULL
  } else if (is.null(startDate) & is.null(endDate)) {
    startDate <- first_date(db, "Accelerometer", participant_id)
    endDate <- last_date(db, "Accelerometer", participant_id)
  }

  else if (!(is(startDate, "Date") | is.character(startDate)) |
           !(is(endDate, "Date") | is.character(endDate))) {
    stop("startDate and endDate must be a character string or date.")
  }

  # Retain only frequencies that appear in the sensor list
  frequency <- frequency[names(frequency) %in% sensor]

  # Get data from db - internal function
  data <- coverage_impl(db, participant_id, frequency, startDate, endDate)

  # Bind all together and make factors
  data <- dplyr::bind_rows(data)
  data$measure <- factor(data$measure)
  data$measure <- factor(data$measure, levels = rev(levels(data$measure)))

  # Plot
  ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = Hour, y = measure, fill = Coverage)) +
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
}

coverage_impl <- function(db, participant_id, frequency, startDate, endDate) {
  # Interesting bug/feature in dbplyr: If participant_id is used in the query,
  # the index of the table is not used. Hence, we rename participant_id to p_id
  p_id <- as.character(participant_id)

  data <- furrr::future_map(.x = names(frequency), .f = ~ {
    tmp_db <- open_db(db@dbname)

    tmp <- dplyr::tbl(tmp_db, .x) %>%
      dplyr::filter(participant_id == p_id) %>%
      dplyr::select(measurement_id, time, date)

    if (!is.null(startDate) && !is.null(endDate)) {
      tmp <- tmp %>%
        dplyr::filter(date >= startDate) %>%
        dplyr::filter(date <= endDate)
    }

    # Remove duplicate IDs with _ for certain sensors
    if (.x %in% c(
      "Accelerometer", "AppUsage", "Bluetooth",
      "Calendar", "Gyroscope", "TextMessage"
    )) {
      tmp <- tmp %>%
        dplyr::mutate(measurement_id = substr(measurement_id, 1, 36)) %>%
        dplyr::distinct()
    }

    tmp <- tmp %>%
      dplyr::mutate(Hour = strftime("%H", time)) %>%
      # dplyr::mutate(Date = date(time)) %>%
      dplyr::count(date, Hour) %>%
      dplyr::group_by(Hour) %>%
      dplyr::summarise(Coverage = sum(n, na.rm = TRUE) / n())

    tmp <- tmp %>%
      dplyr::collect() %>%
      dplyr::mutate(Hour = as.numeric(Hour),
                    Coverage = as.numeric(Coverage))

    RSQLite::dbDisconnect(tmp_db)

    tmp %>%
      # Calculate its relative target frequency ratio
      dplyr::mutate(Coverage = round(Coverage / frequency[.x], 2)) %>%
      # Pour into ggplot format
      dplyr::mutate(measure = .x) %>%
      # Complete missing hours with 0
      tidyr::complete(Hour = 0:23,
                      measure = .x,
                      fill = list(Coverage = 0))
  }, .options = furrr::furrr_options(seed = TRUE))

  names(data) <- names(frequency)

  data
}
