#' Measurement frequencies per sensor
#'
#' A numeric vector containing (an example) of example measurement frequencies per sensor.
#' Such input is needed for \link[mpathsenser]{coverage}.
#'
#' @return This vector contains the following
#' information:
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
#' Noise | 120 | Once every 30 seconds. Microphone cannot be used in the background in Android 11.
#' Weather | 1 | Once per hour.
#' Wifi | 60 |  Once per minute.
#'
#' @export freq
freq <- c(
  Accelerometer = 720,
  AirQuality = 1,
  AppUsage = 2,
  Bluetooth = 60,
  Gyroscope = 720,
  Light = 360,
  Location = 60,
  Memory = 60,
  Noise = 120,
  Weather = 1,
  Wifi = 60
)

#' Create a coverage chart of the sampling rate
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Only applicable to non-reactive sensors with 'continuous' sampling
#'
#' @param db A valid database connection. Schema must be that as it is created by
#' \link[mpathsenser]{open_db}.
#' @param participant_id A character string of _one_ participant ID.
#' @param sensor A character vector containing one or multiple sensors. See
#' \code{\link[mpathsenser]{sensors}} for a list of available sensors. Use \code{NULL} for all
#' available sensors.
#' @param frequency A named numeric vector with sensors as names and the number of expected samples
#' per hour
#' @param relative Show absolute number of measurements or relative to the expected number?
#' Logical value.
#' @param offset Currently not used.
#' @param start_date A date (or convertible to a date using \code{\link[base]{as.Date}}) indicating
#' the earliest date to show. Leave empty for all data. Must be used with \code{end_date}.
#' @param end_date A date (or convertible to a date using \code{\link[base]{as.Date}}) indicating
#' the latest date to show.Leave empty for all data. Must be used with \code{start_date}.
#' @param plot `r lifecycle::badge("deprecated")` Instead of built-in functionality, use
#'  \code{\link[mpathsenser]{plot.coverage}} to plot the output.
#'
#'
#' @return A ggplot of the coverage results if \code{plot} is \code{TRUE} or a tibble containg the
#' hour, type of measure (i.e. sensor), and (relative) coverage.
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
#'   frequency = mpathsenser::freq,
#'   start_date = "2021-01-01",
#'   end_date = "2021-05-01"
#' )
#' }
coverage <- function(db,
                     participant_id,
                     sensor = NULL,
                     frequency = mpathsenser::freq,
                     relative = TRUE,
                     offset = "None",
                     start_date = NULL,
                     end_date = NULL,
                     plot = deprecated()) {
  check_db(db)
  check_arg(participant_id, type = c("character"), n = 1)
  check_sensors(sensor, allow_null = TRUE)
  check_arg(frequency, type = "double")
  check_arg(relative, "logical", n = 1)

  # Check sensors
  if (is.null(sensor) || length(sensor) == 1 && sensor == "All") {
    sensor <- sensors
  }

  # Check participants
  if (!(participant_id %in% get_participants(db)$participant_id)) {
    abort("Participant_id not known.")
  }

  # Check frequency
  if (!relative && !is.numeric(frequency) || is.null(names(frequency))) {
    abort("Frequency must be a named numeric vector")
  }

  # Old plot argument
  if (lifecycle::is_present(plot)) {
    lifecycle::deprecate_warn(
      when = "1.1.1",
      what = "coverage(plot)",
      with = "plot()"
    )
  }

  # Check time subset
  if (grepl("\\d day", offset)) {
    offset <- paste0("-", offset)
  } else if (is.null(offset) || (tolower(offset) == "none")) {
    offset <- NULL
  } else {
    abort("Argument offset must be either 'None', 1 day, or 2, 3, 4, ... days.")
  }

  # Helper function for checking if a string is convertible to date
  convert2date <- function(s) {
    if (!inherits(s, "Date") && !is.character(s)) {
      return(FALSE)
    }
    s <- try(as.Date(s), silent = TRUE)
    return(inherits(s, "Date"))
  }

  # Check start_date, end_date
  if ((!is.null(start_date) && !is.null(end_date)) && !is.null(offset)) {
    warn(c(
      "Argument start_date/end_date and offset cannot be present at the same time. ",
      i = "Ignoring the offset argument."
    ))
    offset <- NULL
  } else if (!(is.null(start_date) || convert2date(start_date)) ||
    !(is.null(end_date) || convert2date(end_date))) {
    abort("start_date and end_date must be NULL, a character string, or date.")
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
  data <- bind_rows(data)
  data$measure <- factor(data$measure)
  data$measure <- factor(data$measure, levels = rev(levels(data$measure)))

  class(data) <- c("coverage", class(data))
  attr(data, "participant_id") <- participant_id
  return(data)
}

#' Plot a coverage overview
#'
#' @param x A tibble with the coverage data coming from \code{\link[mpathsenser]{coverage}}.
#' @param ... Other arguments passed on to methods. Not currently used.
#'
#' @seealso \code{\link[mpathsenser]{coverage}}
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @export
#' @keywords internal
plot.coverage <- function(x, ...) {
  ensure_suggested_package("ggplot2")

  ggplot2::ggplot(
    data = x,
    mapping = ggplot2::aes(x = .data$hour, y = .data$measure, fill = .data$coverage)
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(
      mapping = ggplot2::aes(label = coverage),
      colour = "white"
    ) +
    ggplot2::scale_x_continuous(breaks = 0:23) +
    ggplot2::scale_fill_gradientn(
      colours = c("#d70525", "#645a6c", "#3F7F93"),
      breaks = c(0, 0.5, 1),
      labels = c(0, 0.5, 1),
      limits = c(0, 1)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste0("Coverage for participant ", attr(x, "participant_id")),
      x = "Hour",
      y = "Sensor"
    )
}

coverage_impl <- function(db, participant_id, sensor, frequency, relative, start_date, end_date) {
  # Interesting bug/feature in dbplyr: If participant_id is used in the query, the index of the
  # table is not used. Hence, we rename participant_id to p_id
  p_id <- as.character(participant_id) # nolint

  # Loop over each sensor and calculate the coverage rate for that sensor
  data <- furrr::future_map(.x = sensor, .f = ~ {
    tmp_db <- open_db(NULL, db@dbname)

    # Extract the data for this participant and sensor
    tmp <- dplyr::tbl(tmp_db, .x) %>%
      filter(participant_id == p_id) %>%
      select("measurement_id", "time", "date")

    # Filter by date if needed
    if (!is.null(start_date) && !is.null(end_date)) {
      tmp <- tmp %>%
        filter(date >= start_date) %>%
        filter(date <= end_date)
    }

    # Remove duplicate IDs with _ for certain sensors
    # Removed Accelerometer and Gyroscope from the list, as they are already binned per second
    if (.x %in% c(
      "AppUsage", "Bluetooth",
      "Calendar", "InstalledApps", "TextMessage"
    )) {
      tmp <- tmp %>%
        mutate(measurement_id = substr(.data$measurement_id, 1, 36)) %>%
        distinct()
    }

    # Calculate the number of average measurements per hour i.e. the sum of all measurements in
    # that hour divided by n
    tmp <- tmp %>%
      mutate(hour = strftime("%H", .data$time)) %>%
      # mutate(Date = date(time)) %>%
      dplyr::count(.data$date, .data$hour) %>%
      group_by(.data$hour) %>%
      summarise(coverage = sum(.data$n, na.rm = TRUE) / n())

    # Transfer the result to R's memory and ensure it's numeric
    tmp <- tmp %>%
      collect() %>%
      mutate(hour = as.numeric(.data$hour), coverage = as.numeric(.data$coverage))

    # Disconnect from the temporary database connection
    dbDisconnect(tmp_db)

    # Calculate the relative target frequency ratio by dividing the average number of measurements
    # per hour by the expected number of measurements
    if (relative) {
      tmp <- tmp %>%
        mutate(coverage = round(.data$coverage / frequency[.x], 2))
    }

    tmp %>%
      # Pour into ggplot format
      mutate(measure = .x) %>%
      # Fill in missing hours with 0
      complete(hour = 0:23, measure = .x, fill = list(coverage = 0))
  }, .options = furrr::furrr_options(seed = TRUE))

  # Give the output list the sensor names
  names(data) <- names(sensor)

  data
}
