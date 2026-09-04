#' Measurement frequencies per sensor
#'
#' A numeric vector containing (an example) of example measurement frequencies per sensor.
#' Such input is needed for [coverage()].
#'
#' @returns This vector contains the following
#' information:
#'
#' Sensor | Frequency (per hour) | Full text
#' -------|-----------|----------
#' Accelerometer | 720 | Once per 5 seconds. Can have multiple instances.
#' Activity | 120 | Once per 30 seconds.
#' AppUsage | 2 | Once every 30 minutes. Can have multiple instances.
#' Battery | 60 | Once per minute.
#' Bluetooth | 12 | Once every 5 minutes. Can have multiple instances.
#' BluetoothBeacon | 12 | Once every 5 minutes. Can have multiple instances.
#' Connectivity | 12 | Once every 5 minutes.
#' Device | 60 | Once per minute.
#' Error | 12 | Once every 5 minutes.
#' Heartbeat | 60 | Once per minute.
#' Light | 360 | Once per 10 seconds.
#' Location | 60 | Once every 60 seconds.
#' Memory | 60 | Once per minute
#' Pedometer | 60 | Once per minute.
#' Screen | 12 | Once every 5 minutes.
#' Timezone | 12 | Once every 5 minutes.
#' Weather | 1 | Once per hour.
#' Wifi | 60 | Once per minute.
#'
#' @export freq
#'
#' @examples
#' freq
freq <- c(
  Accelerometer = 720,
  Activity = 120,
  AppUsage = 2,
  Battery = 60,
  Bluetooth = 12,
  BluetoothBeacon = 12,
  Connectivity = 12,
  Device = 60,
  Error = 12,
  Heartbeat = 60,
  Light = 360,
  Location = 60,
  Memory = 60,
  Pedometer = 60,
  Screen = 12,
  Timezone = 12,
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
#' [open_db].
#' @param participant_id A single participant ID. Stored as an unsigned integer;
#'   both an integer/numeric and a character value are accepted.
#' @param sensor A character vector containing one or multiple sensors. See
#' \code{\link[mpathsenser]{sensors}} for a list of available sensors. Use `NULL` for all
#' available sensors.
#' @param frequency A named numeric vector with sensors as names and the number of expected samples
#' per hour
#' @param relative Show absolute number of measurements or relative to the expected number?
#' Logical value.
#' @param offset Currently not used.
#' @param start_date A date (or convertible to a date using [base::as.Date()]) indicating
#' the earliest date to show. Leave empty for all data. Must be used with `end_date`.
#' @param end_date A date (or convertible to a date using [base::as.Date()]) indicating
#' the latest date to show.Leave empty for all data. Must be used with `start_date`.
#' @param plot `r lifecycle::badge("deprecated")` Instead of built-in functionality, use
#'  [plot.coverage()] to plot the output.
#'
#'
#' @returns A ggplot of the coverage results if `plot` is `TRUE` or a tibble containing the
#' hour, type of measure (i.e. sensor), and (relative) coverage.
#' @export
#'
#' @examples
#' \dontrun{
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
#'
#' coverage(
#'   db = db,
#'   participant_id = "12345",
#'   sensor = c("Accelerometer", "Gyroscope"),
#'   frequency = mpathsenser::freq,
#'   start_date = "2021-01-01",
#'   end_date = "2021-05-01"
#' )
#' }
coverage <- function(
  db,
  participant_id,
  sensor = NULL,
  frequency = mpathsenser::freq,
  relative = TRUE,
  offset = "None",
  start_date = NULL,
  end_date = NULL,
  plot = deprecated()
) {
  check_db(db)
  # participant_id is stored as an unsigned integer, but a character or
  # numeric value is accepted so both "12345" and 12345 work.
  check_arg(participant_id, type = c("character", "numeric"), n = 1)
  check_sensors(sensor, allow_null = TRUE)
  check_arg(frequency, type = "numeric")
  check_arg(relative, "logical", n = 1)

  # Check sensors
  if (is.null(sensor)) {
    sensor <- sensors
  }

  # In case the check for all columns present in the database is skipped, keep only sensors that
  # actually occur in the database.
  sensor <- .physical_sensor(sensor)
  sensor <- sensor[tolower(sensor) %in% tolower(mpathsenser::sensors)]

  # Check participants
  if (!(participant_id %in% get_participants(db)$participant_id)) {
    cli_abort("{.val {participant_id}} is not a known participant.")
  }

  # Check frequency
  if (!relative && !is.numeric(frequency) || is.null(names(frequency))) {
    cli_abort("{.arg frequency} must be a named numeric vector.")
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
    cli_abort(c(
      "{.arg offset} must be {.val None}, or a day specification like {.val 1 day}.",
      i = "For example: {.val 1 day}, {.val 2 days}, etc."
    ))
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
    cli_warn(c(
      "Argument start_date/end_date and offset cannot be present at the same time. ",
      i = "Ignoring the offset argument."
    ))
    offset <- NULL
  } else if (
    !(is.null(start_date) || convert2date(start_date)) ||
      !(is.null(end_date) || convert2date(end_date))
  ) {
    cli_abort(
      "{.arg start_date} and {.arg end_date} must be {.code NULL}, a date string, or a {.cls Date}."
    )
  }

  # Retain only frequencies that appear in the sensor list
  frequency <- frequency[names(frequency) %in% sensor]

  # If relative, retain only sensors that have a frequency
  if (relative) {
    sensor <- names(frequency)
  }

  # Calculate coverage from db - internal function
  data <- coverage_impl2(
    db,
    participant_id,
    sensor,
    frequency,
    relative,
    start_date,
    end_date
  )

  # Bind all together and make factors
  data$measure <- factor(data$measure)
  data$measure <- factor(data$measure, levels = rev(levels(data$measure)))

  class(data) <- c("coverage", class(data))
  attr(data, "participant_id") <- participant_id
  attr(data, "relative") <- relative
  return(data)
}

#' Plot a coverage overview
#'
#' @param x A tibble with the coverage data coming from [coverage()].
#' @param ... Other arguments passed on to methods. Not currently used.
#'
#' @seealso [coverage()]
#' @returns A [ggplot2::ggplot] object.
#' @export
#'
#' @examples
#' \dontrun{
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
#'
#' data <- coverage(
#'   db = db,
#'   participant_id = "12345",
#'   sensor = c("Accelerometer", "Gyroscope"),
#'   frequency = mpathsenser::freq,
#'   start_date = "2021-01-01",
#'   end_date = "2021-05-01"
#' )
#'
#' plot(data)
#' }
plot.coverage <- function(x, digits = 2, ...) {
  ensure_suggested_package("ggplot2")

  is_relative <- attr(x, "relative")
  is_relative <- if (is.null(is_relative)) TRUE else is_relative
  participant_id <- attr(x, "participant_id")
  if (!is_relative) {
    x <- x |>
      group_by(.data$measure) |>
      mutate(max_coverage = max(.data$coverage)) |>
      mutate(
        max_coverage = ifelse(.data$max_coverage == 0, 1, .data$max_coverage)
      ) |>
      mutate(scaled_coverage = .data$coverage / max(.data$max_coverage)) |>
      ungroup("measure")

    plot <- ggplot2::ggplot(
      data = x,
      mapping = ggplot2::aes(
        x = .data$hour,
        y = .data$measure,
        fill = .data$scaled_coverage
      )
    )
  } else {
    plot <- ggplot2::ggplot(
      data = x,
      mapping = ggplot2::aes(
        x = .data$hour,
        y = .data$measure,
        fill = .data$coverage
      )
    )
  }

  plot <- plot +
    ggplot2::geom_tile() +
    ggplot2::geom_text(
      mapping = ggplot2::aes(label = round(coverage, digits = digits)),
      colour = "white"
    ) +
    ggplot2::scale_x_continuous(breaks = 0:23) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste0("Coverage for participant ", participant_id),
      x = "Hour",
      y = "Sensor"
    )

  if (is_relative) {
    plot <- plot +
      ggplot2::scale_fill_gradientn(
        colours = c("#d70525", "#645a6c", "#3F7F93"),
        breaks = c(0, 0.5, 1),
        labels = c(0, 0.5, 1),
        limits = c(0, 1),
        name = "coverage"
      )
  } else {
    plot <- plot +
      ggplot2::scale_fill_gradientn(
        colours = c("#d70525", "#645a6c", "#3F7F93"),
        breaks = c(0, 0.5, 1),
        labels = c("low", "medium", "high"),
        limits = c(0, 1),
        name = "coverage"
      )
  }

  plot
}


coverage_impl2 <- function(
  db,
  participant_id,
  sensor,
  frequency,
  relative,
  start_date,
  end_date
) {
  # Get the data for each sensor
  data <- purrr::map(
    .x = sensor,
    .f = \(snsr) {
      db |>
        get_data(
          sensor = snsr,
          participant_id = participant_id,
          start_date = start_date,
          end_date = end_date
        ) |>
        select("time", any_of("device_role_name")) |>
        mutate(measure = snsr)
    }
  )
  names(data) <- sensor

  # For heartbeat, keep only the measurements from the phone (and other primary
  # devices), excluding secondary phones that log their own heartbeats.
  if ("Heartbeat" %in% sensor) {
    data[["Heartbeat"]] <- data[["Heartbeat"]] |>
      filter(
        is.na(.data$device_role_name) | !grepl("^Secondary", .data$device_role_name)
      )
  }

  # For each sensor, calculate the number of average measurements per hour
  data <- purrr::map(
    .x = data,
    .f = \(x) {
      x |>
        distinct(.data$time, .data$measure) |>
        mutate(
          date = as.character(as.Date(.data$time)),
          hour = strftime("%H", .data$time)
        ) |>
        dplyr::count(.data$date, .data$hour, .data$measure)
    }
  )

  # Merge the data
  data <- purrr::reduce(data, dplyr::union)

  # Run the query and bring the result to R
  data <- collect(data)

  # Calculate the min and max date for the complete date sequence. If start_date and end_date are
  # provided, use those. Otherwise, calculate from the data.
  min_date <- if (!is.null(start_date)) {
    as.Date(start_date)
  } else {
    quote(min(as.Date(.data$date)))
  }

  max_date <- if (!is.null(end_date)) {
    as.Date(end_date)
  } else {
    quote(max(as.Date(.data$date)))
  }

  # Make sure that all combinations of hour and measure are present, filling in missing values with 0
  .data <- data |>
    mutate(
      hour = as.character(.data$hour),
      measure = as.character(.data$measure),
      n = as.integer(.data$n)
    ) |>
    complete(
      date = as.character(seq(eval(min_date), eval(max_date), by = "days")),
      hour = sprintf("%02d", 0:23),
      measure = sensor,
      fill = list(n = 0)
    )

  .data <- .data |>
    group_by(.data$hour, .data$measure) |>
    summarise(coverage = mean(.data$n, na.rm = TRUE), .groups = "drop") |>
    mutate(hour = as.integer(.data$hour))

  # Calculate the relative target frequency ratio by dividing the average number of measurements
  # per hour by the expected number of measurements
  if (relative) {
    .data <- .data |>
      mutate(coverage = round(.data$coverage / unname(frequency[.data$measure]), 2))
  } else {
    .data <- .data |>
      mutate(coverage = round(.data$coverage, 2))
  }

  .data
}
