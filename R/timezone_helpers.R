#' Add timezone to measurements in an m-Path Sense database
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function looks for a `Timezone` table in a local m-Path Sense SQLite database and uses it
#'   to assign timezone values to all other sensor tables. For each observation in a sensor table,
#'   it finds the timezone interval that matches the observation's timestamp and adds or updates a
#'   `timezone` column accordingly.
#'
#' @details
#' Note that rerunning this function overwrites existing `timezone` columns in the sensor tables.
#' Also note that if a measurement matches multiple time zones (i.e. a timezone change occurred
#' during the measurement), the timezone with the longest overlap is chosen.
#'
#'
#' @param db A database connection, typically created by [open_db()].
#' @param sensors A character vector of sensor table names to update. Defaults to `NULL` for all supported
#'   sensors.
#' @param .progress Logical; whether to show a progress bar during processing. Defaults to `TRUE`.
#'
#' @return Invisibly returns `TRUE` if all updates complete successfully.
#'
#' @examples
#' \dontrun{
#' # Connect to an m-Path Sense database
#' db <- open_db("path/to/db.sqlite")
#'
#' # Add timezone information to all tables
#' add_timezones_to_db(db)
#'
#' # Disconnect when done
#' close_db(db)
#' }
#'
#' @export
add_timezones_to_db <- function(db, sensors = NULL, .progress = TRUE) {
  check_db(db)
  check_sensors(sensors, allow_null = TRUE)

  if (is.null(sensors)) {
    sensors <- mpathsenser::sensors
  }

  # Do not add the timezone to the timezone table itself to avoid confusion
  if ("timezone" %in% tolower(sensors)) {
    sensors <- sensors[which(tolower(sensors) != "timezone")]
  }

  # Check that the table timezone exists
  if (!DBI::dbExistsTable(db, "Timezone")) {
    cli::cli_abort(
      c(
        "The table `Timezone`  does not exist in the database.",
        i = "Check whether timezone measurements appear in your raw data.",
        i = "If there are, something went wrong when reading in the data.",
        i = "Otherwise, data may have been collected with an older version of m-Path Sense \\
             that did not support timezones."
      )
    )
  }

  # First, get the ordered timezone data and add an end timestamp based on the next observation.
  # This creates a remote tibble of timezones with start and end timestamps for each participant.
  tzs <- dplyr::tbl(db, "Timezone") |>
    mutate(.start = UNIXEPOCH(paste(.data$date, .data$time))) |>
    dbplyr::window_order(.data$participant_id, .data$.start) |>
    group_by(.data$participant_id) |>
    mutate(.end = lead(.data$.start)) |>
    ungroup() |>
    select("participant_id", ".start", ".end", "timezone")

  # Handle special case for observations that occurred either before the first timezone measurement
  # or after the last timezone measurement. We will assume that those belong to the first and last
  # timzone respectively. For the implicit missing first timezone, get the earliest timezone for
  # each participant, set time of this measurement as it's end time, and set start to 0 (1970).
  # We then have timezone intervals spanning from 1970 until the actual first measurement.
  missing_start <- tzs |>
    group_by(.data$participant_id) |>
    dplyr::slice_min(.data$.start, n = 1, with_ties = FALSE, na_rm = TRUE) |>
    mutate(.end = .data$.start, .start = 0)

  # Add the missing first timezones and set the missing end time of each last measurement to a very
  # high number, as SQLite does not support infinite.
  tzs <- tzs |>
    dplyr::rows_append(missing_start) |>
    mutate(.end = ifelse(is.na(.data$.end), 1e11, .data$.end))

  # Copy the results to a new table, so that we do not have to recompute the calculations above
  # for each sensor
  dplyr::copy_to(db, tzs, "temp_tzs", overwrite = TRUE)
  tzs <- dplyr::tbl(db, "temp_tzs")

  # Cleanup after function finishes
  on.exit(DBI::dbExecute(db, "DROP TABLE IF EXISTS temp_tzs"), add = TRUE)

  # Start a progress bar
  if (.progress) {
    pb <- cli::cli_progress_bar(
      "Adding timezones...",
      total = length(sensors),
      clear = FALSE
    )
  }

  for (sensor in sensors) {
    # Do not continue of timezone is already in the table and has no missing values
    if ("timezone" %in% DBI::dbListFields(db, sensor)) {
      n_missing <- dplyr::tbl(db, sensor) |>
        filter(is.na(.data$timezone)) |>
        dplyr::count() |>
        pull("n")

      if (n_missing == 0) {
        next
      }
    }

    # Add a new column for timezone if it does not yet exist
    if (!("timezone" %in% DBI::dbListFields(db, sensor))) {
      DBI::dbExecute(db, paste("ALTER TABLE", sensor, "ADD COLUMN timezone TEXT"))
    }

    # Get the sensor data and the start time of each measurement in seconds
    sensor_data <- dplyr::tbl(db, sensor) |>
      select(-any_of("timezone")) |>
      mutate(start_time = UNIXEPOCH(paste(.data$date, .data$time)))

    # Get the end time of an observation, or set it equal to start time if there is none
    # This ensure that we can use the same type of join for both scenarios
    if ("end_time" %in% colnames(sensor_data)) {
      sensor_data <- sensor_data |>
        mutate(end_time = UNIXEPOCH(.data$end_time)) |>
        mutate(end_time = ifelse(is.na(.data$end_time), .data$start_time, .data$end_time))
    } else {
      sensor_data <- sensor_data |>
        mutate(end_time = .data$start_time)
    }

    # Find out which timezone each measurement belongs to
    updata <- dplyr::left_join(
      x = sensor_data,
      y = tzs,
      by = dplyr::join_by(
        "participant_id",
        overlaps("start_time", "end_time", ".start", ".end")
      )
    )

    # Check if a column matched multiple time zones. If so, take the time zone with the longest
    # overlap
    updata <- updata |>
      mutate(
        overlap_time = pmax(
          0,
          pmin(.data$end_time, .data$.end, na.rm = TRUE) -
            pmax(.data$start_time, .data$.start, na.rm = TRUE),
          na.rm = TRUE
        )
      ) |>
      dplyr::slice_max(.data$overlap_time, by = c("measurement_id", "participant_id")) |>
      select("measurement_id", "timezone")

    # Add the new timezone column to the data
    dplyr::rows_update(
      dplyr::tbl(db, sensor),
      updata,
      by = "measurement_id",
      in_place = TRUE,
      unmatched = "ignore"
    )

    # Update progress bar
    if (.progress) {
      cli::cli_progress_update()
    }
  }

  if (.progress) {
    cli::cli_progress_done()
  }

  invisible(TRUE)
}


#' Convert timestamps to UTC while respecting local timezones
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This functions takes a vector of timestamps and a corresponding vector of time zones and converts
#' it to "localtime" at UTC. If you have a vector of timestamps with varying time zones, you cannot
#' represent them in a single vector with their own time zone. To this end, this function converts
#' each timestamp to its local time, and then force UTC as a time zone for all values.
#'
#' @param x A vector of timestamps, either as [`POSIXct`] or character strings.
#' @param tz  A character vector of timezones corresponding to each element in `x`. If all elements
#'   share the same timezone, a single value can be supplied.
#'
#' @returns A [`POSIXct`] vector in UTC.
#' @export
#'
#' @examples
#' # Single timezone
#' x <- as.POSIXct("2025-05-10 12:00:00", tz = "UTC")
#' with_localtime(x, "Europe/Brussels")
#'
#' # Multiple timezones
#' times <- as.POSIXct(c("2025-05-10 12:00:00", "2025-05-10 12:00:00"), tz = "UTC")
#' tzs <- c("Europe/Brussels", "America/New_York")
#'
#' # Times not appear to be in UTC, but the values are in their local time zone.
#' with_localtime(times, tzs)
with_localtime <- function(x, tz) {
  if (is.character(x)) {
    x <- as.POSIXct(x, tz = "UTC")
  }

  if (!inherits(x, "POSIXt")) {
    cli::cli_abort(c(
      "{.var x} must be a vector of class POSIXt or a character coercible to POSIXt.",
      "x" = "You've supplied a vector of class {.cls {class(x)}}"
    ))
  }

  if (length(unique(tz)) == 1) {
    lubridate::force_tz(lubridate::with_tz(x, tz[1]), "UTC")
  } else {
    purrr::map2_vec(
      x,
      tz,
      \(x, y) lubridate::force_tz(lubridate::with_tz(x, y), "UTC")
    )
  }
}
