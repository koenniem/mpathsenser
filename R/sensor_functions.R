#' Generic helper function from extracting data from a CARP database
#'
#' This is a generic function to help extract data from a CARP database. For some sensors that
#' require a bit more pre-processing, such as app usage and screen time, more specialised functions
#' are available (e.g. \code{\link[CARP]{get_app_usage}} and \code{\link[CARP]{screen_duration}}).
#'
#' @param db A database connection to a CARP database.
#' @param sensor The name of a sensor. See \link[CARP]{sensors} for a list of available sensors.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[CARP]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#' @param start_date Optional search window specifying date where to begin search. Must be convertible to date using \link[base]{as.Date}. Use \link[CARP]{first_date} to find the date of the first entry for a participant.
#' @param end_date Optional search window specifying date where to end search. Must be convertible to date using \link[base]{as.Date}. Use \link[CARP]{last_date} to find the date of the last entry for a participant.
#'
#' @return A lazy \code{\link[dplyr]{tbl}} containing the requested data.
#' @export
#'
#' @examples
#' \dontrun{
#' # Open a database
#' db <- open_db()
#'
#' # Retrieve some data
#' get_data(db, "Accelerometer", "12345")
#'
#' # Or within a specific window
#' get_data(db, "Accelerometer", "12345", "2021-01-01", "2021-01-05")
#' }
get_data <- function(db, sensor, participant_id = NULL, start_date = NULL, end_date = NULL) {
  if (!RSQLite::dbIsValid(db)) stop("Database connection is not valid")

  out <- dplyr::tbl(db, sensor)

  if (!is.null(participant_id)) {
    p_id <- as.character(participant_id)
    out <- dplyr::filter(out, participant_id == p_id)
  }

  if (!is.null(start_date) && !is.null(end_date)) {
    start.date <- methods::is(as.Date(start_date), "Date")
    end.date <- methods::is(as.Date(end_date), "Date")
    if (start.date & end.date) {
      # out <- dplyr::mutate(out, date = DATE(time)) %>%
      out <- dplyr::filter(out, date >= start_date)
      out <- dplyr::filter(out, date <= end_date)
    }
  }

  out
}

#' Extract the date of the first entry
#'
#' A helper function for extracting the first date of entry of (of one or all participant) of one
#' sensor. Note that this function is specific to the first date of a sensor. After all, it
#' wouldn't make sense to extract the first date for a participant of the accelerometer, while the
#' first device measurement occurred a day later.
#'
#' @param db A database connection to a CARP database.
#' @param sensor The name of a sensor. See \link[CARP]{sensors} for a list of available sensors.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[CARP]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#'
#' @return A string in the format "YYYY-mm-dd" of the first entry date.
#' @export
#'
#' @examples
#' \dontrun{
#' db <- open_db()
#' first_date(db, "Accelerometer", "12345")
#' }
first_date <- function(db, sensor, participant_id = NULL) {
  query <- paste0("SELECT MIN(date) AS `min` FROM `", sensor, "`")

  if (!is.null(participant_id)) {
    query <- paste0(query, " WHERE (`participant_id` = '", participant_id, "')")
  }
  RSQLite::dbGetQuery(db, query)[1, 1]
}

#' Extract the date of the last entry
#'
#' A helper function for extracting the last date of entry of (of one or all participant) of one
#' sensor. Note that this function is specific to the last date of a sensor. After all, it
#' wouldn't make sense to extract the last date for a participant of the device info, while the
#' last accelerometer measurement occurred a day later..
#'
#' @param db A database connection to a CARP database.
#' @param sensor The name of a sensor. See \link[CARP]{sensors} for a list of available sensors.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[CARP]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#'
#' @return A string in the format "YYYY-mm-dd" of the last entry date.
#' @export
#'
#' @examples
#' \dontrun{
#' db <- open_db()
#' first_date(db, "Accelerometer", "12345")
#' }
last_date <- function(db, sensor, participant_id = NULL) {
  query <- paste0("SELECT MAX(date) AS `max` FROM `", sensor, "`")

  if (!is.null(participant_id)) {
    query <- paste0(query, " WHERE (`participant_id` = '", participant_id, "')")
  }
  RSQLite::dbGetQuery(db, query)[1, 1]
}



# Function for linking mobile sensing and ESM data
link <- function(x, y, by, offset) {

  if(is.null(x) || !is.data.frame(x)) stop("x must be a data frame")
  if(is.null(y) || !is.data.frame(x)) stop("y must be a data frame")

  if(is.expression(by)) by <- enquote(by)

  # Match sensing data with ESM using a nested join
  # Set a startTime (beep time - offset) and an endTime (beep time)
  dat <- x %>%
    # filter(participant_id == p_id) %>%
    mutate(startTime = time + offset) %>%
    select(participant_id, startTime, endTime = time) %>%
    nest_join(sensing_dat, by = "participant_id", name = "data") %>%
    group_by(participant_id, startTime, endTime) # Group each row to prevent weird behaviour

  # Then, simply remove all rows in the nested tables that are not within the interval specified
  # by startTime and endTime
  if(offset < 0) {
    res <- dat %>%
      mutate(data = map(data, ~filter(., time >= startTime & time <= endTime)))
  } else {
    # Reverse logic if interval occurs after beep
    res <- dat %>%
      mutate(data = map(data, ~filter(., time >= endTime & time <= startTime)))
  }

  res
}

link2 <- function(db, sensor, participant_id, start_date, end_date) {
  # Retrieve mobile sensing data from database
  # sensing_dat <- get_data(db, sensor, participant_id) %>%
  #   collect() %>%
  #   mutate(time = ymd_hms(paste(date, time)))

  # copy participant_id since it has the same name as a column
  # p_id <- participant_id
}

#' Get installed apps
#'
#' Extract installed apps for one or all participants. Contrarily to other get_* functions in
#' this package, start and end dates are not used since installed apps are assumed to be fixed
#' throughout the study.
#'
#' @param db A database connection to a CARP database.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[CARP]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#'
#' @return A character vector of app names.
#' @export
get_installed_apps <- function(db, participant_id = NULL) {
  data <- get_data(db, "InstalledApps", participant_id)
  data <- dplyr::collect(data)
  apps <- paste0(data$apps, collapse = "|")
  apps <- strsplit(apps, "|", fixed = TRUE)
  apps <- unique(unlist(apps))
  apps <- sort(apps)
  apps
}


#' Get app usage per hour
#'
#' This function extracts app usage per hour for either one or multiple participants. If multiple
#' days are selected, the app usage time is averaged.
#'
#' @param db A database connection to a CARP database.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[CARP]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#' @param startDate Optional search window specifying date where to begin search. Must be convertible to date using \code{link[base]{as.Date}}. Use \code{\link[CARP]{first_date}} to find the date of the first entry for a participant.
#' @param endDate Optional search window specifying date where to end search. Must be convertible to date using \code{\link[base]{as.Date}}. Use \code{\link[CARP]{last_date}} to find the date of the last entry for a participant.
#' @param by Either "Total", "Hour", or "Day" indicating how to summarise the results.
#'
#' @importFrom magrittr "%>%"
#'
#' @return A data frame containing a column "App" and a column "Usage" for the hourly app usage.
#' @export
get_app_usage <- function(db, participant_id = NULL,
                          startDate = NULL, endDate = NULL,
                          by = c("Total", "Day", "Hour")) {
  if (!is.null(startDate) & is.null(endDate)) {
    endDate <- startDate
  }
  data <- get_data(db, "AppUsage", participant_id, startDate, endDate) %>%
    dplyr::select(date, time, app, usage) %>%
    dplyr::collect() %>%
    tidyr::drop_na(app, usage)

  if (is.null(by)) {
    data <- data %>%
      dplyr::group_by(date, app) %>%
      dplyr::slice(dplyr::n()) %>%
      dplyr::mutate(usage = usage / 60 / 60)
  } else if (by[1] == "Total" | by[1] == "total") {
    data <- data %>%
    	dplyr::group_by(date, app) %>%
    	dplyr::slice(dplyr::n()) %>%
      dplyr::mutate(usage = usage / 60 / 60) %>%
      dplyr::group_by(app) %>%
      dplyr::summarise(usage = round(mean(usage), 2), .groups = "drop")
  } else if (by[1] == "Hour" | by[1] == "hour") {
    data <- data %>%
    	dplyr::group_by(date, app) %>%
    	dplyr::mutate(prev_usage = dplyr::lag(usage, default = 0)) %>%
    	dplyr::mutate(hour = substr(time, 1, 2)) %>%
    	dplyr::group_by(date, app) %>%
    	dplyr::mutate(duration = usage - prev_usage) %>%
    	dplyr::group_by(hour, date, app) %>%
    	dplyr::summarise(usage = usage / 60 / 60, .groups = "drop") %>%
      dplyr::mutate(hour = as.numeric(hour)) %>%
      tidyr::complete(hour = 0:23, app, fill = list(n = 0))
  } else if (by[1] == "Day" | by[1] == "day") {
    data <- data %>%
    	dplyr::group_by(date, app) %>%
    	dplyr::slice(dplyr::n()) %>%
    	dplyr::mutate(usage = round(usage / 60 / 60, 2))
  } else { # Default case
    data <- data %>%
    	dplyr::group_by(date, app) %>%
    	dplyr::slice(dplyr::n()) %>%
    	dplyr::mutate(usage = usage / 60 / 60)
  }
  return(data)

  colnames(data) <- c("App", "Usage")
  data
}

#' Get a summary of physical activity (recognition)
#'
#' @param db A database connection to a CARP database.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[CARP]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#' @param confidence
#' @param direction
#' @param startDate
#' @param endDate
#' @param by
#'
#' @return
#' @export
#'
#' @examples
get_activity <- function(db, participant_id = NULL, confidence = 70, direction = "forward",
                         startDate = NULL, endDate = NULL, by = c("Total", "Day", "Hour")) {
  data <- get_data(db, "Activity", participant_id) %>%
    dplyr::filter(confidence >= 70) %>%
    compress_activity() %>%
    dplyr::mutate(datetime = paste(date, time))

  if (tolower(direction) == "forward" | tolower(direction) == "forwards") {
    data <- data %>%
      dplyr::mutate(duration = STRFTIME('%s', dplyr::lead(datetime)) - STRFTIME('%s', datetime))
  } else if (tolower(direction) == "backward" | tolower(direction) == "backwards") {
    data <- data %>%
      dplyr::mutate(duration = STRFTIME('%s', datetime) - STRFTIME('%s', dplyr::lag(datetime)))
  } else {
    stop("Invalid direction")
  }

  if (is.null(by) || missing(by) || by[1] == "total" | by[1] == "Total") {
    data <- data %>%
    dplyr::group_by(type)
  } else if (by[1] == "Hour") {
    data <- data %>%
      dplyr::mutate(hour = substr(time, 1, 2)) %>%
      dplyr::group_by(type, date, hour)
  } else if (by[1] == "Day") {
    data <- data %>%
      dplyr::group_by(type, date)
  } else { # Default case
    data <- data %>%
      dplyr::group_by(type)
  }

  data %>%
    dplyr::summarise(duration = sum(duration, na.rm = TRUE), .groups = "drop") %>%
    dplyr::collect()
}

#' Get the device info for one or more participants
#'
#' @param db A database connection to a CARP database.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[CARP]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#'
#' @return A data frame containing device info for each participant
#' @export
device_info <- function(db, participant_id = NULL) {
  get_data(db, "Device", participant_id = participant_id) %>%
    dplyr::select(participant_id, device_id:operating_system) %>%
    dplyr::distinct() %>%
    dplyr::collect()
}

compress_activity <- function(data, direction = "forward") {
  data %>%
    dbplyr::window_order(date, time) %>%
    dplyr::filter(!(dplyr::lead(type) == type && dplyr::lag(type) == type))
}

#' Screen duration by hour or day
#'
#' Calculate the screen duration time where the screen was _unlocked_ (i.e. not just on).
#'
#' @param db A database connection to a CARP database.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[CARP]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#' @param startDate Optional search window specifying date where to begin search. Must be convertible to date using \link[base]{as.Date}. Use \link[CARP]{first_date} to find the date of the first entry for a participant.
#' @param endDate Optional search window specifying date where to end search. Must be convertible to date using \link[base]{as.Date}. Use \link[CARP]{last_date} to find the date of the last entry for a participant.
#' @param by Either "Hour" or "Day" indicating how to summarise the results. Leave empty to get raw screen duration per measurement.
#'
#' @return A tibble with either "hour" and "duration" columns or "date" and "duration" columns depending on the \code{by} argument. Alternatively, if no \code{by} is specified, a remote tibble is returned with the date, time, and duration since the previous measurement.
#' @export
screen_duration <- function(db, participant_id, startDate = NULL, endDate = NULL, by = c("Hour", "Day")) {
  out <- get_data(db, "Screen", participant_id, startDate, endDate) %>%
    dplyr::filter(screen_event != "SCREEN_ON") %>%
    dplyr::mutate(datetime = paste(date, time)) %>%
    dplyr::select(-c(measurement_id, participant_id)) %>%
    dplyr::mutate(next_event = dplyr::lead(screen_event, n = 1)) %>%
    dplyr::mutate(next_time = dplyr::lead(datetime, n = 1)) %>%
    dplyr::filter(screen_event == "SCREEN_UNLOCKED" & next_event == "SCREEN_OFF") %>%
    dplyr::mutate(duration = strftime("%s", next_time) - strftime("%s", datetime))

  if (is.null(by) || missing(by)) {
    out <- out %>%
      dplyr::select(date, time, duration)
  } else if (by[1] == "Hour") {
    out <- out %>%
      dplyr::mutate(hour = strftime("%H", time)) %>%
      dplyr::group_by(hour) %>%
      dplyr::summarise(duration = mean(duration, na.rm = T) / 60) %>%
      dplyr::collect() %>%
      dplyr::mutate(hour = as.numeric(hour)) %>%
      tidyr::complete(hour = 0:23, fill = list(duration = 0))
  } else if (by[1] == "Day") {
    out <- out %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(duration = sum(duration, na.rm = T) / 60 / 60) %>%
      dplyr::collect()
  } else { # Default case
    out <- out %>%
      dplyr::select(date, time, duration)
  }
  return(out)
}

#' Get number of times screen turned on
#'
#' @param db A database connection to a CARP database.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[CARP]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#' @param startDate Optional search window specifying date where to begin search. Must be convertible to date using \link[base]{as.Date}. Use \link[CARP]{first_date} to find the date of the first entry for a participant.
#' @param endDate Optional search window specifying date where to end search. Must be convertible to date using \link[base]{as.Date}. Use \link[CARP]{last_date} to find the date of the last entry for a participant.
#' @param by Either "Total", "Hour", or "Day" indicating how to summarise the results. Defaults to total.
#'
#' @return In case of grouping is by the total amount, returns a single numeric value. For date and hour grouping returns a tibble with columns "date" or "hour" and the number of screen on's "n".
#' @export
n_screen_on <- function(db, participant_id, startDate = NULL, endDate = NULL, by = c("Total", "Hour", "Day")) {
  out <- get_data(db, "Screen", participant_id, startDate, endDate) %>%
    dplyr::select(-c(measurement_id, participant_id)) %>%
    dplyr::filter(screen_event == "SCREEN_ON")

  if (is.null(by)) {
    out <- out %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::pull(n)
  } else if (by[1] == "Total" | by[1] == "total") {
    out <- out %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::pull(n)
  } else if (by[1] == "Hour" | by[1] == "hour") {
    out <- out %>%
      dplyr::mutate(hour = STRFTIME("%H", time)) %>%
      dplyr::count(hour) %>%
      dplyr::collect() %>%
      dplyr::mutate(hour = as.numeric(hour)) %>%
      tidyr::complete(hour = 0:23, fill = list(n = 0))
  } else if (by[1] == "Day" | by[1] == "day") {
    out <- out %>%
      dplyr::count(date) %>%
      dplyr::collect()
  } else { # Default case
    out <- out %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::pull(n)
  }
  return(out)
}

#' Get number of screen unlocks
#'
#' @param db A database connection to a CARP database.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[CARP]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#' @param startDate Optional search window specifying date where to begin search. Must be convertible to date using \link[base]{as.Date}. Use \link[CARP]{first_date} to find the date of the first entry for a participant.
#' @param endDate Optional search window specifying date where to end search. Must be convertible to date using \link[base]{as.Date}. Use \link[CARP]{last_date} to find the date of the last entry for a participant.
#' @param by Either "Total", "Hour", or "Day" indicating how to summarise the results. Defaults to total.
#'
#' @return In case of grouping is by the total amount, returns a single numeric value. For date and hour grouping returns a tibble with columns "date" or "hour" and the number of screen unlocks "n".
#' @export
n_screen_unlocks <- function(db, participant_id, startDate = NULL, endDate = NULL, by = c("Total", "Hour", "Day")) {
  out <- get_data(db, "Screen", participant_id, startDate, endDate) %>%
    dplyr::select(-c(measurement_id, participant_id)) %>%
    dplyr::filter(screen_event == "SCREEN_UNLOCKED")

  if (is.null(by)) {
    out <- out %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::pull(n)
  } else if (by[1] == "Total" | by[1] == "total") {
    out <- out %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::pull(n)
  } else if (by[1] == "Hour" | by[1] == "hour") {
    out <- out %>%
      dplyr::mutate(hour = STRFTIME("%H", time)) %>%
      dplyr::count(hour) %>%
      dplyr::collect() %>%
      dplyr::mutate(hour = as.numeric(hour)) %>%
      tidyr::complete(hour = 0:23, fill = list(n = 0))
  } else if (by[1] == "Day" | by[1] == "day") {
    out <- out %>%
      dplyr::count(date) %>%
      dplyr::collect()
  } else { # Default case
    out <- out %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::pull(n)
  }
  return(out)
}


#' Get step count
#'
#' Extracts the number of steps per hour as sensed by the underlying operating system.
#'
#' @param db A database connection to a CARP database.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[CARP]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#' @param startDate Optional search window specifying date where to begin search. Must be convertible to date using \link[base]{as.Date}. Use \link[CARP]{first_date} to find the date of the first entry for a participant.
#' @param endDate Optional search window specifying date where to end search. Must be convertible to date using \link[base]{as.Date}. Use \link[CARP]{last_date} to find the date of the last entry for a participant.
#'
#' @return A tibble with the "date", "hour", and the number of "steps".
#' @export
step_count <- function(db, participant_id, startDate = NULL, endDate = NULL) {
  get_data(db, "Pedometer", participant_id, startDate, endDate) %>%
    dplyr::mutate(hour = STRFTIME("%H", time)) %>%
    dplyr::arrange(date, time) %>%
    dplyr::mutate(next_count = dplyr::lead(step_count, default = NA)) %>%
    dplyr::mutate(step_count = ifelse(step_count > next_count, 0, step_count)) %>%
    dplyr::mutate(steps = next_count - step_count) %>%
    dplyr::group_by(date, hour) %>%
    dplyr::summarise(steps = sum(steps, na.rm = TRUE), .groups = "drop") %>%
    dplyr::collect()
}


#' Moving average for values in CARP DB
#'
#' @param db A database connection to a CARP database.
#' @param sensor The name of a sensor. See \link[CARP]{sensors} for a list of available sensors.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[CARP]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#' @param ... Unquoted names of columns of the \code{sensor} table.
#' @param n The number of observations to average over.
#' @param startDate Optional search window specifying date where to begin search. Must be convertible to date using \link[base]{as.Date}. Use \link[CARP]{first_date} to find the date of the first entry for a participant.
#' @param endDate Optional search window specifying date where to end search. Must be convertible to date using \link[base]{as.Date}. Use \link[CARP]{last_date} to find the date of the last entry for a participant.
#'
#' @return A tibble with the same columns as the input, modified to be a moving average.
#' @export
#'
#' @examples
#' \dontrun{
#' get_moving_average(db, "Light", "27624", mean_lux, max_lux, n = 5)
#' }
moving_average <- function(db, sensor, participant_id, ..., n,
                               startDate = NULL, endDate = NULL) {
  cols <- dplyr::ensyms(...)

  # SELECT
  query <- "SELECT datetime, "

  # Calculate moving average
  avgs <- lapply(cols, function(x) {
    paste0(
      "avg(`",  x,"`) OVER (",
      "ORDER BY CAST (strftime('%s', datetime) AS INT) ",
      "RANGE BETWEEN ", n / 2, " PRECEDING ",
      "AND ", n / 2, " FOLLOWING",
      ") AS ", x
    )
  })

  avgs <- paste0(avgs, collapse = ", ")
  query <- paste0(query, avgs)

  # FROM
  query <- paste0(query, " FROM (SELECT `date` || 'T' || `time` AS `datetime`, ",
                  paste0(cols, collapse = ", "),
                  " FROM ", sensor)

  # Where
  query <- paste0(query, " WHERE (`participant_id` = '", participant_id, "')")

  if(!is.null(startDate) & !is.null(endDate)) {
    query <- paste0(query, " AND (`date` BETWEEN '", startDate, "' AND '", endDate,"')")
  }

  # Closing parenthesis
  query <- paste0(query, ")")

  # Get data
  RSQLite::dbGetQuery(db, query)
}

decrypt_gps <- function(data, key) {
  if(!is.raw(key)) {
    key <- sodium::hex2bin(key)
  }

  data %>%
    dplyr::collect() %>%
    dplyr::mutate(
      dplyr::across(c(latitude, longitude), ~ {
        lapply(.x, sodium::hex2bin) %>%
          lapply(sodium::simple_decrypt, key) %>%
          lapply(rawToChar) %>%
          unlist
      }))
}
