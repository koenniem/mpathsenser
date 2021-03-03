#' Generic helper function from extracting data from a CARP database
#'
#' This is a generic funcation to help extract data from a CARP database. For some sensors that
#' require a bit more pre-processing, such as app usage and screen time, more specialised functions
#' are available (e.g. \code{\link[CARP]{get_app_usage}} and \code{\link[CARP]{screen_duration}}).
#'
#' @param db A database connection to a CARP database.
#' @param sensor The name of a sensor. See \link[CARP]{sensors} for a list of available sensors.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[CARP]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#' @param startDate Optional search window specifying date where to begin search. Must be convertible to date using \link[base]{as.Date}. Use \link[CARP]{first_date} to find the date of the first entry for a participant.
#' @param endDate Optional search window specifying date where to end search. Must be convertible to date using \link[base]{as.Date}. Use \link[CARP]{last_date} to find the date of the last entry for a participant.
#'
#' @return A lazy \code{\link[dplyr]{tbl}} containg the requested data.
#' @export
#'
#' @examples
#' # Open a database
#' db <- open_db()
#'
#' # Retrieve some data
#' get_data(db, "Accelerometer", "12345")
#'
#' # Or within a specific window
#' get_data(db, "Accelerometer", "12345", "2021-01-01", "2021-01-05")
get_data <- function(db, sensor, participant_id = NULL, startDate = NULL, endDate = NULL) {
  out <- dplyr::tbl(db, sensor)

  if (!is.null(participant_id)) {
    p_id <- as.character(participant_id)
    out <- dplyr::filter(out, participant_id == p_id)
  }

  if (!is.null(startDate) && !is.null(endDate)) {
    start.date <- is(as.Date(startDate), "Date")
    end.date <- is(as.Date(endDate), "Date")
    if (start.date & end.date) {
      # out <- dplyr::mutate(out, date = DATE(time)) %>%
      out <- dplyr::filter(out, date >= startDate)
      out <- dplyr::filter(out, date <= endDate)
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
#' db <- open_db()
#' first_date(db, "Accelerometer", "12345")
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
#' db <- open_db()
#' first_date(db, "Accelerometer", "12345")
last_date <- function(db, sensor, participant_id = NULL) {
  query <- paste0("SELECT MAX(date) AS `max` FROM `", sensor, "`")

  if (!is.null(participant_id)) {
    query <- paste0(query, " WHERE (`participant_id` = '", participant_id, "')")
  }
  RSQLite::dbGetQuery(db, query)[1, 1]
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
#'
#' @return A data frame containing a column "App" and a column "Usage" for the hourly app usage.
#' @export
#'
#' @importFrom magrittr "%>%"
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

  } else if (by[1] == "Total" | by[1] == "total") {
    data <- data %>%
    	dplyr::group_by(date, app) %>%
    	dplyr::slice(n()) %>%
      dplyr::mutate(usage = usage / 60 / 60) %>%
      dplyr::group_by(app) %>%
      dplyr::summarise(usage = round(mean(usage), 2), .groups = "drop")
  } else if (by[1] == "Hour" | by[1] == "hour") {
    data <- data %>%
    	dplyr::group_by(date, app) %>%
    	dplyr::mutate(prev_usage = lag(usage, default = 0)) %>%
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
    	dplyr::slice(n()) %>%
    	dplyr::mutate(usage = round(usage / 60 / 60, 2))
  } else { # Default case
    data <- data %>%
    	dplyr::group_by(date, app) %>%
    	dplyr::slice(n()) %>%
    	dplyr::mutate(usage = usage / 60 / 60)
  }
  return(data)

  colnames(data) <- c("App", "Usage")
  data
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
      dplyr::summarise(n = n()) %>%
      dplyr::pull(n)
  } else if (by[1] == "Total" | by[1] == "total") {
    out <- out %>%
      dplyr::summarise(n = n()) %>%
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
      dplyr::summarise(n = n()) %>%
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
      dplyr::summarise(n = n()) %>%
      dplyr::pull(n)
  } else if (by[1] == "Total" | by[1] == "total") {
    out <- out %>%
      dplyr::summarise(n = n()) %>%
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
      dplyr::summarise(n = n()) %>%
      dplyr::pull(n)
  }
  return(out)
}
