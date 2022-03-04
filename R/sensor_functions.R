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
#' @param start_date Optional search window specifying date where to begin search. Must be
#' convertible to date using \link[base]{as.Date}. Use \link[CARP]{first_date} to find the date of
#' the first entry for a participant.
#' @param end_date Optional search window specifying date where to end search. Must be convertible
#' to date using \link[base]{as.Date}. Use \link[CARP]{last_date} to find the date of the last
#' entry for a participant.
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
  if (!DBI::dbIsValid(db)) stop("Database connection is not valid")

  out <- dplyr::tbl(db, sensor)

  if (!is.null(participant_id)) {
    p_id <- as.character(participant_id)
    out <- dplyr::filter(out, participant_id == p_id)
  }

  maybe_date <- function(x) {
    !is.na(as.Date(as.character(x), tz = "UTC", format = "%Y-%m-%d"))
  }

  if (!is.null(start_date) && maybe_date(start_date)) {
    out <- dplyr::filter(out, date >= start_date)
  }

  if (!is.null(end_date) && maybe_date(end_date)) {
    out <- dplyr::filter(out, date <= end_date)
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
  DBI::dbGetQuery(db, query)[1, 1]
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
  DBI::dbGetQuery(db, query)[1, 1]
}


#' Match y to the time scale of x
#'
#' Function for linking mobile sensing and ESM data
#'
#' assumption: both x and y have column "time" containing \link[base]{DateTimeClasses}
#'
#' @param x,y A pair of data frames or data frame extensions (e.g. a tibble). Both \code{x} and
#' \code{y} must have a column called \code{time}.
#' @param by If NULL, the default, \code{*_join()} will perform a natural join, using all variables
#' in common across \code{x} and \code{y}. A message lists the variables so that you can check
#' they're correct; suppress the message by supplying by explicitly.
#'
#' To join by different variables on x and y, use a named vector. For example,
#' \code{by = c("a" = "b")} will match \code{x$a} to \code{y$b}
#'
#' To join by multiple variables, use a vector with length > 1. For example, by = c("a", "b") will
#' match \code{x$a} to \code{y$a} and \code{x$b} to \code{y$b}. Use a named vector to match
#' different variables in x and y. For example, \code{by = c("a" = "b", "c" = "d")} will match
#' \code{x$a} to \code{y$b} and \code{x$c} to \code{y$d}.
#'
#' To perform a cross-join, generating all combinations of \code{x} and \code{y}, use
#' \code{by = character()}.
#' @param offset The time window in which y is to be matched to x. Must be convertible to a period
#' by \link[lubridate]{as.period}.
#'
#' @return A tibble with the data of \code{x} with a new column \code{data} with the matched data
#' of \code{y} according to \code{offset}.
#' @export
link <- function(x, y, by = NULL, offset) {

  if (is.null(x) || !is.data.frame(x)) stop("x must be a data frame")
  if (is.null(y) || !is.data.frame(y)) stop("y must be a data frame")
  if (!is.null(by) && !is.character(by))
    stop("by must be a character vector of variables to join by")
  if (is.null(offset) || !(is.character(offset) | lubridate::is.period(offset) | is.numeric(offset)))
    stop("offset must be a character vector, numeric vector, or a period")

  if (is.character(offset) | is.numeric(offset)) offset <- lubridate::as.period(offset)
  if (is.na(offset)) stop(paste("Invalid offset specified. Try something like '30 minutes' or",
                               "lubridate::minutes(30). Note that negative values do not work when",
                               "specifying character vectors, instead use minutes(-30) or -1800."))

  # Check for time column
  if (!("time" %in% colnames(x) & "time" %in% colnames(y)))
    stop("column 'time' must be present in both x and y")
  if (!lubridate::is.POSIXct(x$time)) stop("column 'time' in x must be a POSIXct")
  if (!lubridate::is.POSIXct(y$time)) stop("column 'time' in y must be a POSIXct")

  # Do not perform matching when x and y are identical
  if (identical(x, y) || isTRUE(dplyr::all_equal(x, y))) stop("x and y are identical")

  # Match sensing data with ESM using a nested join
  # Set a startTime (beep time - offset) and an endTime (beep time)
  res <- x %>%
    dplyr::mutate(startTime = time + offset) %>%
    dplyr::mutate(endTime = time) %>%
    dplyr::nest_join(y, by = by, name = "data") %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::group_by(id)  # Group each row to prevent weird behaviour

  # Then, simply remove all rows in the nested tables that are not within the interval specified
  # by startTime and endTime
  if (offset < 0) {
    res$data <- purrr::pmap(list(res$data, res$startTime, res$endTime),
                            function(data, startTime, endTime) {
                              data[data$time >= startTime & data$time <= endTime, ]
                            })
  } else {
    # Reverse logic if interval occurs after beep
    res$data <- purrr::pmap(list(res$data, res$startTime, res$endTime),
                            function(data, startTime, endTime) {
                              data[data$time >= endTime & data$time <= startTime, ]
                            })
  }

  res <- res %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(id, startTime, endTime))

  res
}

#' Link two sensors OR one sensor and an external data frame
#'
#' This function is specific to CARP databases. It is a wrapper around \link[CARP]{link} but
#' extracts data in the database for you.
#'
#' @param db A database connection to a CARP database.
#' @param sensor_one The name of a primary sensor. See \link[CARP]{sensors} for a list of available
#' sensors.
#' @param sensor_two The name of a secondary sensor. See \link[CARP]{sensors} for a list of
#' available sensors. Cannot be used together with \code{external}.
#' @param offset The time window in which y is to be matched to x. Must be convertible to a period
#'  by \link[lubridate]{as.period}.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[CARP]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#' @param start_date Optional search window specifying date where to begin search. Must be
#' convertible to date using \link[base]{as.Date}. Use \link[CARP]{first_date} to find the date of
#' the first entry for a participant.
#' @param end_date Optional search window specifying date where to end search. Must be convertible
#' to date using \link[base]{as.Date}. Use \link[CARP]{last_date} to find the date of the last
#' entry for a participant.
#' @param external Optionally, specify an external data frame. Cannot be used at the same time as
#' a second sensor. This data frame must have a column called \code{time}.
#' @param reverse Switch \code{sensor_one} with either \code{sensor_two} or \code{external}?
#' Particularly useful in combination with \code{external}.
#' @param ignore_large Safety override to prevent long wait times. Set to \code{TRUE} to do this
#' function on lots of data.
#'
#' @return A tibble with the data of \code{sensor_one} with a new column \code{data} with the
#' matched data of either \code{sensor_two} or \code{external} according to \code{offset}. The
#' other way around when \code{reverse = TRUE}.
#' @export
link2 <- function(db, sensor_one, sensor_two = NULL, offset, participant_id = NULL,
                  start_date = NULL, end_date = NULL, external = NULL, reverse = FALSE,
                  ignore_large = FALSE) {

  if (!DBI::dbIsValid(db)) stop("Database connection is not valid")
  if (is.null(external) & is.null(sensor_two))
    stop("either a second sensor or an external data frame must be supplied")
  if (!is.null(external) & !is.null(sensor_two))
    stop("only a second sensor or an external data frame can be supplied, but not both")
  if (!is.null(external) && !is.data.frame(external)) stop("external must be a data frame")
  if (!is.null(sensor_two) && !is.character(sensor_two))
    stop("sensor_two must be a character vector")

  # See if data is not incredibly large
  if (!ignore_large) {
    n <- sum(get_nrows(db, c(sensor_one, sensor_two), participant_id, start_date, end_date),
             nrow(external))
    if (n > 100000) {
      stop("the total number of rows is higher than 100000. Use ignore_large = TRUE to continue")
    }
  }

  if (!is.null(sensor_two)) {
    dat_two <- get_data(db, sensor_two, participant_id, start_date, end_date) %>%
      dplyr::mutate(time = paste(date, time)) %>%
      dplyr::select(-date) %>%
      dplyr::collect() %>%
      dplyr::mutate(time = as.POSIXct(time, format = "%F %H:%M:%OS"))
  } else {
    dat_two <- external
  }

  # Get dates of dat_two to shrink dat_one as much as possible
  dates <- unique(as.Date(dat_two$time))

  dat_one <- get_data(db, sensor_one, participant_id, start_date, end_date) %>%
    dplyr::filter(date %in% dates) %>%
    dplyr::mutate(time = paste(date, time)) %>%
    dplyr::select(-date) %>%
    dplyr::collect() %>%
    dplyr::mutate(time = as.POSIXct(time, format = "%F %H:%M:%OS"))


  if (is.null(external) & reverse) {
    tmp <- dat_one
    dat_one <- dat_two
    dat_two <- tmp
  } else if (!is.null(external) & !reverse) {
    tmp <- dat_one
    dat_one <- external
    dat_two <- tmp
  }

  link(dat_one, dat_two, "participant_id", offset)

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
  get_data(db, "InstalledApps", participant_id) %>%
    dplyr::filter(!is.na(app)) %>%
    dplyr::distinct(app) %>%
    dplyr::arrange(app) %>%
    dplyr::collect()
}


#' Find the category of an app on the Google Play Store
#'
#' This function scrapes the Google Play Store by using \code{name} as the search term. From there
#' it selects the first result in the list and its corresponding category and package name.
#'
#' @param name The name of the app to search for.
#' @param num Which result should be selected in the list of search results. Defaults to one.
#'
#' @return A list containing the following fields:
#'
#' | | |
#' |-------|-----------|
#'| package | the package name that was selected from the Google Play search|
#'| genre | the corresponding genre of this package|
#'
#' @export
#'
#' @examples
#' app_category("whatsapp")
#'
#' # Example of a generic app name where we can't find a specific app
#' app_category("weather") # Weather forecast channel
#'
#' # Get OnePlus weather
#' app_category("net.oneplus.weather")
app_category <- function(name, num = 1) {
  name <- paste0(strsplit(name, " ")[[1]], collapse = "%20")
  session <- rvest::session(paste0("https://play.google.com/store/search?q=", name, "&c=apps"))
  links <- rvest::html_elements(session, css = ".JC71ub")
  links <- rvest::html_attr(links, "href")
  session <- rvest::session_jump_to(session, links[num])

  genre <- rvest::html_nodes(session, xpath = '//*[@itemprop="genre"]')

  list(package = gsub("^.+?(?<=\\?id=)", "", links[num], perl = TRUE),
       genre = rvest::html_text(genre))

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
#' @param start_date Optional search window specifying date where to begin search. Must be
#' convertible to date using \code{link[base]{as.Date}}. Use \code{\link[CARP]{first_date}} to
#'  find the date of the first entry for a participant.
#' @param end_date Optional search window specifying date where to end search. Must be convertible
#' to date using \code{\link[base]{as.Date}}. Use \code{\link[CARP]{last_date}} to find the date
#' of the last entry for a participant.
#' @param by Either "Total", "Hour", or "Day" indicating how to summarise the results.
#'
#' @importFrom magrittr "%>%"
#'
#' @return A data frame containing a column "App" and a column "Usage" for the hourly app usage.
#' @export
get_app_usage <- function(db, participant_id = NULL,
                          start_date = NULL, end_date = NULL,
                          by = c("Total", "Day", "Hour")) {
  if (!is.null(start_date) & is.null(end_date)) {
    end_date <- start_date
  }
  data <- get_data(db, "AppUsage", participant_id, start_date, end_date) %>%
    dplyr::select(date, time, app, usage) %>%
    dplyr::collect() %>%
    tidyr::drop_na(app, usage) %>%
    dplyr::group_by(date, app)

  if (is.null(by)) {
    data <- data %>%
      dplyr::slice(dplyr::n()) %>%
      dplyr::mutate(usage = usage / 60 / 60)
  } else if (by[1] == "Total" | by[1] == "total") {
    data <- data %>%
    	dplyr::slice(dplyr::n()) %>%
      dplyr::mutate(usage = usage / 60 / 60) %>%
      dplyr::group_by(app) %>%
      dplyr::summarise(usage = round(mean(usage), 2), .groups = "drop")
  } else if (by[1] == "Hour" | by[1] == "hour") {
    data <- data %>%
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
    	dplyr::slice(dplyr::n()) %>%
    	dplyr::mutate(usage = round(usage / 60 / 60, 2))
  } else { # Default case
    data <- data %>%
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
#' @param confidence The minimum confidence (0-100) that should be assigned to an observation by
#' Activity Recognition.
#' @param direction The directionality of the duration calculation, i.e. \eqn{t_{t-1} - t} or
#' \eqn{t - t_{t+1}}.
#' @param start_date Optional search window specifying date where to begin search. Must be
#' convertible to date using \code{link[base]{as.Date}}. Use \code{\link[CARP]{first_date}} to find
#' the date of the first entry for a participant.
#' @param end_date Optional search window specifying date where to end search. Must be convertible
#' to date using \code{\link[base]{as.Date}}. Use \code{\link[CARP]{last_date}} to find the date of
#' the last entry for a participant.
#' @param by Either "Total", "Hour", or "Day" indicating how to summarise the results.
#'
#' @return A data frame containing a column "activity" and a column "duration" for the hourly
#' activity duration.
#' @export
get_activity <- function(db, participant_id = NULL, confidence = 70, direction = "forward",
                         start_date = NULL, end_date = NULL, by = c("Total", "Day", "Hour")) {
  data <- get_data(db, "Activity", participant_id) %>%
    dplyr::filter(confidence >= 70) %>%
    compress_activity() %>%
    dplyr::mutate(datetime = paste(date, time))

  if (tolower(direction) == "forward" | tolower(direction) == "forwards") {
    data <- data %>%
      dplyr::mutate(duration = STRFTIME("%s", dplyr::lead(datetime)) - STRFTIME("%s", datetime))
  } else if (tolower(direction) == "backward" | tolower(direction) == "backwards") {
    data <- data %>%
      dplyr::mutate(duration = STRFTIME("%s", datetime) - STRFTIME("%s", dplyr::lag(datetime)))
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
    dplyr::select(participant_id, device_id:platform) %>%
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
#' @param start_date Optional search window specifying date where to begin search. Must be
#' convertible to date using \link[base]{as.Date}. Use \link[CARP]{first_date} to find the date of
#' the first entry for a participant.
#' @param end_date Optional search window specifying date where to end search. Must be convertible
#' to date using \link[base]{as.Date}. Use \link[CARP]{last_date} to find the date of the last
#' entry for a participant.
#' @param by Either "Hour" or "Day" indicating how to summarise the results. Leave empty to get raw
#' screen duration per measurement.
#'
#' @return A tibble with either "hour" and "duration" columns or "date" and "duration" columns
#' depending on the \code{by} argument. Alternatively, if no \code{by} is specified, a remote
#' tibble is returned with the date, time, and duration since the previous measurement.
#' @export
screen_duration <- function(db,
                            participant_id,
                            start_date = NULL,
                            end_date = NULL,
                            by = c("Hour", "Day")) {

  out <- get_data(db, "Screen", participant_id, start_date, end_date) %>%
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
#' @param start_date Optional search window specifying date where to begin search. Must be
#' convertible to date using \link[base]{as.Date}. Use \link[CARP]{first_date} to find the date of
#' the first entry for a participant.
#' @param end_date Optional search window specifying date where to end search. Must be convertible
#' to date using \link[base]{as.Date}. Use \link[CARP]{last_date} to find the date of the last
#' entry for a participant.
#' @param by Either "Total", "Hour", or "Day" indicating how to summarise the results. Defaults to
#' total.
#'
#' @return In case of grouping is by the total amount, returns a single numeric value. For date and
#' hour grouping returns a tibble with columns "date" or "hour" and the number of screen on's "n".
#' @export
n_screen_on <- function(db,
                        participant_id,
                        start_date = NULL,
                        end_date = NULL,
                        by = c("Total", "Hour", "Day")) {
  out <- get_data(db, "Screen", participant_id, start_date, end_date) %>%
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
#' @param start_date Optional search window specifying date where to begin search. Must be
#' convertible to date using \link[base]{as.Date}. Use \link[CARP]{first_date} to find the date of
#' the first entry for a participant.
#' @param end_date Optional search window specifying date where to end search. Must be convertible
#' to date using \link[base]{as.Date}. Use \link[CARP]{last_date} to find the date of the last
#' entry for a participant.
#' @param by Either "Total", "Hour", or "Day" indicating how to summarise the results. Defaults to
#' total.
#'
#' @return In case of grouping is by the total amount, returns a single numeric value. For date and
#' hour grouping returns a tibble with columns "date" or "hour" and the number of screen unlocks
#' "n".
#' @export
n_screen_unlocks <- function(db,
                             participant_id,
                             start_date = NULL,
                             end_date = NULL,
                             by = c("Total", "Hour", "Day")) {
  out <- get_data(db, "Screen", participant_id, start_date, end_date) %>%
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
#' @param start_date Optional search window specifying date where to begin search. Must be
#' convertible to date using \link[base]{as.Date}. Use \link[CARP]{first_date} to find the date of
#' the first entry for a participant.
#' @param end_date Optional search window specifying date where to end search. Must be convertible
#' to date using \link[base]{as.Date}. Use \link[CARP]{last_date} to find the date of the last
#' entry for a participant.
#'
#' @return A tibble with the "date", "hour", and the number of "steps".
#' @export
step_count <- function(db, participant_id, start_date = NULL, end_date = NULL) {
  get_data(db, "Pedometer", participant_id, start_date, end_date) %>%
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
#' @param ... Unquoted names of columns of the \code{sensor} table to average over.
#' @param n The number of observations to average over.
#' @param start_date Optional search window specifying date where to begin search. Must be
#' convertible to date using \link[base]{as.Date}. Use \link[CARP]{first_date} to find the date of
#' the first entry for a participant.
#' @param end_date Optional search window specifying date where to end search. Must be convertible
#' to date using \link[base]{as.Date}. Use \link[CARP]{last_date} to find the date of the last
#' entry for a participant.
#'
#' @return A tibble with the same columns as the input, modified to be a moving average.
#' @export
#'
#' @examples
#' \dontrun{
#' get_moving_average(db, "Light", "12345", mean_lux, max_lux, n = 5)
#' }
moving_average <- function(db, sensor, participant_id, ..., n, start_date = NULL, end_date = NULL) {
  cols <- rlang::ensyms(...)

  # SELECT
  query <- "SELECT datetime, "

  # Calculate moving average
  avgs <- lapply(cols, function(x) {
    paste0(
      "avg(`",  x, "`) OVER (",
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

  if (!is.null(start_date) & !is.null(end_date)) {
    query <- paste0(query, " AND (`date` BETWEEN '", start_date, "' AND '", end_date, "')")
  }

  # Closing parenthesis
  query <- paste0(query, ")")

  # Get data
  DBI::dbGetQuery(db, query)
}


#' Identify gaps in CARP mobile sensing data
#'
#' Oftentimes in mobile sensing, gaps appear in the data as a result of the participant
#' accidentally closing the app or the operating system killing the app to save power. This can
#' lead to issues later on during data analysis when it becomes unclear whether there are no
#' measurements because no events occurred or because the app quit in that period. For example,
#' if no screen on/off event occur in a 6-hour period, it can either mean the participant did not
#' turn on their phone in that period or that the app simply quit and potential events were missed.
#' In the latter case, the 6-hour missing period has to be compensated by either removing this
#' interval altogether or by subtracting the gap from the interval itself (see examples).
#'
#' While any sensor can be used for identifying gaps, it is best to choose a sensor with a very
#' high, near-continuous sample rate such as the accelerometer or gyroscope. This function then
#' creates time between two subsequent measurements and returns the period in which this time was
#' larger than \code{max_gap}.
#'
#' Note that the \code{from} and \code{to} columns in the output are character vectors in UTC time.
#'
#' @param db A database connection to a CARP database.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[CARP]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#' @param max_gap The maximum time (in seconds) between to subsequent measurements.
#' @param sensor The name of a sensor to use for identifying gaps. See \link[CARP]{sensors} for a list of available sensors.
#'
#' @return A lazy tibble containing the time period of the gaps.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find the gaps for a participant and convert to datetime
#' gaps <- identify_gaps(db, "12345", max_gap = 60) %>%
#'   mutate(across(c(to, from), ymd_hms)) %>%
#'   mutate(across(c(to, from), with_tz, "Europe/Brussels"))
#'
#' # Get some sensor data and calculate a statistic, e.g. the time spent walking
#' # You can also do this with larger intervals, e.g. the time spent walking per hour
#' walking_time <- get_data(db, "Activity", "12345") %>%
#'   collect() %>%
#'   mutate(datetime = ymd_hms(paste(date, time))) %>%
#'   mutate(datetime = with_tz(datetime, "Europe/Brussels")) %>%
#'   arrange(datetime) %>%
#'   mutate(prev_time = lag(datetime)) %>%
#'   mutate(duration = datetime - prev_time) %>%
#'   filter(type == "WALKING")
#'
#' # Find out if a gap occurs in the time intervals
#' walking_time %>%
#'   rowwise() %>%
#'   mutate(gap = any(gaps$from >= prev_time & gaps$to <= datetime))
#' }
identify_gaps <- function(db, participant_id = NULL, max_gap = 60, sensor = "Accelerometer") {
  get_data(db, sensor, participant_id) %>%
    dplyr::mutate(datetime = DATETIME(paste(date, time))) %>%
    dplyr::select(participant_id, datetime) %>%
    dplyr::group_by(participant_id) %>%
    dbplyr::window_order(datetime) %>%
    dplyr::mutate(to = dplyr::lag(datetime)) %>%
    dplyr::mutate(gap = STRFTIME('%s', datetime) - STRFTIME('%s', to)) %>%
    dplyr::filter(gap >= 60) %>%
    dplyr::ungroup() %>%
    dplyr::select(participant_id, from = datetime, to, gap) %>%
    dplyr::collect()
}
