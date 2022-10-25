#' Generic helper function from extracting data from an m-Path Sense database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This is a generic function to help extract data from an m-Path sense database. For some sensors
#' that require a bit more pre-processing, such as app usage and screen time, more specialised
#' functions are available (e.g. \code{\link[mpathsenser]{get_app_usage}} and
#' \code{\link[mpathsenser]{screen_duration}}).
#'
#' @param db A database connection to an m-Path Sense database.
#' @param sensor The name of a sensor. See \link[mpathsenser]{sensors} for a list of available
#' sensors.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[mpathsenser]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#' @param start_date Optional search window specifying date where to begin search. Must be
#' convertible to date using \link[base]{as.Date}. Use \link[mpathsenser]{first_date} to find the
#' date of the first entry for a participant.
#' @param end_date Optional search window specifying date where to end search. Must be convertible
#' to date using \link[base]{as.Date}. Use \link[mpathsenser]{last_date} to find the date of the
#' last entry for a participant.
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
  if (!dbIsValid(db)) {
    abort("Database connection is not valid")
  }
  if (!is.character(sensor) | length(sensor) == 0 | length(sensor) > 1) {
    abort("sensor should be a character vector of size 1")
  }
  if (!(tolower(sensor) %in% tolower(sensors))) {
    msg <- sprintf("Sensor '%s' does not exist.", sensor)
    abort(c(msg,
            i = "See `mpathsenser::sensors` for the full list of available sensors."))
  }

  out <- dplyr::tbl(db, sensor)

  if (!is.null(participant_id)) {
    p_id <- as.character(participant_id)
    out <- filter(out, participant_id == p_id)
  }

  maybe_date <- function(x) {
    !is.na(as.Date(as.character(x), tz = "UTC", format = "%Y-%m-%d"))
  }

  if (!is.null(start_date) && maybe_date(start_date)) {
    out <- filter(out, date >= start_date)
  }

  if (!is.null(end_date) && maybe_date(end_date)) {
    out <- filter(out, date <= end_date)
  }

  out
}

#' Extract the date of the first entry
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' A helper function for extracting the first date of entry of (of one or all participant) of one
#' sensor. Note that this function is specific to the first date of a sensor. After all, it
#' wouldn't make sense to extract the first date for a participant of the accelerometer, while the
#' first device measurement occurred a day later.
#'
#' @inheritParams get_data
#'
#' @return A string in the format 'YYYY-mm-dd' of the first entry date.
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
#' @description
#' `r lifecycle::badge("stable")`
#'
#' A helper function for extracting the last date of entry of (of one or all participant) of one
#' sensor. Note that this function is specific to the last date of a sensor. After all, it
#' wouldn't make sense to extract the last date for a participant of the device info, while the
#' last accelerometer measurement occurred a day later.
#'
#' @inheritParams get_data
#'
#' @return A string in the format 'YYYY-mm-dd' of the last entry date.
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

#' Get installed apps
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Extract installed apps for one or all participants. Contrarily to other get_* functions in
#' this package, start and end dates are not used since installed apps are assumed to be fixed
#' throughout the study.
#'
#' @param db A database connection to a mpathsenser database.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[mpathsenser]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#'
#' @return A tibble containing app names.
#' @export
get_installed_apps <- function(db, participant_id = NULL) {
  get_data(db, "InstalledApps", participant_id) %>%
    filter(!is.na(.data$app)) %>%
    distinct(.data$app) %>%
    arrange(.data$app) %>%
    collect()
}


#' Find the category of an app on the Google Play Store
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function scrapes the Google Play Store by using \code{name} as the search term. From there
#' it selects the first result in the list and its corresponding category and package name.
#'
#' @param name The name of the app to search for.
#' @param num Which result should be selected in the list of search results. Defaults to one.
#' @param rate_limit The time interval to keep between queries, in seconds. If the rate limit is too
#' low, the Google Play Store may reject further requests or even ban your entirely.
#' @param exact In m-Path Sense, the app names of the AppUsage sensor are the last part of the app's
#' package names. When \code{exact}  is \code{TRUE}, the function guarantees that \code{name} is
#' exactly equal to the last part of the selected package from the search results. Note that when
#' \code{exact} is \code{TRUE}, it interacts with \code{num} in the sense that it no longer selects
#' the top search result but instead the top search result that matches the last part of the package
#' name.
#'
#' @section Warning:
#' Do not abuse this function or you will be banned by the Google Play Store. The minimum delay
#' between requests seems to be around 5 seconds, but this is untested. Also make sure not to do
#' batch lookups, as many subsequent requests will get you blocked as well.
#'
#' @return A list containing the following fields:
#'
#' \tabular{ll}{
#'   package \tab the package name that was selected from the Google Play search \cr
#'   genre   \tab the corresponding genre of this package
#' }
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
app_category <- function(name, num = 1, rate_limit = 5, exact = TRUE) {
  # Check if required packages are available
  if (!requireNamespace("curl", quietly = TRUE)) {
    abort(c(
      "package curl is needed for this function to work. ",
      i = "Please install it using install.packages(\"curl\")"
    ))
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    abort(c(
      "package httr is needed for this function to work. ",
      i = "Please install it using install.packages(\"httr\")"
    ))
  }
  if (!requireNamespace("rvest", quietly = TRUE)) {
    abort(c(
      "package rvest is needed for this function to work. ",
      i = "Please install it using install.packages(\"rvest\")"
    ))
  }

  res <- data.frame(app = name, package = rep(NA, length(name)), genre = rep(NA, length(name)))

  if (requireNamespace("progressr", quietly = TRUE)) {
    p <- progressr::progressor(steps = length(name))
  }

  for (i in seq_along(name)) {
    res[i, 2:3] <- app_category_impl(name[i], num, exact)

    if (requireNamespace("progressr", quietly = TRUE)) {
      p()
    }

    if (length(name) > 1) {
      Sys.sleep(rate_limit)
    }
  }

  res
}

app_category_impl <- function(name, num, exact) {
  # Replace illegal characters
  name <- iconv(name, from = "UTF-8", to = "ASCII//TRANSLIT")
  name <- gsub("[^[:alnum:] .@]", " ", name, perl = TRUE)
  name <- gsub(" ", "%20", name)

  query <- paste0("https://play.google.com/store/search?q=", name, "&c=apps")

  ua <- httr::user_agent("Mozilla/5.0 (Windows NT 10.0; WOW64; rv:70.0) Gecko/20100101 Firefox/70.0")

  session <- httr::GET(query, ua)

  if (!httr::http_error(session)) {
    session <- httr::content(session)
  } else {
    return(list(package = NA, genre = NA))
  }

  # Get the link
  links <- session %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href") %>%
    purrr::keep(~ grepl("^\\/store\\/apps\\/details\\?id=.*$", .x))

  if (length(links) == 0) {
    return(list(package = NA, genre = NA))
  }

  # Check if the name occurs in any of the package names
  # If so, select the num (usually first) link from this list
  if (exact) {
    name_detected <- vapply(links,
                            function(x) grepl(paste0("\\.", name, "$/i"), x),
                            FUN.VALUE = logical(1))
    if (any(name_detected)) {
      links <- links[name_detected]
      link <- links[num]
    } else {
      link <- links[num]
    }
  } else {
    link <- links[num]
  }


  if (is.na(link)) {
    return(list(package = NA, genre = NA))
  }

  if (!grepl("^https://play.google.com", link)) {
    link <- paste0("https://play.google.com", link)
  }

  session <- httr::GET(link, ua)

  if (!httr::http_error(session)) {
    session <- httr::content(session)
  } else {
    return(list(package = NA, genre = NA))
  }

  # Extract the genre and return results
  genre <- session %>%
    rvest::html_element(xpath = ".//script[contains(., 'applicationCategory')]") %>%
    rvest::html_text() %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("applicationCategory")
  list(package = gsub("^.+?(?<=\\?id=)", "", link, perl = TRUE), genre = genre)
}


#' Get app usage per hour
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function extracts app usage per hour for either one or multiple participants. If multiple
#' days are selected, the app usage time is averaged.
#'
#' @inheritParams get_data
#' @param by Either 'Total', 'Hour', or 'Day' indicating how to summarise the results.
#'
#' @return A data frame containing a column 'app' and a column 'usage' for the hourly app usage.
#' @export
get_app_usage <- function(db,
                          participant_id = NULL,
                          start_date = NULL,
                          end_date = NULL,
                          by = c("Total", "Day", "Hour")) {
  if (!is.null(start_date) & is.null(end_date)) {
    end_date <- start_date
  }
  data <- get_data(db, "AppUsage", participant_id, start_date, end_date) %>%
    select("date", "time", "app", "usage") %>%
    collect() %>%
    drop_na("app", "usage") %>%
    group_by(.data$date, .data$app)

  if (is.null(by)) {
    data <- data %>%
      slice(n()) %>%
      mutate(usage = .data$usage / 60 / 60)
  } else if (by[1] == "Total" | by[1] == "total") {
    data <- data %>%
      slice(n()) %>%
      mutate(usage = .data$usage / 60 / 60) %>%
      group_by(.data$app) %>%
      summarise(usage = round(mean(.data$usage), 2), .groups = "drop")
  } else if (by[1] == "Hour" | by[1] == "hour") {
    data <- data %>%
      mutate(prev_usage = lag(.data$usage, default = 0)) %>%
      mutate(hour = substr(.data$time, 1, 2)) %>%
      group_by(.data$date, .data$app) %>%
      mutate(duration = .data$usage - .data$prev_usage) %>%
      group_by(.data$hour, .data$date, .data$app) %>%
      summarise(usage = .data$usage / 60 / 60, .groups = "drop") %>%
      mutate(hour = as.numeric(.data$hour)) %>%
      complete(hour = 0:23, .data$app, fill = list(n = 0))
  } else if (by[1] == "Day" | by[1] == "day") {
    data <- data %>%
      slice(n()) %>%
      mutate(usage = round(.data$usage / 60 / 60, 2))
  } else {
    # Default case
    data <- data %>%
      slice(n()) %>%
      mutate(usage = .data$usage / 60 / 60)
  }
  return(data)
}

#' Get a summary of physical activity (recognition)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams get_data
#' @param data A data frame containing the activity data. See \link[mpathsenser]{get_data} for
#' retrieving activity data from an mpathsenser database.
#' @param confidence The minimum confidence (0-100) that should be assigned to an observation by
#' Activity Recognition.
#' @param direction The directionality of the duration calculation, i.e. \eqn{t - t_{t-1}} or
#' \eqn{t_{t+1} - t}.
#' @param by Either 'Total', 'Hour', or 'Day' indicating how to summarise the results.
#'
#' @return A tibble containing a column 'activity' and a column 'duration' for the hourly
#' activity duration.
#' @export
activity_duration <- function(
    data = NULL,
    db = NULL,
    participant_id = NULL,
    confidence = 70,
    direction = "forward",
    start_date = NULL,
    end_date = NULL,
    by = c("Total", "Day", "Hour")
) {
  if (is.null(data) & is.null(db)) {
    abort("Either data or db must be specified")
  }

  if (!is.null(data) & !is.null(db)) {
    abort("Either data or db must be specified, but not both")
  }

  if (!is.null(data)) {

  } else {
    data <- get_data(db, "Activity", participant_id, start_date, end_date) %>%
      filter(.data$confidence >= .data$confidence) %>%
      compress_activity() %>%
      mutate(datetime = paste(.data$date, .data$time))
  }

  if (tolower(direction) == "forward" | tolower(direction) == "forwards") {
    data <- data %>%
      mutate(duration = STRFTIME("%s", lead(.data$datetime)) - STRFTIME("%s", .data$datetime))
  } else if (tolower(direction) == "backward" | tolower(direction) == "backwards") {
    data <- data %>%
      mutate(duration = STRFTIME("%s", .data$datetime) - STRFTIME("%s", lag(.data$datetime)))
  } else {
    stop("Invalid direction")
  }

  if (is.null(by) || missing(by) || by[1] == "total" | by[1] == "Total") {
    data <- group_by(data, .data$type)
  } else if (by[1] == "Hour") {
    data <- data %>%
      mutate(hour = substr(.data$time, 1, 2)) %>%
      group_by(.data$type, .data$date, .data$hour)
  } else if (by[1] == "Day") {
    data <- group_by(data, .data$type, .data$date)
  } else {
    # Default case
    data <- group_by(data, .data$type)
  }

  data %>%
    summarise(duration = sum(.data$duration, na.rm = TRUE), .groups = "drop") %>%
    collect()
}

#' Get the device info for one or more participants
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @inheritParams get_data
#'
#' @return A tibble containing device info for each participant
#' @export
device_info <- function(db, participant_id = NULL) {
  get_data(db, "Device", participant_id = participant_id) %>%
    select("participant_id", .data$device_id:.data$platform) %>%
    distinct() %>%
    collect()
}

compress_activity <- function(data, direction = "forward") {
  data %>%
    arrange(.data$date, .data$time) %>%
    filter(!(lead(.data$type) == .data$type & lag(.data$type) == .data$type))
}

#' Screen duration by hour or day
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Calculate the screen duration time where the screen was _unlocked_ (i.e. not just on).
#'
#' @inheritParams get_data
#' @param by Either 'Hour' or 'Day' indicating how to summarise the results. Leave empty to get raw
#' screen duration per measurement.
#'
#' @return A tibble with either 'hour' and 'duration' columns or 'date' and 'duration' columns
#' depending on the \code{by} argument. Alternatively, if no \code{by} is specified, a remote
#' tibble is returned with the date, time, and duration since the previous measurement.
#' @export
screen_duration <- function(db,
                            participant_id,
                            start_date = NULL,
                            end_date = NULL,
                            by = c("Hour", "Day")) {
  out <- get_data(db, "Screen", participant_id, start_date, end_date) %>%
    filter(.data$screen_event != "SCREEN_ON") %>%
    mutate(datetime = paste(.data$date, .data$time)) %>%
    arrange(.data$participant_id, .data$datetime) %>%
    distinct(.data$participant_id,
             .data$date,
             .data$time,
             .data$datetime,
             .data$screen_event) %>%
    mutate(next_event = lead(.data$screen_event, n = 1)) %>%
    mutate(next_time = lead(.data$datetime, n = 1)) %>%
    filter(.data$screen_event == "SCREEN_UNLOCKED" & .data$next_event == "SCREEN_OFF") %>%
    mutate(duration = strftime("%s", .data$next_time) - strftime("%s", .data$datetime))

  if (is.null(by) || missing(by)) {
    out <- select(out, "date", "time", "duration")
  } else if (by[1] == "Hour") {
    out <- out %>%
      mutate(hour = strftime("%H", .data$time)) %>%
      group_by(.data$hour) %>%
      summarise(duration = mean(.data$duration, na.rm = TRUE) / 60) %>%
      collect() %>%
      mutate(hour = as.numeric(.data$hour)) %>%
      complete(hour = 0:23, fill = list(duration = 0))
  } else if (by[1] == "Day") {
    out <- out %>%
      group_by(.data$date) %>%
      summarise(duration = sum(.data$duration, na.rm = TRUE) / 60 / 60) %>%
      collect()
  } else {
    # Default case
    out <- select(out, "date", "time", "duration")
  }
  return(out)
}

#' Get number of times screen turned on
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams get_data
#' @param by Either 'Total', 'Hour', or 'Day' indicating how to summarise the results. Defaults to
#' total.
#'
#' @return In case grouping is by the total amount, returns a single numeric value. For date and
#' hour grouping returns a tibble with columns 'date' or 'hour' and the number of screen on's 'n'.
#' @export
n_screen_on <- function(db,
                        participant_id,
                        start_date = NULL,
                        end_date = NULL,
                        by = c("Total", "Hour", "Day")) {
  lifecycle::signal_stage("experimental", "n_screen_on()")

  out <- get_data(db, "Screen", participant_id, start_date, end_date) %>%
    select(-c("measurement_id", "participant_id")) %>%
    filter(.data$screen_event == "SCREEN_ON")

  if (is.null(by)) {
    out <- out %>%
      summarise(n = n()) %>%
      pull("n")
  } else if (by[1] == "Total" | by[1] == "total") {
    out <- out %>%
      summarise(n = n()) %>%
      pull("n")
  } else if (by[1] == "Hour" | by[1] == "hour") {
    out <- out %>%
      mutate(hour = STRFTIME("%H", .data$time)) %>%
      dplyr::count(.data$hour) %>%
      collect() %>%
      mutate(hour = as.numeric(.data$hour)) %>%
      complete(hour = 0:23, fill = list(n = 0))
  } else if (by[1] == "Day" | by[1] == "day") {
    out <- out %>%
      dplyr::count(.data$date) %>%
      collect()
  } else {
    # Default case
    out <- out %>%
      summarise(n = n()) %>%
      pull("n")
  }
  return(out)
}

#' Get number of screen unlocks
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams get_data
#' @param by Either 'Total', 'Hour', or 'Day' indicating how to summarise the results. Defaults to
#' total.
#'
#' @return In case grouping is by the total amount, returns a single numeric value. For date and
#' hour grouping returns a tibble with columns 'date' or 'hour' and the number of screen unlocks
#' 'n'.
#' @export
n_screen_unlocks <- function(db,
                             participant_id,
                             start_date = NULL,
                             end_date = NULL,
                             by = c("Total", "Hour", "Day")) {
  lifecycle::signal_stage("experimental", "n_screen_unlocks()")

  out <- get_data(db, "Screen", participant_id, start_date, end_date) %>%
    select(-c("measurement_id", "participant_id")) %>%
    filter(.data$screen_event == "SCREEN_UNLOCKED")

  if (is.null(by)) {
    out <- out %>%
      summarise(n = n()) %>%
      pull("n")
  } else if (by[1] == "Total" | by[1] == "total") {
    out <- out %>%
      summarise(n = n()) %>%
      pull("n")
  } else if (by[1] == "Hour" | by[1] == "hour") {
    out <- out %>%
      mutate(hour = STRFTIME("%H", .data$time)) %>%
      dplyr::count(.data$hour) %>%
      collect() %>%
      mutate(hour = as.numeric(.data$hour)) %>%
      complete(hour = 0:23, fill = list(n = 0))
  } else if (by[1] == "Day" | by[1] == "day") {
    out <- out %>%
      dplyr::count(.data$date) %>%
      collect()
  } else {
    # Default case
    out <- out %>%
      summarise(n = n()) %>%
      pull("n")
  }
  return(out)
}


#' Get step count
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Extracts the number of steps per hour as sensed by the underlying operating system.
#'
#' @inheritParams get_data
#'
#' @return A tibble with the 'date', 'hour', and the number of 'steps'.
#' @export
step_count <- function(db, participant_id = NULL, start_date = NULL, end_date = NULL) {
  lifecycle::signal_stage("experimental", "step_count()")

  get_data(db, "Pedometer", participant_id, start_date, end_date) %>%
    mutate(hour = STRFTIME("%H", .data$time)) %>%
    group_by(.data$participant_id, .data$date, .data$hour) %>%
    window_order(.data$time, .data$step_count) %>%
    mutate(next_count = lead(.data$step_count, default = NA)) %>%
    mutate(step_count = ifelse(.data$step_count > .data$next_count, NA, .data$step_count)) %>%
    mutate(steps = .data$next_count - .data$step_count) %>%
    group_by(.data$participant_id, .data$date, .data$hour) %>%
    summarise(steps = sum(.data$steps, na.rm = TRUE), .groups = "drop") %>%
    collect()
}

#' Moving average for values in an mpathsenser database
#'
#' #' @description
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams get_data
#' @param participant_id A character string identifying a single participant. Use get_participants
#' to retrieve all participants from the database.
#' @param ... Unquoted names of columns of the \code{sensor} table to average over.
#' @param n The number of observations to average over.
#'
#' @return A tibble with the same columns as the input, modified to be a moving average.
#' @export
#'
#' @examples
#' \dontrun{
#' get_moving_average(db, "Light", "12345", mean_lux, max_lux, n = 5)
#' }
moving_average <- function(db, sensor, participant_id, ..., n, start_date = NULL, end_date = NULL) {
  lifecycle::signal_stage("experimental", "moving_average()")

  cols <- rlang::ensyms(...)

  # SELECT
  query <- "SELECT datetime, "

  # Calculate moving average
  avgs <- lapply(cols, function(x) {
    paste0(
      "avg(`", x, "`) OVER (", "ORDER BY CAST (strftime('%s', datetime) AS INT) ",
      "RANGE BETWEEN ", n / 2, " PRECEDING ", "AND ", n / 2, " FOLLOWING", ") AS ", x
    )
  })

  avgs <- paste0(avgs, collapse = ", ")
  query <- paste0(query, avgs)

  # FROM
  query <- paste0(
    query, " FROM (SELECT `date` || 'T' || `time` AS `datetime`, ",
    paste0(cols, collapse = ", "), " FROM ", sensor
  )

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


#' Identify gaps in mpathsenser mobile sensing data
#'
#' @description `r lifecycle::badge("stable")`
#'
#'   Oftentimes in mobile sensing, gaps appear in the data as a result of the participant
#'   accidentally closing the app or the operating system killing the app to save power. This can
#'   lead to issues later on during data analysis when it becomes unclear whether there are no
#'   measurements because no events occurred or because the app quit in that period. For example, if
#'   no screen on/off event occur in a 6-hour period, it can either mean the participant did not
#'   turn on their phone in that period or that the app simply quit and potential events were
#'   missed. In the latter case, the 6-hour missing period has to be compensated by either removing
#'   this interval altogether or by subtracting the gap from the interval itself (see examples).
#'
#' @details While any sensor can be used for identifying gaps, it is best to choose a sensor with a
#'   very high, near-continuous sample rate such as the accelerometer or gyroscope. This function
#'   then creates time between two subsequent measurements and returns the period in which this time
#'   was larger than \code{min_gap}.
#'
#'   Note that the \code{from} and \code{to} columns in the output are character vectors in UTC
#'   time.
#'
#' @section Warning: Depending on the sensor that is used to identify the gaps (though this is
#'   typically the highest frequency sensor, such as the accelerometer or gyroscope), there may be a
#'   small delay between the start of the gap and the _actual_ start of the gap. For example, if the
#'   accelerometer samples every 5 seconds, it may be after 4.99 seconds after the last
#'   accelerometer measurement (so just before the next measurement), the app was killed. However,
#'   within that time other measurements may still have taken place, thereby technically occurring
#'   "within" the gap. This is especially important if you want to use these gaps in
#'   \code{\link[mpathsenser]{add_gaps}} since this issue may lead to erroneous results.
#'
#'   An easy way to solve this problem is by taking into account all the sensors of interest when
#'   identifying the gaps, thereby ensuring there are no measurements of these sensors within the
#'   gap. One way to account for this is to (as in this example) search for gaps 5 seconds longer
#'   than you want and then afterwards increasing the start time of the gaps by 5 seconds.
#'
#' @inheritParams get_data
#' @param min_gap The minimum time (in seconds) passed between two subsequent measurements for it to
#'   be considered a gap.
#'
#' @return A tibble containing the time period of the gaps. The structure of this tibble is as
#'   follows:
#'
#'   \tabular{ll}{ participant_id \tab the \code{participant_id} of where the gap occurred \cr from
#'   \tab the time of the last measurement before the gap \cr to             \tab the time of the
#'   first measurement after the gap \cr gap            \tab the time passed between from and to, in
#'   seconds }
#' @export
#'
#' @examples
#' \dontrun{
#' # Find the gaps for a participant and convert to datetime
#' gaps <- identify_gaps(db, "12345", min_gap = 60) %>%
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
identify_gaps <- function(db, participant_id = NULL, min_gap = 60, sensor = "Accelerometer") {
  # Get the data for each sensor
  data <- purrr::map(sensor, ~{
    get_data(db, .x, participant_id) %>%
      mutate(datetime = DATETIME(paste(.data$date, .data$time))) %>%
      select("participant_id", "datetime")
  })

  # Merge all together
  data <- purrr::reduce(data, dplyr::union_all)

  # Then, calculate the gap duration
  data %>%
    group_by(.data$participant_id) %>%
    window_order(.data$datetime) %>%
    mutate(to = lead(.data$datetime)) %>%
    ungroup() %>%
    mutate(gap = STRFTIME('%s', .data$to) - STRFTIME('%s', .data$datetime)) %>%
    filter(.data$gap >= min_gap) %>%
    select("participant_id", from = .data$datetime, "to", "gap") %>%
    collect()
}


#'Add gap periods to sensor data
#'
#'@description `r lifecycle::badge("experimental")`
#'
#'  Since there may be many gaps in mobile sensing data, it is pivotal to pay attention in the
#'  analysis to them. This function adds known gaps to data as "measurements", thereby allowing
#'  easier calculations for, for example, finding the duration. For instance, consider a participant
#'  spent 30 minutes walking. However, if it is known there is gap of 15 minutes in this interval,
#'  we should somehow account for it. \code{add_gaps} accounts for this by adding the gap data to
#'  sensors data by splitting intervals where gaps occur.
#'
#'@details In the example of 30 minutes walking where a 15 minute gap occurred (say after 5
#'  minutes), \code{add_gaps} adds two rows: one after 5 minutes of the start of the interval
#'  indicating the start of the gap(if needed containing values from \code{fill}), and one after 20
#'  minutes of the start of the interval signalling the walking activity. Then, when calculating
#'  time differences between subsequent measurements, the gap period is appropriately accounted for.
#'  Note that if multiple measurements occurred before the gap, they will both be continued after
#'  the gap.
#'
#'@inheritSection identify_gaps Warning
#'
#'@param data A data frame containing the activity data. See \link[mpathsenser]{get_data} for
#'  retrieving activity data from an mpathsenser database.
#'@param gaps A data frame (extension) containing the gap data. See
#'  \link[mpathsenser]{identify_gaps} for retrieving gap data from an mpathsenser database. It
#'  should at least contain the columns \code{from} and \code{to} (both in a date-time format), as
#'  well as any specified columns in \code{by}.
#'@param by A character vector indicating the variable(s) to match by, typically the participant
#'  IDs. If NULL, the default, \code{*_join()} will perform a natural join, using all variables in
#'  common across \code{x} and \code{y}.
#'@param fill A named list of the columns to fill with default values for the extra measurements
#'  that are added because of the gaps.
#'
#'@seealso \code{\link[mpathsenser]{identify_gaps}} for finding gaps in the sampling;
#'  \code{\link[mpathsenser]{link_gaps}} for finding which gaps occur in the data;
#'
#'@return A tibble containing the data and the added gaps.
#'@export
#'
#'@examples
#' # Define some data
#' dat <- data.frame(
#'   participant_id = "12345",
#'   time = as.POSIXct(c("2022-05-10 10:00:00", "2022-05-10 10:30:00", "2022-05-10 11:30:00")),
#'   type = c("WALKING", "STILL", "RUNNING"),
#'   confidence = c(80, 100, 20)
#' )
#'
#' # Get the gaps from identify_gaps, but in this example define them ourselves
#' gaps <- data.frame(
#'   participant_id = "12345",
#'   from = as.POSIXct(c("2022-05-10 10:05:00", "2022-05-10 10:50:00")),
#'   to = as.POSIXct(c("2022-05-10 10:20:00", "2022-05-10 10:10:00"))
#' )
#'
#' # Now add the gaps to the data
#' add_gaps(data = dat,
#'          gaps = gaps,
#'          by = "participant_id")
#'
#' # You can use fill if  you want to get rid of those pesky NA's
#' add_gaps(data = dat,
#'          gaps = gaps,
#'          by = "participant_id",
#'          fill = list(type = "GAP", confidence = 100))
add_gaps <- function(data, gaps, by = NULL, fill = NULL) {
  by_names <- colnames(select(data, {{ by }}))
  if (!all(by_names %in% c(colnames(data), colnames(gaps)))) {
    stop(paste(by, "must be a column in both data and gaps."))
  }

  # Pour the gaps in a different format so that they can be added to the sensor data as
  # "measurements". Also provide each gap pair (i.e. from and to) with an ID so they can be matched
  # later on.
  # Only assign the values from fill to the start of the gap, as we want the end of the gap to be
  # NA when there is no prior data
  prepared_gaps <- gaps %>%
    select({{ by }}, "from", "to") %>%
    mutate(gap_id = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = c("from", "to"),
                        names_to = "gap_type",
                        values_to = "time") %>%
    rlang::exec(.fn = mutate, !!!fill) %>%
    mutate(across(names(fill), ~ifelse(gap_type == "to", NA, .x)))

  # Assign groups numbers to the data based on their time stamp and by column In principle, each row
  # is its own group, but if their are multiple measurements with the same time stamp they will get
  # the same group number
  #
  # This is one of the big design decision in this function: Each gap after a measurement should
  # have fill in their starting row (i.e. "from") and continue the previous measurement when the gap
  # ends. However, this is not the case when there is no prior data (or any data at all), in which
  # case there should be NA. Also when there are multiple gaps after each other, the end point of
  # the gap should be the lag of from, then lag2 of from, then lag3 of from, etcetera. Of course
  # this is infeasible as we don't know how many subsequent gaps there are in the data. Hence, we
  # have these row_ids. The basic idea is that we assign to all gaps following a measurement (or
  # multiple measurements with the same time stamp) with row_id "123" the same row_id "123", like
  # so:
  # participant_id  time                    event row_id
  # 12345           2022-05-10 10:00:00     a     123
  # 12345           2022-05-10 10:10:00     GAP   NA => 123
  # 12345           2022-05-10 10:20:00     NA    NA => 123
  #
  # See below for how to continue this sequence, but know that this is why there are row_ids.
  data <- data %>%
    arrange(across(c({{ by }}, "time"))) %>%
    group_by(across(c({{ by }}, "time"))) %>%
    mutate(row_id = dplyr::cur_group_id()) %>%
    ungroup()

  # Remove the time stamps as they are contained in the row_id (with multiple measurements at the
  # same time having the same row_id)
  # This data frame will be used later to match data to the gaps' row_ids.
  lead_data <- data %>%
    select(-"time")

  data %>%
    # Add the gaps to the data
    bind_rows(prepared_gaps) %>%
    # Sort the data to get the correct order, i.e. measurement followed by their respective gaps.
    arrange(across(c({{ by }}, "time"))) %>%
    # As in the example above, fill the row_ids belonging to the data downwards to each gap. By
    # doing this, each gap (no matter how many following the measurement) is now associated with the
    # previous measurement, solving the multiple-gap-problem.
    tidyr::fill("row_id", .direction = "down") %>%
    # Then, nest confidence and type by time to calculate the "lag - 2" for the end of gaps "to".
    # This is necessary because if two measurements at the same time were present just before the
    # gap, they should also both continue after the gap.
    #
    # Note: The code below is equivalent to
    # group_by(participant_id, time, gap_type, gap_id) %>%
    # nest() %>%
    # ungroup() %>%
    # or
    # group_by(across(c({{ by }}, .data$time))) %>%
    # nest(data = !c(.data$gap_id, .data$gap_type, .data$row_id)) %>%
    # ungroup() %>%
    #
    # This means that if there is a (or multiple) measurement of the same participant at the same
    # time and also the start or end of a gap (gap_type "from" or "to"), there will two groups: one
    # with the measurements that are not the gap, and one with the gap measurement, while both
    # having the same participant_id and time stamp. For example:
    #
    # participant_id  time      type    gap_type  gap_id  row_id
    # 12345           10:00:00  STILL   NA        NA      1
    # 12345           10:00:00  ACTIVE  NA        NA      1
    # 12345           10:00:00  GAP     from      1       2
    #
    # Nesting then results in the following:
    # participant_id  time    gap_type  gap_id  row_id  data
    # 12345           10:00:00   NA        NA      1     <tibble [2 × 1]>
    # 12345           10:00:00   from      1       2     <tibble [1 × 1]>
    #
    # Creating the from_lag column as below, it would mean that row 2 would get the data  from row
    # 1, which is intended behaviour. If all 3 rows would be nested in the same tibble, we would get
    # the measurement before that in from_lag, even though there were more recent measurements.
    # Besides, any other nesting would inevitably include gap_type and gap_id in the nested tibble,
    # breaking the code.
    nest(data = !c({{ by }}, "time", "gap_id", "gap_type", "row_id")) %>%
    # Now, match the data (without the gaps) to each corresponding row_id. Thus, in some cases data
    # and data2 will be identical. Only for the end points of gaps, set data to data2.
    dplyr::nest_join(lead_data, by = c("participant_id", "row_id"), name = "data2") %>%
    mutate(data = ifelse(!is.na(.data$gap_type) & .data$gap_type == "to",
                                .data$data2,
                                .data$data)) %>%
    # Lastly, unnest the data to get the original (and modified for "to") nested data, and ungroup
    # and cleanup
    # Make sure not to remove empty data tibbles as these are true NA's, i.e. either gaps where
    # fill was not specified or gaps where there was no prio data present
    unnest("data", keep_empty = TRUE) %>%
    ungroup() %>%
    select(-c("gap_id", "gap_type", "data2", "row_id"))
}
