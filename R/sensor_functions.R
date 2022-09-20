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
#' get_data(db, 'Accelerometer', '12345')
#'
#' # Or within a specific window
#' get_data(db, 'Accelerometer', '12345', '2021-01-01', '2021-01-05')
#' }
get_data <- function(db, sensor, participant_id = NULL, start_date = NULL, end_date = NULL) {
  if (!DBI::dbIsValid(db))
    stop("Database connection is not valid")
  if (!is.character(sensor) | length(sensor) == 0 | length(sensor) > 1) {
    stop("sensor should be a character vector of size 1")
  }
  if (!(sensor %in% sensors) & !(sensor %in% tolower(sensors))) {
    stop("this sensor does not exist")
  }

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
#' first_date(db, 'Accelerometer', '12345')
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
#' first_date(db, 'Accelerometer', '12345')
#' }
last_date <- function(db, sensor, participant_id = NULL) {
  query <- paste0("SELECT MAX(date) AS `max` FROM `", sensor, "`")

  if (!is.null(participant_id)) {
    query <- paste0(query, " WHERE (`participant_id` = '", participant_id, "')")
  }
  DBI::dbGetQuery(db, query)[1, 1]
}


link_impl <- function(x, y, by, offset_before, offset_after, add_before, add_after) {
  # Match sensing data with ESM using a nested join
  # Set a start_time (beep time - offset) and an end_time (beep time)
  data <- x %>%
    tibble::as_tibble() %>%
    dplyr::mutate(start_time = as.integer(.data$time - offset_before)) %>%
    dplyr::mutate(end_time = as.integer(.data$time + offset_after)) %>%
    dplyr::select(by, .data$start_time, .data$end_time) %>%
    dplyr::mutate(row_id = dplyr::row_number()) # For rematching later

  data <- data %>%
    dplyr::left_join(y, by = by) %>%
    dplyr::mutate(time_int = as.integer(.data$time)) %>%
    dplyr::arrange(by, time)

  # The main data, i.e. data within the interval
  data_main <- data %>%
    dplyr::filter(.data$time_int >= .data$start_time & .data$time_int <= .data$end_time) %>%
    dplyr::select(-.data$time_int) %>%
    tidyr::nest(data = -c(by, .data$start_time, .data$end_time, .data$row_id)) %>%
    dplyr::select(.data$row_id, .data$data)

  # Add the last measurement before start_time
  if (add_before) {
    tz <- lubridate::tz(x$time)
    data_before <- data %>%
      dplyr::group_by(.data$row_id) %>%
      dplyr::filter(.data$time_int < .data$start_time) %>%
      dplyr::arrange(.data$time_int) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(original_time = .data$time) %>%
      dplyr::mutate(time = lubridate::as_datetime(.data$start_time, tz = tz)) %>%
      dplyr::select(-.data$time_int) %>%
      tidyr::nest(data_before = -c(by, .data$start_time, .data$end_time, .data$row_id)) %>%
      dplyr::select(.data$row_id, .data$data_before)

    # Add to the main result
    data_main <- data_main %>%
      dplyr::left_join(data_before, by = "row_id") %>%
      dplyr::mutate(data = purrr::map2(.data$data_before, .data$data, dplyr::bind_rows)) %>%
      dplyr::select(-.data$data_before)
  }

  # Add the first measurements after end_time
  if (add_after) {
    tz <- lubridate::tz(x$time)
    data_after <- data %>%
      dplyr::group_by(.data$row_id) %>%
      dplyr::filter(.data$time_int > .data$end_time) %>%
      dplyr::arrange(.data$time_int) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(original_time = .data$time) %>%
      dplyr::mutate(time = lubridate::as_datetime(.data$end_time, tz = tz)) %>%
      dplyr::select(-.data$time_int) %>%
      tidyr::nest(data_after = -c(by, .data$start_time, .data$end_time, .data$row_id)) %>%
      dplyr::select(.data$row_id, .data$data_after)

    # Add to the main result
    data_main <- data_main %>%
      dplyr::left_join(data_after, by = "row_id") %>%
      dplyr::mutate(data = purrr::map2(.data$data, .data$data_after, dplyr::bind_rows)) %>%
      dplyr::select(-.data$data_after)
  }

  # Create an empty tibble (prototype) by retrieving rows with time before UNIX start (not possible)
  # This is needed to fill in the data entries where there would otherwise be nothing left
  # because nothing matched within the start_time and end_time
  proto <- tibble::as_tibble(y[y$time < 0, setdiff(colnames(y), by)])
  if (add_before | add_after) {
    proto$original_time <- as.POSIXct(vector(mode = "double"))

    # In case data_main is empty, applying the solution below leads to NA in the next step causing
    # proto not to be applied (since it's not null)
    if (nrow(data_main) > 0) {
      # Add column original_time in cases where it's missing
      for (i in seq_along(data_main$data)) {
        if (!any("original_time" == colnames(data_main$data[[i]]))) {
          data_main$data[[i]]$original_time <- as.POSIXct(NA)
        }
      }
    }
  }

  res <- x %>%
    tibble::as_tibble() %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    dplyr::left_join(data_main, by = "row_id") %>%
    dplyr::mutate(data = ifelse(lapply(.data$data, is.null), list(proto), .data$data)) %>%
    dplyr::select(-.data$row_id)

  res
}

#' Match y to the time scale of x
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#'   One of the key tasks in analysing mobile sensing data is being able to link it to other data.
#'   For example, when analysing physical activity data, it could be of interest to know how much
#'   time a participant spent exercising before or after an ESM beep to evaluate their stress level.
#'   \code{link} allows you to map two data frames to each other that are on different time scales,
#'   based on a pre-specified offset before and/or after. This function assumes that both \code{x}
#'   and \code{y} have a column called \code{time} containing \link[base]{DateTimeClasses}.
#'
#' @details \code{y} is matched to the time scale of \code{x} by means of time windows. These time
#'   windows are defined as the period between \code{x - offset_before} and \code{x + offset_after}.
#'   Note that either \code{offset_before} or \code{offset_after} can be 0, but not both. The
#'   "interval" of the measurements is therefore the associated time window for each measurement of
#'   \code{x} and the data of \code{y} that also falls within this period. For example, an
#'   \code{offset_before}  of \link[lubridate]{minutes}(30) means to match all data of \code{y} that
#'   occurred *before* each measurement in \code{x}. An \code{offset_after} of 900 (i.e. 15 minutes)
#'   means to match all data of \code{y} that occurred *after* each measurement in \code{x}. When
#'   both \code{offset_before} and \code{offset_after}  are specified, it means all data of \code{y}
#'   is matched in an interval of 30 minutes before and 15 minutes after each measurement of
#'   \code{x}, thus combining the two arguments.
#'
#'   The arguments \code{add_before} and \code{add_after} let you decide whether you want to add the
#'   last measurement before the interval and/or the first measurement after the interval
#'   respectively. This could be useful when you want to know which type of event occurred right
#'   before or after the interval of the measurement. For example, at \code{offset_before = 1800},
#'   the may indicate that a participant was running 20 minutes before a measurement in \code{x},
#'   However, with just that information there is no way of knowing what the participant was doing
#'   the first 10 minutes of the interval. The same principle applies to after the interval. When
#'   \code{add_before} is set to \code{TRUE}, the last measurement of \code{y} occurring before the
#'   interval of \code{x} is added to the output data as the first row, having the **\code{time} of
#'   \code{x - offset_before}**. When \code{add_after} is set to \code{TRUE}, the first measurement
#'   of \code{y} occurring after the interval of \code{x} is added to the output data as the last
#'   row, having the **\code{time} of \code{x + offset_after}**.This way, it is easier to calculate
#'   the difference to other measurements of \code{y} later (within the same interval).
#'   Additionally, an extra column (\code{original_time}) is added in the nested \code{data} column,
#'   which is the original time of the \code{y} measurement and \code{NULL} for every other
#'   observation. This may be useful to check if the added measurement isn't too distant (in time)
#'   from the others.
#'
#' @section Warning: Note that setting \code{add_before} and \code{add_after} each add one row to
#'   each nested \code{tibble} of the \code{data} column. Thus, if you are only interested in the
#'   total count (e.g. the number of total screen changes), remember to set these arguments to FALSE
#'   or make sure to filter out rows that do _note_ have an \code{original_time}. Simply subtracting
#'   1 or 2 does not work as not all measurements in \code{x} may have a measurement in \code{y}
#'   before or after (and thus no row is added).
#'
#'
#' @param x,y A pair of data frames or data frame extensions (e.g. a tibble). Both \code{x} and
#'   \code{y} must have a column called \code{time}.
#' @param by A character vector indicating the variable(s) to match by, typically the participant
#'   IDs. If NULL, the default, \code{*_join()} will perform a natural join, using all variables in
#'   common across \code{x} and \code{y}. Therefore, all data will be mapped to each other based on
#'   the time stamps of \code{x} and \code{y}. A message lists the variables so that you can check
#'   they're correct; suppress the message by supplying by explicitly.
#'
#'   To join by different variables on \code{x} and \code{y}, use a named vector. For example,
#'   \code{by = c('a' = 'b')} will match \code{x$a} to \code{y$b}
#'
#'   To join by multiple variables, use a vector with length > 1. For example, by = c('a', 'b') will
#'   match \code{x$a} to \code{y$a} and \code{x$b} to \code{y$b}. Use a named vector to match
#'   different variables in x and y. For example, \code{by = c('a' = 'b', 'c' = 'd')} will match
#'   \code{x$a} to \code{y$b} and \code{x$c} to \code{y$d}.
#' @param offset_before The time before each measurement in \code{x} that denotes the period in
#'   which \code{y} is matched. Must be convertible to a period by \link[lubridate]{as.period}.
#' @param offset_after The time after each measurement in \code{x} that denotes the period in which
#'   \code{y} is matched. Must be convertible to a period by \link[lubridate]{as.period}.
#' @param add_before Logical value. Do you want to add the last measurement before the start of each
#'   interval?
#' @param add_after Logical value. Do you want to add the first measurement after the end of each
#'   interval?
#' @param split An optional grouping variable to split the computation by. When working with large
#'   data sets, the computation can grow so large it no longer fits in your computer's working
#'   memory (after which it will probably fall back on the swap file, which is very slow). Splitting
#'   the computation trades some computational efficiency for a large decrease in RAM usage. This
#'   argument defaults to \code{by} to automatically suppress some of its RAM usage.
#'
#' @return A tibble with the data of \code{x} with a new column \code{data} with the matched data of
#'   \code{y} according to \code{offset_before} and \code{offset_after}.
#'
#' @export
link <- function(x,
                 y,
                 by = NULL,
                 offset_before = 0,
                 offset_after = 0,
                 add_before = FALSE,
                 add_after = FALSE,
                 split = by) {

  if (!is.data.frame(x))
    stop("x must be a data frame", call. = FALSE)
  if (!is.data.frame(y))
    stop("y must be a data frame", call. = FALSE)
  if (!is.null(by) && !is.character(by))
    stop("by must be a character vector of variables to join by", call. = FALSE)

  # offset checks
  if ((is.null(offset_before) | all(offset_before == 0)) &
      (is.null(offset_after) | all(offset_after == 0)))
    stop("either offset_before or offset_after must not be 0 or NULL, or both", call. = FALSE)
  if (!is.null(offset_before) && !(is.character(offset_before)
                                   | lubridate::is.period(offset_before)
                                   | is.numeric(offset_before)))
    stop("offset_before must be a character vector, numeric vector, or a period", call. = FALSE)
  if (!is.null(offset_after) && !(is.character(offset_after)
                                 | lubridate::is.period(offset_after)
                                 | is.numeric(offset_after)))
    stop("offset_before must be a character vector, numeric vector, or a period", call. = FALSE)
  if (is.character(offset_before) | is.numeric(offset_before))
    offset_before <- lubridate::as.period(offset_before)
  if (is.character(offset_after) | is.numeric(offset_after))
    offset_after <- lubridate::as.period(offset_after)
  if (is.na(offset_before) | is.na(offset_after))
    stop(paste("Invalid offset specified. Try something like '30 minutes', ",
               "lubridate::minutes(30), or 1800."), call. = FALSE)
  if(!is.null(offset_before) && offset_before < 0) {
    offset_before <- abs(offset_before)
    warning(paste("offset_before must be a positive period (i.e. greater than 0).",
                  "Taking the absolute value"), call. = FALSE)
  }
  if(!is.null(offset_after) && offset_after < 0) {
    offset_after <- abs(offset_after)
    warning(paste("offset_after must be a positive period (i.e. greater than 0).",
                  "Taking the absolute value"), call. = FALSE)
  }


  # Check for time column
  if (!("time" %in% colnames(x) & "time" %in% colnames(y)))
    stop("column 'time' must be present in both x and y", call. = FALSE)
  if (!lubridate::is.POSIXct(x$time))
    stop("column 'time' in x must be a POSIXct", call. = FALSE)
  if (!lubridate::is.POSIXct(y$time))
    stop("column 'time' in y must be a POSIXct", call. = FALSE)

  # Do not perform matching when x and y are identical
  if (identical(x, y) || isTRUE(dplyr::all_equal(x, y)))
    stop("x and y are identical", call. = FALSE)

  if (!is.null(split)) {
    if (is.numeric(split)) {
      x <- split(x, rep(1:split, each = ceiling(nrow(x) / split), length.out = nrow(x)))
    } else {
      x <- dplyr::group_split(x, dplyr::across(by))
    }
  } else {
    x <- list(x)
  }

  x %>%
    furrr::future_map(~ link_impl(.x, y, by, offset_before, offset_after, add_before, add_after),
                      .options = furrr::furrr_options(seed = TRUE)) %>%
    dplyr::bind_rows()
}

#' Link two sensors OR one sensor and an external data frame using an \code{mpathsenser} database
#'
#' @description
#' `r lifecycle::badge("stable")`
#' This function is specific to mpathsenser databases. It is a wrapper around
#' \link[mpathsenser]{link} but extracts data in the database for you.
#'
#' @inheritParams get_data
#' @inheritParams link
#' @param sensor_one The name of a primary sensor. See \link[mpathsenser]{sensors} for a list of
#' available sensors.
#' @param sensor_two The name of a secondary sensor. See \link[mpathsenser]{sensors} for a list of
#' available sensors. Cannot be used together with \code{external}.
#' @param external Optionally, specify an external data frame. Cannot be used at the same time as
#' a second sensor. This data frame must have a column called \code{time}.
#' @param reverse Switch \code{sensor_one} with either \code{sensor_two} or \code{external}?
#' Particularly useful in combination with \code{external}.
#' @param ignore_large Safety override to prevent long wait times. Set to \code{TRUE} to do this
#' function on lots of data.
#'
#' @seealso \code{\link[mpathsenser]{link}}
#'
#' @return A tibble with the data of \code{sensor_one} with a new column \code{data} with the
#' matched data of either \code{sensor_two} or \code{external} according to \code{offset}. The
#' other way around when \code{reverse = TRUE}.
#' @export
link_db <- function(db,
                  sensor_one,
                  sensor_two = NULL,
                  external = NULL,
                  offset_before = 0,
                  offset_after = 0,
                  add_before = FALSE,
                  add_after = FALSE,
                  participant_id = NULL,
                  start_date = NULL,
                  end_date = NULL,
                  reverse = FALSE,
                  ignore_large = FALSE) {

  if (!DBI::dbIsValid(db))
    stop("Database connection is not valid")
  if (is.null(external) & is.null(sensor_two))
    stop("either a second sensor or an external data frame must be supplied")
  if (!is.null(external) & !is.null(sensor_two))
    stop("only a second sensor or an external data frame can be supplied, but not both")
  if (!is.null(external) && !is.data.frame(external))
    stop("external must be a data frame")
  if (!is.null(sensor_two) && !is.character(sensor_two))
    stop("sensor_two must be a character vector")

  # See if data is not incredibly large
  if (!ignore_large) {
    n <- sum(get_nrows(db, c(sensor_one, sensor_two), participant_id, start_date, end_date),
             nrow(external))
    if (n > 1e+05) {
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
    if (any(format(external$time, "%Z") != "UTC")) {
      warning(paste0("external data is not using UTC as a time zone, unlike the data in the ",
                     "database. Consider converting the time column to UTC."), call. = FALSE)
    }

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

  link(x = dat_one,
       y = dat_two,
       by = "participant_id",
       offset_before = offset_before,
       offset_after = offset_after,
       add_before = add_before,
       add_after = add_after)

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
    dplyr::filter(!is.na(app)) %>%
    dplyr::distinct(app) %>%
    dplyr::arrange(app) %>%
    dplyr::collect()
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
#' app_category('whatsapp')
#'
#' # Example of a generic app name where we can't find a specific app
#' app_category('weather') # Weather forecast channel
#'
#' # Get OnePlus weather
#' app_category('net.oneplus.weather')
app_category <- function(name, num = 1, rate_limit = 5, exact = TRUE) {
  # Check if required packages are available
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop(paste0("package curl is needed for this function to work. ",
                "Please install it using install.packages(\"curl\")"),
         call. = FALSE)
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop(paste0("package httr is needed for this function to work. ",
                "Please install it using install.packages(\"httr\")"),
         call. = FALSE)
  }
  if (!requireNamespace("rvest", quietly = TRUE)) {
    stop(paste0("package rvest is needed for this function to work. ",
                "Please install it using install.packages(\"rvest\")"),
         call. = FALSE)
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
    name_detected <- vapply(links, function(x) grepl(paste0("\\.", name, "$/i"), x), FUN.VALUE = logical(1))
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
    rvest::html_element(xpath=".//script[contains(., 'applicationCategory')]") %>%
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
  } else {
    # Default case
    data <- data %>%
      dplyr::slice(dplyr::n()) %>%
      dplyr::mutate(usage = usage / 60 / 60)
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
activity_duration <- function(data = NULL,
                              db = NULL,
                              participant_id = NULL,
                              confidence = 70,
                              direction = "forward",
                              start_date = NULL,
                              end_date = NULL,
                              by = c("Total", "Day", "Hour")) {
  if (is.null(data) & is.null(db)) {
    stop("Either data or db must be specified")
  }

  if (!is.null(data) & !is.null(db)) {
    stop("Either data or db must be specified, but not both")
  }

  if (!is.null(data)) {

  } else {
    data <- get_data(db, "Activity", participant_id, start_date, end_date) %>%
      dplyr::filter(confidence >= confidence) %>%
      compress_activity() %>%
      dplyr::mutate(datetime = paste(date, time))
  }

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
  } else {
    # Default case
    data <- data %>%
      dplyr::group_by(type)
  }

  data %>%
    dplyr::summarise(duration = sum(duration, na.rm = TRUE), .groups = "drop") %>%
    dplyr::collect()
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
    dplyr::select(participant_id, device_id:platform) %>%
    dplyr::distinct() %>%
    dplyr::collect()
}

compress_activity <- function(data, direction = "forward") {
  data %>%
    dbplyr::window_order(date, time) %>%
    dplyr::filter(!(dplyr::lead(type) == type & dplyr::lag(type) == type))
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
    dplyr::filter(screen_event != "SCREEN_ON") %>%
    dplyr::mutate(datetime = paste(date, time)) %>%
    dbplyr::window_order(participant_id, datetime) %>%
    dplyr::distinct(participant_id, date, time, datetime, screen_event) %>%
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
      dplyr::summarise(duration = mean(duration, na.rm = TRUE) / 60) %>%
      dplyr::collect() %>%
      dplyr::mutate(hour = as.numeric(hour)) %>%
      tidyr::complete(hour = 0:23, fill = list(duration = 0))
  } else if (by[1] == "Day") {
    out <- out %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(duration = sum(duration, na.rm = TRUE) / 60 / 60) %>%
      dplyr::collect()
  } else {
    # Default case
    out <- out %>%
      dplyr::select(date, time, duration)
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
  } else {
    # Default case
    out <- out %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::pull(n)
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
  } else {
    # Default case
    out <- out %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::pull(n)
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
  get_data(db, "Pedometer", participant_id, start_date, end_date) %>%
    dplyr::mutate(hour = STRFTIME("%H", time)) %>%
    dplyr::group_by(participant_id, date, hour) %>%
    dbplyr::window_order(time, step_count) %>%
    dplyr::mutate(next_count = dplyr::lead(step_count, default = NA)) %>%
    dplyr::mutate(step_count = ifelse(step_count > next_count, NA, step_count)) %>%
    dplyr::mutate(steps = next_count - step_count) %>%
    dplyr::group_by(participant_id, date, hour) %>%
    dplyr::summarise(steps = sum(steps, na.rm = TRUE), .groups = "drop") %>%
    dplyr::collect()
}

#' Moving average for values in an mpathsenser database
#'
#'#' @description
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
#' get_moving_average(db, 'Light', '12345', mean_lux, max_lux, n = 5)
#' }
moving_average <- function(db, sensor, participant_id, ..., n, start_date = NULL, end_date = NULL) {
  cols <- dplyr::ensyms(...)

  # SELECT
  query <- "SELECT datetime, "

  # Calculate moving average
  avgs <- lapply(cols, function(x) {
    paste0("avg(`", x, "`) OVER (", "ORDER BY CAST (strftime('%s', datetime) AS INT) ",
           "RANGE BETWEEN ", n / 2, " PRECEDING ", "AND ", n / 2, " FOLLOWING", ") AS ", x)
  })

  avgs <- paste0(avgs, collapse = ", ")
  query <- paste0(query, avgs)

  # FROM
  query <- paste0(query, " FROM (SELECT `date` || 'T' || `time` AS `datetime`, ",
                  paste0(cols, collapse = ", "), " FROM ", sensor)

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
#' @description
#' `r lifecycle::badge("stable")`
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
#' @details
#' While any sensor can be used for identifying gaps, it is best to choose a sensor with a very
#' high, near-continuous sample rate such as the accelerometer or gyroscope. This function then
#' creates time between two subsequent measurements and returns the period in which this time was
#' larger than \code{min_gap}.
#'
#' Note that the \code{from} and \code{to} columns in the output are character vectors in UTC time.
#'
#' @inheritParams get_data
#' @param min_gap The minimum time (in seconds) passed between two subsequent measurements for it
#' to be considered a gap..
#'
#' @return A tibble containing the time period of the gaps. The strucute of this tibble is as
#' follows:
#'
#' \tabular{ll}{
#'   participant_id \tab the \code{participant_id} of where the gap occurred \cr
#'   from           \tab the time of the last measurement before the gap \cr
#'   to             \tab the time of the first measurement after the gap \cr
#'   gap            \tab the time passed between from and to, in seconds
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # Find the gaps for a participant and convert to datetime
#' gaps <- identify_gaps(db, '12345', min_gap = 60) %>%
#'   mutate(across(c(to, from), ymd_hms)) %>%
#'   mutate(across(c(to, from), with_tz, 'Europe/Brussels'))
#'
#' # Get some sensor data and calculate a statistic, e.g. the time spent walking
#' # You can also do this with larger intervals, e.g. the time spent walking per hour
#' walking_time <- get_data(db, 'Activity', '12345') %>%
#'   collect() %>%
#'   mutate(datetime = ymd_hms(paste(date, time))) %>%
#'   mutate(datetime = with_tz(datetime, 'Europe/Brussels')) %>%
#'   arrange(datetime) %>%
#'   mutate(prev_time = lag(datetime)) %>%
#'   mutate(duration = datetime - prev_time) %>%
#'   filter(type == 'WALKING')
#'
#' # Find out if a gap occurs in the time intervals
#' walking_time %>%
#'   rowwise() %>%
#'   mutate(gap = any(gaps$from >= prev_time & gaps$to <= datetime))
#' }
identify_gaps <- function(db, participant_id = NULL, min_gap = 60, sensor = "Accelerometer") {
  get_data(db, sensor, participant_id) %>%
    dplyr::mutate(datetime = DATETIME(paste(date, time))) %>%
    dplyr::select(participant_id, datetime) %>%
    dplyr::group_by(participant_id) %>%
    dbplyr::window_order(datetime) %>%
    dplyr::mutate(from = dplyr::lag(datetime)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(gap = STRFTIME("%s", datetime) - STRFTIME("%s", from)) %>%
    dplyr::filter(gap >= min_gap) %>%
    dplyr::select(participant_id, from, to = datetime, gap) %>%
    dplyr::collect()
}

bin_duration_impl <- function(data, date) {

  date <- as.integer(date)

  start_time <- data$start_time
  end_time <- data$end_time

  floor_start <- data$floor_start
  floor_end <- data$floor_end
  ceiling_start <- data$ceiling_start

  x <- dplyr::case_when(

    # throw out cases where end_time is NA => not useful
    is.na(start_time) | is.na(end_time) ~ 0L,

    # don't count when date is not in the interval
    !(floor_start <= date & date <= end_time) ~ 0L,

    # date is now within interval

    # start hour and end hour are the same
    floor_start == floor_end ~ end_time - start_time,

    # What if the date is within the start hour?
    date == floor_start ~ ceiling_start - start_time,

    # What if the data is within the end hour?
    date == floor_end ~ end_time - floor_end,

    # Date is not within start hour or end hour, so it must be a full hour
    TRUE ~ 3600L
  )

  sum(x, na.rm = TRUE)
}

#' Bin duration in variable time series
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' In time series with variable measurements, an often recurring task is calculating the total time
#' spent (i.e. the duration) in fixed bins, for example per hour or day. However, this may be
#' difficult when two subsequent measurements are in different bins or span over multiple bins.
#'
#' Note that non-grouped columns are discarded in the output.
#'
#' @param data A data frame or tibble containing the time series.
#' @param start_time The column name of the start time of the interval, a POSIXt.
#' @param end_time The column name of the end time of the interval, a POSIXt.
#' @param by A binning specification, either \code{hour} or \code{day}.
#'
#' @return A tibble containing the group columns (if any), date, hour (if \code{by = "hour"}),
#' and the duration in seconds.
#' @export
#'
#' @examples
#' data <- tibble::tibble(
#'   participant_id = 1,
#'   datetime = c("2022-06-21 15:00:00", "2022-06-21 15:55:00",
#'                "2022-06-21 17:05:00", "2022-06-21 17:10:00"),
#'   confidence = 100,
#'   type = "WALKING"
#' )
#'
#' # get the duration per hour, even if the interval is longer than one hour
#' data %>%
#'   dplyr::mutate(datetime = as.POSIXct(datetime)) %>%
#'   dplyr::mutate(lead = dplyr::lead(datetime)) %>%
#'   bin_duration(start_time = datetime,
#'                end_time = lead,
#'                by = "hour")
#'
#' data <- tibble::tibble(
#'   participant_id = 1,
#'   datetime = c("2022-06-21 15:00:00", "2022-06-21 15:55:00",
#'                "2022-06-21 17:05:00", "2022-06-21 17:10:00"),
#'   confidence = 100,
#'   type = c("STILL", "WALKING", "STILL", "WALKING")
#' )
#' # binned_intervals also takes into account the prior grouping structure
#' data %>%
#'   dplyr::mutate(datetime = as.POSIXct(datetime)) %>%
#'   dplyr::mutate(lead = dplyr::lead(datetime)) %>%
#'   dplyr::group_by(participant_id, type) %>%
#'   bin_duration(start_time = datetime,
#'                end_time = lead,
#'                by = "hour")
bin_duration <- function(data, start_time, end_time, by = c("hour", "day")) {

  # Select relevant columns
  data <- data %>%
    dplyr::select(dplyr::group_cols(),
                  start_time = {{ start_time }},
                  end_time = {{ end_time }})

  group_vars <- dplyr::group_vars(data)
  by <- match.arg(by)

  # check that start_time and end_time are a datetime, or try to convert
  if (!lubridate::is.POSIXt(data$start_time) & !lubridate::is.POSIXt(data$end_time)) {
    data$start_time <- as.POSIXct(data$start_time)
    data$end_time <- as.POSIXct(data$end_time)
  }

  # Generate output structure with unique hours per day, keeping the grouping structure
  res <- data %>%
    tidyr::pivot_longer(cols = c(.data$start_time, .data$end_time),
                        names_to = NULL,
                        values_to = "date") %>%
    dplyr::mutate(date = lubridate::floor_date(.data$date, by)) %>%
    dplyr::distinct() %>%
    tidyr::drop_na(.data$date) %>%
    tidyr::complete(date = seq.POSIXt(min(.data$date, na.rm = TRUE),
                                      max(.data$date, na.rm = TRUE), by = by))

  # The magic:
  # 1. Join the data to the hours or days and unnest
  # 2. Calculate some variables used later in the implementation functions (where the actual
  # calculation happens)
  # 3. Group by the by variable, and calculate the duration
  res <- res %>%
    dplyr::nest_join(data, by = group_vars) %>%
    tidyr::unnest(.data$data) %>%
    dplyr::mutate(floor_start = lubridate::floor_date(.data$start_time, by)) %>%
    dplyr::mutate(floor_end = lubridate::floor_date(.data$end_time, by)) %>%
    dplyr::mutate(ceiling_start = lubridate::ceiling_date(.data$start_time, by,
                                                          change_on_boundary = TRUE)) %>%
    dplyr::mutate(date_int = as.integer(date)) %>%
    dplyr::mutate(dplyr::across(c(.data$start_time, .data$end_time, .data$floor_start,
                                  .data$floor_end, .data$ceiling_start),
                  as.integer)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, "date")))) %>%
    dplyr::summarise(duration = bin_duration_impl(dplyr::cur_data(), .data$date_int),
                     .groups = "drop_last")

  # Make the by column look pretty
  if (by == "hour") {
    res <- res %>%
      dplyr::mutate(hour = lubridate::hour(.data$date)) %>%
      dplyr::mutate(date = lubridate::date(.data$date)) %>%
      dplyr::select(dplyr::group_cols(), .data$date, .data$hour, .data$duration)
  } else if (by == "day") {
    res <- res %>%
      dplyr::mutate(date = lubridate::date(.data$date))
  }

  res
}
