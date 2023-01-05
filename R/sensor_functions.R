#' Extract data from an m-Path Sense database
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is a convenience function to help extract data from an m-Path sense database. For some
#' sensors that require a bit more pre-processing, such as app usage and screen time, more
#' specialised functions are available (e.g. \code{\link[mpathsenser]{app_usage}} and
#' \code{\link[mpathsenser]{screen_duration}}).
#'
#' @param db A database connection to an m-Path Sense database.
#' @param sensor The name of a sensor. See \link[mpathsenser]{sensors} for a list of available
#'   sensors.
#' @param participant_id A character string identifying a single participant. Use
#'   \code{\link[mpathsenser]{get_participants}} to retrieve all participants from the database.
#'   Leave empty to get data for all participants.
#' @param start_date Optional search window specifying date where to begin search. Must be
#'   convertible to date using \link[base]{as.Date}. Use \link[mpathsenser]{first_date} to find the
#'   date of the first entry for a participant.
#' @param end_date Optional search window specifying date where to end search. Must be convertible
#'   to date using \link[base]{as.Date}. Use \link[mpathsenser]{last_date} to find the date of the
#'   last entry for a participant.
#'
#' @returns A lazy \code{\link[dplyr]{tbl}} containing the requested data.
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
  check_db(db)
  check_sensors(sensor, n = 1)
  check_arg(participant_id, type = c("character", "integerish"), allow_null = TRUE)
  check_arg(sensor, "character", n = 1)
  check_arg(start_date, type = c("character", "POSIXt"), n = 1, allow_null = TRUE)
  check_arg(end_date, type = c("character", "POSIXt"), n = 1, allow_null = TRUE)

  sensor <- tolower(sensor)
  out <- dplyr::tbl(db, sensor)

  if (!is.null(participant_id)) {
    p_id <- as.character(participant_id)
    out <- filter(out, .data$participant_id %in% p_id)
  }

  maybe_date <- function(x) {
    !is.na(as.Date(as.character(x), tz = "UTC", format = "%Y-%m-%d"))
  }

  if (!is.null(start_date) && maybe_date(start_date)) {
    out <- filter(out, .data$date >= start_date)
  }

  if (!is.null(end_date) && maybe_date(end_date)) {
    out <- filter(out, .data$date <= end_date)
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
#' @returns A string in the format 'YYYY-mm-dd' of the first entry date.
#' @export
#'
#' @examples
#' \dontrun{
#' db <- open_db()
#' first_date(db, "Accelerometer", "12345")
#' }
first_date <- function(db, sensor, participant_id = NULL) {
  check_db(db)
  check_arg(sensor, "character", n = 1)

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
#' @returns A string in the format 'YYYY-mm-dd' of the last entry date.
#' @export
#'
#' @examples
#' \dontrun{
#' db <- open_db()
#' first_date(db, "Accelerometer", "12345")
#' }
last_date <- function(db, sensor, participant_id = NULL) {
  check_db(db)
  check_arg(sensor, c("character", "integerish"), n = 1, allow_null = TRUE)

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
#' @param db A database connection to an mpathsenser database.
#' @param participant_id A character string identifying a single participant. Use
#' \code{\link[mpathsenser]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#'
#' @returns A tibble containing app names.
#' @export
installed_apps <- function(db, participant_id = NULL) {
  check_db(db)

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
#' @returns A list containing the following fields:
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
  ensure_suggested_package("curl")
  ensure_suggested_package("httr")
  ensure_suggested_package("rvest")
  check_arg(name, "character")
  check_arg(num, "integerish", n = 1)
  check_arg(rate_limit, "double", n = 1)
  check_arg(exact, "logical", n = 1)

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

  ua <- httr::user_agent(
    "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:70.0) Gecko/20100101 Firefox/70.0"
  )

  session <- httr::GET(query, ua)

  if (!httr::http_error(session)) {
    session <- httr::content(session)
  } else {
    return(list(package = NA, genre = NA)) # nocov
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
                            function(x) grepl(paste0("\\.", tolower(name), "$"), tolower(x)),
                            FUN.VALUE = logical(1)
    )
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
    return(list(package = NA, genre = NA)) # nocov
  }

  # Extract the genre and return results
  genre <- session %>%
    rvest::html_element(xpath = ".//script[contains(., 'applicationCategory')]") %>%
    rvest::html_text() %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("applicationCategory")
  list(package = gsub("^.+?(?<=\\?id=)", "", link, perl = TRUE), genre = genre)
}

#' Get the device info for one or more participants
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @inheritParams get_data
#'
#' @returns A tibble containing device info for each participant
#' @export
device_info <- function(db, participant_id = NULL) {
  get_data(db, "Device", participant_id = participant_id) %>%
    select("participant_id", "device_id":"platform") %>%
    distinct() %>%
    collect()
}

#' Find the home network SSID
#'
#' @description `r lifecycle::badge("stable")`
#'
#'   One way of measuring time spent at home is to measure the time connected to the home Wi-Fi
#'   network (see [wifi_time_home()]). To do this, you first need to which network the home Wi-Fi
#'   network is. `wifi_home_ssid()` attempts to find the most likely candidate to be the home Wi-Fi
#'   network.
#'
#' @details It is not an easy task to determine which SSID (service set ID) from a set of (hashed)
#'   SSIDs is most likely someone's home network. One way to do this is by combining it with GPS
#'   data, but there we face the same problem in that we do not know where someone's "home" is
#'   (unless it is asked somehow). Thus, this function solves the problem in two steps:
#'
#'   1. Find the most often occurring (connected) network SSID between 12AM and 6AM, when people are
#'   generally home the most.
#'   2. If no result was found (e.g. because of airplane mode, phone turned
#'   off, etcetera), take the most often occurring SSID throughout the entire day.
#'
#' @section SSID vs BSSID vs IP: You may have noticed that in the mpathsenser database there are
#'   entries in the Wi-Fi table for SSIDs, BSSIDs, and IP addresses. However, this function focusses
#'   solely on SSIDs.
#'
#'   The IP addresses refer to the interal IP address of the network the person is currently
#'   connected to. Consequently, most values are actually empty as only Wi-Fi networks that the
#'   person is currently connected to have an IP address. Moreover, these are internal IP addresses,
#'   so they may be replicated throughout disparate networks and within the same network, the
#'   internal IP address may change over time. Consequently, IP addresses are not used in this
#'   function.
#'
#'   As for BSSIDs basic service set identifier (BSSID), the relationship between SSIDs and BSSIDs
#'   is that one SSIDs may be associated with multiple BSSIDs, as there are unique to an access
#'   point (e.g. a router).Thus, if a home contains multiple access points, it will have multiple
#'   BSSIDs.
#'
#' @param data A data frame containing the Wi-Fi data with at least the columns `time` and `ssid`.
#' @param exclude Optionally, a character vector of values to exclude from the SSID list. This is
#'   particularly useful if gaps have already been added to the data using [add_gaps()].
#'
#' @family Wi-Fi functions
#'
#' @return A `tibble` of home network SSIDs, grouped according to `data`.
#' @export
wifi_home_ssid <- function(data, exclude = "GAP") {
  check_arg(data, "data.frame")
  check_arg(exclude, "character", allow_null = TRUE)

  data <- data %>%
    select(any_of("participant_id"), all_of(c("time", "ssid"))) %>%
    filter(!(.data$ssid %in% exclude)) %>%
    drop_na("ssid")

  nightly_ssids <- data %>%
    mutate(hour = lubridate::hour(.data$time)) %>%
    filter(.data$hour >= 0L & .data$hour < 6L) %>%
    dplyr::count(.data$ssid, sort = TRUE) %>%
    slice(1) %>%
    select(-"n")

  if (dplyr::is.grouped_df(data)) {
    data %>%
      dplyr::anti_join(nightly_ssids, by = dplyr::group_vars(data)) %>%
      dplyr::count(ssid, sort = TRUE) %>%
      slice(1) %>%
      select(-"n") %>%
      bind_rows(nightly_ssids) %>%
      arrange(across(c(dplyr::group_vars(.)))) %>%
      return()
  } else if (nrow(nightly_ssids) > 0) {
    return(nightly_ssids)
  } else if (nrow(nightly_ssids) == 0) {
    data %>%
      dplyr::count(ssid, sort = TRUE) %>%
      slice(1) %>%
      select(ssid) %>%
      return()
  } else {
    return(tibble(
      ssid = NA
    ))
  }
}

#' Find recurring network SSIDs
#'
#' To provide estimate of how often someone revisits places, you must first find out which
#' "significant" places exist. In a similar vein as [location_unique_places()], this function
#' extract the most often recurring "places" through the associated network SSIDs (assuming most
#' places have Wi-Fi networks in the vicinity).
#'
#' @inheritParams wifi_home_ssid
#' @param n The number of recurring SSIDs to extract.
#'
#' @family Wi-Fi functions
#'
#' @return A `tibble` of recurring network SSIDs, grouped according to `data`.
#' @export
wifi_recurring_ssids <- function(data, n = 10, exclude = "GAP") {

  check_arg(data, "data.frame")
  check_arg(exclude, "character", allow_null = TRUE)

  data <- data %>%
    select(any_of("participant_id"), all_of(c("time", "ssid"))) %>%
    filter(!(.data$ssid %in% exclude)) %>%
    drop_na("ssid")

  data %>%
    count(.data$ssid, sort = TRUE) %>%
    slice_head(n = n) %>%
    select(-"n")
}

#' Moving average for values in an mpathsenser database
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @inheritParams get_data
#' @param cols Character vectors of the columns in the \code{sensor} table to average over.
#' @param n The number of seconds to average over. The index of the result will be centered compared
#'   to the rolling window of observations.
#' @param participant_id A character vector identifying one or multiple participants.
#'
#' @returns A tibble with the same columns as the input, modified to be a moving average.
#' @export
#'
#' @examples
#' \dontrun{
#' path <- system.file("testdata", "test.db", package = "mpathsenser")
#' db <- open_db(NULL, path)
#' moving_average(
#'   db = db,
#'   sensor = "Light",
#'   cols = c("mean_lux", "max_lux"),
#'   n = 5, # seconds
#'   participant_id = "12345"
#' )
#' close_db(db)
#' }
moving_average <- function(db,
                           sensor,
                           cols,
                           n,
                           participant_id = NULL,
                           start_date = NULL,
                           end_date = NULL) {
  lifecycle::signal_stage("experimental", "moving_average()")
  check_db(db)
  check_sensors(sensor, n = 1)
  check_arg(cols, "character")
  check_arg(n, "numeric")
  check_arg(participant_id, c("character", "integerish"), allow_null = TRUE)
  check_arg(start_date, c("character", "POSIXt"), n = 1, allow_null = TRUE)
  check_arg(end_date, c("character", "POSIXt"), n = 1, allow_null = TRUE)

  # SELECT
  query <- "SELECT `participant_id`, `datetime`, "

  # Calculate moving average
  avgs <- lapply(cols, function(x) {
    paste0(
      "avg(`", x, "`) OVER (",
      "PARTITION BY `participant_id` ",
      "ORDER BY UNIXEPOCH(`datetime`) ",
      "RANGE BETWEEN ", n / 2, " PRECEDING ", "AND ", n / 2, " FOLLOWING", ") AS `", x, "`"
    )
  })

  avgs <- paste0(avgs, collapse = ", ")
  query <- paste0(query, avgs)

  # FROM
  query <- paste0(
    query, " FROM (SELECT `participant_id`, `date` || 'T' || `time` AS `datetime`, ",
    paste0("`", cols, "`", collapse = ", "), " FROM `", sensor, "`"
  )

  # Where
  if (!is.null(participant_id)) {
    query <- paste0(
      query, " WHERE (",
      paste0("`participant_id` = '", participant_id, "'", collapse = " OR "),
      ")"
    )
  }

  if (!is.null(start_date) && !is.null(end_date)) {
    query <- paste0(query, " AND (`date` BETWEEN '", start_date, "' AND '", end_date, "')")
  }

  # Closing parenthesis
  query <- paste0(query, ")")

  # Get data
  dplyr::tbl(db, dplyr::sql(query))
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
#' @param sensor One or multiple sensors. See \link[mpathsenser]{sensors} for a list of available
#'   sensors.
#' @param min_gap The minimum time (in seconds) passed between two subsequent measurements for it to
#'   be considered a gap.
#'
#' @returns A tibble containing the time period of the gaps. The structure of this tibble is as
#'   follows:
#'
#'   \tabular{ll}{ participant_id \tab the `participant_id` of where the gap occurred \cr from
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
  check_db(db)
  check_arg(min_gap, "numeric", n = 1)
  check_sensors(sensor)

  # Get the data for each sensor
  data <- purrr::map(sensor, ~ {
    get_data(db, .x, participant_id) %>%
      mutate(datetime = DATETIME(paste(.data$date, .data$time))) %>%
      select("participant_id", "datetime")
  })

  # Merge all together
  data <- Reduce(dplyr::union, data)

  # Then, calculate the gap duration
  data %>%
    dbplyr::window_order(.data$participant_id, .data$datetime) %>%
    group_by(.data$participant_id) %>%
    mutate(to = lead(.data$datetime)) %>%
    ungroup() %>%
    mutate(gap = UNIXEPOCH(.data$to) - UNIXEPOCH(.data$datetime)) %>%
    filter(.data$gap >= min_gap) %>%
    select("participant_id", from = "datetime", "to", "gap") %>%
    collect()
}


#' Add gap periods to sensor data
#'
#' @description `r lifecycle::badge("stable")`
#'
#'   Since there may be many gaps in mobile sensing data, it is pivotal to pay attention to them in
#'   the analysis. This function adds known gaps to data as "measurements", thereby allowing easier
#'   calculations for, for example, finding the duration. For instance, consider a participant spent
#'   30 minutes walking. However, if it is known there is gap of 15 minutes in this interval, we
#'   should somehow account for it. `add_gaps` accounts for this by adding the gap data to
#'   sensors data by splitting intervals where gaps occur.
#'
#' @details In the example of 30 minutes walking where a 15 minute gap occurred (say after 5
#'   minutes), `add_gaps()` adds two rows: one after 5 minutes of the start of the interval
#'   indicating the start of the gap(if needed containing values from `fill`), and one after 20
#'   minutes of the start of the interval signalling the walking activity. Then, when calculating
#'   time differences between subsequent measurements, the gap period is appropriately accounted
#'   for. Note that if multiple measurements occurred before the gap, they will both be continued
#'   after the gap.
#'
#' @inheritSection identify_gaps Warning
#'
#' @param data A data frame containing the data. See [get_data()] for retrieving data from an
#'   mpathsenser database.
#' @param gaps A data frame (extension) containing the gap data. See [identify_gaps()] for
#'   retrieving gap data from an mpathsenser database. It should at least contain the columns `from`
#'   and `to` (both in a date-time format), as well as any specified columns in `by`.
#' @param by A character vector indicating the variable(s) to match by, typically the participant
#'   IDs. If NULL, the default, `*_join()` will perform a natural join, using all variables in
#'   common across `x and `y`.
#' @param continue Whether to continue the measurement(s) prior to the gap once the gap ends.
#' @param fill A named list of the columns to fill with default values for the extra measurements
#'   that are added because of the gaps.
#'
#' @seealso [identify_gaps()] for finding gaps in the sampling; [link_gaps()] for linking gaps to
#'   ESM data, analogous to [link()].
#'
#' @returns A tibble containing the data and the added gaps.
#' @export
#'
#' @examples
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
#'   to = as.POSIXct(c("2022-05-10 10:20:00", "2022-05-10 11:10:00"))
#' )
#'
#' # Now add the gaps to the data
#' add_gaps(
#'   data = dat,
#'   gaps = gaps,
#'   by = "participant_id"
#' )
#'
#' # You can use fill if  you want to get rid of those pesky NA's
#' add_gaps(
#'   data = dat,
#'   gaps = gaps,
#'   by = "participant_id",
#'   fill = list(type = "GAP", confidence = 100)
#' )
add_gaps <- function(data, gaps, by = NULL, continue = FALSE, fill = NULL) {
  check_arg(data, "data.frame")
  check_arg(gaps, "data.frame")
  check_arg(by, "character", allow_null = TRUE)
  check_arg(continue, "logical")
  check_arg(fill, "list", allow_null = TRUE)

  if (!is.null(by)) {
    err <- try(
      {
        select(data, all_of({{ by }}))
        select(gaps, all_of({{ by }}))
      },
      silent = TRUE
    )

    if (inherits(err, "try-error")) {
      abort(paste0(
        "Column(s) ", paste0("\"", by, "\"", collapse = ", "),
        " must be present in both `data` and `gaps`."
      ))
    }

    # Remove gaps that do not occur in the data based on the `by` column
    gaps <- dplyr::semi_join(gaps, data, by = rlang::as_name(rlang::enquo(by)))
  }

  # If we don't want to continue the previous measurement after the gap, we can simply add the
  # gaps to the data and sort
  if (!continue) {
    gaps <- gaps %>%
      select({{ by }}, time = "from") %>%
      mutate(!!!fill)

    data <- data %>%
      bind_rows(gaps) %>%
      arrange(across(c({{ by }}, "time"))) %>%
      distinct() %>%
      tibble::as_tibble() # Ensure consistent output format

    return(data)
  }

  # Pour the gaps in a different format so that they can be added to the sensor data as
  # "measurements". Also provide each gap pair (i.e. from and to) with an ID so they can be matched
  # later on.
  # Only assign the values from fill to the start of the gap, as we want the end of the gap to be
  # NA when there is no prior data
  prepared_gaps <- gaps %>%
    select({{ by }}, "from", "to") %>%
    mutate(gap_id = dplyr::row_number()) %>%
    tidyr::pivot_longer(
      cols = c("from", "to"),
      names_to = "gap_type",
      values_to = "time"
    ) %>%
    mutate(!!!fill) %>%
    mutate(across(names(fill), ~ ifelse(gap_type == "to", NA, .x)))

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

  # Add the gaps to the data
  data <- bind_rows(data, prepared_gaps)

  # Sort the data to get the correct order, i.e. measurement followed by their respective gaps.
  data <- arrange(data, across(c({{ by }}, "time")))

  # As in the example above, fill the row_ids belonging to the data downwards to each gap. By
  # doing this, each gap (no matter how many following the measurement) is now associated with the
  # previous measurement, solving the multiple-gap-problem.
  data <- tidyr::fill(data, "row_id", .direction = "down")

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
  data <- nest(data, data = !c({{ by }}, "time", "gap_id", "gap_type", "row_id"))

  # Now, match the data (without the gaps) to each corresponding row_id. Thus, in some cases data
  # and data2 will be identical. Only for the end points of gaps, set data to data2.
  data <- dplyr::nest_join(data, lead_data, by = c(rlang::as_name(rlang::enquo(by)), "row_id"),
                           name = "data2") %>%
    mutate(data = ifelse(!is.na(.data$gap_type) & .data$gap_type == "to",
                         .data$data2,
                         .data$data
    ))

  # Lastly, unnest the data to get the original (and modified for "to") nested data, and ungroup
  # and cleanup
  # Make sure not to remove empty data tibbles as these are true NA's, i.e. either gaps where
  # fill was not specified or gaps where there was no prio data present
  data <- data %>%
    unnest("data", keep_empty = TRUE) %>%
    ungroup() %>%
    select(-c("gap_id", "gap_type", "data2", "row_id"))

  # Finally, filter out duplicates that may occur when the gap ends exactly at the same time as
  # when another measurement begins
  data <- distinct(data)
  data
}
