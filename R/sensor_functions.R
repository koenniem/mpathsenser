.normalize_get_data_date <- function(x, arg) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, c("Date", "POSIXt"))) {
    value <- if (inherits(x, "Date")) as.numeric(x) else as.numeric(as.POSIXct(x, tz = "UTC"))
    if (is.na(x) || !is.finite(value)) {
      cli_abort(
        c(
          "{.arg {arg}} must be a non-missing, finite date or timestamp.",
          x = "You supplied {.val {x}}."
        ),
        arg = arg
      )
    }
    return(x)
  }

  date <- as.Date(NA_character_)
  if (isTRUE(grepl("^\\d{4}-\\d{2}-\\d{2}$", x))) {
    date <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  }

  if (is.na(date) || format(date, "%Y-%m-%d") != x) {
    cli_abort(
      c(
        "{.arg {arg}} must be a valid date in {.val YYYY-MM-DD} format.",
        x = "You supplied {.val {x}}."
      ),
      arg = arg
    )
  }

  date
}

#' Extract data from an m-Path Sense database
#'
#' @description `r lifecycle::badge("stable")`
#'
#'   This is a convenience function to help extract data from an m-Path sense database.
#'
#' @details Note that this function returns a lazy (also called remote) `tibble`. This means that
#'   the data is not actually in R until you call a function that pulls the data from the database.
#'   This is useful for various functions in this package that work with a lazy tibble, for example
#'   [identify_gaps()]. You may manually want to modify this lazy `tibble` by using `dplyr`
#'   functions such as [dplyr::filter()] or [dplyr::mutate()] before pulling the data into R. These
#'   functions will be executed in-database, and will therefore be much faster than having to first
#'   pull all data into R and then possibly removing a large part of it. Importantly, data can
#'   pulled into R using [dplyr::collect()]. Measurement timestamps are
#'   returned as absolute `TIMESTAMPTZ` values. Use [collect_local()]
#'   or the explicit `_local` views for participant-local wall-clock values.
#'
#'
#' @param db A database connection to an m-Path Sense database.
#' @param sensor The name of a sensor or its derived `_local` or `_with_local` view.
#'   See \link[mpathsenser]{sensors} for the physical sensor names.
#' @param participant_id A single participant identifier. Use
#'   \code{\link[mpathsenser]{get_participants}} to retrieve all participants from the database.
#'   Leave empty to get data for all participants. Participant ids are stored as unsigned
#'   integers, so an integer, numeric, or character value is accepted.
#' @param start_date An optional inclusive lower bound. Character values must use
#'   `YYYY-MM-DD`; a `Date` selects that whole calendar day. These date-only
#'   bounds use UTC for `time` columns (including `_with_local`) and
#'   local wall time for `_local` views. A `POSIXt` value is an exact instant for
#'   `time`; `_local` views use its clock fields in its timezone, or
#'   UTC when no timezone attribute is set.
#' @param end_date An optional upper bound with the same types and timezone rules
#'   as `start_date`. A character or `Date` value includes the whole day, ending
#'   just before the following midnight; a `POSIXt` value includes its exact
#'   timestamp.
#'
#' @returns A lazy \code{\link[dplyr]{tbl}} containing the requested data.
#' @export
#'
#' @examples
#' db <- example_db()
#'
#' # Retrieve some data
#' get_data(db, "Pedometer", participant_id = 372780)
#'
#' # Or within a specific window
#' get_data(db, "Pedometer", participant_id = 372780, "2026-09-30", "2026-10-01")
#'
#' close_db(db)
#'
get_data <- function(
  db,
  sensor,
  participant_id = NULL,
  start_date = NULL,
  end_date = NULL
) {
  check_db(db)
  check_sensors(sensor, n = 1, include_views = TRUE)
  check_arg(participant_id, type = c("character", "integerish", "numeric"), allow_null = TRUE)
  check_arg(sensor, "character", n = 1)
  check_arg(start_date, type = c("character", "Date", "POSIXt"), n = 1, allow_null = TRUE)
  check_arg(end_date, type = c("character", "Date", "POSIXt"), n = 1, allow_null = TRUE)
  start_date <- .normalize_get_data_date(start_date, "start_date")
  end_date <- .normalize_get_data_date(end_date, "end_date")

  sensor <- as.character(sensor)
  local_view <- grepl("_local$", tolower(sensor)) && !grepl("_with_local$", tolower(sensor))
  out <- tbl(db, sensor)
  attr(out, "mpathsenser_sensor") <- sensor

  if (!is.null(participant_id)) {
    p_id <- as.character(participant_id)
    out <- filter(out, .data$participant_id %in% p_id)
  }

  local_boundary <- function(x) {
    if (inherits(x, "POSIXt")) {
      time_zone <- attr(x, "tzone")
      if (
        length(time_zone) == 0L ||
          is.na(time_zone[[1]]) ||
          !nzchar(time_zone[[1]])
      ) {
        time_zone <- "UTC"
      } else {
        time_zone <- time_zone[[1]]
      }
      clock <- format(
        as.POSIXct(x, tz = "UTC"),
        format = "%Y-%m-%d %H:%M:%OS6",
        tz = time_zone
      )
      return(as.POSIXct(clock, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))
    }

    as.POSIXct(x, tz = "UTC")
  }

  # dbplyr renders POSIXct values as timezone-naive TIMESTAMP literals. Build
  # an epoch-based TIMESTAMPTZ expression so DuckDB's session timezone cannot
  # shift bounds used with UTC columns.
  utc_boundary <- function(x) {
    epoch <- as.numeric(as.POSIXct(x, tz = "UTC"))
    quoted_epoch <- DBI::dbQuoteLiteral(db, epoch)
    dbplyr::sql(paste0("to_timestamp(", as.character(quoted_epoch), ")"))
  }

  if (!is.null(start_date)) {
    start_boundary <- if (local_view) local_boundary(start_date) else utc_boundary(start_date)
    out <- filter(out, .data$time >= start_boundary)
  }

  if (!is.null(end_date)) {
    if (inherits(end_date, "POSIXt")) {
      end_boundary <- if (local_view) local_boundary(end_date) else utc_boundary(end_date)
      out <- filter(out, .data$time <= end_boundary)
    } else {
      end_boundary <- if (local_view) {
        local_boundary(end_date + 1)
      } else {
        utc_boundary(end_date + 1)
      }
      out <- filter(out, .data$time < end_boundary)
    }
  }

  # Physical sensor tables expose absolute TIMESTAMPTZ values directly.
  out
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
#' @param participant_id A single participant identifier (stored as an unsigned integer; an
#' integer, numeric, or character value is accepted). Use
#' \code{\link[mpathsenser]{get_participants}} to retrieve all participants from the database.
#' Leave empty to get data for all participants.
#'
#' @returns A tibble containing app names.
#' @export
#'
#' @examples
#' \dontrun{
#' db <- example_db()
#'
#' # Get installed apps for all participants
#' installed_apps(db)
#'
#' # Get installed apps for a single participant
#' installed_apps(db, 372780)
#' }
installed_apps <- function(db, participant_id = NULL) {
  check_db(db)

  # The InstalledApps sensor no longer exists; installed apps are derived from
  # the apps that appear in the AppUsage data
  get_data(db, "AppUsage", participant_id) |>
    filter(!is.na(.data$app)) |>
    distinct(.data$app) |>
    arrange(.data$app) |>
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
#' @param name Character app names to search for; missing values are skipped.
#' @param num Which result should be selected in the list of search results. Defaults to one.
#' @param rate_limit The time interval to keep between queries, in seconds. If the rate limit is too
#' low, the Google Play Store may reject further requests or even ban your entirely.
#' @param exact In m-Path Sense, the app names of the AppUsage sensor are the last part of the app's
#' package names. When \code{exact}  is \code{TRUE}, the function guarantees that \code{name} is
#' exactly equal to the last part of the selected package from the search results. Note that when
#' \code{exact} is \code{TRUE}, it interacts with \code{num} in the sense that it no longer selects
#' the top search result but instead the top search result that matches the last part of the package
#' name.
#' @inheritParams read_mpath_sense
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
app_category <- function(name, num = 1, rate_limit = 5, exact = TRUE, .progress = TRUE) {
  # Check if required packages are available
  ensure_suggested_package("curl")
  ensure_suggested_package("httr")
  ensure_suggested_package("rvest")
  check_arg(name, "character")
  check_arg(num, "integerish", n = 1)
  check_arg(rate_limit, "double", n = 1)
  check_arg(exact, "logical", n = 1)

  res <- data.frame(app = name, package = rep(NA, length(name)), genre = rep(NA, length(name)))

  if (.progress) {
    cli_progress_bar(total = length(name))
  }

  requested <- FALSE
  for (i in seq_along(name)) {
    if (!is.na(name[i])) {
      if (requested) {
        Sys.sleep(rate_limit)
      }

      res[i, 2:3] <- tryCatch(
        app_category_impl(name[i], num, exact),
        error = \(e) list(package = NA, genre = NA)
      )
      requested <- TRUE
    }

    if (.progress) {
      cli_progress_update()
    }
  }

  res
}

app_category_impl <- function(name, num, exact) {
  # Replace illegal characters in app name
  name <- iconv(name, from = "UTF-8", to = "ASCII//TRANSLIT")
  name <- gsub("[^[:alnum:] .@]", " ", name, perl = TRUE)
  name <- gsub(" ", "%20", name)

  query <- paste0("https://play.google.com/store/search?q=", name, "&c=apps")

  ua <- httr::user_agent(
    "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:70.0) Gecko/20100101 Firefox/70.0"
  )

  session <- tryCatch(
    httr::GET(query, ua),
    error = \(e) e
  )

  if (!inherits(session, "error") && !httr::http_error(session)) {
    session <- httr::content(session)
  } else {
    return(list(package = NA, genre = NA)) # nocov
  }

  # Get the link
  links <- session |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    purrr::keep(~ grepl("^\\/store\\/apps\\/details\\?id=.*$", .x))

  if (length(links) == 0) {
    return(list(package = NA, genre = NA))
  }

  # Check if the name occurs in any of the package names
  # If so, select the num (usually first) link from this list
  if (exact) {
    name_detected <- vapply(
      links,
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

  session <- tryCatch(
    httr::GET(link, ua),
    error = \(e) e
  )

  if (!inherits(session, "error") && !httr::http_error(session)) {
    session <- httr::content(session)
  } else {
    return(list(package = NA, genre = NA)) # nocov
  }

  # Extract the genre and return results
  genre <- session |>
    rvest::html_element(xpath = ".//script[contains(., 'applicationCategory')]") |>
    rvest::html_text() |>
    jsonlite::fromJSON() |>
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
#'
#' @examples
#' \dontrun{
#' # Open the example database
#' db <- example_db()
#'
#' # Get device info for all participants
#' device_info(db)
#'
#' # Get device info for a specific participant
#' device_info(db, participant_id = 372780)
#' }
device_info <- function(db, participant_id = NULL) {
  get_data(db, "Device", participant_id = participant_id) |>
    select(-any_of(c("measurement_id", "date", "time", "device_data", "source_file_id"))) |>
    distinct() |>
    collect()
}


.moving_average_window_seconds <- function(window) {
  rlang::check_required(window)

  if (length(window) != 1L) {
    cli_abort("{.arg window} must have length one.", arg = "window")
  }

  if (is.character(window)) {
    window <- tryCatch(
      suppressWarnings(lubridate::as.period(window)),
      error = function(e) NULL
    )
  }

  if (lubridate::is.period(window)) {
    calendar_fields <- c(window@year, window@month)
    if (anyNA(calendar_fields)) {
      cli_abort("{.arg window} must be a valid duration.", arg = "window")
    }
    if (any(calendar_fields != 0)) {
      cli_abort(
        "{.arg window} cannot include calendar-dependent years or months.",
        arg = "window"
      )
    }
    seconds <- lubridate::period_to_seconds(window)
  } else if (lubridate::is.duration(window)) {
    seconds <- as.numeric(window, units = "seconds")
  } else if (is.numeric(window)) {
    seconds <- as.numeric(window)
  } else {
    cli_abort(
      "{.arg window} must be seconds, a character duration, or a lubridate Period/Duration.",
      arg = "window"
    )
  }

  if (length(seconds) != 1L || !is.finite(seconds) || seconds <= 0) {
    cli_abort("{.arg window} must be a finite, positive duration.", arg = "window")
  }

  as.numeric(seconds)
}

#' Moving average for values in an mpathsenser database
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Calculate sample-weighted averages over centered elapsed-time windows.
#'
#' @inheritParams get_data
#' @param cols A non-empty, unique character vector of numeric sensor columns to average.
#' @param window The total centered window width. A positive finite number is
#'   interpreted as seconds; a character value is parsed by
#'   [lubridate::as.period()], or supply a lubridate `Period` or `Duration`.
#'   Periods containing years or months are not allowed. Days and smaller units
#'   are treated as fixed elapsed durations.
#' @param participant_id A vector identifying one or more participants (stored
#'   as unsigned integers; integer, numeric, or character values are accepted).
#'
#' @details For a target observation at time `t`, the closed window is
#'   `[t - window / 2, t + window / 2]`. Every source observation in that
#'   interval contributes once, so the result is sample-weighted rather than
#'   time-weighted and is suitable for irregularly sampled data. A row 80 seconds
#'   after a target is excluded by `window = 60`, even if it is the next row.
#'
#'   Missing measurements are ignored as in SQL `AVG`; if a window contains no
#'   non-missing values, the result is `NA_real_`. Duplicate timestamps remain
#'   separate observations and each target row receives one result. Windows are
#'   partitioned by participant and never combine participants.
#'
#'   Participant and date filters are applied by [get_data()] before window
#'   membership is calculated. Rows outside those filters cannot contribute to
#'   a boundary target. Character and `Date` bounds select whole days under
#'   [get_data()]'s timezone rules; `POSIXt` bounds select exact instants.
#'   Window membership uses the selected view's `time`: UTC instants for
#'   physical sensors and `_with_local` views, wall-clock values for `_local`
#'   views. Use UTC time when windows must reflect elapsed seconds across
#'   daylight-saving transitions.
#'
#'   The result is a lazy dbplyr table with exactly `participant_id`, `datetime`
#'   (the target `time`), and the requested averages in `cols` order. No sensor
#'   observations are collected until the caller requests them.
#'
#' @returns A lazy table with one row per filtered sensor observation.
#' @export
#'
#' @examples
#' \dontrun{
#' local({
#'   db <- create_db(path = NULL, db_name = ":memory:")
#'   on.exit(close_db(db), add = TRUE)
#'
#'   DBI::dbExecute(
#'     db,
#'     "INSERT INTO raw.Accelerometer
#'        (participant_id, time, x_mean, source_file_id, source_row_id,
#'         source_measurement_id)
#'      VALUES
#'        (12345, TIMESTAMPTZ '2024-01-01 00:00:00+00', 1, 1, 1, 1),
#'        (12345, TIMESTAMPTZ '2024-01-01 00:00:10+00', 2, 1, 2, 1),
#'        (12345, TIMESTAMPTZ '2024-01-01 00:01:30+00', 3, 1, 3, 1)"
#'   )
#'
#'   # At 00:00:10, the 00:01:30 row is outside the centered 60-second window.
#'   moving_average(
#'     db,
#'     sensor = "Accelerometer",
#'     cols = "x_mean",
#'     window = 60,
#'     participant_id = 12345
#'   ) |>
#'     dplyr::collect()
#' })
#' }
moving_average <- function(
  db,
  sensor,
  cols,
  window,
  participant_id = NULL,
  start_date = NULL,
  end_date = NULL
) {
  lifecycle::signal_stage("experimental", "moving_average()")
  window_seconds <- .moving_average_window_seconds(window)
  check_arg(cols, "character")
  if (
    length(cols) == 0L ||
      anyNA(cols) ||
      any(!nzchar(cols)) ||
      anyDuplicated(cols) > 0L
  ) {
    cli_abort(
      "{.arg cols} must be a non-empty vector of unique, non-missing column names.",
      arg = "cols"
    )
  }

  filtered_data <- get_data(db, sensor, participant_id, start_date, end_date)
  sensor_columns <- DBI::dbGetQuery(
    db,
    "SELECT column_name, data_type
     FROM information_schema.columns
     WHERE table_schema = 'main' AND lower(table_name) = lower(?)
     ORDER BY ordinal_position",
    params = list(sensor)
  )
  numeric_columns <- grepl(
    "^(U?TINYINT|U?SMALLINT|U?INTEGER|U?BIGINT|U?HUGEINT|FLOAT|REAL|DOUBLE|DECIMAL|NUMERIC|BIGNUM)([[:space:]]|\\(|$)",
    toupper(sensor_columns$data_type)
  )
  invalid_cols <- cols[
    !(cols %in% sensor_columns$column_name[numeric_columns]) |
      cols %in% c("participant_id", "time")
  ]
  if (length(invalid_cols) > 0L) {
    cli_abort(
      c(
        "{.arg cols} must name existing numeric measurement columns, excluding {.var participant_id} and {.var time}.",
        "x" = "Invalid column{?s}: {.field {invalid_cols}}."
      ),
      arg = "cols"
    )
  }

  data <- filtered_data |>
    select("participant_id", "time", all_of(cols))

  # dbplyr's numeric window frames render ROWS, so use a quoted DuckDB RANGE frame.
  quoted_participant <- as.character(DBI::dbQuoteIdentifier(db, "participant_id"))
  quoted_time <- as.character(DBI::dbQuoteIdentifier(db, "time"))
  quoted_datetime <- as.character(DBI::dbQuoteIdentifier(db, "datetime"))
  quoted_source <- as.character(DBI::dbQuoteIdentifier(db, "moving_average_source"))
  quoted_cols <- as.character(DBI::dbQuoteIdentifier(db, cols))
  quoted_half_window <- as.character(DBI::dbQuoteLiteral(db, window_seconds / 2))
  window_expressions <- sprintf(
    paste(
      "AVG(%s) OVER (PARTITION BY %s ORDER BY epoch(%s)",
      "RANGE BETWEEN %s PRECEDING AND %s FOLLOWING) AS %s"
    ),
    quoted_cols,
    quoted_participant,
    quoted_time,
    quoted_half_window,
    quoted_half_window,
    quoted_cols
  )

  select_expressions <- c(
    quoted_participant,
    sprintf("%s AS %s", quoted_time, quoted_datetime),
    paste(window_expressions, collapse = ", ")
  )

  query <- sprintf(
    "SELECT %s FROM (%s) AS %s",
    paste(select_expressions, collapse = ", "),
    dbplyr::sql_render(data),
    quoted_source
  )

  tbl(db, dbplyr::sql(query))
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
#'   Note that the \code{from} and \code{to} columns in the output are POSIXct timestamps in UTC
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
#' @param x A database connection, a lazy table, or a data frame containing at least the columns
#'   `participant_id` and `time`. A data frame must store `time` as `POSIXct`.
#' @param participant_id A single participant identifier (stored as an unsigned integer; an
#'   integer, numeric, or character value is accepted). Leave empty to include all participants.
#' @param min_gap The minimum time (in seconds) passed between two subsequent measurements for it to
#'   be considered a gap.
#' @param sensor One or multiple sensors. See \link[mpathsenser]{sensors} for a list of available
#'   sensors. The default `NULL` uses every sensor, which gives the most accurate gaps but takes
#'   longer; pass a single high-frequency sensor such as `"Accelerometer"` for a quick scan. Sensor
#'   views such as `"Accelerometer_local"` are not accepted.
#' @param ... Not used.
#'
#' @returns For a database connection or a lazy table, a lazy table containing the time period of
#'   the gaps. Call \code{\link[dplyr]{collect}} to load it into R. For a data frame, a tibble. The
#'   structure is as follows:
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
#' gaps <- identify_gaps(db, "12345", min_gap = 60) |>
#'   collect() |>
#'   mutate(across(c(to, from), ymd_hms)) |>
#'   mutate(across(c(to, from), with_tz, "Europe/Brussels"))
#'
#' # Get some sensor data and calculate a statistic, e.g. the time spent walking
#' # You can also do this with larger intervals, e.g. the time spent walking per hour
#' walking_time <- get_data(db, "Activity", "12345") |>
#'   collect() |>
#'   mutate(datetime = ymd_hms(paste(date, time))) |>
#'   mutate(datetime = with_tz(datetime, "Europe/Brussels")) |>
#'   arrange(datetime) |>
#'   mutate(prev_time = lag(datetime)) |>
#'   mutate(duration = datetime - prev_time) |>
#'   filter(type == "WALKING")
#'
#' # Find out if a gap occurs in the time intervals
#' walking_time |>
#'   rowwise() |>
#'   mutate(gap = any(gaps$from >= prev_time & gaps$to <= datetime))
#' }
identify_gaps <- function(x, participant_id = NULL, min_gap = 60, ...) {
  check_arg(participant_id, c("character", "numeric"), allow_null = TRUE)
  check_arg(min_gap, "numeric", n = 1)
  UseMethod("identify_gaps")
}

#' @rdname identify_gaps
#' @export
identify_gaps.default <- function(x, participant_id = NULL, min_gap = 60, ...) {
  cli_abort(
    "{.arg x} must be a database connection, a lazy table, or a data frame."
  )
}

#' @rdname identify_gaps
#' @export
identify_gaps.duckdb_connection <- function(
  x,
  participant_id = NULL,
  min_gap = 60,
  sensor = NULL,
  ...
) {
  check_db(x)

  # NULL means every sensor; view names are rejected because their wall-clock
  # timestamps would change gap durations across a DST change.
  sensors <- check_sensors(sensor, allow_null = TRUE, include_views = FALSE, resolve = TRUE)

  data <- sensors |>
    map(~ get_data(x, .x, participant_id) |> select("participant_id", "time")) |>
    reduce(dplyr::union)

  .identify_gaps_impl(data, min_gap)
}

#' @rdname identify_gaps
#' @export
identify_gaps.tbl_sql <- function(x, participant_id = NULL, min_gap = 60, ...) {
  .check_gap_columns(x)
  x <- .filter_gap_participant(x, participant_id)
  .identify_gaps_impl(x, min_gap)
}

#' @rdname identify_gaps
#' @export
identify_gaps.data.frame <- function(x, participant_id = NULL, min_gap = 60, ...) {
  .check_gap_columns(x)
  if (!inherits(x$time, "POSIXt")) {
    cli_abort("{.arg x} must store {.arg time} as {.cls POSIXct}.")
  }
  x <- .filter_gap_participant(x, participant_id)
  .identify_gaps_impl(x, min_gap)
}

# One gap per participant between consecutive measurements. `epoch()` is
# DuckDB-side and `difftime()` local; the shared pipeline keeps the two paths
# identical beyond that.
.identify_gaps_impl <- function(data, min_gap) {
  data <- if (.is_lazy_tbl(data)) {
    window_order(data, .data$participant_id, .data$time)
  } else {
    arrange(data, .data$participant_id, .data$time)
  }

  data |>
    group_by(.data$participant_id) |>
    mutate(to = lead(.data$time)) |>
    ungroup() |>
    .add_gap_duration() |>
    filter(.data$gap >= min_gap) |>
    mutate(from = .data$time) |>
    select("participant_id", "from", "to", "gap") |>
    # A top-level arrange() gives the lazy query its ORDER BY; window_order()
    # only orders the window frame, leaving the result order plan-dependent.
    arrange(.data$participant_id, .data$from, .data$to)
}

.add_gap_duration <- function(data) {
  if (.is_lazy_tbl(data)) {
    mutate(data, gap = as.numeric(epoch(.data$to) - epoch(.data$time)))
  } else {
    mutate(data, gap = as.numeric(difftime(.data$to, .data$time, units = "secs")))
  }
}

.check_gap_columns <- function(data) {
  missing <- setdiff(c("participant_id", "time"), colnames(data))
  if (length(missing) > 0) {
    cli_abort("{.arg data} must contain the column{?s} {.var {missing}}.")
  }

  invisible(data)
}

.filter_gap_participant <- function(data, participant_id) {
  if (is.null(participant_id)) {
    return(data)
  }

  filter(data, .data$participant_id %in% as.character(participant_id))
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
#' @param data A data frame or a lazy table containing the data. See [get_data()] for retrieving
#'   data from an mpathsenser database. A lazy table produces a lazy result, a data frame a tibble.
#'   On the lazy path, naive `TIMESTAMP` columns (as dbplyr writes copied local data) are treated
#'   as UTC wall-clock time; [get_data()] tables use `TIMESTAMPTZ` and are read as exact instants.
#' @param gaps A data frame or a lazy table containing the gap data. See [identify_gaps()] for
#'   retrieving gap data from an mpathsenser database. It should at least contain the columns `from`
#'   and `to` (both in a date-time format), as well as any specified columns in `by`. It must come
#'   from the same source as `data`, or `copy = TRUE` must be set.
#' @param by A character vector indicating the variable(s) to match by, typically the participant
#'   IDs. If `NULL`, the default, the join uses all columns the two inputs have in common; when
#'   they share none, the call errors and asks for `by`.
#' @param continue Whether to continue the measurement(s) prior to the gap once the gap ends.
#' @param fill A named list of the columns to fill with default values for the extra measurements
#'   that are added because of the gaps. On the lazy path, `POSIXct` values keep their instant,
#'   assuming the target column is `TIMESTAMPTZ` (as [get_data()] returns) or naive UTC wall-clock
#'   time.
#' @param copy Whether to copy the local side when `data` and `gaps` come from different sources.
#'   Defaults to `FALSE`, which errors for mixed inputs.
#'
#' @seealso [identify_gaps()] for finding gaps in the sampling; [link_gaps()] for linking gaps to
#'   ESM data, analogous to [link()].
#'
#' @returns A lazy table containing the data and the added gaps when `data` is a lazy table, and a
#'   tibble when `data` is a data frame. A lazy result is ordered by the `by` columns and then
#'   `time`; rows that share those values (for example a measurement and a gap row at the same
#'   timestamp) may come back in either order.
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
#' # You can use fill if you want to get rid of those pesky NA's
#' add_gaps(
#'   data = dat,
#'   gaps = gaps,
#'   by = "participant_id",
#'   fill = list(type = "GAP", confidence = 100)
#' )

add_gaps <- function(data, gaps, by = NULL, continue = FALSE, fill = NULL, copy = FALSE) {
  check_arg(data, c("data.frame", "tbl_sql"))
  check_arg(gaps, c("data.frame", "tbl_sql"))
  check_arg(by, "character", allow_null = TRUE)
  check_arg(continue, "logical")
  check_arg(fill, "list", allow_null = TRUE)
  check_arg(copy, "logical")

  lazy <- .is_lazy_tbl(data)
  gaps <- .normalize_gap_source(data, gaps, lazy, copy)
  by <- .resolve_gap_by(data, gaps, by)

  # POSIXct fill values must be identified before they become SQL literals.
  posix_fill <- names(fill)[vapply(fill, inherits, logical(1), "POSIXt")]
  fill <- .prepare_gap_fill(fill, data)

  # dbplyr writes POSIXct as naive UTC-clock TIMESTAMP while package tables are
  # TIMESTAMPTZ; re-anchor both sides at UTC so neither is shifted by DuckDB's
  # session timezone when they are appended or joined.
  data <- .normalize_gap_timestamps(data, c("time", posix_fill), lazy)
  gaps <- .normalize_gap_timestamps(gaps, c("from", "to"), lazy)

  # Gaps without a matching stream in the data are dropped.
  gaps <- dplyr::semi_join(gaps, data, by = by) |>
    mutate(.gaps_id = row_number())

  visible <- unique(c(colnames(data), names(fill)))

  # Every gap gets a start row carrying the fill values, and with
  # `continue = TRUE` an end row copying the measurements at the latest data
  # timestamp at or before `to`. `.gaps_rank` orders rows at equal timestamps:
  # 0 for data, 2 * gap id for a gap start, 2 * gap id + 1 for its
  # continuation, so data rows come first and chained gaps keep their order.
  out <- .append_gap_rows(
    .align_gap_columns(data |> mutate(.gaps_rank = 0L), visible, lazy),
    .align_gap_columns(
      gaps |>
        transmute(
          !!!rlang::syms(by),
          time = .data$from,
          !!!fill,
          .gaps_rank = 2L * .data$.gaps_id
        ),
      visible,
      lazy
    ),
    lazy
  )

  if (continue) {
    out <- .append_gap_rows(
      out,
      .align_gap_columns(.continue_gaps(data, gaps, by, lazy), visible, lazy),
      lazy
    )
  }

  .order_gap_output(out, by, lazy, continue)
}

# `data` decides the result type. A mixed pair is moved onto one source once;
# lazy data copies the local gaps into DuckDB, local data collects lazy gaps.
.normalize_gap_source <- function(data, gaps, lazy, copy) {
  if (lazy == .is_lazy_tbl(gaps)) {
    return(gaps)
  }

  if (!copy) {
    cli_abort(
      "{.arg data} and {.arg gaps} must come from the same source. Set \\
      {.arg copy} to {.code TRUE} to move the local table."
    )
  }

  if (lazy) {
    dplyr::auto_copy(data, gaps, copy = TRUE)
  } else {
    collect(gaps)
  }
}

# dbplyr renders POSIXct literals as naive timestamps that DuckDB casts with
# the session timezone; an epoch literal keeps the instant.
.prepare_gap_fill <- function(fill, data) {
  if (is.null(fill) || !.is_lazy_tbl(data)) {
    return(fill)
  }

  db <- dbplyr::remote_con(data)
  lapply(fill, function(value) {
    if (inherits(value, "POSIXt")) .timestamp_sql_literal(value, db) else value
  })
}

# dbplyr renders POSIXct values as naive TIMESTAMP of their UTC clock, whereas
# package tables use TIMESTAMPTZ. Anchoring both at UTC keeps the same instant
# regardless of the DuckDB session timezone. Local data is already POSIXct.
.normalize_gap_timestamps <- function(data, columns, lazy) {
  if (!lazy) {
    return(data)
  }

  columns <- intersect(columns, colnames(data))
  for (column in columns) {
    data <- mutate(
      data,
      !!rlang::sym(column) := make_timestamptz(epoch_us(.data[[column]]))
    )
  }

  data
}

# rows_append() requires the same columns; bind_rows() aligns locally.
.align_gap_columns <- function(data, visible, lazy) {
  if (!lazy) {
    return(data)
  }

  missing <- setdiff(visible, colnames(data))
  if (length(missing) > 0) {
    data <- mutate(data, !!!rlang::rep_named(missing, list(NA)))
  }

  select(data, all_of(visible), ".gaps_rank")
}

.append_gap_rows <- function(data, gaps, lazy) {
  if (lazy) {
    dplyr::rows_append(data, gaps)
  } else {
    bind_rows(data, gaps)
  }
}

.order_gap_output <- function(out, by, lazy, continue) {
  if (lazy) {
    # The final arrange() must be top-level; dbplyr drops ORDER BY elsewhere.
    out |>
      select(-".gaps_rank") |>
      distinct() |>
      arrange(across(all_of(by)), .data$time)
  } else {
    out <- out |>
      arrange(across(all_of(by)), .data$time, .data$.gaps_rank) |>
      select(-".gaps_rank")

    if (continue) {
      # The previous implementation put the join columns and time first for
      # continuations; keep that order for compatibility.
      key_columns <- colnames(out)[colnames(out) %in% c(by, "time")]
      out <- select(out, all_of(key_columns), all_of(setdiff(colnames(out), key_columns)))
    }

    out |>
      distinct() |>
      as_tibble()
  }
}

# Resolve `by`, performing the natural join when it is NULL.
.resolve_gap_by <- function(data, gaps, by) {
  if (is.null(by)) {
    by <- intersect(colnames(data), colnames(gaps))
    if (length(by) == 0) {
      cli_abort(
        "No columns are shared between {.arg data} and {.arg gaps}; supply {.arg by}."
      )
    }
    return(by)
  }

  if (length(setdiff(by, intersect(colnames(data), colnames(gaps)))) > 0) {
    cli_abort(
      "Column{?s} {.code {by}} must be present in both {.arg data} and {.arg gaps}."
    )
  }

  by
}

# A continuation copies the measurements at the latest data timestamp at or
# before each gap end. Data rows sort before gap ends at equal timestamps, so
# a measurement exactly at `to` is the one continued and `distinct()` collapses
# the duplicate it produces. A gap with no prior measurement keeps NA.
.continue_gaps <- function(data, gaps, by, lazy) {
  gap_ends <- gaps |>
    transmute(
      !!!rlang::syms(by),
      time = .data$to,
      .gaps_is_end = 1L,
      .gaps_to = .data$to,
      .gaps_id = .data$.gaps_id
    )
  data_times <- data |>
    distinct(across(all_of(by)), .data$time) |>
    mutate(.gaps_is_end = 0L, .gaps_prev_time = .data$time)
  if (lazy) {
    # rows_append() does not accept columns the other side lacks.
    data_times <- mutate(data_times, .gaps_to = NA, .gaps_id = NA)
  }

  events <- .append_gap_rows(data_times, gap_ends, lazy)

  events <- if (lazy) {
    events |>
      group_by(across(all_of(by))) |>
      window_order(.data$time, .data$.gaps_is_end)
  } else {
    events |>
      arrange(across(all_of(by)), .data$time, .data$.gaps_is_end) |>
      group_by(across(all_of(by)))
  }

  events |>
    tidyr::fill(.gaps_prev_time, .direction = "down") |>
    ungroup() |>
    filter(.data$.gaps_is_end == 1L) |>
    left_join(data, by = c(by, .gaps_prev_time = "time"), relationship = "many-to-many") |>
    mutate(time = .data$.gaps_to, .gaps_rank = 2L * .data$.gaps_id + 1L) |>
    select(-c(".gaps_is_end", ".gaps_prev_time", ".gaps_to", ".gaps_id"))
}
