link_impl <- function(x, y, by, offset_before, offset_after, add_before, add_after) {

  # Match sensing data with ESM using a nested join
  # Set a start_time (beep time - offset) and an end_time (beep time)
  data <- x %>%
    dplyr::mutate(x_time = as.integer(.data$time)) %>%
    dplyr::mutate(start_time = .data$x_time - offset_before) %>%
    dplyr::mutate(end_time = .data$x_time + offset_after) %>%
    dplyr::select(by, .data$start_time, .data$end_time) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(row_id = dplyr::row_number()) # For rematching later

  # Filter y based on by
  y <- dplyr::semi_join(y, data, by = by)

  data <- data %>%
    dplyr::left_join(y, by = by) %>%
    dplyr::mutate(y_time = as.integer(.data$time))

  # The main data, i.e. data within the interval
  data_main <- data %>%
    dplyr::filter(.data$y_time >= .data$start_time & .data$y_time <= .data$end_time) %>%
    dplyr::arrange(by, .data$y_time) %>%
    dplyr::select(-.data$y_time) %>%
    tidyr::nest(data = -c(by, .data$start_time, .data$end_time, .data$row_id)) %>%
    dplyr::select(.data$row_id, .data$data)

  # Merge back with original data
  # Bug: if this happens after merging data_before and data_after, they would be lost in the case
  # no data was retained in data_main as all the row_ids are deleted as well
  data_main <- x %>%
    tibble::as_tibble() %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    dplyr::left_join(data_main, by = "row_id")

  # Add the last measurement before start_time
  if (add_before) {
    tz <- attr(x$time, "tz")
    data_before <- data %>%
      dplyr::filter(.data$y_time < .data$start_time) %>%
      dplyr::group_by(.data$row_id) %>%
      dplyr::slice_max(.data$y_time, with_ties = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(original_time = .data$time) %>%
      dplyr::mutate(time = lubridate::as_datetime(.data$start_time, tz = tz)) %>%
      dplyr::select(-.data$y_time) %>%
      tidyr::nest(data_before = !c(by, .data$start_time, .data$end_time, .data$row_id)) %>%
      dplyr::select(.data$row_id, .data$data_before)

    # Add to the main result
    data_main <- data_main %>%
      dplyr::left_join(data_before, by = "row_id") %>%
      dplyr::mutate(data = purrr::map2(.data$data_before, .data$data, dplyr::bind_rows)) %>%
      dplyr::select(-.data$data_before)
  }

  # Add the first measurements after end_time
  if (add_after) {
    tz <- attr(x$time, "tz")
    data_after <- data %>%
      dplyr::filter(.data$y_time > .data$end_time) %>%
      dplyr::group_by(.data$row_id) %>%
      dplyr::slice_min(.data$y_time, with_ties = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(original_time = .data$time) %>%
      dplyr::mutate(time = lubridate::as_datetime(.data$end_time, tz = tz)) %>%
      dplyr::select(-.data$y_time) %>%
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
  proto <- tibble::as_tibble(y[0, setdiff(colnames(y), by)])
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

  res <- data_main %>%
    dplyr::mutate(data = ifelse(lapply(.data$data, is.null), list(proto), .data$data)) %>%
    dplyr::select(-.data$row_id)

  res
}

#' Link y to the time scale of x
#'
#' @description `r lifecycle::badge("stable")`
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
#'   before or after the interval of the measurement. For example, at \code{offset_before = "30
#'   minutes"}, the data may indicate that a participant was running 20 minutes before a measurement
#'   in \code{x}, However, with just that information there is no way of knowing what the
#'   participant was doing the first 10 minutes of the interval. The same principle applies to after
#'   the interval. When \code{add_before} is set to \code{TRUE}, the last measurement of \code{y}
#'   occurring before the interval of \code{x} is added to the output data as the first row, having
#'   the **\code{time} of \code{x - offset_before}** (i.e. the start of the interval). When
#'   \code{add_after} is set to \code{TRUE}, the first measurement of \code{y} occurring after the
#'   interval of \code{x} is added to the output data as the last row, having the **\code{time} of
#'   \code{x + offset_after}** (i.e. the end of the interval). This way, it is easier to calculate
#'   the difference to other measurements of \code{y} later (within the same interval).
#'   Additionally, an extra column (\code{original_time}) is added in the nested \code{data} column,
#'   which is the original time of the \code{y} measurement and \code{NULL} for every other
#'   observation. This may be useful to check if the added measurement isn't too distant (in time)
#'   from the others. Note that multiple rows may be added if there were multiple measurements in
#'   \code{y} at exactly the same time.
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
  if (!is.data.frame(x)) {
    stop("x must be a data frame", call. = FALSE)
  }
  if (!is.data.frame(y)) {
    stop("y must be a data frame", call. = FALSE)
  }
  if (!is.null(by) && !is.character(by)) {
    stop("by must be a character vector of variables to join by", call. = FALSE)
  }

  # offset checks
  if ((is.null(offset_before) | all(offset_before == 0)) &
      (is.null(offset_after) | all(offset_after == 0))) {
    stop("either offset_before or offset_after must not be 0 or NULL, or both", call. = FALSE)
  }
  if (!is.null(offset_before) && !(is.character(offset_before) |
                                   lubridate::is.period(offset_before) |
                                   is.numeric(offset_before))) {
    stop("offset_before must be a character vector, numeric vector, or a period", call. = FALSE)
  }
  if (!is.null(offset_after) && !(is.character(offset_after) |
                                  lubridate::is.period(offset_after) |
                                  is.numeric(offset_after))) {
    stop("offset_before must be a character vector, numeric vector, or a period", call. = FALSE)
  }
  if (is.character(offset_before) | is.numeric(offset_before)) {
    offset_before <- lubridate::as.period(offset_before)
    offset_before <- as.integer(as.double(offset_before))
  }
  if (is.character(offset_after) | is.numeric(offset_after)) {
    offset_after <- lubridate::as.period(offset_after)
    offset_after <- as.integer(as.double(offset_after))
  }
  if (is.na(offset_before) | is.na(offset_after)) {
    stop(paste(
      "Invalid offset specified. Try something like '30 minutes', ",
      "lubridate::minutes(30), or 1800."
    ), call. = FALSE)
  }
  if (!is.null(offset_before) && offset_before < 0) {
    offset_before <- abs(offset_before)
    warning(paste(
      "offset_before must be a positive period (i.e. greater than 0).",
      "Taking the absolute value"
    ), call. = FALSE)
  }
  if (!is.null(offset_after) && offset_after < 0) {
    offset_after <- abs(offset_after)
    warning(paste(
      "offset_after must be a positive period (i.e. greater than 0).",
      "Taking the absolute value"
    ), call. = FALSE)
  }


  # Check for time column
  if (!("time" %in% colnames(x) & "time" %in% colnames(y))) {
    stop("column 'time' must be present in both x and y", call. = FALSE)
  }
  if (!lubridate::is.POSIXct(x$time)) {
    stop("column 'time' in x must be a POSIXct", call. = FALSE)
  }
  if (!lubridate::is.POSIXct(y$time)) {
    stop("column 'time' in y must be a POSIXct", call. = FALSE)
  }

  # Do not perform matching when x and y are identical
  if (identical(x, y) || isTRUE(dplyr::all_equal(x, y))) {
    stop("x and y are identical", call. = FALSE)
  }

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
                      .options = furrr::furrr_options(seed = TRUE)
    ) %>%
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
  if (!DBI::dbIsValid(db)) {
    stop("Database connection is not valid")
  }
  if (is.null(external) & is.null(sensor_two)) {
    stop("either a second sensor or an external data frame must be supplied")
  }
  if (!is.null(external) & !is.null(sensor_two)) {
    stop("only a second sensor or an external data frame can be supplied, but not both")
  }
  if (!is.null(external) && !is.data.frame(external)) {
    stop("external must be a data frame")
  }
  if (!is.null(sensor_two) && !is.character(sensor_two)) {
    stop("sensor_two must be a character vector")
  }

  # See if data is not incredibly large
  if (!ignore_large) {
    n <- sum(
      get_nrows(db, c(sensor_one, sensor_two), participant_id, start_date, end_date),
      nrow(external)
    )
    if (n > 1e+05) {
      stop("the total number of rows is higher than 100000. Use ignore_large = TRUE to continue")
    }
  }

  if (!is.null(sensor_two)) {
    dat_two <- get_data(db, sensor_two, participant_id, start_date, end_date) %>%
      dplyr::mutate(time = paste(date, time)) %>%
      dplyr::select(-date) %>%
      dplyr::collect() %>%
      dplyr::mutate(time = as.POSIXct(time, format = "%F %H:%M:%OS", tz = "UTC"))
  } else {
    if (any(format(external$time, "%Z") != "UTC")) {
      warning(paste0(
        "external data is not using UTC as a time zone, unlike the data in the ",
        "database. Consider converting the time column to UTC."
      ), call. = FALSE)
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
    dplyr::mutate(time = as.POSIXct(time, format = "%F %H:%M:%OS", "UTC"))


  if (is.null(external) & reverse) {
    tmp <- dat_one
    dat_one <- dat_two
    dat_two <- tmp
  } else if (!is.null(external) & !reverse) {
    tmp <- dat_one
    dat_one <- external
    dat_two <- tmp
  }

  link(
    x = dat_one,
    y = dat_two,
    by = "participant_id",
    offset_before = offset_before,
    offset_after = offset_after,
    add_before = add_before,
    add_after = add_after
  )
}

#' Link gaps to (ESM) data
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'  Gaps in mobile sensing data typically occur when the app is stopped by the operating system or
#'  the user. While small gaps may not pose problems with analyses, greater gaps may cause bias or
#'  skew your data. As a result, gap data should be considered in order to inspect and limit their
#'  influence. This function, like \link[mpathsenser]{link}, allows you to connect gaps to other
#'  data (usually ESM/EMA data) within a user-specified time range.
#'
#' @param data A data frame or an extension to a data frame (e.g. a tibble). While gap data can be
#'   linked to any other type of data, ESM data is most commonly used.
#' @param gaps A data frame (extension) containing the gap data. See
#'   \link[mpathsenser]{identify_gaps} for retrieving gap data from an mpathsenser database. It
#'   should at least contain the columns \code{from} and \code{to} (both in a date-time format), as
#'   well as any specified columns in \code{by}.
#' @inheritParams link
#' @param raw_data Whether to include the raw data (i.e. the matched gap data) to the output as
#'   gap_data.
#'
#' @seealso \code{\link[mpathsenser]{bin_data}} for linking two sets of intervals to each other;
#' \code{\link[mpathsenser]{identify_gaps}} for finding gaps in the sampling;
#' \code{\link[mpathsenser]{add_gaps}} for adding gaps to data;
#'
#' @return The original \code{data} with an extra column \code{duration} indicating the gap during
#'   within the interval in seconds (if \code{duration}  is \code{TRUE}), or an extra column called
#'   \code{gap_data} containing the gaps within the interval. The function ensures all durations and
#'   gap time stamps are within the range of the interval.
#' @export
link_gaps <- function(data, gaps, by = NULL, offset_before = 0, offset_after = 0, raw_data = FALSE) {

  # Argument checking
  if (!is.data.frame(data)) {
    stop("data must be a data frame", call. = FALSE)
  }
  if (!is.data.frame(gaps)) {
    stop("gaps must be a data frame", call. = FALSE)
  }
  if (!is.null(by) && !is.character(by)) {
    stop("by must be a character vector of variables to join by", call. = FALSE)
  }

  # offset checks
  if ((is.null(offset_before) | all(offset_before == 0)) &
      (is.null(offset_after) | all(offset_after == 0))) {
    stop("either offset_before or offset_after must not be 0 or NULL, or both", call. = FALSE)
  }
  if (!is.null(offset_before) && !(is.character(offset_before) |
                                   lubridate::is.period(offset_before) |
                                   is.numeric(offset_before))) {
    stop("offset_before must be a character vector, numeric vector, or a period", call. = FALSE)
  }
  if (!is.null(offset_after) && !(is.character(offset_after) |
                                  lubridate::is.period(offset_after) |
                                  is.numeric(offset_after))) {
    stop("offset_before must be a character vector, numeric vector, or a period", call. = FALSE)
  }

  # Convert offset_before to integer time
  if (is.character(offset_before) | is.numeric(offset_before)) {
    offset_before <- lubridate::as.period(offset_before)
    offset_before <- as.integer(as.double(offset_before))
  }

  # Convert offset_after to integer time
  if (is.character(offset_after) | is.numeric(offset_after)) {
    offset_after <- lubridate::as.period(offset_after)
    offset_after <- as.integer(as.double(offset_after))
  }

  if (is.na(offset_before) | is.na(offset_after)) {
    stop(paste(
      "Invalid offset specified. Try something like '30 minutes', ",
      "lubridate::minutes(30), or 1800."
    ), call. = FALSE)
  }
  if (!is.null(offset_before) && offset_before < 0) {
    offset_before <- abs(offset_before)
    warning(paste(
      "offset_before must be a positive period (i.e. greater than 0).",
      "Taking the absolute value"
    ), call. = FALSE)
  }
  if (!is.null(offset_after) && offset_after < 0) {
    offset_after <- abs(offset_after)
    warning(paste(
      "offset_after must be a positive period (i.e. greater than 0).",
      "Taking the absolute value"
    ), call. = FALSE)
  }

  # Check for time column in data
  if (!("time" %in% colnames(data))) {
    stop("column 'time' must be present in data", call. = FALSE)
  }
  # Check for time column
  if (!("from" %in% colnames(gaps) & "to" %in% colnames(gaps))) {
    stop("column 'from' and 'to' must be present in gaps.", call. = FALSE)
  }
  if (!lubridate::is.POSIXct(data$time)) {
    stop("column 'time' in x must be a POSIXct", call. = FALSE)
  }

  # Check that gap or gap_data is not already present in data
  if ("gap" %in% colnames(data)) {
    stop("column 'gap' should not already be present in data")
  }
  if (raw_data & "gap_data" %in% colnames(data)) {
    stop("column 'gap_data' should not already be present in data")
  }

  # Calculate the start and end time of the interval (in seconds) of each row in data
  # Only retain gaps that are (partially) within or span over the interval
  data_gaps <- data %>%
    dplyr::select(by, .data$time) %>%
    dplyr::mutate(time_int = as.integer(.data$time)) %>%
    dplyr::mutate(start_interval = .data$time_int - offset_before) %>%
    dplyr::mutate(end_interval = .data$time_int + offset_after)

  data_gaps <- data_gaps %>%
    dplyr::left_join(gaps, by = by) %>%
    dplyr::mutate(dplyr::across(c(.data$from, .data$to), as.integer)) %>%
    dplyr::filter(.data$from < .data$end_interval & .data$to > .data$start_interval)

  # Set gaps time stamps out of the interval to the interval's bounds
  data_gaps <- data_gaps %>%
    dplyr::mutate(from = ifelse(.data$from < .data$start_interval,
                                .data$start_interval,
                                .data$from
    )) %>%
    dplyr::mutate(to = ifelse(.data$to > .data$end_interval, .data$end_interval, .data$to)) %>%
    dplyr::mutate(gap = .data$to - .data$from)

  if (raw_data) {
    # Transform from and to back to POSIXct and nest the data
    data_gaps <- data_gaps %>%
      dplyr::select(by, .data$time, .data$from, .data$to, .data$gap) %>%
      dplyr::mutate(from = as.POSIXct(.data$from,
                                      origin = "1970-01-01",
                                      tz = attr(gaps$from, "tzone")
      )) %>%
      dplyr::mutate(to = as.POSIXct(.data$to,
                                    origin = "1970-01-01",
                                    tz = attr(gaps$to, "tzone")
      )) %>%
      dplyr::group_by(dplyr::across(c(by, .data$time))) %>%
      tidyr::nest(gap_data = c(.data$from, .data$to, .data$gap)) %>%
      dplyr::ungroup()

    # Add gaps at beep level
    data_gaps$gap <- vapply(data_gaps$gap_data, function(x) sum(x$gap, na.rm = TRUE),
                            FUN.VALUE = double(1)
    )

    # Create empty data frame in case no results are found
    proto <- tibble::tibble(
      from = as.POSIXct(vector(mode = "double"),
                        origin = "1970-01-01",
                        tz = attr(gaps$from, "tzone")
      ),
      to = as.POSIXct(vector(mode = "double"),
                      origin = "1970-01-01", tz =
                        attr(gaps$from, "tzone")
      ),
      gap = integer(0)
    )
  } else {
    data_gaps <- data_gaps %>%
      dplyr::group_by(dplyr::across(c(by, .data$time))) %>%
      dplyr::summarise(gap = sum(.data$gap), .groups = "drop")
  }

  # Merge with ESM data
  data <- data %>%
    tibble::as_tibble() %>%
    dplyr::left_join(data_gaps, by = c(by, "time")) %>%
    dplyr::mutate(gap = ifelse(is.na(.data$gap), 0, .data$gap))

  if (raw_data) {
    data <- data %>%
      dplyr::mutate(gap_data = ifelse(lapply(.data$gap_data, is.null), list(proto), .data$gap_data))
  }

  data
}

# Link intervals of y within intervals of x
link_intervals <- function(x, x_start, x_end,
                           y, y_start, y_end,
                           by = NULL,
                           name = "data") {
  tz <- attr(dplyr::pull(y, {{ y_start }}), "tz")
  res <- x %>%
    dplyr::left_join(y, by = by) %>%
    dplyr::mutate(dplyr::across(c({{ y_start }}, {{ y_end }}), as.integer)) %>%
    dplyr::filter({{ y_start }} < {{ x_end }} & {{ y_end }} > {{ x_start }})

  # Set gaps time stamps out of the interval to the interval's bounds
  res <- res %>%
    dplyr::mutate(!!rlang::ensym(y_start) := ifelse({{ y_start }} < {{ x_start }},
                                                    {{ x_start }},
                                                    {{ y_start }}
    )) %>%
    dplyr::mutate(!!rlang::ensym(y_end) := ifelse({{ y_end }} > {{ x_end }},
                                                  {{ x_end }},
                                                  {{ y_end }}
    )) %>%
    dplyr::mutate(dplyr::across(c({{ y_start }}, {{ y_end }}), lubridate::as_datetime, tz = tz))

  out <- x %>%
    dplyr::nest_join(res,
                     by = c(
                       by, rlang::expr_text(rlang::call_args(rlang::enquo(x_start))[[2]]),
                       rlang::expr_text(rlang::call_args(rlang::enquo(x_end))[[2]])
                     ),
                     name = name
    )
  out
}

#' Create bins in variable time series
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' In time series with variable measurements, an often recurring task is calculating the total time
#' spent (i.e. the duration) in fixed bins, for example per hour or day. However, this may be
#' difficult when two subsequent measurements are in different bins or span over multiple bins.
#'
#' @param data A data frame or tibble containing the time series.
#' @param start_time The column name of the start time of the interval, a POSIXt.
#' @param end_time The column name of the end time of the interval, a POSIXt.
#' @param by A binning specification.
#' @param fixed Whether to create fixed bins. If \code{TRUE}, bins will be rounded to, for example,
#' whole hours or days (depending on \code{by}). If \code{FALSE}, bins will be created based on the
#' first timestamp.
#'
#' @seealso \code{\link[mpathsenser]{link_gaps}} for linking gaps to data.
#' @return A tibble containing the group columns (if any), date, hour (if \code{by = "hour"}), and
#'   the duration in seconds.
#' @export
#'
#' @examples
#' data <- tibble::tibble(
#'   participant_id = 1,
#'   datetime = c(
#'     "2022-06-21 15:00:00", "2022-06-21 15:55:00",
#'     "2022-06-21 17:05:00", "2022-06-21 17:10:00"
#'   ),
#'   confidence = 100,
#'   type = "WALKING"
#' )
#'
#' # get bins per hour, even if the interval is longer than one hour
#' data %>%
#'   dplyr::mutate(datetime = as.POSIXct(datetime)) %>%
#'   dplyr::mutate(lead = dplyr::lead(datetime)) %>%
#'   bin_data(
#'     start_time = datetime,
#'     end_time = lead,
#'     by = "hour"
#'   )
#'
#' # Alternatively, you can give an integer value to by to create custom-sized
#' # bins, but only if fixed = FALSE. Not that these bins are not rounded to,
#' # as in this example 30 minutes, but rather depends on the earliest time
#' # in the group.
#' data %>%
#'   dplyr::mutate(datetime = as.POSIXct(datetime)) %>%
#'     dplyr::mutate(lead = dplyr::lead(datetime)) %>%
#'     bin_data(
#'       start_time = datetime,
#'       end_time = lead,
#'       by = 1800L,
#'       fixed = FALSE
#'     )
#'
#' # More complicated data for showcasing grouping:
#' data <- tibble::tibble(
#'   participant_id = 1,
#'   datetime = c(
#'     "2022-06-21 15:00:00", "2022-06-21 15:55:00",
#'     "2022-06-21 17:05:00", "2022-06-21 17:10:00"
#'   ),
#'   confidence = 100,
#'   type = c("STILL", "WALKING", "STILL", "WALKING")
#' )
#'
#' # binned_intervals also takes into account the prior grouping structure
#' out <- data %>%
#'   dplyr::mutate(datetime = as.POSIXct(datetime)) %>%
#'   dplyr::mutate(lead = dplyr::lead(datetime)) %>%
#'   dplyr::group_by(participant_id, type) %>%
#'   bin_data(
#'     start_time = datetime,
#'     end_time = lead,
#'     by = "hour"
#'   )
#' print(out)
#'
#' # To get the duration for each bin (note to change the variable names in sum):
#' purrr::map_dbl(out$bin_data, ~ sum(.x$lead - .x$datetime, na.rm = TRUE))
#'
#' # Or:
#' out %>%
#'   tidyr::unnest(bin_data) %>%
#'   dplyr::mutate(duration = .data$lead - .data$datetime) %>%
#'   dplyr::group_by(date, .add = TRUE) %>%
#'   dplyr::summarise(duration = sum(.data$duration), .groups = "drop")
bin_data <- function(data, start_time, end_time, by = c("sec", "min", "hour", "day"), fixed = TRUE) {
  group_vars <- dplyr::group_vars(data)

  if (!is.null(by) && is.character(by)) {
    by <- match.arg(by, c("sec", "min", "hour", "day"))
    by_duration <- c(sec = 1L, min = 60L, hour = 3600L, day = 86400L)
    by_duration <- by_duration[grepl(by, names(by_duration))]
  } else if (is.numeric(by) & !fixed) {
    by_duration <- by
  } else {
    stop("by must be one of 'sec', 'min', 'hour', or 'day', or a numeric value if fixed = FALSE.")
  }

  tz <- attr(dplyr::pull(data, {{ start_time }}), "tz")

  # check that start_time and end_time are a datetime, or try to convert
  if (!lubridate::is.POSIXt(dplyr::pull(data, {{ start_time }})) &
      !lubridate::is.POSIXt(dplyr::pull(data, {{ end_time }}))) {
    data <- data %>%
      dplyr::mutate(!!rlang::ensym(start_time) := as.POSIXct({{ start_time }}, origin = "1970-01-01")) %>%
      dplyr::mutate(!!rlang::ensym(end_time) := as.POSIXct({{ end_time }}, origin = "1970-01-01"))
  }

  # Generate output structure with unique hours per day, keeping the grouping structure
  out <- data %>%
    tidyr::pivot_longer(
      cols = c({{ start_time }}, {{ end_time }}),
      names_to = NULL,
      values_to = "date"
    )

  if (fixed) {
    out <- out %>%
      dplyr::mutate(date = lubridate::floor_date(.data$date, by))
  }

  out <- out %>%
    dplyr::distinct() %>%
    tidyr::drop_na(.data$date) %>%
    dplyr::mutate(date = as.integer(.data$date)) %>%
    dplyr::summarise(date = seq.int(min(.data$date, na.rm = TRUE),
                                    max(.data$date, na.rm = TRUE) + by_duration,
                                    by = by_duration
    ))

  out <- out %>%
    dplyr::mutate(bin_start = .data$date, bin_end = dplyr::lead(.data$date)) %>%
    tidyr::drop_na(.data$bin_end)

  out <- link_intervals(
    x = out, x_start = .data$bin_start, x_end = .data$bin_end,
    y = data, y_start = {{ start_time }}, y_end = {{ end_time }},
    by = group_vars,
    name = "bin_data"
  )

  out <- out %>%
    dplyr::select(-c(.data$bin_start, .data$bin_end)) %>%
    dplyr::mutate(date = lubridate::as_datetime(.data$date, tz = tz)) %>%
    dplyr::rename(bin = .data$date)

  out
}

# #' Bin duration in variable time series
# #'
# #' @description `r lifecycle::badge("deprecated")`
# #'
# #' This function has been deprecated in favour of the more general \link[mpathsenser]{bin_data} for
# #' finding interval data in equally-spaced bins. Therefore, \code{bin_duration} is nothing more than
# #' a wrapper around \link[mpathsenser]{bin_data}.
# #'
# #' In time series with variable measurements, an often recurring task is calculating the total time
# #' spent (i.e. the duration) in fixed bins, for example per hour or day. However, this may be
# #' difficult when two subsequent measurements are in different bins or span over multiple bins.
# #'
# #' @details Note that non-grouped columns are discarded in the output.
# #'
# #' @param data A data frame or tibble containing the time series.
# #' @param start_time The column name of the start time of the interval, a POSIXt.
# #' @param end_time The column name of the end time of the interval, a POSIXt.
# #' @param by A binning specification, either \code{hour} or \code{day}.
# #'
# #' @return A tibble containing the group columns (if any), date, hour (if \code{by = "hour"}), and
# #'   the duration in seconds.
# #' @export
# #'
# #' @examples
# #' data <- tibble::tibble(
# #'   participant_id = 1,
# #'   datetime = c(
# #'     "2022-06-21 15:00:00", "2022-06-21 15:55:00",
# #'     "2022-06-21 17:05:00", "2022-06-21 17:10:00"
# #'   ),
# #'   confidence = 100,
# #'   type = "WALKING"
# #' )
# #'
# #' # get the duration per hour, even if the interval is longer than one hour
# #' data %>%
# #'   dplyr::mutate(datetime = as.POSIXct(datetime)) %>%
# #'   dplyr::mutate(lead = dplyr::lead(datetime)) %>%
# #'   bin_duration(
# #'     start_time = datetime,
# #'     end_time = lead,
# #'     by = "hour"
# #'   )
# #'
# #' data <- tibble::tibble(
# #'   participant_id = 1,
# #'   datetime = c(
# #'     "2022-06-21 15:00:00", "2022-06-21 15:55:00",
# #'     "2022-06-21 17:05:00", "2022-06-21 17:10:00"
# #'   ),
# #'   confidence = 100,
# #'   type = c("STILL", "WALKING", "STILL", "WALKING")
# #' )
# #' # binned_intervals also takes into account the prior grouping structure
# #' data %>%
# #'   dplyr::mutate(datetime = as.POSIXct(datetime)) %>%
# #'   dplyr::mutate(lead = dplyr::lead(datetime)) %>%
# #'   dplyr::group_by(participant_id, type) %>%
# #'   bin_duration(
# #'     start_time = datetime,
# #'     end_time = lead,
# #'     by = "hour"
# #'   )
# bin_duration <- function(data, start_time, end_time, by = c("hour", "day")) {
#   lifecycle::deprecate_warn("1.0.4", "bin_duration()", "bin_data()")
#
#   group_vars <- dplyr::group_vars(data)
#   by <- match.arg(by)
#
#   out <- bin_data(data, {{ start_time }}, {{ end_time }}, by)
#
#   out <- out %>%
#     tidyr::unnest(.data$bin_data, keep_empty = TRUE) %>%
#     dplyr::mutate(duration = {{ end_time }} - {{ start_time }}) %>%
#     dplyr::group_by(.data$bin, .add = TRUE) %>%
#     dplyr::summarise(duration = sum(.data$duration), .groups = "drop") %>%
#     dplyr::rename(date = .data$bin)
#
#   # Make the by column look pretty
#   if (by == "hour") {
#     out <- out %>%
#       dplyr::mutate(hour = lubridate::hour(.data$date)) %>%
#       dplyr::mutate(date = lubridate::date(.data$date)) %>%
#       dplyr::select(dplyr::group_cols(), .data$date, .data$hour, .data$duration)
#   } else if (by == "day") {
#     out <- out %>%
#       dplyr::mutate(date = lubridate::date(.data$date))
#   }
#
#   out
# }