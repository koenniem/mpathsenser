link_impl <- function(x,
                      y,
                      by,
                      start_time,
                      end_time,
                      y_time,
                      offset_before,
                      offset_after,
                      add_before,
                      add_after,
                      name) {

  # Force variables to be evaluated, or somehow it cannot be found later on.
  force(add_before)
  force(add_after)
  force(name)

  # Filter y to keep only `by` instances that occur in x
  if (!rlang::is_null(by) && length(by) != 0) {
    y <- dplyr::semi_join(y, x, by = by)
  }

  # Prepare x
  # Create a row_id or rematching later
  x <- x %>%
    tibble::as_tibble() %>%
    mutate(.row_id = dplyr::row_number())

  # If no end_time is specified, calculate the start and end time of the interval using offset
  # Else, take the start time and end time as specified by the user
  if (is.null(end_time) || length(end_time) == 0) {
    data <- x %>%
      mutate(across(.env$start_time, as.integer, .names = ".x_time")) %>%
      mutate(.start_time = .data$.x_time - offset_before) %>%
      mutate(.end_time = .data$.x_time + offset_after) %>%
      select({{ by }}, ".start_time", ".end_time", ".row_id")
  } else {
    # Ensure column names for x and y do not clash
    # also easier to work with
    data <- x %>%
      dplyr::rename(.start_time = .env$start_time) %>%
      dplyr::rename(.end_time = .env$end_time) %>%
      mutate(.start_time = as.integer(.data$.start_time)) %>%
      mutate(.end_time = as.integer(.data$.end_time)) %>%
      select({{ by }}, ".start_time", ".end_time", ".row_id")
  }

  # Match sensing data with ESM using a left join
  data <- data %>%
    dplyr::left_join(y, by = by, multiple = "all", relationship = "many-to-many") %>%
    mutate(across(dplyr::all_of(y_time), as.integer, .names = ".y_time")) %>%
    tidyr::drop_na(".start_time", ".end_time")

  # The main data, i.e. data exactly within the interval
  data_main <- data %>%
    filter(.data$.y_time >= .data$.start_time & .data$.y_time <= .data$.end_time) %>%
    arrange(across(c({{ by }}, ".y_time"))) %>%
    select(-".y_time") %>%
    nest({{ name }} := !c({{ by }}, ".start_time", ".end_time", ".row_id")) %>%
    select(dplyr::all_of(c(".row_id", name)))

  # Merge back with original data
  # Bug: if this happens after merging data_before and data_after, they would be lost in the case
  # no data was retained in data_main as all the row_ids are deleted as well
  data_main <- x %>%
    dplyr::left_join(data_main, by = ".row_id", multiple = "all", relationship = "many-to-many")

  # Add the last measurement before start_time
  tz <- attr(pull(y, {{ y_time }}), "tz")
  if (add_before) {
    # Calculate in which groups there is a measurement that equals start_time
    equal_to_start <- data %>%
      filter(.data$.y_time == .data$.start_time) %>%
      distinct(.data$.row_id)

    data_before <- data %>%
      dplyr::anti_join(equal_to_start, by = ".row_id") %>%
      filter(.data$.y_time < .data$.start_time) %>%
      group_by(.data$.row_id) %>%
      dplyr::slice_max(order_by = .data$.y_time, n = 1, with_ties = TRUE) %>%
      ungroup() %>%
      mutate(across(dplyr::all_of(y_time), .names = "original_time")) %>%
      mutate({{ y_time }} := lubridate::as_datetime(.data$.start_time, tz = tz)) %>%
      select(-".y_time") %>%
      nest(data_before = !c({{ by }}, ".start_time", ".end_time", ".row_id")) %>%
      select(".row_id", "data_before")

    # Add to the main result
    data_main <- data_main %>%
      dplyr::left_join(data_before, by = ".row_id", multiple = "all", relationship = "many-to-many") %>%
      mutate({{ name }} := purrr::map2(data_before, !!rlang::ensym(name), bind_rows)) %>%
      select(-"data_before")
  }

  # Add the first measurements after end_time
  if (add_after) {
    # Calculate in which groups there is a measurement that equals start_time
    equal_to_end <- data %>%
      filter(.data$.y_time == .data$.end_time) %>%
      distinct(.data$.row_id)

    data_after <- data %>%
      dplyr::anti_join(equal_to_end, by = ".row_id") %>%
      filter(.data$.y_time > .data$.end_time) %>%
      group_by(.data$.row_id) %>%
      dplyr::slice_min(order_by =  .data$.y_time, n = 1, with_ties = TRUE) %>%
      ungroup() %>%
      mutate(across(dplyr::all_of(y_time), .names = "original_time")) %>%
      mutate({{ y_time }} := lubridate::as_datetime(.data$.end_time, tz = tz)) %>%
      select(-".y_time") %>%
      nest(data_after = !c({{ by }}, ".start_time", ".end_time", ".row_id")) %>%
      select(".row_id", "data_after")

    # Add to the main result
    data_main <- data_main %>%
      dplyr::left_join(data_after, by = ".row_id", multiple = "all", relationship = "many-to-many") %>%
      mutate({{ name }} := purrr::map2(!!rlang::ensym(name), data_after, bind_rows)) %>%
      select(-"data_after")
  }

  # Create an empty tibble (prototype) by retrieving rows with time before UNIX start (not possible)
  # This is needed to fill in the data entries where there would otherwise be nothing left
  # because nothing matched within the start_time and end_time
  proto <- tibble::as_tibble(y[0, ]) %>%
    select(-{{ by }})
  if (add_before || add_after) {
    proto$original_time <- as.POSIXct(vector(mode = "double"))

    # In case data_main is empty, applying the solution below leads to NA in the next step causing
    # proto not to be applied (since it's not null)
    if (nrow(data_main) > 0) {
      # Add column original_time in cases where it's missing
      for (i in seq_len(nrow(data_main))) {
        if (!any("original_time" == colnames(pull(data_main, dplyr::all_of(name))[[i]]))) {
          data_main$data[[i]]$original_time <- as.POSIXct(NA, tz = tz)
        }
      }
    }
  }

  res <- data_main %>%
    mutate({{ name }} := ifelse(test = lapply(X = !!rlang::ensym(name),
                                              FUN = \(x) {
                                                is.null(x) || identical(x, NA) || nrow(x) == 0
                                              }),
                                yes = list(proto),
                                no = !!rlang::ensym(name))) %>%
    select(-".row_id")

  res
}

#' Link y to the time scale of x
#'
#' @description `r lifecycle::badge("stable")`
#'
#'   One of the key tasks in analysing mobile sensing data is being able to link it to other data.
#'   For example, when analysing physical activity data, it could be of interest to know how much
#'   time a participant spent exercising before or after an ESM beep to evaluate their stress level.
#'   [link()] allows you to map two data frames to each other that are on different time scales,
#'   based on a pre-specified offset before and/or after. This function assumes that both `x` and
#'   `y` have a column called `time` containing \link[base]{DateTimeClasses}.
#'
#' @details `y` is matched to the time scale of `x` by means of time windows. These time windows are
#'   defined as the period between `x - offset_before` and `x + offset_after`. Note that either
#'   `offset_before` or `offset_after` can be 0, but not both. The "interval" of the measurements is
#'   therefore the associated time window for each measurement of `x` and the data of `y` that also
#'   falls within this period. For example, an `offset_before`  of
#'   \code{\link[lubridate]{minutes}(30)} means to match all data of `y` that occurred *before* each
#'   measurement in `x`. An `offset_after` of 900 (i.e. 15 minutes) means to match all data of `y`
#'   that occurred *after* each measurement in `x`. When both `offset_before` and `offset_after` are
#'   specified, it means all data of `y` is matched in an interval of 30 minutes before and 15
#'   minutes after each measurement of `x`, thus combining the two arguments.
#'
#'   The arguments `add_before` and `add_after` let you decide whether you want to add the last
#'   measurement before the interval and/or the first measurement after the interval respectively.
#'   This could be useful when you want to know which type of event occurred right before or after
#'   the interval of the measurement. For example, at `offset_before = "30 minutes"`, the data may
#'   indicate that a participant was running 20 minutes before a measurement in `x`, However, with
#'   just that information there is no way of knowing what the participant was doing the first 10
#'   minutes of the interval. The same principle applies to after the interval. When `add_before` is
#'   set to `TRUE`, the last measurement of `y` occurring before the interval of `x` is added to the
#'   output data as the first row, having the **`time` of \code{x - offset_before}** (i.e. the start
#'   of the interval). When `add_after` is set to `TRUE`, the first measurement of `y` occurring
#'   after the interval of `x` is added to the output data as the last row, having the **`time` of
#'   `x + offset_after`** (i.e. the end of the interval). This way, it is easier to calculate the
#'   difference to other measurements of `y` later (within the same interval). Additionally, an
#'   extra column (`original_time`) is added in the nested `data` column, which is the original time
#'   of the `y` measurement and `NULL` for every other observation. This may be useful to check if
#'   the added measurement isn't too distant (in time) from the others. Note that multiple rows may
#'   be added if there were multiple measurements in `y` at exactly the same time. Also, if there
#'   already is a row with a timestamp exactly equal to the start of the interval (for `add_before =
#'   TRUE`) or to the end of the interval `(add_after = TRUE`), no extra row is added.
#'
#' @section Warning: Note that setting `add_before` and `add_after` each add one row to each nested
#'   \code{tibble} of the `data` column. Thus, if you are only interested in the total count (e.g.
#'   the number of total screen changes), remember to set these arguments to FALSE or make sure to
#'   filter out rows that do _not_ have an `original_time`. Simply subtracting 1 or 2 does not work
#'   as not all measurements in `x` may have a measurement in `y` before or after (and thus no row
#'   is added).
#'
#'
#' @param x,y A pair of data frames or data frame extensions (e.g. a tibble). Both `x` and `y` must
#'   have a column called `time`.
#' @param by A character vector indicating the variable(s) to match by, typically the participant
#'   IDs. If NULL, the default, `*_join()` will perform a natural join, using all variables in
#'   common across `x` and `y`. Therefore, all data will be mapped to each other based on the time
#'   stamps of `x` and `y`. A message lists the variables so that you can check they're correct;
#'   suppress the message by supplying by explicitly.
#'
#'   To join by different variables on `x` and `y`, use a named vector. For example, `by = c('a' =
#'   'b')` will match `x$a` to `y$b`.
#'
#'   To join by multiple variables, use a vector with `length > 1`. For example, `by = c('a', 'b')`
#'   will match `x$a` to `y$a` and `x$b` to `y$b`. Use a named vector to match different variables
#'   in `x` and `y`. For example, `by = c('a' = 'b', 'c' = 'd')` will match `x$a` to `y$b` and `x$c`
#'   to `y$d`.
#'
#'   To perform a cross-join (when `x` and `y` have no variables in common), use `by = character()`.
#'   Note that the `split` argument will then be set to 1.
#' @param time The name of the column containing the timestamps in `x`.
#' @param end_time Optionally, the name of the column containing the end time in `x`. If specified,
#'   it means `time` defines the start time of the interval and `end_time` the end time. Note that
#'   this cannot be used at the same time as `offset_before` or `offset_after`.
#' @param y_time The name of the column containing the timestamps in `y`.
#' @param offset_before The time before each measurement in `x` that denotes the period in which `y`
#'   is matched. Must be convertible to a period by [lubridate::as.period()].
#' @param offset_after The time after each measurement in `x` that denotes the period in which `y`
#'   is matched. Must be convertible to a period by [lubridate::as.period()].
#' @param add_before Logical value. Do you want to add the last measurement before the start of each
#'   interval?
#' @param add_after Logical value. Do you want to add the first measurement after the end of each
#'   interval?
#' @param name The name of the column containing the nested `y` data.
#' @param split An optional grouping variable to split the computation by. When working with large
#'   data sets, the computation can grow so large it no longer fits in your computer's working
#'   memory (after which it will probably fall back on the swap file, which is very slow). Splitting
#'   the computation trades some computational efficiency for a large decrease in RAM usage. This
#'   argument defaults to `by` to automatically suppress some of its RAM usage.
#'
#' @returns A tibble with the data of `x` with a new column `data` with the matched data of `y`
#'   according to `offset_before` and `offset_after`.
#'
#' @export
#'
#' @examples
#' # Define some data
#' x <- data.frame(
#'   time = rep(seq.POSIXt(as.POSIXct("2021-11-14 13:00:00"), by = "1 hour", length.out = 3), 2),
#'   participant_id = c(rep("12345", 3), rep("23456", 3)),
#'   item_one = rep(c(40, 50, 60), 2)
#' )
#'
#' # Define some data that we want to link to x
#' y <- data.frame(
#'   time = rep(seq.POSIXt(as.POSIXct("2021-11-14 12:50:00"), by = "5 min", length.out = 30), 2),
#'   participant_id = c(rep("12345", 30), rep("23456", 30)),
#'   x = rep(1:30, 2)
#' )
#'
#' # Now link y within 30 minutes before each row in x
#' # until the measurement itself:
#' link(
#'   x = x,
#'   y = y,
#'   by = "participant_id",
#'   time = time,
#'   y_time = time,
#'   offset_before = "30 minutes"
#' )
#'
#' # We can also link y to a period both before and after
#' # each measurement in x.
#' # Also note that time, end_time and y_time accept both
#' # quoted names as well as character names.
#' link(
#'   x = x,
#'   y = y,
#'   by = "participant_id",
#'   time = "time",
#'   y_time = "time",
#'   offset_before = "15 minutes",
#'   offset_after = "15 minutes"
#' )
#'
#' # It can be important to also know the measurements
#' # just preceding the interval or just after the interval.
#' # This adds an extra column called 'original_time' in the
#' # nested data, containing the original time stamp. The
#' # actual timestamp is set to the start time of the interval.
#' link(
#'   x = x,
#'   y = y,
#'   by = "participant_id",
#'   time = time,
#'   y_time = time,
#'   offset_before = "15 minutes",
#'   offset_after = "15 minutes",
#'   add_before = TRUE,
#'   add_after = TRUE
#' )
#'
#' # If you participant_id is not important to you
#' # (i.e. the measurements are interchangeable),
#' # you can ignore them by leaving by empty.
#' # However, in this case we'll receive a warning
#' # since x and y have no other columns in common
#' # (except time, of course). Thus, we can perform
#' # a cross-join:
#' link(
#'   x = x,
#'   y = y,
#'   by = character(),
#'   time = time,
#'   y_time = time,
#'   offset_before = "30 minutes"
#' )
#'
#' # Alternatively, we can specify custom intervals.
#' # That is, we can create variable intervals
#' # without using fixed offsets.
#' x <- data.frame(
#'   start_time = rep(
#'     x = as.POSIXct(c("2021-11-14 12:40:00",
#'                      "2021-11-14 13:30:00",
#'                      "2021-11-14 15:00:00")),
#'     times = 2),
#'   end_time = rep(
#'     x = as.POSIXct(c("2021-11-14 13:20:00",
#'                      "2021-11-14 14:10:00",
#'                      "2021-11-14 15:30:00")),
#'     times = 2),
#'   participant_id = c(rep("12345", 3), rep("23456", 3)),
#'   item_one = rep(c(40, 50, 60), 2)
#' )
#' link(
#'   x = x,
#'   y = y,
#'   by = "participant_id",
#'   time = start_time,
#'   end_time = end_time,
#'   y_time = time,
#'   add_before = TRUE,
#'   add_after = TRUE
#' )
link <- function(x,
                 y,
                 by = NULL,
                 time,
                 end_time = NULL,
                 y_time,
                 offset_before = 0,
                 offset_after = 0,
                 add_before = FALSE,
                 add_after = FALSE,
                 name = "data",
                 split = by) {
  check_arg(x, type = "data.frame")
  check_arg(y, type = "data.frame")
  check_arg(by, type = "character", allow_null = TRUE)
  check_arg(add_before, type = "logical")
  check_arg(add_after, type = "logical")
  check_arg(name, type = "character")

  if (missing(time)) {
    lifecycle::deprecate_warn(
      when = "1.1.2",
      what = "link(time = 'must not be missing')",
      details = c(
        i = paste("Due to backwards compatiblity, `time` defaults to",
                  "'time' for now."),
        i = paste("Please make this argument explicit to prevent your",
                  "code from breaking in a future version.")
      ))
    time <- "time"
  }

  if (missing(y_time)) {
    lifecycle::deprecate_warn(
      when = "1.1.2",
      what = "link(y_time = 'must not be missing')",
      details = c(
        i = paste("Due to backwards compatiblity, `y_time` defaults to",
                  "'time' for now."),
        i = paste("Please make this argument explicit to prevent your",
                  "code from breaking in a future version.")
      ))
    y_time <- "time"
  }

  # Check that not end_time and any offset are used at the same time
  if (!missing(end_time) && (!missing(offset_before) || !missing(offset_after))) {
    abort("`end_time` and `offset_before` or `offset_after` cannot be used at the same time.")
  }

  # Check offsets if end_time is not missing
  if (missing(end_time)) {
    offsets <- check_offset(offset_before, offset_after)
    offset_before <- offsets$offset_before
    offset_after <- offsets$offset_after
  }

  # Do not perform matching when x and y are identical
  if (identical(x, y) || isTRUE(all.equal(x, y))) {
    abort("`x` and `y` are identical.")
  }

  # Get the start_time, end_time, and y_time as characters and check their validity
  x <- ungroup(x)
  y <- ungroup(y)
  start_time <- colnames(select(x, {{ time }}))
  if (!missing(end_time)) {
    end_time <- colnames(select(x, {{ end_time }}))
  }
  y_time <- colnames(select(y, {{ y_time }}))
  by <- colnames(select(x, {{ by }}))

  check_arg(start_time, "character", n = 1)
  check_arg(end_time, "character", n = 1, allow_null = TRUE)
  check_arg(y_time, "character", n = 1)

  # Check the time columns
  check_arg(pull(x, start_time), "POSIXt", arg = "time")
  if (!is.null(end_time)) {
    check_arg(pull(x, end_time), "POSIXt", arg = "end_time")
  }
  check_arg(pull(y, y_time), "POSIXt", arg = "y_time")

  # Split up the data for computation efficiency, either based on a numeric value (fixed group size)
  # or a variable
  if (!is.null(split)) {
    if (is.numeric(split)) {
      x <- split(x, rep(1:split, each = ceiling(nrow(x) / split), length.out = nrow(x)))
    } else {
      x <- dplyr::group_split(x, across({{ by }}))
    }
  } else {
    x <- list(x)
  }

  # Temporarily override future global max size options
  old <- options(future.globals.maxSize = .Machine$double.xmax)
  on.exit(options(old))

  x %>%
    furrr::future_map(
      ~ link_impl(x = .x,
                  y = y,
                  by = by,
                  start_time = start_time,
                  end_time = end_time,
                  y_time = y_time,
                  offset_before = offset_before,
                  offset_after = offset_after,
                  add_before = add_before,
                  add_after = add_after,
                  name = name),
      .options = furrr::furrr_options(seed = TRUE)
    ) %>%
    bind_rows()
}

#' Link two sensors OR one sensor and an external data frame using an mpathsenser database
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#'   This function is specific to mpathsenser databases. It is a wrapper around [link()] but
#'   extracts data in the database for you. It is now soft deprecated as I feel this function's use
#'   is limited in comparison to [link()].
#'
#' @inheritParams get_data
#' @inheritParams link
#' @param sensor_one The name of a primary sensor. See \link[mpathsenser]{sensors} for a list of
#'   available sensors.
#' @param sensor_two The name of a secondary sensor. See \link[mpathsenser]{sensors} for a list of
#'   available sensors. Cannot be used together with `external`.
#' @param external Optionally, specify an external data frame. Cannot be used at the same time as a
#'   second sensor. This data frame must have a column called `time`.
#' @param external_time The name of the column containing the timestamps in `external`.
#' @param reverse Switch `sensor_one` with either `sensor_two` or `external`? Particularly useful in
#'   combination with `external`.
#' @param ignore_large Safety override to prevent long wait times. Set to `TRUE` to do this function
#'   on lots of data.
#'
#' @seealso [link()]
#'
#' @returns A tibble with the data of `sensor_one` with a new column `data` with the matched data of
#'   either `sensor_two` or `external` according to `offset_before` or `offset_after`. The other way
#'   around when `reverse = TRUE`.
#' @export
link_db <- function(db,
                    sensor_one,
                    sensor_two = NULL,
                    external = NULL,
                    external_time = "time",
                    offset_before = 0,
                    offset_after = 0,
                    add_before = FALSE,
                    add_after = FALSE,
                    participant_id = NULL,
                    start_date = NULL,
                    end_date = NULL,
                    reverse = FALSE,
                    ignore_large = FALSE) {
  # Soft deprecate warning
  lifecycle::deprecate_warn("1.1.2", "link_db()", "link()")

  check_db(db)
  check_arg(sensor_one, type = "character", n = 1)
  check_arg(sensor_two, type = "character", n = 1, allow_null = TRUE)
  check_arg(external, type = "data.frame", allow_null = TRUE)
  check_arg(external_time, type = "character", n = 1)
  check_arg(participant_id, type = "character", allow_null = TRUE)
  check_arg(reverse, type = "logical", n = 1)
  check_arg(ignore_large, type = "logical", n = 1)

  if ((is.null(external) && is.null(sensor_two)) || (!is.null(external) && !is.null(sensor_two))) {
    abort("Either a second sensor or an external data frame must be supplied.")
  }

  # See if data is not incredibly large
  if (!ignore_large) {
    n <- sum(
      get_nrows(db, c(sensor_one, sensor_two), participant_id, start_date, end_date),
      nrow(external)
    )
    if (n > 1e+05) {
      abort("the total number of rows is higher than 100000. Use ignore_large = TRUE to continue")
    }
  }

  if (!is.null(sensor_two)) {
    dat_two <- get_data(db, sensor_two, participant_id, start_date, end_date) %>%
      mutate(time = paste(.data$date, .data$time)) %>%
      select(-"date") %>%
      collect() %>%
      mutate(time = as.POSIXct(.data$time, format = "%F %H:%M:%OS", tz = "UTC"))
    y_time <- "time"
  } else {
    check_arg(pull(external, external_time), "POSIXt")
    if (any(format(pull(external, external_time), "%Z") != "UTC")) {
      warn(c(
        "`external` is not using UTC as a time zone, unlike the data in the database.",
        i = "Consider converting the time column to UTC."
      ))
    }

    y_time <- external_time
    dat_two <- external
  }

  # Get dates of dat_two to shrink dat_one as much as possible
  dates <- unique(as.Date(dat_two$time))

  dat_one <- get_data(db, sensor_one, participant_id, start_date, end_date) %>%
    filter(.data$date %in% dates) %>%
    mutate(time = paste(.data$date, .data$time)) %>%
    select(-"date") %>%
    collect() %>%
    mutate(time = as.POSIXct(.data$time, format = "%F %H:%M:%OS", "UTC"))
  time <- "time"

  if (reverse) {
    tmp_time <- time
    time <- y_time
    y_time <- tmp_time
  }

  if (is.null(external) && reverse) {
    tmp <- dat_one
    dat_one <- dat_two
    dat_two <- tmp
  } else if (!is.null(external) && !reverse) {
    tmp <- dat_one
    dat_one <- external
    dat_two <- tmp
  }

  link(
    x = dat_one,
    y = dat_two,
    by = "participant_id",
    time = {{ time }},
    y_time = {{ y_time }},
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
#'   Gaps in mobile sensing data typically occur when the app is stopped by the operating system or
#'   the user. While small gaps may not pose problems with analyses, greater gaps may cause bias or
#'   skew your data. As a result, gap data should be considered in order to inspect and limit their
#'   influence. This function, analogous to [link()], allows you to connect gaps to other data
#'   (usually ESM/EMA data) within a user-specified time range.
#'
#' @param data A data frame or an extension to a data frame (e.g. a tibble). While gap data can be
#'   linked to any other type of data, ESM data is most commonly used.
#' @param gaps A data frame (extension) containing the gap data. See [identify_gaps()] for
#'   retrieving gap data from an mpathsenser database. It should at least contain the columns `from`
#'   and `to` (both in a date-time format), as well as any specified columns in `by`.
#' @inheritParams link
#' @param raw_data Whether to include the raw data (i.e. the matched gap data) to the output as
#'   gap_data.
#'
#' @seealso [bin_data()] for linking two sets of intervals to each other; [identify_gaps()] for
#'   finding gaps in the sampling; [add_gaps()] for adding gaps to sensor data;
#'
#' @returns The original `data` with an extra column `duration` indicating the gap during within the
#'   interval in seconds (if `duration`  is `TRUE`), or an extra column called `gap_data` containing
#'   the gaps within the interval. The function ensures all durations and gap time stamps are within
#'   the range of the interval.
#' @export
link_gaps <- function(data,
                      gaps,
                      by = NULL,
                      offset_before = 0,
                      offset_after = 0,
                      raw_data = FALSE) {
  # Argument checking
  check_arg(data, type = "data.frame")
  check_arg(gaps, type = "data.frame")
  check_arg(by, type = "character", allow_null = TRUE)
  check_arg(raw_data, type = "logical", n = 1)

  offsets <- check_offset(offset_before, offset_after)
  offset_before <- offsets$offset_before
  offset_after <- offsets$offset_after

  # Check for time column in data
  if (!("time" %in% colnames(data))) {
    abort("Column `time` must be present in `data`")
  }
  # Check for time column
  if (!("from" %in% colnames(gaps) && "to" %in% colnames(gaps))) {
    abort("Column `from` and `to` must be present in `gaps`.")
  }
  if (!lubridate::is.POSIXct(data$time)) {
    abort("Column `time` in `data` must be a POSIXct.")
  }

  # Check that gap or gap_data is not already present in data
  if ("gap" %in% colnames(data)) {
    abort("column 'gap' should not already be present in data")
  }
  if (raw_data && "gap_data" %in% colnames(data)) {
    abort("column 'gap_data' should not already be present in data")
  }

  # Calculate the start and end time of the interval (in seconds) of each row in data
  # Only retain gaps that are (partially) within or span over the interval
  data_gaps <- data %>%
    select({{ by }}, "time") %>%
    mutate(time_int = as.integer(.data$time)) %>%
    mutate(start_interval = .data$time_int - offset_before) %>%
    mutate(end_interval = .data$time_int + offset_after)

  data_gaps <- data_gaps %>%
    dplyr::left_join(gaps, by = by, multiple = "all", relationship = "many-to-many") %>%
    mutate(across(c("from", "to"), as.integer)) %>%
    filter(.data$from < .data$end_interval & .data$to > .data$start_interval)

  # Set gaps time stamps out of the interval to the interval's bounds
  data_gaps <- data_gaps %>%
    mutate(from = ifelse(.data$from < .data$start_interval,
      .data$start_interval,
      .data$from
    )) %>%
    mutate(to = ifelse(.data$to > .data$end_interval, .data$end_interval, .data$to)) %>%
    mutate(gap = .data$to - .data$from)

  if (raw_data) {
    # Transform from and to back to POSIXct and nest the data
    data_gaps <- data_gaps %>%
      select({{ by }}, "time", "from", "to", "gap") %>%
      mutate(from = as.POSIXct(.data$from,
        origin = "1970-01-01",
        tz = attr(gaps$from, "tzone")
      )) %>%
      mutate(to = as.POSIXct(.data$to,
        origin = "1970-01-01",
        tz = attr(gaps$to, "tzone")
      )) %>%
      group_by(across(c({{ by }}, "time"))) %>%
      nest(gap_data = c("from", "to", "gap")) %>%
      ungroup()

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
      group_by(across(c({{ by }}, "time"))) %>%
      summarise(gap = sum(.data$gap), .groups = "drop")
  }

  # Merge with ESM data
  data <- data %>%
    tibble::as_tibble() %>%
    dplyr::left_join(data_gaps, by = c(by, "time"), multiple = "all", relationship = "many-to-many") %>%
    mutate(gap = ifelse(is.na(.data$gap), 0, .data$gap))

  if (raw_data) {
    data <- data %>%
      mutate(gap_data = ifelse(lapply(.data$gap_data, is.null), list(proto), .data$gap_data))
  }

  data
}

# Link intervals of y within intervals of x
link_intervals <- function(x, x_start, x_end,
                           y, y_start, y_end,
                           by = NULL,
                           name = "data") {
  check_arg(x, "data.frame")
  check_arg(y, "data.frame")
  check_arg(by, "character", allow_null = TRUE)
  check_arg(name, "character", n = 1)

  tz <- attr(pull(y, {{ y_start }}), "tz")

  # Calculate which values in y are within x's bounds
  if (length(by) == 0 && utils::packageVersion("dplyr") >= "1.1.0") {

    res <- dplyr::cross_join(x, y)
  } else {
    join_by <- dplyr::join_by(
      !!!by
    )

    res <- dplyr::left_join(x, y, by = join_by, multiple = "all", relationship = "many-to-many")
  }

  res <- res %>%
    mutate(across(c({{ y_start }}, {{ y_end }}), as.integer)) %>%
    filter(
      (
        (is.na({{ y_end }} & {{ y_start }} >= {{ x_start }} & {{ y_start }} < {{ x_end }})) &
        (is.na({{ y_start }} & {{ y_end }} >= {{ x_start }} & {{ y_end }} < {{ x_end }}))
      ) |
        ({{ y_start }} < {{ x_end }} & {{ y_end }} > {{ x_start }})
    )

  # Set gaps time stamps out of the interval to the interval's bounds
  res <- res %>%
    mutate({{ y_start }} := ifelse({{ y_start }} < {{ x_start }},
      {{ x_start }},
      {{ y_start }}
    )) %>%
    mutate({{ y_end }} := ifelse({{ y_end }} > {{ x_end }},
      {{ x_end }},
      {{ y_end }}
    )) %>%
    mutate(across(c({{ y_start }}, {{ y_end }}), \(.x) lubridate::as_datetime(.x, tz = tz)))

  out <- x %>%
    dplyr::nest_join(res,
                     by = c(
                       by,
                       colnames(mutate(ungroup(x), {{ x_start }}, .keep = "used")),
                       colnames(mutate(ungroup(x), {{ x_end }}, .keep = "used"))
                     ),
                     name = name)
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
#' @param fixed Whether to create fixed bins. If `TRUE`, bins will be rounded to, for example,
#' whole hours or days (depending on `by`). If `FALSE`, bins will be created based on the
#' first timestamp.
#'
#' @seealso [link_gaps()] for linking gaps to data.
#' @returns A tibble containing the group columns (if any), date, hour (if `by = "hour"`), and
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
#'   dplyr::mutate(lead = dplyr::lead(datetime)) %>%
#'   bin_data(
#'     start_time = datetime,
#'     end_time = lead,
#'     by = 1800L,
#'     fixed = FALSE
#'   )
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
#'   dplyr::group_by(participant_id) %>%
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
#' purrr::map_dbl(
#'   out$bin_data,
#'   ~ sum(as.double(.x$lead) - as.double(.x$datetime),
#'     na.rm = TRUE
#'   )
#' )
#'
#' # Or:
#' out %>%
#'   tidyr::unnest(bin_data, keep_empty = TRUE) %>%
#'   dplyr::mutate(duration = .data$lead - .data$datetime) %>%
#'   dplyr::group_by(bin, .add = TRUE) %>%
#'   dplyr::summarise(duration = sum(.data$duration, na.rm = TRUE), .groups = "drop")
bin_data <- function(data,
                     start_time,
                     end_time,
                     by = c("sec", "min", "hour", "day"),
                     fixed = TRUE) {
  check_arg(data, "data.frame")
  check_arg(fixed, "logical")

  group_vars <- dplyr::group_vars(data)

  if (!is.null(by) && is.character(by)) {
    by <- match.arg(by, c("sec", "min", "hour", "day"))
    by_duration <- c(sec = 1L, min = 60L, hour = 3600L, day = 86400L)
    by_duration <- by_duration[grepl(by, names(by_duration))]
  } else if (is.numeric(by) && !fixed) {
    by_duration <- by
  } else {
    abort(paste(
      "`by` must be one of 'sec', 'min', 'hour', or 'day',",
      "or a numeric value if `fixed = FALSE`."
    ))
  }

  tz <- attr(pull(data, {{ start_time }}), "tz")

  # check that start_time and end_time are a datetime, or try to convert
  if (!lubridate::is.POSIXt(pull(data, {{ start_time }})) ||
    !lubridate::is.POSIXt(pull(data, {{ end_time }}))) {
    data <- data %>%
      mutate({{ start_time }} := as.POSIXct({{ start_time }}, origin = "1970-01-01")) %>%
      mutate({{ end_time }} := as.POSIXct({{ end_time }}, origin = "1970-01-01"))
  }

  # Generate output structure with unique hours per day, keeping the grouping structure
  out <- data %>%
    tidyr::pivot_longer(
      cols = c({{ start_time }}, {{ end_time }}),
      names_to = NULL,
      values_to = "bin_start"
    )

  if (fixed) {
    out <- out %>%
      mutate(bin_start = trunc(.data$bin_start, by))
      # mutate(bin_start = lubridate::floor_date(.data$bin_start, by))
  }

  out <- out %>%
    distinct() %>%
    drop_na("bin_start")

  if (utils::packageVersion("dplyr") >= "1.1.0") {# nocov start
    groups <- dplyr::group_vars(out)
    out <- out %>%
      dplyr::reframe(bin_start = seq.POSIXt(
        from = min(.data$bin_start, na.rm = TRUE),
        to = max(.data$bin_start, na.rm = TRUE) + by_duration,
        by = by_duration
      ))

    # Regroup after reframe
    if (length(groups) > 0) {
      out <- group_by(out, dplyr::pick(dplyr::all_of(groups)))
    }
  } else {
    out <- out %>%
      summarise(bin_start = seq.POSIXt(
        from = min(.data$bin_start, na.rm = TRUE),
        to = max(.data$bin_start, na.rm = TRUE) + by_duration,
        by = by_duration
      ))
  } #nocov end

  if (by == "day") {
    out <- out %>%
      mutate(bin_start = round.POSIXt(.data$bin_start, units = by))
  }

  out <- out %>%
    mutate(bin_start = as.integer(as.POSIXct(.data$bin_start)))


  out <- out %>%
    mutate(bin_end = lead(.data$bin_start)) %>%
    drop_na("bin_end")

  out <- link_intervals(
    x = out, x_start = .data$bin_start, x_end = .data$bin_end,
    y = data, y_start = {{ start_time }}, y_end = {{ end_time }},
    by = group_vars,
    name = "bin_data"
  )

  out <- out %>%
    mutate(bin_start = lubridate::as_datetime(.data$bin_start, tz = tz)) %>%
    dplyr::rename(bin = "bin_start") %>%
    select(-"bin_end")

  out
}
