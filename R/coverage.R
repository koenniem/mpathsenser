#' Construct an expected-interval vector for coverage()
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `coverage_frequency()` is a convenience helper for the `expected` argument of
#' [coverage()]. Each argument is the expected number of seconds between two
#' consecutive measurements of that sensor, matching the interval with which
#' m-Path Sense samples the sensor.
#'
#' @param Accelerometer,AppUsage,Battery,Bluetooth,Connectivity,Device,Error,Heartbeat,Light,Location,Memory,Timezone,Weather,Wifi
#'   Expected interval in seconds for that sensor. The defaults are the current
#'   m-Path Sense defaults and can be overridden individually.
#'
#' @returns A named numeric vector of intervals in seconds, suitable for the
#'   `expected` argument of [coverage()].
#' @export
#'
#' @examples
#' coverage_frequency(Weather = 1200)
coverage_frequency <- function(
  Accelerometer = 120,
  AppUsage = 300,
  Battery = 120,
  Bluetooth = 600,
  Connectivity = 120,
  Device = 60,
  Error = 300,
  Heartbeat = 300,
  Light = 120,
  Location = 120,
  Memory = 160,
  Timezone = 1800,
  Weather = 120,
  Wifi = 120
) {
  intervals <- c(
    Accelerometer = Accelerometer,
    AppUsage = AppUsage,
    Battery = Battery,
    Bluetooth = Bluetooth,
    Connectivity = Connectivity,
    Device = Device,
    Error = Error,
    Heartbeat = Heartbeat,
    Light = Light,
    Location = Location,
    Memory = Memory,
    Timezone = Timezone,
    Weather = Weather,
    Wifi = Wifi
  )

  if (!is.numeric(intervals) || any(!is.finite(intervals)) || any(intervals <= 0)) {
    cli_abort("All {.fn coverage_frequency} intervals must be positive, finite numbers.")
  }

  intervals
}

#' Coverage chart of the sampling rate
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Computes the (relative) number of measurements per sensor and participant.
#' Only applicable to non-reactive sensors with 'continuous' sampling.
#'
#' The calculation is performed entirely inside DuckDB and the result is a lazy
#' table; call [dplyr::collect()] to bring it into R. Coverage is computed
#' **within** each participant, over that participant's own observation span
#' (`first` to `last` measurement). Bins inside the span without observations
#' are zero-filled. Only the first and last bins of the participant span are
#' prorated: for example, if observation starts at 13:50, the 13:00--14:00 bin
#' can reach 100% based on the eligible final 10 minutes. Interior bins always
#' use their complete duration.
#'
#' The `metric` argument selects what a bin measures. `"count"` (the default)
#' counts the distinct observation instants in each bin, optionally relative to
#' the expected sampling rate. `"time"` requires `expected` and measures the
#' fraction of the eligible time in each bin that is covered by the union of the
#' observation intervals `[time, time + expected)`; duplicated or overlapping
#' observations count once, so temporal coverage cannot exceed 1.
#'
#' @param db A valid database connection. Schema must be that as it is created by
#'   [open_db].
#' @param participant_id A participant ID, or a vector of participant IDs.
#'   Stored as an unsigned integer; both an integer/numeric and a character
#'   value are accepted. Use `NULL` for all participants.
#' @param sensor A character vector containing one or multiple sensors. See
#'   \code{\link[mpathsenser]{sensors}} for a list of available sensors. Use
#'   `NULL` for all available sensors.
#' @param expected Optional named numeric vector with sensors as names and the
#'   expected sampling interval in seconds. Use [coverage_frequency()] to
#'   construct one. When given, only sensors present in `expected` are returned
#'   (explicitly requested sensors that are dropped produce a warning). For
#'   `metric = "count"`, coverage is relative to the expected number of
#'   measurements; for `metric = "time"`, the intervals also define how long
#'   each observation covers. Use `NULL` for absolute counts.
#' @param metric What to measure per bin: `"count"` (the default) for the number
#'   of distinct observations, or `"time"` for the fraction of the eligible time
#'   covered by the union of the intervals `[time, time + expected)`. `"time"`
#'   requires `expected`; its eligible span ends one expected interval after the
#'   last observation, so the terminal observation contributes its full interval.
#' @param by The time resolution at which observations are counted. One of
#'   `"minute"`, `"hour"`, `"day"`, `"week"`, or `"month"`.
#' @param cycle The cycle within which the counts are averaged, or `NULL` to
#'   return the full series. One of `NULL`, `"hour"`, `"day"`, `"week"`,
#'   `"month"`, or `"year"`. `cycle` must be coarser than `by`.
#' @param label Whether to label `cycle = "week"` output with weekday
#'   abbreviations (as in [lubridate::wday()]). When `FALSE`, `day_of_week` is
#'   returned as an integer (1 = `week_start`).
#' @param week_start Day the week starts on: 1 = Monday, 7 = Sunday. Defaults to
#'   `getOption("lubridate.week.start", 7)` and also defines the boundary of
#'   `by = "week"` bins.
#' @param local Whether to bin observations using the participant-local timezone
#'   stored in the `timezone` column (via the `_with_local` views). Defaults to
#'   `TRUE`; use `FALSE` to bin by the canonical UTC timestamps. If a timezone
#'   change makes local time move backwards, the participant span uses the local
#'   wall-clock hull so the generated spine remains non-empty.
#' @param start_date A date (or convertible to a date using [base::as.Date()])
#'   indicating the earliest date to include. Leave empty for all data.
#' @param end_date A date (or convertible to a date using [base::as.Date()])
#'   indicating the latest date to include. Leave empty for all data.
#'
#' @returns A lazy `tbl` of class `"coverage"`. Columns are `participant_id`,
#'   the cycle position columns (or `time` when `cycle = NULL`), `measure`, and
#'   `coverage`. `collect()` rounds `coverage` to two digits, turns `measure`
#'   into a factor, and labels `day_of_week` when applicable.
#' @export
#'
#' @examples
#' \dontrun{
#' # Absolute coverage per hour of the day
#' coverage(db, participant_id = "12345", sensor = "Accelerometer")
#'
#' # Relative coverage per week, averaged per day of the week
#' coverage(
#'   db,
#'   sensor = "Accelerometer",
#'   expected = coverage_frequency(Accelerometer = 5),
#'   by = "day",
#'   cycle = "week"
#' )
#'
#' # The full minute-level series for one participant
#' coverage(db, "12345", by = "minute", cycle = NULL)
#'
#' # Temporal coverage of a sensor expected to sample every 5 seconds
#' coverage(
#'   db,
#'   "12345",
#'   sensor = "Accelerometer",
#'   expected = coverage_frequency(Accelerometer = 5),
#'   metric = "time",
#'   cycle = NULL
#' )
#' }
coverage <- function(
  db,
  participant_id = NULL,
  sensor = NULL,
  expected = NULL,
  metric = c("count", "time"),
  by = "hour",
  cycle = "day",
  label = TRUE,
  week_start = getOption("lubridate.week.start", 7),
  local = TRUE,
  start_date = NULL,
  end_date = NULL
) {
  check_db(db)
  check_arg(
    participant_id,
    type = c("character", "integerish", "numeric"),
    allow_null = TRUE
  )
  check_sensors(sensor, allow_null = TRUE)
  check_arg(by, "character", n = 1)
  check_arg(cycle, "character", n = 1, allow_null = TRUE)
  check_arg(label, "logical", n = 1)
  check_arg(local, "logical", n = 1)
  check_arg(week_start, type = c("numeric", "integerish"), n = 1)
  check_arg(expected, type = "numeric", allow_null = TRUE)
  metric <- match.arg(metric, c("count", "time"))

  # Participants
  participants <- get_participants(db)$participant_id
  if (length(participants) == 0) {
    cli_abort("The database does not contain any participants.")
  }

  if (!is.null(participant_id)) {
    requested <- as.character(participant_id)
    unknown <- setdiff(requested, as.character(participants))
    if (length(unknown) > 0) {
      cli_abort(
        "Participant{?s} {.val {unknown}} could not be found in the {.pkg mpathsenser} database."
      )
    }
  }

  # Sensors
  sensor_explicit <- !is.null(sensor)
  if (is.null(sensor)) {
    sensor <- sensors
  }
  sensor <- .physical_sensor(sensor)
  sensor <- sensor[tolower(sensor) %in% tolower(sensors)]
  if (length(sensor) == 0) {
    cli_abort("No valid sensors to compute coverage for.")
  }

  # Expected intervals
  if (!is.null(expected)) {
    if (is.null(names(expected))) {
      cli_abort("{.arg expected} must be a named numeric vector.")
    }
    if (any(!is.finite(expected)) || any(expected <= 0)) {
      cli_abort("{.arg expected} must contain positive, finite intervals.")
    }

    keep <- sensor[sensor %in% names(expected)]
    dropped <- setdiff(sensor, keep)
    if (length(dropped) > 0 && sensor_explicit) {
      cli_warn(c(
        "Dropping sensor{?s} without an expected interval: {.val {dropped}}.",
        i = "Add them to {.arg expected} to compute their relative coverage."
      ))
    }
    sensor <- keep
    if (length(sensor) == 0) {
      cli_abort("No sensors left after filtering by {.arg expected}.")
    }
  }

  if (identical(metric, "time") && is.null(expected)) {
    cli_abort(c(
      "{.arg metric} = {.val time} requires {.arg expected}.",
      i = "Pass {.fn coverage_frequency} to define the expected sampling interval per sensor."
    ))
  }

  # Time resolution and cycle
  by_rank <- c(minute = 1L, hour = 2L, day = 3L, week = 4L, month = 5L)
  cycle_rank <- c(hour = 2L, day = 3L, week = 4L, month = 5L, year = 6L)
  by <- match.arg(by, names(by_rank))
  cycle <- if (is.null(cycle)) {
    NULL
  } else {
    match.arg(cycle, names(cycle_rank))
  }
  if (!is.null(cycle) && by_rank[[by]] >= cycle_rank[[cycle]]) {
    cli_abort(c(
      "{.arg cycle} must be coarser than {.arg by}.",
      i = "{.val {by}} bins cannot be summarised within a {.val {cycle}} cycle."
    ))
  }

  if (!rlang::is_integerish(week_start) || week_start < 1 || week_start > 7) {
    cli_abort("{.arg week_start} must be a whole number between 1 (Monday) and 7 (Sunday).")
  }
  week_start <- as.integer(week_start)

  # Check dates
  convert2date <- function(s) {
    if (!inherits(s, "Date") && !is.character(s)) {
      return(FALSE)
    }
    s <- try(as.Date(s), silent = TRUE)
    return(inherits(s, "Date"))
  }
  if (
    !(is.null(start_date) || convert2date(start_date)) ||
      !(is.null(end_date) || convert2date(end_date))
  ) {
    cli_abort(
      "{.arg start_date} and {.arg end_date} must be {.code NULL}, a date string, or a {.cls Date}."
    )
  }

  # Build the lazy query: one relation per sensor (per-bin counts, or distinct
  # observation instants for the temporal metric), then a raw-SQL wrapper for
  # the participant spans, zero-filled spine, interval union, cycle positions,
  # and relative values.
  relations <- purrr::map(
    sensor,
    ~ if (identical(metric, "time")) {
      .coverage_instants_branch(
        db = db,
        sensor = .x,
        participant_id = participant_id,
        local = local,
        start_date = start_date,
        end_date = end_date
      )
    } else {
      .coverage_counts_branch(
        db = db,
        sensor = .x,
        participant_id = participant_id,
        by = by,
        local = local,
        week_start = week_start,
        start_date = start_date,
        end_date = end_date
      )
    }
  )
  relations <- purrr::reduce(relations, dplyr::union_all)
  relation_sql <- as.character(dbplyr::sql_render(relations))

  query <- .coverage_sql(
    relation_sql = relation_sql,
    sensor = sensor,
    expected = expected,
    by = by,
    cycle = cycle,
    week_start = week_start,
    local = local,
    metric = metric
  )

  out <- dplyr::tbl(db, dbplyr::sql(query))
  class(out) <- c("coverage", class(out))
  attr(out, "participant_id") <- participant_id
  attr(out, "expected") <- expected
  attr(out, "metric") <- metric
  attr(out, "by") <- by
  attr(out, "cycle") <- cycle
  attr(out, "label") <- label
  attr(out, "week_start") <- week_start
  attr(out, "local") <- local
  out
}

#' Materialise a coverage result
#'
#' @param x A lazy coverage table created by [coverage()].
#' @param ... Passed on to the underlying `collect()` method.
#'
#' @returns A tibble with rounded coverage values, factorised `measure`, and
#'   labelled `day_of_week` when applicable.
#' @keywords internal
#' @exportS3Method dplyr::collect
collect.coverage <- function(x, ...) {
  out <- NextMethod()
  requested <- attr(x, "participant_id")

  if (nrow(out) == 0) {
    if (is.null(requested)) {
      cli_abort("No observations found for the requested sensors and time range.")
    }
    cli_abort(
      "No observations found for participant{?s} {.val {requested}} in the requested time range."
    )
  }

  if (!is.null(requested) && "participant_id" %in% names(out)) {
    present <- unique(as.character(out$participant_id))
    missing <- setdiff(as.character(requested), present)
    if (length(missing) > 0) {
      cli_warn(c(
        "No data found for participant{?s} {.val {missing}}; skipping.",
        i = "Only participants with observations are returned."
      ))
    }
  }

  if ("day_of_week" %in% names(out) && isTRUE(attr(x, "label"))) {
    out$day_of_week <- .coverage_label_day_of_week(
      out$day_of_week,
      attr(x, "week_start") %||% 7L
    )
  }

  if ("coverage" %in% names(out)) {
    out$coverage <- round(out$coverage, 2)
  }

  if ("measure" %in% names(out)) {
    out$measure <- factor(
      out$measure,
      levels = rev(sort(unique(as.character(out$measure))))
    )
  }

  class(out) <- c("coverage", class(out))
  for (nm in c(
    "participant_id",
    "expected",
    "metric",
    "by",
    "cycle",
    "label",
    "week_start",
    "local"
  )) {
    attr(out, nm) <- attr(x, nm)
  }
  out
}

#' Plot a coverage overview
#'
#' @param x A coverage result coming from [coverage()], either lazy or already
#'   collected.
#' @param digits Number of digits to round the coverage values to. Defaults to 2.
#' @param type The plot type. `"auto"` (the default) uses a line graph for the
#'   full series (`cycle = NULL`) and a heatmap for cycle profiles. Use
#'   `"line"` or `"heatmap"` to force one of the two.
#' @param ... Other arguments passed on to methods. Not currently used.
#'
#' @seealso [coverage()]
#' @returns A [ggplot2::ggplot] object.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- coverage(db, participant_id = "12345", sensor = "Accelerometer")
#' plot(data)
#' }
plot.coverage <- function(
  x,
  digits = 2,
  type = c("auto", "line", "heatmap"),
  ...
) {
  ensure_suggested_package("ggplot2")
  type <- match.arg(type)

  x <- dplyr::collect(x)
  is_relative <- !is.null(attr(x, "expected"))
  participant_id <- attr(x, "participant_id")
  cycle <- attr(x, "cycle")
  if (identical(type, "auto")) {
    type <- if (is.null(cycle)) "line" else "heatmap"
  }

  title <- if (is.null(participant_id)) {
    "Coverage for all participants"
  } else if (length(participant_id) == 1) {
    paste0("Coverage for participant ", participant_id)
  } else {
    "Coverage per participant"
  }

  # Position columns are stored big -> small. The finest one is on the x axis;
  # the coarser ones become facet rows. For the full series, time is on x.
  position_cols <- intersect(
    c(
      "day_of_week",
      "month",
      "day_of_month",
      "week_of_month",
      "week_of_year",
      "hour",
      "minute"
    ),
    names(x)
  )
  if (is.null(cycle)) {
    x_col <- "time"
    facet_cols <- character(0)
  } else {
    x_col <- position_cols[length(position_cols)]
    facet_cols <- position_cols[-length(position_cols)]
  }

  multiple_participants <- length(unique(x$participant_id)) > 1
  add_facets <- function(plot) {
    if (length(facet_cols) > 0) {
      if (multiple_participants) {
        return(
          plot +
            ggplot2::facet_grid(
              rows = ggplot2::vars(!!!rlang::syms(facet_cols)),
              cols = ggplot2::vars(participant_id),
              scales = "free_y"
            )
        )
      }
      return(
        plot +
          ggplot2::facet_grid(
            rows = ggplot2::vars(!!!rlang::syms(facet_cols)),
            scales = "free_y"
          )
      )
    }
    if (multiple_participants) {
      return(plot + ggplot2::facet_wrap(~participant_id))
    }
    plot
  }

  if (identical(type, "line")) {
    plot <- ggplot2::ggplot(
      x,
      ggplot2::aes(
        x = .data[[x_col]],
        y = .data$coverage,
        colour = .data$measure
      )
    ) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = title,
        x = if (identical(x_col, "time")) "Time" else x_col,
        y = if (is_relative) "Relative coverage" else "Coverage",
        colour = "Sensor"
      )
    return(add_facets(plot))
  }

  # Heatmap
  if (is_relative) {
    fill_col <- "coverage"
  } else {
    x <- x |>
      group_by(.data$measure) |>
      mutate(max_coverage = max(.data$coverage)) |>
      mutate(
        max_coverage = ifelse(.data$max_coverage == 0, 1, .data$max_coverage)
      ) |>
      mutate(scaled_coverage = .data$coverage / max(.data$max_coverage)) |>
      ungroup("measure")
    fill_col <- "scaled_coverage"
  }

  plot <- ggplot2::ggplot(
    x,
    ggplot2::aes(
      x = .data[[x_col]],
      y = .data$measure,
      fill = .data[[fill_col]]
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(
      mapping = ggplot2::aes(label = round(coverage, digits = digits)),
      colour = "white"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title,
      x = if (identical(x_col, "time")) "Time" else x_col,
      y = "Sensor"
    )

  if (is_relative) {
    plot <- plot +
      ggplot2::scale_fill_gradientn(
        colours = c("#d70525", "#645a6c", "#3F7F93"),
        breaks = c(0, 0.5, 1),
        labels = c(0, 0.5, 1),
        limits = c(0, 1),
        name = "coverage"
      )
  } else {
    plot <- plot +
      ggplot2::scale_fill_gradientn(
        colours = c("#d70525", "#645a6c", "#3F7F93"),
        breaks = c(0, 0.5, 1),
        labels = c("low", "medium", "high"),
        limits = c(0, 1),
        name = "coverage"
      )
  }

  add_facets(plot)
}

# -- internal helpers ----------------------------------------------------------

# Expected interval (seconds) -> SQL interval step for one bin of `by`.
.coverage_sql_step <- function(by) {
  switch(
    by,
    minute = "INTERVAL 1 MINUTE",
    hour = "INTERVAL 1 HOUR",
    day = "INTERVAL 1 DAY",
    week = "INTERVAL 1 WEEK",
    month = "INTERVAL 1 MONTH"
  )
}

# The bin expression for a timestamp column. Week bins start on `week_start`.
.coverage_sql_trunc <- function(column, by, week_start = 1L) {
  if (identical(by, "week") && week_start != 1L) {
    shift <- week_start - 1L
    sprintf(
      "date_trunc('week', %s - INTERVAL %d DAY) + INTERVAL %d DAY",
      column,
      shift,
      shift
    )
  } else {
    sprintf("date_trunc('%s', %s)", by, column)
  }
}

# Position columns (big -> small) for a cycle profile.
.coverage_sql_positions <- function(cycle, by, week_start = 1L) {
  positions <- character(0)

  minute <- "CAST(EXTRACT(minute FROM bin) AS INTEGER)"
  hour <- "CAST(EXTRACT(hour FROM bin) AS INTEGER)"

  if (identical(cycle, "hour")) {
    positions["minute"] <- minute
  } else if (identical(cycle, "day")) {
    positions["hour"] <- hour
    if (identical(by, "minute")) {
      positions["minute"] <- minute
    }
  } else if (identical(cycle, "week")) {
    positions["day_of_week"] <- sprintf(
      "CAST(((EXTRACT(isodow FROM bin)::INTEGER - %d + 7) %% 7) + 1 AS INTEGER)",
      week_start
    )
    if (by %in% c("hour", "minute")) {
      positions["hour"] <- hour
    }
    if (identical(by, "minute")) {
      positions["minute"] <- minute
    }
  } else if (identical(cycle, "month")) {
    if (identical(by, "week")) {
      positions["week_of_month"] <-
        "CAST(DATE_DIFF('week', date_trunc('month', bin), bin) + 1 AS INTEGER)"
    } else {
      positions["day_of_month"] <- "CAST(EXTRACT(day FROM bin) AS INTEGER)"
      if (by %in% c("hour", "minute")) {
        positions["hour"] <- hour
      }
      if (identical(by, "minute")) {
        positions["minute"] <- minute
      }
    }
  } else if (identical(cycle, "year")) {
    if (identical(by, "week")) {
      positions["week_of_year"] <-
        "CAST(DATE_DIFF('week', date_trunc('year', bin), bin) + 1 AS INTEGER)"
    } else {
      positions["month"] <- "CAST(EXTRACT(month FROM bin) AS INTEGER)"
      if (!identical(by, "month")) {
        positions["day_of_month"] <- "CAST(EXTRACT(day FROM bin) AS INTEGER)"
      }
      if (by %in% c("hour", "minute")) {
        positions["hour"] <- hour
      }
      if (identical(by, "minute")) {
        positions["minute"] <- minute
      }
    }
  }

  positions
}

# The shared view choice and filters for one sensor branch. Filtering always
# uses the canonical `time`, also when `local = TRUE`, so the local and UTC
# branches see exactly the same observations.
.coverage_filter_branch <- function(
  db,
  sensor,
  participant_id,
  local,
  start_date,
  end_date
) {
  view <- if (local) paste0(sensor, "_with_local") else sensor

  out <- dplyr::tbl(db, view)

  if (!is.null(participant_id)) {
    out <- dplyr::filter(out, .data$participant_id %in% !!as.character(participant_id))
  }

  if (identical(sensor, "Heartbeat")) {
    out <- dplyr::filter(
      out,
      is.na(.data$device_role_name) | !grepl("^Secondary", .data$device_role_name)
    )
  }

  if (!is.null(start_date)) {
    start_limit <- as.Date(start_date)
    out <- dplyr::filter(out, .data$time >= !!start_limit)
  }
  if (!is.null(end_date)) {
    end_limit <- as.Date(end_date) + 1
    out <- dplyr::filter(out, .data$time <= !!end_limit)
  }

  out
}

# One lazy per-sensor counts branch for the coverage query.
.coverage_counts_branch <- function(
  db,
  sensor,
  participant_id,
  by,
  local,
  week_start,
  start_date,
  end_date
) {
  time_col <- if (local) "time_local" else "time"

  out <- .coverage_filter_branch(
    db = db,
    sensor = sensor,
    participant_id = participant_id,
    local = local,
    start_date = start_date,
    end_date = end_date
  )

  out <- dplyr::mutate(
    out,
    bin = dbplyr::sql(.coverage_sql_trunc(time_col, by, week_start))
  )

  if (local) {
    out <- dplyr::summarise(
      out,
      n = dplyr::n_distinct(.data$time),
      bin_first = min(.data$time),
      bin_last = max(.data$time),
      bin_first_local = dbplyr::sql("arg_min(time_local, time)"),
      bin_last_local = dbplyr::sql("arg_max(time_local, time)"),
      .by = c("participant_id", "bin")
    )
  } else {
    out <- dplyr::summarise(
      out,
      n = dplyr::n_distinct(.data$time),
      bin_first = min(.data$time),
      bin_last = max(.data$time),
      .by = c("participant_id", "bin")
    )
  }

  dplyr::mutate(out, measure = sensor)
}

# One lazy per-sensor distinct-instant branch for the temporal coverage metric.
# Distinctness is on the canonical `time`, so a duplicated (or re-imported)
# observation cannot create a second interval; the matching local wall-clock
# value is kept for binning when `local = TRUE`.
.coverage_instants_branch <- function(
  db,
  sensor,
  participant_id,
  local,
  start_date,
  end_date
) {
  out <- .coverage_filter_branch(
    db = db,
    sensor = sensor,
    participant_id = participant_id,
    local = local,
    start_date = start_date,
    end_date = end_date
  )

  if (local) {
    out <- dplyr::summarise(
      out,
      time_local = min(.data$time_local),
      .by = c("participant_id", "time")
    )
  } else {
    out <- dplyr::select(out, "participant_id", "time")
    out <- dplyr::distinct(out)
  }

  dplyr::mutate(out, measure = sensor)
}

# Wrap the per-sensor relation in the spans/spine/cycle SQL.
.coverage_sql <- function(
  relation_sql,
  sensor,
  expected,
  by,
  cycle,
  week_start,
  local,
  metric
) {
  step <- .coverage_sql_step(by)
  first_bin <- .coverage_sql_trunc("first_time", by, week_start)

  sensor_list <- paste0(
    "'",
    gsub("'", "''", sensor),
    "'",
    collapse = ", "
  )

  expected_cte <- ""
  if (!is.null(expected)) {
    rows <- sprintf(
      "('%s', %s)",
      gsub("'", "''", names(expected)),
      format(expected, scientific = FALSE, trim = TRUE)
    )
    expected_cte <- sprintf(
      "expected AS (SELECT * FROM (VALUES %s) AS e(measure, interval_seconds)),\n",
      paste(rows, collapse = ", ")
    )
  }

  if (identical(metric, "time")) {
    prefix <- .coverage_sql_intervals(
      relation_sql = relation_sql,
      expected_cte = expected_cte,
      by = by,
      week_start = week_start,
      local = local
    )
    # `last_time` is an exclusive interval end, so the final bin is the one that
    # contains the last covered instant (`last_time - 1 microsecond`), not a bin
    # that merely touches that boundary.
    last_bin <- .coverage_sql_trunc(
      "last_time - INTERVAL 1 MICROSECOND",
      by,
      week_start
    )
    # Local wall-clock values can move backwards after a timezone change;
    # the hull keeps the participant spine non-empty without reordering rows.
    spans <- "min(seg_start) AS first_time, max(seg_end) AS last_time"
    spans_from <- "intervals"
    numerator <- "CAST(COALESCE(c.covered, 0) AS DOUBLE) AS covered"
    interval_col <- ""
    value_join <- paste(
      "  LEFT JOIN bin_coverage c",
      "    ON c.participant_id = s.participant_id",
      "   AND c.measure = sn.measure",
      "   AND c.bin = s.bin",
      sep = "\n"
    )
    # Timestamps are microsecond precision; use one microsecond only as a
    # zero-duration guard so subsecond expected intervals remain meaningful.
    value <- "LEAST(covered / GREATEST(covered_seconds, 0.000001), 1)"
  } else {
    prefix <- sprintf("counts AS (\n%s\n),\n%s", relation_sql, expected_cte)
    last_bin <- .coverage_sql_trunc("last_time", by, week_start)
    spans <- if (local) {
      # Use the local wall-clock hull: a timezone change can reverse the
      # chronological endpoint values, which cannot parameterise a series.
      paste(
        "min(bin_first_local) AS first_time,",
        "max(bin_last_local) AS last_time"
      )
    } else {
      "min(bin_first) AS first_time, max(bin_last) AS last_time"
    }
    spans_from <- "counts"
    expected_join <- ""
    interval_col <- ""
    if (!is.null(expected)) {
      expected_join <- "\n   LEFT JOIN expected e ON e.measure = sn.measure"
      interval_col <- ",\n         e.interval_seconds"
    }
    numerator <- "CAST(COALESCE(c.n, 0) AS DOUBLE) AS n"
    value_join <- paste0(
      "  LEFT JOIN counts c",
      "\n    ON c.participant_id = s.participant_id",
      "\n   AND c.measure = sn.measure",
      "\n   AND c.bin = s.bin",
      expected_join
    )
    value <- if (is.null(expected)) {
      "n"
    } else {
      "n / (GREATEST(covered_seconds, interval_seconds) / interval_seconds)"
    }
  }

  if (is.null(cycle)) {
    select <- sprintf(
      "SELECT participant_id, bin AS time, measure, %s AS coverage\nFROM joined\nORDER BY participant_id, time, measure",
      value
    )
  } else {
    positions <- .coverage_sql_positions(cycle, by, week_start)
    position_select <- paste(
      sprintf("%s AS %s", positions, names(positions)),
      collapse = ", "
    )
    position_names <- paste(names(positions), collapse = ", ")
    select <- sprintf(
      paste(
        "SELECT participant_id, %s, measure, AVG(%s) AS coverage",
        "FROM joined",
        "GROUP BY participant_id, %s, measure",
        "ORDER BY participant_id, %s, measure"
      ),
      position_select,
      value,
      position_names,
      position_names
    )
  }

  sprintf(
    paste(
      "WITH %s",
      "spans AS (",
      "  SELECT participant_id, %s",
      "  FROM %s",
      "  GROUP BY participant_id",
      "),",
      "spine AS (",
      "  SELECT participant_id, unnest(generate_series(%s, %s, %s)) AS bin",
      "  FROM spans",
      "),",
      "sensor_list AS (SELECT unnest([%s]) AS measure),",
      "joined AS (",
      "  SELECT s.participant_id, s.bin, sn.measure,",
      "         %s,",
      "         GREATEST(DATE_DIFF('microsecond', GREATEST(s.bin, sp.first_time),",
      "                   LEAST(s.bin + %s, sp.last_time)) / 1000000.0, 0) AS covered_seconds%s",
      "  FROM spine s",
      "  JOIN spans sp ON sp.participant_id = s.participant_id",
      "  CROSS JOIN sensor_list sn",
      "%s",
      ")",
      "%s"
    ),
    prefix,
    spans,
    spans_from,
    first_bin,
    last_bin,
    step,
    sensor_list,
    numerator,
    step,
    interval_col,
    value_join,
    select
  )
}

# The temporal CTEs for metric = "time": expand every distinct observation into
# the half-open interval [seg_start, seg_end), merge intervals per participant
# and sensor with the gaps-and-islands running maximum, and sum the merged
# (union) duration per intersecting bin. An interval is clipped to each bin it
# crosses, so a segment spanning a bin boundary contributes to both bins.
.coverage_sql_intervals <- function(
  relation_sql,
  expected_cte,
  by,
  week_start,
  local
) {
  time_col <- if (local) "time_local" else "time"
  step <- .coverage_sql_step(by)
  # Bins are generated per island from its first bin to the bin containing its
  # last covered instant; `- 1 microsecond` keeps a half-open interval that ends
  # exactly on a boundary out of the next bin.
  first_seg <- .coverage_sql_trunc("i.seg_start", by, week_start)
  last_seg <- .coverage_sql_trunc(
    "i.seg_end - INTERVAL 1 MICROSECOND",
    by,
    week_start
  )

  sprintf(
    paste(
      "instants AS (",
      "%s",
      "),",
      "%s",
      "intervals AS (",
      "  SELECT i.participant_id, i.measure, i.time,",
      "         i.%s AS seg_start,",
      "         i.%s + TO_SECONDS(e.interval_seconds) AS seg_end",
      "  FROM instants i",
      "  JOIN expected e ON e.measure = i.measure",
      "),",
      "%s,",
      "bin_coverage AS (",
      "  SELECT i.participant_id, i.measure, b.bin,",
      "         SUM(DATE_DIFF('microsecond', GREATEST(i.seg_start, b.bin),",
      "                       LEAST(i.seg_end, b.bin + %s)) / 1000000.0) AS covered",
      "  FROM islands i,",
      "  LATERAL UNNEST(generate_series(%s, %s, %s)) AS b(bin)",
      "  GROUP BY i.participant_id, i.measure, b.bin",
      "),",
      sep = "\n"
    ),
    relation_sql,
    expected_cte,
    time_col,
    time_col,
    .coverage_sql_islands(),
    step,
    first_seg,
    last_seg,
    step
  )
}

# Gaps-and-islands merge for ordered half-open intervals keyed by
# (participant_id, measure). A segment only starts a new island when it begins
# after the running maximum of all previous segment ends; using the running
# maximum (not just the previous end) is what merges nested intervals, and
# touching intervals (`seg_start == prior_end`) merge because the union duration
# is unchanged. Expects an `intervals` CTE with (participant_id, measure,
# seg_start, seg_end).
.coverage_sql_islands <- function() {
  paste(
    "prior AS (",
    "  SELECT *,",
    "         MAX(seg_end) OVER (PARTITION BY participant_id, measure",
    "                            ORDER BY seg_start, seg_end",
    "                            ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS prior_end",
    "  FROM intervals",
    "),",
    "marked AS (",
    "  SELECT *, CASE WHEN prior_end IS NULL OR seg_start > prior_end",
    "                 THEN 1 ELSE 0 END AS new_island",
    "  FROM prior",
    "),",
    "islands AS (",
    "  SELECT participant_id, measure, MIN(seg_start) AS seg_start,",
    "         MAX(seg_end) AS seg_end",
    "  FROM (",
    "    SELECT *, SUM(new_island) OVER (PARTITION BY participant_id, measure",
    "                                   ORDER BY seg_start, seg_end",
    "                                   ROWS UNBOUNDED PRECEDING) AS grp",
    "    FROM marked",
    "  )",
    "  GROUP BY participant_id, measure, grp",
    ")",
    sep = "\n"
  )
}

# Turn the integer day_of_week into a labelled factor (lubridate::wday style).
.coverage_label_day_of_week <- function(x, week_start = 7L) {
  week_start <- as.integer(week_start)
  reference <- as.Date("1970-01-05") + ((week_start - 1L) %% 7L) + 0:6
  labels <- lubridate::wday(reference, label = TRUE, week_start = week_start)
  factor(x, levels = 1:7, labels = levels(labels))
}
