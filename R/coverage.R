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
#' are zero-filled. The first and last bins of the span are prorated in relative
#' mode, so a participant who starts in the middle of a day or week can still
#' reach 100% coverage for that bin.
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
#'   (explicitly requested sensors that are dropped produce a warning) and the
#'   coverage is relative to
#'   the expected number of measurements. Use `NULL` for absolute counts.
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
#'   `TRUE`; use `FALSE` to bin by the canonical UTC timestamps.
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
#' }
coverage <- function(
  db,
  participant_id = NULL,
  sensor = NULL,
  expected = NULL,
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

  # Build the lazy query: per-sensor counts, then a raw-SQL wrapper for the
  # participant spans, zero-filled spine, cycle positions, and relative values.
  counts <- purrr::map(
    sensor,
    ~ .coverage_counts_branch(
      db = db,
      sensor = .x,
      participant_id = participant_id,
      by = by,
      local = local,
      week_start = week_start,
      start_date = start_date,
      end_date = end_date
    )
  )
  counts <- purrr::reduce(counts, dplyr::union_all)
  counts_sql <- as.character(dbplyr::sql_render(counts))

  query <- .coverage_sql(
    counts_sql = counts_sql,
    sensor = sensor,
    expected = expected,
    by = by,
    cycle = cycle,
    week_start = week_start,
    local = local
  )

  out <- dplyr::tbl(db, dbplyr::sql(query))
  class(out) <- c("coverage", class(out))
  attr(out, "participant_id") <- participant_id
  attr(out, "expected") <- expected
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
      ungroup(.data$measure)
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
  view <- if (local) paste0(sensor, "_with_local") else sensor
  time_col <- if (local) "time_local" else "time"

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

# Wrap the per-sensor counts in the spans/spine/cycle SQL.
.coverage_sql <- function(
  counts_sql,
  sensor,
  expected,
  by,
  cycle,
  week_start,
  local
) {
  step <- .coverage_sql_step(by)
  first_bin <- .coverage_sql_trunc("first_time", by, week_start)
  last_bin <- .coverage_sql_trunc("last_time", by, week_start)

  spans <- if (local) {
    paste(
      "arg_min(bin_first_local, bin_first) AS first_time,",
      "arg_max(bin_last_local, bin_last) AS last_time"
    )
  } else {
    "min(bin_first) AS first_time, max(bin_last) AS last_time"
  }

  sensor_list <- paste0(
    "'",
    gsub("'", "''", sensor),
    "'",
    collapse = ", "
  )

  if (is.null(expected)) {
    expected_cte <- ""
    expected_join <- ""
    interval_col <- ""
    value <- "n"
  } else {
    rows <- sprintf(
      "('%s', %s)",
      gsub("'", "''", names(expected)),
      format(expected, scientific = FALSE, trim = TRUE)
    )
    expected_cte <- sprintf(
      "expected AS (SELECT * FROM (VALUES %s) AS e(measure, interval_seconds)),\n",
      paste(rows, collapse = ", ")
    )
    expected_join <- "\n  LEFT JOIN expected e ON e.measure = sn.measure"
    interval_col <- ",\n         e.interval_seconds"
    value <- "n / (GREATEST(covered_seconds, interval_seconds) / interval_seconds)"
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
      "WITH counts AS (",
      "%s",
      "),",
      "spans AS (",
      "  SELECT participant_id, %s",
      "  FROM counts",
      "  GROUP BY participant_id",
      "),",
      "spine AS (",
      "  SELECT participant_id, unnest(generate_series(%s, %s, %s)) AS bin",
      "  FROM spans",
      "),",
      "%ssensor_list AS (SELECT unnest([%s]) AS measure),",
      "joined AS (",
      "  SELECT s.participant_id, s.bin, sn.measure,",
      "         CAST(COALESCE(c.n, 0) AS DOUBLE) AS n,",
      "         GREATEST(DATE_DIFF('second', GREATEST(s.bin, sp.first_time),",
      "                   LEAST(s.bin + %s, sp.last_time)), 0) AS covered_seconds%s",
      "  FROM spine s",
      "  JOIN spans sp ON sp.participant_id = s.participant_id",
      "  CROSS JOIN sensor_list sn",
      "  LEFT JOIN counts c",
      "    ON c.participant_id = s.participant_id",
      "   AND c.measure = sn.measure",
      "   AND c.bin = s.bin%s",
      ")",
      "%s"
    ),
    counts_sql,
    spans,
    first_bin,
    last_bin,
    step,
    expected_cte,
    sensor_list,
    step,
    interval_col,
    expected_join,
    select
  )
}

# Turn the integer day_of_week into a labelled factor (lubridate::wday style).
.coverage_label_day_of_week <- function(x, week_start = 7L) {
  week_start <- as.integer(week_start)
  reference <- as.Date("1970-01-05") + ((week_start - 1L) %% 7L) + 0:6
  labels <- lubridate::wday(reference, label = TRUE, week_start = week_start)
  factor(x, levels = 1:7, labels = levels(labels))
}
