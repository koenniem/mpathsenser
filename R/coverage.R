# Coverage: absolute counts and proportional coverage per sensor and bin.
#
# Times, durations, and slot grids use UTC; each observation is assigned to
# the bin of the timezone that applies at its instant. A local bin that
# occurs twice, such as on both sides of a DST fold, sums both contributions.

# -- constants -----------------------------------------------------------------

.coverage_ios_sensors <- c("AppUsage", "Light", "Memory", "Screen")

# -- public entry points -------------------------------------------------------

#' Construct expected sampling intervals
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `coverage_expected()` returns the default sampling interval, in seconds, for
#' each sensor with a defined interval. Override individual values as needed.
#' The result can be passed to [coverage_proportional()].
#'
#' @param Accelerometer,AppUsage,Battery,Bluetooth,Connectivity,Device,Error,Heartbeat,Light,Location,Memory,Timezone,Weather,Wifi
#'   Expected interval in seconds for that sensor. Defaults are the current
#'   m-Path Sense intervals.
#' @param ... Additional sensor intervals, named with sensor names. Names are
#'   matched case-insensitively, for example `accelerometer = 60`. Unknown or
#'   duplicate names, and a sensor supplied both as a formal argument and in
#'   `...`, throw an error.
#'
#' @returns A named numeric vector of expected intervals in seconds.
#' @family coverage functions
#' @export
#'
#' @examples
#' coverage_expected()
#' coverage_expected(Accelerometer = 60)
coverage_expected <- function(
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
  Wifi = 120,
  ...
) {
  values <- list(
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

  # Validate every interval on its own before concatenation, so logical or
  # otherwise coercible overrides cannot slip through.
  for (name in names(values)) {
    values[[name]] <- .coverage_check_expected_override(name, values[[name]])
  }

  intervals <- unlist(values, use.names = TRUE)

  # Track which sensors arrived through a formal argument so a `...` override
  # for the same sensor can be rejected.
  call_names <- setdiff(names(formals()), "...")
  env <- environment()
  supplied <- call_names[vapply(
    call_names,
    \(name) !eval(bquote(missing(.(as.name(name)))), envir = env),
    logical(1)
  )]

  overrides <- list(...)
  if (length(overrides) > 0L) {
    override_names <- names(overrides)
    if (is.null(override_names) || any(!nzchar(override_names))) {
      cli_abort("Every override in {.arg ...} must be named with a sensor name.")
    }

    standard <- .standard_sensor_names(override_names)
    unknown <- standard[!standard %in% sensors]
    if (length(unknown) > 0L) {
      cli_abort("Sensor{?s} {.val {unknown}} could not be found.")
    }

    duplicated <- standard[duplicated(standard)]
    if (length(duplicated) > 0L) {
      cli_abort("Sensor{?s} {.val {duplicated}} {?was/were} supplied more than once.")
    }

    clashes <- intersect(standard, supplied)
    if (length(clashes) > 0L) {
      cli_abort(
        "Sensor{?s} {.val {clashes}} {?was/were} supplied as {?a/}formal argument and in {.arg ...}."
      )
    }

    for (i in seq_along(overrides)) {
      intervals[[standard[[i]]]] <- .coverage_check_expected_override(
        standard[[i]],
        overrides[[i]]
      )
    }
  }

  intervals
}

#' Count sensor measurements over time bins
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `coverage_frequency()` returns the absolute number of distinct measurement
#' times in each bin, per participant and sensor. Bins may use calendar-aligned
#' units (`"minute"`, `"hour"`, `"day"`, `"week"`, or `"month"`) or a custom
#' fixed width in seconds. Monthly bins follow actual calendar months. The
#' result is a lazy DuckDB table; use `collect()` to materialise it. For iOS
#' participants, `AppUsage`, `Light`, `Memory`, and
#' `Screen` are marked `NA`; missing `Device` platform information produces a
#' warning because availability cannot be determined.
#'
#' The full series is returned. To plot an average daily profile, pass the
#' result to `plot(..., cycle = "day")`. To calculate another summary yourself,
#' use dplyr verbs on the returned lazy table before collecting it.
#'
#' @param db A valid database connection. Schema must be that created by
#'   [open_db].
#' @param participant_id A participant ID, or a vector of participant IDs.
#'   Stored as an unsigned integer; integer, numeric, and character values are
#'   accepted. Use `NULL` for all participants.
#' @param sensor A character vector of sensors. Use `NULL` for all available
#'   sensors.
#' @param by Bin width: one of `"minute"`, `"hour"`, `"day"`, `"week"`, or
#'   `"month"`, or a positive number of seconds. Defaults to `"hour"`.
#' @param local Whether bins use participant-local wall-clock time from the
#'   `_with_local` views. Defaults to `TRUE`; use `FALSE` for UTC bins.
#' @param start_date,end_date Optional dates delimiting the included
#'   observations. Values must be dates or date strings.
#' @param week_start Day the week starts on: 1 = Monday, 7 = Sunday. Defaults to
#'   `getOption("lubridate.week.start", 7)`.
#'
#' @returns A lazy `tbl` of class `"coverage"` with `participant_id`, `time`
#'   (the bin start), `sensor`, and `coverage` (the absolute measurement count).
#'   Bins with no observations are zero-filled within each participant's span.
#' @family coverage functions
#' @export
#'
#' @examples
#' \dontrun{
#' db <- example_db()
#'
#' frequency <- coverage_frequency(
#'   db,
#'   participant_id = "372780",
#'   sensor = "Accelerometer",
#'   by = "hour"
#' )
#' plot(frequency, cycle = "day")
#'
#' # Average coverage for each hour of each day with dplyr
#' coverage_frequency(db, sensor = "Accelerometer", by = "minute") |>
#'   dplyr::mutate(date = as.Date(time), hour = lubridate::hour(time)) |>
#'   dplyr::group_by(participant_id, sensor, date, hour) |>
#'   dplyr::summarise(coverage = mean(coverage, na.rm = TRUE), .groups = "drop")
#' }
coverage_frequency <- function(
  db,
  participant_id = NULL,
  sensor = NULL,
  by = "hour",
  local = TRUE,
  start_date = NULL,
  end_date = NULL,
  week_start = getOption("lubridate.week.start", 7)
) {
  # Check the arguments against the database before building the query.
  validated <- .coverage_validate_frequency(
    db = db,
    participant_id = participant_id,
    sensor = sensor,
    by = by,
    local = local,
    start_date = start_date,
    end_date = end_date,
    week_start = week_start
  )

  # Collect the validated arguments into one `spec` list, with the package's
  # sensor names and the bin width for each sensor.
  spec <- .coverage_normalise_spec(validated)

  # Local dates and bins require the timezone for each observation. When all
  # observations are in UTC, no timezone-specific handling is needed.
  spec <- .coverage_resolve_local(spec)

  # Build the observation relation for each selected sensor and combine them.
  observations <- .coverage_relation_observations(spec)

  # Build the timezone episodes, offset segments, and observation span, and
  # apply the date filter on the local calendar.
  context <- .coverage_observation_context(spec, observations)

  # Count measurements per bin.
  coverage <- .coverage_assemble_count(spec, context, proportional = FALSE)

  # Attach the class and attributes used by collection and plotting.
  .coverage_table(spec, coverage)
}

#' Calculate proportional sensor coverage
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `coverage_proportional()` compares the observed measurements or covered time
#' with the expected sampling interval for each sensor. The result is a lazy
#' DuckDB table with one value per bin, participant, and sensor.
#'
#' By default, each sensor's bin width equals its expected interval.
#' Alternatively, `by` may be one of `"minute"`, `"hour"`, `"day"`, `"week"`,
#' or `"month"` for calendar-aligned bins, or a positive number of seconds for
#' fixed-width bins. A fixed-width `by` shorter than a sensor's expected
#' interval is raised to that interval for that sensor, and a warning reports
#' the adjustment; other sensors keep the requested width. Calendar units keep
#' their actual boundaries and are never widened.
#'
#' With `metric = "count"`, each value is the distinct measurement count divided
#' by the expected number of measurements in the eligible part of that bin.
#' With `metric = "interval"`, each observation covers
#' `[time, time + expected)` and overlapping intervals count only once. With
#' `metric = "bin"`, each expected-interval slot counts at most once, based on
#' whether it contains an included observation. Each participant-sensor grid is
#' anchored at its first included observation and extended across the shared
#' participant span; a sensor with no observations is anchored at the span start.
#' The denominator counts eligible slot starts. A slot crossing an output-bin
#' boundary belongs to the bin containing its start, and the span ends at the
#' latest occupied slot's end. The last returned bin is always the one containing
#' the last included observation, so a trailing expected window is attributed to
#' that bin rather than adding a later bin. For iOS participants, `AppUsage`,
#' `Light`, `Memory`, and `Screen` are marked `NA`; a warning is issued when
#' `Device` platform information is missing.
#'
#' `expected` is required, but it may be supplied directly or constructed with
#' [coverage_expected()]. Sensor names in `expected` are matched
#' case-insensitively, and the output always uses the canonical capitalisation.
#'
#' @param db A valid database connection. Schema must be that created by
#'   [open_db].
#' @param expected A named numeric vector of positive finite sampling intervals
#'   in seconds, with sensor names. Only sensors with a supplied interval are
#'   included. Count-based values can exceed 1 when measurements are more
#'   frequent than expected. Use [coverage_expected()] to start with the default
#'   intervals.
#' @param participant_id A participant ID, or a vector of participant IDs.
#'   Stored as an unsigned integer; integer, numeric, and character values are
#'   accepted. Use `NULL` for all participants.
#' @param sensor A character vector of sensors. Use `NULL` to select all sensors
#'   named in `expected`.
#' @param by Bin width: one of `"minute"`, `"hour"`, `"day"`, `"week"`, or
#'   `"month"`, or a positive number of seconds. `NULL` (the default) uses each
#'   sensor's expected interval separately. If an explicit fixed width is
#'   smaller than a sensor's expected interval, it is raised to that interval
#'   for that sensor and a warning is issued. Calendar units keep their actual
#'   boundaries; a calendar month is not replaced by a fixed width even when it
#'   can be shorter than the expected interval.
#' @param metric What to compare: `"count"` (the default) for the proportion of
#'   expected measurements, `"interval"` for the fraction of eligible time
#'   covered by the union of observation intervals, or `"bin"` for the
#'   proportion of expected-interval slots containing an observation.
#' @param local Whether bins use participant-local wall-clock time from the
#'   `_with_local` views. Defaults to `TRUE`; use `FALSE` for UTC bins.
#' @param start_date,end_date Optional dates delimiting the included
#'   observations. Values must be dates or date strings.
#' @param week_start Day the week starts on: 1 = Monday, 7 = Sunday. Defaults to
#'   `getOption("lubridate.week.start", 7)`.
#'
#' @returns A lazy `tbl` of class `"coverage"` with `participant_id`, `time`
#'   (the bin start), `sensor`, and `coverage`. Bins with no observations are
#'   zero-filled within each participant's span. For `metric = "bin"`, a bin
#'   with no eligible expected-slot starts has `NA` coverage.
#' @family coverage functions
#' @export
#'
#' @examples
#' \dontrun{
#' db <- example_db()
#'
#' proportional <- coverage_proportional(
#'   db,
#'   expected = coverage_expected(),
#'   participant_id = "372780"
#' )
#' plot(proportional, cycle = "day")
#'
#' # Use a custom expected interval and 15-minute bins
#' coverage_proportional(
#'   db,
#'   expected = c(Accelerometer = 60),
#'   sensor = "Accelerometer",
#'   by = 900
#' )
#' }
coverage_proportional <- function(
  db,
  expected,
  participant_id = NULL,
  sensor = NULL,
  by = NULL,
  metric = c("count", "interval", "bin"),
  local = TRUE,
  start_date = NULL,
  end_date = NULL,
  week_start = getOption("lubridate.week.start", 7)
) {
  metric <- match.arg(metric)
  # Check the arguments and drop sensors without an `expected` interval.
  validated <- .coverage_validate_proportional(
    db = db,
    expected = expected,
    participant_id = participant_id,
    sensor = sensor,
    by = by,
    metric = metric,
    local = local,
    start_date = start_date,
    end_date = end_date,
    week_start = week_start
  )

  # Collect the validated arguments into one `spec` list, including `expected`.
  spec <- .coverage_normalise_spec(validated)

  # Skip timezone handling when all observations are in UTC.
  spec <- .coverage_resolve_local(spec)

  # Build the observation relation for each selected sensor and combine them.
  observations <- .coverage_relation_observations(spec)

  # Build the timezone episodes, offset segments, and observation span, and
  # apply the date filter on the local calendar.
  context <- .coverage_observation_context(spec, observations)

  # `metric` selects what the numerator and denominator count.
  coverage <- switch(
    spec$metric,
    count = .coverage_assemble_count(spec, context, proportional = TRUE),
    interval = .coverage_assemble_interval(spec, context),
    bin = .coverage_assemble_bin(spec, context)
  )

  # Attach the class and attributes used by collection and plotting.
  .coverage_table(spec, coverage)
}

#' Materialise a coverage result
#'
#' @param x A lazy coverage table created by [coverage_frequency()] or
#'   [coverage_proportional()].
#' @param ... Passed on to the underlying `collect()` method.
#'
#' @returns A tibble with rounded coverage values and factorised `sensor`.
#' @keywords internal
#' @exportS3Method dplyr::collect
collect.coverage <- function(x, ...) {
  requested <- attr(x, "participant_id")
  out <- NextMethod()
  # Sort in R: arranging the lazy coverage table and then collecting it would
  # redispatch to this method. Only the aggregated bins are sorted here;
  # transformed queries that dropped the bin columns are left as they are.
  if (all(c("participant_id", "time", "sensor") %in% names(out))) {
    out <- arrange(out, .data$participant_id, .data$time, .data$sensor)
  }

  if (nrow(out) == 0L) {
    if (is.null(requested)) {
      cli_abort("No observations found for the requested sensors and time range.")
    }
    cli_abort(
      "No observations found for participant{?s} {.val {requested}} in the requested time range."
    )
  }

  # Warn when an explicitly requested participant produced no observations.
  if (!is.null(requested) && "participant_id" %in% names(out)) {
    present <- unique(as.character(out$participant_id))
    missing <- setdiff(as.character(requested), present)
    if (length(missing) > 0L) {
      cli_warn(c(
        "No data found for participant{?s} {.val {missing}}; skipping.",
        i = "Only participants with observations are returned."
      ))
    }
  }

  # Round values for display, factor the sensors, and restore metadata/class.
  out$coverage <- round(out$coverage, 2)
  out$sensor <- factor(
    out$sensor,
    levels = rev(sort(unique(as.character(out$sensor))))
  )

  class(out) <- unique(c("coverage", class(out)))

  for (name in c(
    "participant_id",
    "coverage_type",
    "expected",
    "metric",
    "by",
    "week_start",
    "local"
  )) {
    attr(out, name) <- attr(x, name)
  }

  out
}

#' Plot a coverage overview
#'
#' @description
#' Draw a line chart, a sensor heatmap, or a participant-strip overview.
#' Multiple participants are averaged by default for lines and heatmaps; use
#' `aggregate = NULL` to facet them. `type = "bar"` shows one row per participant
#' and facets by sensor.
#'
#' @param x A coverage result from [coverage_frequency()] or
#'   [coverage_proportional()], either lazy or already collected.
#' @param digits Number of digits to show in heatmap labels. Defaults to 2.
#' @param type The plot type. `"auto"` (the default) uses a line graph when
#'   `cycle = NULL` and a heatmap otherwise. Use `"line"`, `"heatmap"`, or
#'   `"bar"` to force a layout. Bar plots place participants on the y-axis and
#'   facet by sensor.
#' @param cycle The calendar cycle used to average the values for plotting:
#'   `NULL`, `"hour"`, `"day"`, `"week"`, `"month"`, or `"year"`. Defaults to
#'   `NULL`, which plots the full time series. Each cycle is one calendar
#'   position: minute of the hour, hour of the day, weekday, day of the month
#'   (week of the month for weekly bins), or month (week of the year for weekly
#'   bins). Finer bins are averaged into that position. This only changes the
#'   plot; use dplyr `summarise()` on the returned table for other summaries.
#' @param aggregate How to combine multiple participants or sensors. `"auto"`
#'   (the default) averages participants for line and heatmap plots and keeps
#'   sensor facets for bar plots. `"participant_id"` averages across
#'   participants for line and heatmap plots. `"sensor"` averages across sensors
#'   for bar plots. `NULL` performs no averaging and facets the remaining
#'   dimension: participants for line/heatmap plots, sensors for bar plots.
#' @param label Whether to label weekday positions with abbreviations, as in
#'   [lubridate::wday()].
#' @param week_start Day the week starts on: 1 = Monday, 7 = Sunday. Defaults
#'   to the value used when constructing `x` (or `getOption("lubridate.week.start", 7)`).
#' @param ... Other arguments passed on to methods. Not currently used.
#'
#' @returns A [ggplot2::ggplot] object.
#' @family coverage functions
#' @export
#'
#' @examples
#' \dontrun{
#' db <- example_db()
#'
#' plot(coverage_frequency(db, sensor = "Accelerometer"), cycle = "day")
#' plot(coverage_proportional(db, coverage_expected()))
#' }
plot.coverage <- function(
  x,
  digits = 2,
  type = c("auto", "line", "heatmap", "bar"),
  cycle = NULL,
  aggregate = "auto",
  label = TRUE,
  week_start = NULL,
  ...
) {
  ensure_suggested_package("ggplot2")
  type <- match.arg(type)
  if (!is.null(aggregate)) {
    aggregate <- match.arg(aggregate, c("auto", "participant_id", "sensor"))
  }
  cycle <- if (is.null(cycle)) {
    NULL
  } else {
    match.arg(cycle, c("hour", "day", "week", "month", "year"))
  }
  check_arg(label, "logical", n = 1)

  # Collect first so plotting works on plain data frame columns.
  x <- collect(x)

  if (is.null(week_start)) {
    week_start <- attr(x, "week_start")
  }
  if (is.null(week_start)) {
    week_start <- getOption("lubridate.week.start", 7)
  }

  week_start <- check_week_start(week_start)

  is_proportional <- identical(attr(x, "coverage_type"), "proportional")
  metric <- attr(x, "metric")
  participant_id <- attr(x, "participant_id")
  by_spec <- attr(x, "by")

  if (!is.null(cycle) && .coverage_cycle_is_not_coarser(cycle, by_spec, x)) {
    cli_abort(c(
      "{.arg cycle} must be coarser than the largest {.arg by} bin.",
      i = "Choose a longer cycle or a smaller {.arg by} interval."
    ))
  }

  if (identical(type, "auto")) {
    type <- if (is.null(cycle)) "line" else "heatmap"
  }

  # Resolve the default: lines and sensor heatmaps average participants, while
  # participant strips keep their sensor facets.
  if (identical(aggregate, "auto")) {
    aggregate <- if (identical(type, "bar")) NULL else "participant_id"
  }
  if (identical(type, "bar") && identical(aggregate, "participant_id")) {
    cli_abort(c(
      "{.arg aggregate} cannot aggregate across participants for {.val bar} plots.",
      i = "Participants are already the rows; use {.val NULL} or {.val sensor}."
    ))
  }
  if (!identical(type, "bar") && identical(aggregate, "sensor")) {
    cli_abort(c(
      "{.arg aggregate} = {.val sensor} is only supported for type {.val bar}.",
      i = "Use {.arg type} = {.val bar} to average across sensors."
    ))
  }

  data_participants <- unique(x$participant_id)
  multiple_participants <- length(data_participants) > 1L

  # The title reflects the aggregation and whether participants are faceted.
  title <- if (is.null(participant_id) && !multiple_participants) {
    "Coverage for all participants"
  } else if (!is.null(participant_id) && length(participant_id) == 1L) {
    paste0("Coverage for participant ", participant_id)
  } else if (identical(aggregate, "participant_id") && !identical(type, "bar")) {
    "Coverage across participants"
  } else {
    "Coverage per participant"
  }

  # With a cycle, aggregate bins into calendar positions; without one, plot
  # the original bin times directly.
  if (is.null(cycle)) {
    position_col <- NULL
    x_col <- "time"
  } else {
    position_col <- .coverage_cycle_position(cycle, by_spec)
    # Derive all calendar components a cycle can use; `position_col` selects
    # the one that becomes the x-axis.
    x <- x |>
      mutate(
        month = month(.data$time),
        day_of_month = mday(.data$time),
        day_of_week = wday(
          .data$time,
          label = label,
          week_start = week_start
        ),
        hour = hour(.data$time),
        minute = minute(.data$time),
        second = second(.data$time),
        week_of_month = .coverage_week_number(.data$time, "month", week_start),
        week_of_year = .coverage_week_number(.data$time, "year", week_start)
      ) |>
      # Average repeated bins within each cycle position; groups whose
      # coverage is entirely missing stay NA.
      group_by(across(all_of(c("participant_id", "sensor", position_col)))) |>
      summarise(
        coverage = mean(.data$coverage, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        coverage = if_else(is.nan(.data$coverage), NA_real_, .data$coverage)
      )
    x_col <- position_col
  }

  # Collapse the selected dimension, keeping groups whose coverage is
  # entirely missing as NA.
  if (identical(aggregate, "participant_id")) {
    x <- x |>
      group_by(across(all_of(c("sensor", x_col)))) |>
      summarise(
        coverage = mean(.data$coverage, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        coverage = if_else(is.nan(.data$coverage), NA_real_, .data$coverage)
      )
  } else if (identical(aggregate, "sensor")) {
    if (!is_proportional) {
      x <- .coverage_add_scaled_coverage(x)
    }
    value_col <- if (is_proportional) "coverage" else "scaled_coverage"
    x <- x |>
      group_by(across(all_of(c("participant_id", x_col)))) |>
      summarise(
        coverage = mean(.data[[value_col]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        coverage = if_else(is.nan(.data$coverage), NA_real_, .data$coverage)
      )
  }

  # Count heatmaps and bar strips scale each sensor to its own maximum so one
  # shared fill scale stays comparable. Sensor averages were scaled above.
  if (!is_proportional && !identical(aggregate, "sensor") && !identical(type, "line")) {
    x <- .coverage_add_scaled_coverage(x)
  }

  add_participant_facets <- function(plot) {
    if (is.null(aggregate) && multiple_participants) {
      return(plot + ggplot2::facet_wrap(~participant_id))
    }
    plot
  }

  # Line chart of coverage over time or over the cycle's x position.
  if (identical(type, "line")) {
    plot <- ggplot2::ggplot(
      x,
      ggplot2::aes(
        x = .data[[x_col]],
        y = .data$coverage,
        colour = .data$sensor
      )
    ) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = title,
        x = if (identical(x_col, "time")) "Time" else x_col,
        y = if (!is_proportional) {
          "Measurements"
        } else if (identical(metric, "interval")) {
          "Proportion of time covered"
        } else if (identical(metric, "bin")) {
          "Proportion of occupied expected slots"
        } else {
          "Proportion of expected measurements"
        },
        colour = "Sensor"
      )
    return(add_participant_facets(plot))
  }

  # Participant lanes: one row per participant, coloured by coverage. Sensor
  # facets keep many participants readable without a facet per participant.
  if (identical(type, "bar")) {
    participant_levels <- rev(sort(unique(x$participant_id)))
    x <- x |>
      mutate(
        participant_id = factor(.data$participant_id, levels = participant_levels)
      )

    fill_col <- if (!is_proportional && is.null(aggregate)) {
      "scaled_coverage"
    } else {
      "coverage"
    }
    fill_label <- if (is_proportional) {
      "Proportion"
    } else if (is.null(aggregate)) {
      "Scaled count"
    } else {
      "Mean scaled count"
    }

    plot <- ggplot2::ggplot(
      x,
      ggplot2::aes(
        x = .data[[x_col]],
        y = .data$participant_id,
        fill = .data[[fill_col]]
      )
    ) +
      ggplot2::geom_tile() +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = title,
        x = if (identical(x_col, "time")) "Time" else x_col,
        y = "Participant",
        fill = fill_label
      )

    if (is.null(aggregate)) {
      plot <- plot + ggplot2::facet_wrap(~sensor, ncol = 1)
    }

    scale_max <- if (!is_proportional) {
      1
    } else if (identical(metric, "count")) {
      max(c(1, x$coverage), na.rm = TRUE)
    } else {
      1
    }

    return(plot + .coverage_fill_scale(is_proportional, scale_max))
  }

  # Tile grid with the value printed in each cell.
  fill_col <- if (is_proportional) "coverage" else "scaled_coverage"
  plot <- ggplot2::ggplot(
    x,
    ggplot2::aes(
      x = .data[[x_col]],
      y = .data$sensor,
      fill = .data[[fill_col]]
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(
      mapping = ggplot2::aes(label = round(.data$coverage, digits = digits)),
      colour = "white"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title,
      x = if (identical(x_col, "time")) "Time" else x_col,
      y = "Sensor",
      fill = if (is_proportional) "Proportion" else "Scaled count"
    )

  scale_max <- if (!is_proportional) {
    1
  } else if (identical(metric, "count")) {
    max(c(1, x$coverage), na.rm = TRUE)
  } else {
    1
  }

  add_participant_facets(plot + .coverage_fill_scale(is_proportional, scale_max))
}

# -- validation ----------------------------------------------------------------

# Check the frequency arguments and return them in one validated list.
.coverage_validate_frequency <- function(
  db,
  participant_id,
  sensor,
  by,
  local,
  start_date,
  end_date,
  week_start
) {
  check_db(db)
  check_arg(
    participant_id,
    type = c("character", "integerish", "numeric"),
    allow_null = TRUE
  )
  sensors <- check_sensors(sensor, allow_null = TRUE, resolve = TRUE)
  check_arg(local, "logical", n = 1)

  participants <- check_participants(db, participant_id)

  week_start <- check_week_start(week_start)
  by <- .coverage_check_by(by, required = TRUE)
  dates <- check_dates(start_date, end_date)

  list(
    db = db,
    participant_id = participant_id,
    participants = participants,
    sensors = sensors,
    sensor_explicit = !is.null(sensor),
    local = local,
    week_start = week_start,
    by = by,
    coverage_type = "frequency",
    start_date = dates$start_date,
    end_date = dates$end_date
  )
}

# Check the proportional arguments and resolve `expected` against the selected sensors.
.coverage_validate_proportional <- function(
  db,
  expected,
  participant_id,
  sensor,
  by,
  metric,
  local,
  start_date,
  end_date,
  week_start
) {
  check_db(db)
  check_arg(
    participant_id,
    type = c("character", "integerish", "numeric"),
    allow_null = TRUE
  )
  sensors <- check_sensors(sensor, allow_null = TRUE, resolve = TRUE)
  check_arg(local, "logical", n = 1)

  participants <- check_participants(db, participant_id)

  expected <- .coverage_check_expected(expected, sensors, sensor_explicit = !is.null(sensor))

  week_start <- check_week_start(week_start)
  by <- .coverage_check_by(by, required = FALSE)
  dates <- check_dates(start_date, end_date)

  list(
    db = db,
    participant_id = participant_id,
    participants = participants,
    sensors = expected$sensors,
    sensor_explicit = !is.null(sensor),
    local = local,
    week_start = week_start,
    by = by,
    metric = metric,
    coverage_type = "proportional",
    expected = expected$expected,
    start_date = dates$start_date,
    end_date = dates$end_date
  )
}

# -- normalisation -------------------------------------------------------------

# Combine validated inputs into the `spec` list used by the later stages. The
# proportional fields are NULL for frequency coverage.
.coverage_normalise_spec <- function(validated) {
  spec <- list(
    db = validated$db,
    participant_id = validated$participant_id,
    participants = validated$participants,
    sensors = validated$sensors,
    sensor_explicit = validated$sensor_explicit,
    local = validated$local,
    week_start = validated$week_start,
    expected = validated$expected,
    metric = validated$metric,
    coverage_type = validated$coverage_type,
    start_date = validated$start_date,
    end_date = validated$end_date,
    bin_spec = .coverage_make_bin_specs(
      validated$by,
      validated$sensors,
      expected = validated$expected
    )
  )

  spec$ios_sensors <- any(spec$sensors %in% .coverage_ios_sensors)
  if (spec$ios_sensors) {
    .coverage_warn_missing_platforms(spec$db, spec$participants, spec$participant_id)
  }

  spec
}

# -- shared argument checks ----------------------------------------------------

# Validate the expected intervals and keep only the selected sensors.
.coverage_check_expected <- function(expected, sensors, sensor_explicit) {
  check_arg(expected, type = "numeric")

  if (
    is.null(names(expected)) ||
      length(expected) == 0L ||
      anyNA(names(expected)) ||
      any(!nzchar(names(expected)))
  ) {
    cli_abort("{.arg expected} must be a named numeric vector of sensor intervals.")
  }

  names(expected) <- .standard_sensor_names(names(expected))
  if (anyDuplicated(names(expected))) {
    cli_abort("{.arg expected} must not contain duplicate sensor names.")
  }
  if (any(!is.finite(expected)) || any(expected < 0.000001)) {
    cli_abort("{.arg expected} must contain finite intervals of at least one microsecond.")
  }

  keep <- sensors[sensors %in% names(expected)]
  dropped <- setdiff(sensors, keep)
  if (length(dropped) > 0L && sensor_explicit) {
    cli_warn(c(
      "Dropping sensor{?s} without an expected interval: {.val {dropped}}.",
      i = "Add them to {.arg expected} to compute proportional coverage."
    ))
  }
  if (length(keep) == 0L) {
    cli_abort("No sensors left after filtering by {.arg expected}.")
  }

  expected <- stats::setNames(expected[keep], keep)
  list(expected = expected, sensors = keep)
}

# Validate one override for a sensor's expected interval.
.coverage_check_expected_override <- function(name, value) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      !is.finite(value) ||
      value < 0.000001
  ) {
    cli_abort("{.arg {name}} must be a positive, finite number of seconds.")
  }
  as.numeric(value)
}

# Validate `by` as a calendar unit or seconds; frequency requires it.
.coverage_check_by <- function(by, required) {
  if (is.null(by)) {
    if (required) {
      cli_abort("{.arg by} cannot be {.val NULL} for {.fn coverage_frequency}.")
    }
    return(NULL)
  }
  if (is.character(by)) {
    return(match.arg(by, c("minute", "hour", "day", "week", "month")))
  }

  check_arg(by, type = c("numeric", "integerish"), n = 1)
  if (!is.finite(by) || by < 0.000001) {
    cli_abort("{.arg by} must be a positive, finite number of seconds.")
  }

  as.numeric(by)
}

# -- sensor names and bin specifications ---------------------------------------

# Seconds in a fixed bin unit; NA for calendar months, which have no fixed length.
.coverage_unit_seconds <- function(unit) {
  switch(
    unit,
    minute = 60,
    hour = 3600,
    day = 86400,
    week = 604800,
    month = NA_real_,
    NA_real_
  )
}

# Fixed `by` below a sensor's expected interval widens to that interval; calendar
# months keep their actual boundaries and are never widened.
.coverage_make_bin_specs <- function(by, sensors, expected) {
  if (is.null(by)) {
    return(as.list(stats::setNames(expected[sensors], sensors)))
  }
  # NA seconds mean a calendar month, which keeps its own boundaries.
  by_seconds <- if (is.character(by)) .coverage_unit_seconds(by) else by
  too_small <- if (is.na(by_seconds)) {
    character(0)
  } else {
    sensors[expected[sensors] > by_seconds]
  }

  spec <- as.list(stats::setNames(rep(by, length(sensors)), sensors))
  if (length(too_small) > 0L) {
    .coverage_warn_bin_adjustment(by, too_small, expected)
    spec[too_small] <- as.list(expected[too_small])
  }

  spec
}

# Warn about sensors whose requested bin was widened to their expected
# interval, listing each adjustment.
.coverage_warn_bin_adjustment <- function(by, too_small, expected) {
  requested_width <- .coverage_bin_width_label(by)
  adjustments <- map_chr(too_small, \(sensor) {
    paste0(
      sensor,
      ": ",
      requested_width,
      " is shorter than ",
      .coverage_seconds_label(expected[[sensor]]),
      "; using ",
      .coverage_seconds_label(expected[[sensor]])
    )
  })
  cli_warn(c(
    "The requested {.arg by} is shorter than {.arg expected} for some sensors.",
    i = "{.val {adjustments}}.",
    i = paste(
      "Adjusted bin widths are sensor-specific and may differ",
      "between sensors."
    )
  ))
}

# Render a number of seconds as a plain string for warning text.
.coverage_seconds_label <- function(seconds) {
  paste0(format(seconds, trim = TRUE, scientific = FALSE), " seconds")
}

# Describe a bin width, expanding character units to their seconds.
.coverage_bin_width_label <- function(by) {
  if (is.character(by)) {
    return(paste0(by, " (", .coverage_seconds_label(.coverage_unit_seconds(by)), ")"))
  }
  .coverage_seconds_label(by)
}

# Collapse a uniform bin spec to a named vector for the output attribute.
.coverage_compact_bin_spec <- function(bin_spec) {
  if (!is.list(bin_spec)) {
    return(bin_spec)
  }
  types <- vapply(bin_spec, typeof, character(1))
  if (length(unique(types)) == 1L) {
    return(unlist(bin_spec, use.names = TRUE))
  }
  bin_spec
}

# One row per selected sensor with its bin unit and expected interval.
.coverage_settings <- function(spec) {
  spec$sensors |>
    map(\(sensor) {
      by <- spec$bin_spec[[sensor]]
      tibble(
        sensor = sensor,
        unit = if (is.character(by)) by else NA_character_,
        seconds = if (is.numeric(by)) as.numeric(by) else .coverage_unit_seconds(by),
        expected_seconds = if (is.null(spec$expected)) NA_real_ else unname(spec$expected[[sensor]])
      )
    }) |>
    bind_rows()
}

# -- relation building ---------------------------------------------------------

# Build the lazy observation relation covering every selected sensor.
.coverage_relation_observations <- function(spec) {
  relations <- map(spec$sensors, \(sensor) .coverage_observations(spec, sensor))

  # Combine the per-sensor relations into one lazy observation relation.
  reduce(relations, union_all)
}

# One row per selected sensor with the observation time and timezone.
.coverage_observations <- function(spec, sensor) {
  # No distinct() here: each later stage deduplicates where it needs to, so a
  # full observation scan would be wasted work.
  .coverage_filter_sensor(spec, sensor) |>
    select("participant_id", "time", "timezone") |>
    mutate(sensor = sensor)
}

# Participant, Heartbeat, and UTC date filtering for one sensor. The local date
# window is applied later, after each observation's timezone is known (see
# `.coverage_apply_dates()`).
.coverage_filter_sensor <- function(spec, sensor) {
  view <- if (spec$local) paste0(sensor, "_with_local") else sensor

  out <- tbl(spec$db, view)

  if (!is.null(spec$participant_id)) {
    out <- filter(out, .data$participant_id %in% !!as.character(spec$participant_id))
  }

  if (identical(sensor, "Heartbeat")) {
    out <- filter(
      out,
      is.na(.data$device_role_name) | !grepl("^Secondary", .data$device_role_name)
    )
  }

  if (!spec$local) {
    if (!is.null(spec$start_date)) {
      out <- filter(out, .data$time >= !!spec$start_date)
    }
    if (!is.null(spec$end_date)) {
      out <- filter(out, .data$time < !!(spec$end_date + 1))
    }
  }

  out
}

# TRUE if any selected observation has a non-UTC timezone, i.e. if local bins
# can differ from UTC bins in the requested range. Stops at the first such row.
# The date filter is deliberately ignored: a missing timezone inside the range
# can inherit a non-UTC zone from before it; empty strings count as missing.
.coverage_has_wall_clock <- function(spec) {
  probe <- spec
  probe$local <- FALSE
  probe$start_date <- NULL
  probe$end_date <- NULL

  relations <- map(probe$sensors, \(sensor) {
    # Only the timezone column is needed; keeping the relations narrow avoids
    # rendering the full sensor views.
    .coverage_filter_sensor(probe, sensor) |>
      select("timezone")
  })

  found <- relations |>
    reduce(union_all) |>
    filter(
      !is.na(.data$timezone),
      .data$timezone != "",
      .data$timezone != "UTC"
    ) |>
    utils::head(1) |>
    collect()
  nrow(found) > 0
}

# `local = TRUE` needs timezone handling only when some selected observation
# has a non-UTC timezone. Otherwise the local bins equal the UTC bins and the
# episode work would only produce one UTC episode, so use the UTC path and keep
# the requested value for the output attribute.
.coverage_resolve_local <- function(spec) {
  spec$local_requested <- spec$local

  if (spec$local) {
    spec$local <- .coverage_has_wall_clock(spec)
  }
  spec
}

# -- timezone episodes and offset segments -------------------------------------

# Build the timezone context for local mode: episodes, offset segments, and the
# observation span. UTC mode needs none of it.
.coverage_observation_context <- function(spec, observations) {
  if (!spec$local) {
    return(list(observations = observations, segments = NULL, spans = NULL))
  }

  episodes <- .coverage_build_episodes(observations) |> collect()
  # The episode query already computes the observation span; carry it along so
  # the count metric does not scan the observations a second time.
  spans <- episodes |>
    distinct(.data$participant_id, .data$span_start, .data$span_end) |>
    select("participant_id", "span_start", "span_end")
  episodes <- .coverage_extend_episodes(episodes, spec)

  segments <- .coverage_offset_segments(episodes, spec$db)
  observations <- .coverage_apply_dates(observations, segments, spec)

  list(observations = observations, segments = segments, spans = spans)
}

# Interval and bin spans reach past the last observation, so extend the last
# episode to cover any DST transition in that tail before splitting it into
# offset segments.
.coverage_extend_episodes <- function(episodes, spec) {
  if (nrow(episodes) == 0L || is.null(spec$expected)) {
    return(episodes)
  }
  bound <- max(as.numeric(episodes$ep_end)) + max(spec$expected)

  terminal <- !duplicated(episodes$participant_id, fromLast = TRUE)
  episodes$ep_end[terminal] <- as.POSIXct(
    pmax(as.numeric(episodes$ep_end[terminal]), bound),
    origin = "1970-01-01",
    tz = "UTC"
  )
  episodes
}

# Build timezone episodes from timezone changes. Missing timezones inherit the
# last known value (or UTC before the first known value), so only rows with a
# timezone can start an episode; the leading UTC stretch is added separately.
# The span columns let the count metric reuse the min/max scan.
.coverage_build_episodes <- function(observations) {
  observed <- observations |>
    select("participant_id", "time", "timezone")

  # Observation span per participant, reused by the count metric.
  spans <- observed |>
    summarise(
      span_start = min(.data$time, na.rm = TRUE),
      span_end = max(.data$time, na.rm = TRUE),
      .by = "participant_id"
    )
  starts <- .coverage_episode_starts(observed)

  # A first known timezone that starts after the span start needs a UTC
  # prefix episode covering the leading stretch.
  first_start <- starts |>
    summarise(
      ep_start = min(.data$ep_start, na.rm = TRUE),
      .by = "participant_id"
    )
  prefix <- spans |>
    left_join(first_start, by = "participant_id") |>
    filter(is.na(.data$ep_start) | .data$ep_start > .data$span_start) |>
    transmute(
      .data$participant_id,
      ep_start = .data$span_start,
      timezone = "UTC"
    )

  # Merge prefix and change points, then close each episode at the next start.
  union_all(prefix, starts) |>
    left_join(spans, by = "participant_id") |>
    group_by(.data$participant_id) |>
    window_order(.data$ep_start) |>
    mutate(ep_end = coalesce(lead(.data$ep_start), .data$span_end)) |>
    ungroup() |>
    select(
      "participant_id",
      "ep_start",
      "ep_end",
      "timezone",
      "span_start",
      "span_end"
    )
}

# Missing or empty timezones cannot start an episode, so drop them before the
# window step; data with no timezones then skips the sort. When rows at one
# instant disagree, keep the largest name so the tie-break is deterministic.
.coverage_episode_starts <- function(observed) {
  observed |>
    filter(!is.na(.data$timezone), .data$timezone != "") |>
    group_by(.data$participant_id, .data$time) |>
    summarise(timezone = max(.data$timezone, na.rm = TRUE), .groups = "drop") |>
    group_by(.data$participant_id) |>
    window_order(.data$time) |>
    mutate(previous = lag(.data$timezone)) |>
    ungroup() |>
    filter(is.na(.data$previous) | .data$previous != .data$timezone) |>
    select("participant_id", ep_start = "time", "timezone")
}

# Empty offset-segment relation with the column structure callers expect.
.coverage_empty_segments <- function() {
  tibble(
    participant_id = integer(),
    episode_id = integer(),
    seg_start = as.POSIXct(character(), tz = "UTC"),
    seg_end = as.POSIXct(character(), tz = "UTC"),
    timezone = character(),
    offset_seconds = numeric(),
    is_terminal = logical()
  )
}

# Cut one episode at the DST transitions inside it, using the transitions
# already looked up per timezone.
.coverage_split_episode <- function(episode, episode_id, transitions) {
  cuts <- transitions[[episode$timezone]]
  cuts <- cuts[cuts > episode$ep_start & cuts < episode$ep_end]
  bounds <- sort(unique(c(episode$ep_start, episode$ep_end, cuts)))
  starts <- if (length(bounds) == 1L) bounds else bounds[-length(bounds)]
  ends <- if (length(bounds) == 1L) bounds else bounds[-1L]

  tibble(
    participant_id = episode$participant_id,
    episode_id = episode_id,
    seg_start = starts,
    seg_end = ends,
    timezone = episode$timezone
  )
}

# Split every episode at DST transitions, then record the constant UTC
# offset of each resulting segment.
.coverage_offset_segments <- function(episodes, db) {
  if (nrow(episodes) == 0L) {
    return(.coverage_empty_segments())
  }

  # DST change points per timezone, shared by every episode using that zone.
  transitions <- .coverage_zone_transitions(db, episodes)

  # Cut each episode at the transitions that fall strictly inside it.
  segments <- seq_len(nrow(episodes)) |>
    map(\(i) .coverage_split_episode(episodes[i, ], i, transitions)) |>
    bind_rows()
  if (nrow(segments) == 0L) {
    return(.coverage_empty_segments())
  }

  # Offsets stay constant within a segment, so one lookup per segment start.
  segments$offset_seconds <- .coverage_offsets_at(
    db,
    segments$timezone,
    segments$seg_start
  )$offset_seconds

  segments |>
    # Sort before marking the last segment, which carries the inclusive end.
    arrange(.data$participant_id, .data$seg_start) |>
    group_by(.data$participant_id) |>
    mutate(is_terminal = row_number() == n()) |>
    ungroup() |>
    select(
      "participant_id",
      "episode_id",
      "seg_start",
      "seg_end",
      "timezone",
      "offset_seconds",
      "is_terminal"
    )
}

# Find each timezone's offset changes: an hourly grid of UTC offsets locates the
# brackets, which `.coverage_refine_transitions()` then narrows down.
.coverage_zone_transitions <- function(db, episodes) {
  from <- min(episodes$ep_start)
  to <- max(episodes$ep_end)
  zones <- unique(episodes$timezone)

  # Compute change points once per zone, then look them up per episode.
  transitions <- map(zones, \(timezone) {
    if (identical(timezone, "UTC")) {
      return(as.POSIXct(character(), tz = "UTC"))
    }
    grid <- .coverage_zone_offset_grid(db, timezone, from, to)
    # Bracket every hourly step where the offset changes, then refine it.
    changes <- which(diff(grid$offset_seconds) != 0)
    if (length(changes) == 0L) {
      return(as.POSIXct(character(), tz = "UTC"))
    }
    brackets <- tibble(
      bracket_id = seq_along(changes),
      timezone = timezone,
      lo = grid$instant[changes],
      hi = grid$instant[changes + 1L],
      offset_lo = grid$offset_seconds[changes]
    )
    .coverage_refine_transitions(db, brackets)
  })

  stats::setNames(transitions, zones)
}

# Hourly UTC-offset grid for one timezone, collected so changes can be found in R.
.coverage_zone_offset_grid <- function(db, timezone, from, to) {
  .coverage_inline(db, tibble(from = from, to = to)) |>
    mutate(
      instant = unnest(generate_series(.data$from, .data$to, to_seconds(3600))),
      offset_seconds = epoch(to_local_time(.data$instant, !!timezone)) -
        epoch(.data$instant)
    ) |>
    arrange(.data$instant) |>
    select("instant", "offset_seconds") |>
    collect()
}

# The session disables DuckDB insertion-order preservation, so batched lookups
# carry an explicit row key and are re-ordered before they are returned.
.coverage_offsets_at <- function(db, timezone, time) {
  if (length(time) == 0L) {
    return(tibble(offset_seconds = numeric()))
  }
  input <- tibble(
    id = seq_along(time),
    timezone = timezone,
    time = time
  )

  .coverage_inline(db, input) |>
    mutate(
      offset_seconds = epoch(to_local_time(.data$time, .data$timezone)) -
        epoch(.data$time)
    ) |>
    arrange(.data$id) |>
    select("offset_seconds") |>
    collect()
}

# DST transitions are whole-second instants in the tz database, so one pass
# over the seconds inside each hourly bracket finds every change exactly. A
# bisection would need roughly 30 DB round trips per transition instead.
.coverage_refine_transitions <- function(db, brackets) {
  .coverage_inline(db, brackets) |>
    mutate(
      second = unnest(generate_series(
        time_bucket(to_seconds(1), .data$lo),
        .data$hi,
        to_seconds(1)
      ))
    ) |>
    mutate(
      offset_seconds = epoch(to_local_time(.data$second, .data$timezone)) -
        epoch(.data$second)
    ) |>
    filter(.data$offset_seconds != .data$offset_lo) |>
    summarise(
      transition = min(.data$second, na.rm = TRUE),
      .by = c("bracket_id", "timezone")
    ) |>
    arrange(.data$bracket_id) |>
    collect() |>
    pull("transition")
}

# Convert a whole-microsecond value to a UTC POSIXct instant.
.coverage_time_from_us <- function(microseconds) {
  as.POSIXct(microseconds / 1e6, origin = "1970-01-01", tz = "UTC")
}

# Half-open date windows. In local mode the window is the local calendar day,
# which depends on each observation's timezone.
.coverage_apply_dates <- function(observations, segments, spec) {
  if (is.null(spec$start_date) && is.null(spec$end_date)) {
    return(observations)
  }
  if (!spec$local) {
    out <- observations
    if (!is.null(spec$start_date)) {
      out <- filter(out, .data$time >= !!spec$start_date)
    }
    if (!is.null(spec$end_date)) {
      out <- filter(out, .data$time < !!(spec$end_date + 1))
    }
    return(out)
  }

  windows <- .coverage_local_date_windows(segments, spec)

  if (nrow(windows) == 0L) {
    return(filter(observations, FALSE))
  }

  observations |>
    inner_join(
      .coverage_inline(spec$db, windows),
      by = join_by(participant_id, time >= from, time <= to)
    ) |>
    filter(.data$time < .data$to | .data$closed) |>
    select("participant_id", "sensor", "time", "timezone") |>
    distinct()
}

# UTC [from, to] window of each segment's local calendar day. The upper bound
# is half-open, except that the last segment keeps an observation exactly on the
# bound unless that instant is already the next local midnight.
.coverage_local_date_windows <- function(segments, spec) {
  start <- if (is.null(spec$start_date)) {
    -Inf
  } else {
    as.numeric(as.POSIXct(spec$start_date, tz = "UTC"))
  }
  end <- if (is.null(spec$end_date)) Inf else as.numeric(as.POSIXct(spec$end_date + 1, tz = "UTC"))
  seg_start <- as.numeric(segments$seg_start)
  seg_end <- as.numeric(segments$seg_end)

  # Intersect the segment with the requested window in local time.
  local_end <- end - segments$offset_seconds
  from <- pmax(seg_start, start - segments$offset_seconds)
  to <- pmin(seg_end, local_end)
  closed <- segments$is_terminal & seg_end < local_end
  keep <- to >= from

  # Round bounds to whole microseconds to match DuckDB's timestamp resolution.
  tibble(
    participant_id = segments$participant_id[keep],
    from = .coverage_time_from_us(round(from[keep] * 1e6)),
    to = .coverage_time_from_us(round(to[keep] * 1e6)),
    closed = closed[keep]
  )
}

# -- spans, segments, and cells ------------------------------------------------

# Clip the offset segments to the observation span and mark the last one so the
# metric can include an observation exactly on the span end.
.coverage_fit_segments <- function(segments, spans, inclusive_end) {
  # Interval and bin spans reach past the last observation, but no cell may
  # start after it; `label_end` caps the terminal segment's last label.
  if (!"label_end" %in% names(spans)) {
    spans$label_end <- spans$span_end
  }

  if (is.null(segments)) {
    # UTC mode has no episodes: use one segment per participant span.
    segments <- spans |>
      mutate(
        episode_id = 1L,
        seg_start = .data$span_start,
        seg_end = .data$span_end,
        timezone = "UTC",
        offset_seconds = 0,
        is_terminal = TRUE
      )
  } else {
    # Clip each segment to the span and drop the ones that no longer overlap.
    segments <- segments |>
      inner_join(spans, by = "participant_id") |>
      filter(.data$seg_end >= .data$span_start, .data$seg_start <= .data$span_end) |>
      mutate(
        seg_start = if_else(
          .data$seg_start < .data$span_start,
          .data$span_start,
          .data$seg_start
        ),
        seg_end = if_else(
          .data$seg_end > .data$span_end,
          .data$span_end,
          .data$seg_end
        )
      ) |>
      # Re-mark the last surviving segment and extend it to the span end.
      arrange(.data$participant_id, .data$seg_start) |>
      group_by(.data$participant_id) |>
      mutate(
        is_terminal = row_number() == n(),
        seg_end = if_else(.data$is_terminal, .data$span_end, .data$seg_end)
      ) |>
      ungroup() |>
      filter(!(.data$seg_start == .data$seg_end & !.data$is_terminal))
  }

  # `include_end` controls whether an observation exactly on the span end counts.
  segments |>
    mutate(include_end = .data$is_terminal & inclusive_end) |>
    select(
      "participant_id",
      "episode_id",
      "seg_start",
      "seg_end",
      "timezone",
      "offset_seconds",
      "is_terminal",
      "label_end",
      "include_end"
    )
}

# Lookup table that maps observations to their offset segment: each segment's
# start, UTC offset, and half-open match end. The 1us stays in SQL because
# `copy_inline()` cannot add a sub-second value to a POSIXct column.
.coverage_join_segments <- function(db, segments) {
  .coverage_inline(db, segments) |>
    transmute(
      .data$participant_id,
      .data$seg_start,
      seg_end_match = if_else(
        .data$include_end,
        .data$seg_end + to_seconds(0.000001),
        .data$seg_end
      ),
      seg_offset_seconds = .data$offset_seconds
    )
}

# Group sensors that use the same bin width so their bins can be computed
# together.
.coverage_bin_groups <- function(settings) {
  keys <- paste0(
    ifelse(is.na(settings$unit), "fixed", settings$unit),
    ":",
    settings$seconds
  )
  split(settings, keys)
}

# One cell per local bin inside each segment, plus empty cells for the
# spring-forward gap. Each cell stores the UTC range it covers so contributions
# can be joined and clipped to it.
.coverage_bin_cells <- function(spec, segments) {
  settings <- .coverage_settings(spec)
  segments_lazy <- .coverage_inline(spec$db, segments)
  # Local bin labels differ from UTC labels only when a segment has a non-zero
  # offset. With all offsets zero the labels are the same, so the count query
  # can skip the segment join.
  wall_clock <- spec$local && any(segments$offset_seconds != 0)

  # Build cells for each bin-spec group, then merge the groups.
  cells <- settings |>
    .coverage_bin_groups() |>
    map(\(group) {
      .coverage_cells_for_group(spec$db, segments_lazy, group, spec$week_start, wall_clock)
    })
  cells <- reduce(cells, union_all)

  # Spring-forward gaps have no segment; add their empty cells.
  gaps <- .coverage_gap_cells(segments, settings)

  if (nrow(gaps) > 0L) {
    cells <- union_all(
      cells,
      .coverage_inline(spec$db, gaps, types = c(bin_start = "TIMESTAMP"))
    )
  }

  cells
}

# Expand one group of sensors with the same bin width into bin cells per segment.
.coverage_cells_for_group <- function(db, segments, group, week_start, wall_clock) {
  unit <- group$unit[[1]]
  seconds <- group$seconds[[1]]
  step <- .coverage_bin_step_expr(unit, seconds)
  settings <- .coverage_inline(db, group)

  # UTC labels stay TIMESTAMPTZ so they match the count query's bin labels;
  # local labels need the timezone conversion. Interior segments stop half-open
  # so the next segment owns the boundary bin; the terminal segment stops at the
  # last observation so no bin starts after it.
  label_bound <- expr(if_else(
    .data$is_terminal,
    .data$label_end,
    .data$seg_end - to_seconds(0.000001)
  ))
  local_start <- if (wall_clock) {
    expr(to_local_time(.data$seg_start, .data$timezone))
  } else {
    expr(.data$seg_start)
  }
  local_end <- if (wall_clock) {
    expr(to_local_time(!!label_bound, .data$timezone))
  } else {
    label_bound
  }

  segments |>
    # Each segment expands to one row per bin label that intersects it.
    cross_join(settings) |>
    mutate(
      local_start = !!local_start,
      local_end = !!local_end,
      first_label = !!(.coverage_bin_start_expr(
        .data$local_start,
        unit,
        seconds,
        week_start
      )),
      last_label = !!(.coverage_bin_start_expr(
        .data$local_end,
        unit,
        seconds,
        week_start
      )),
      bin_start = unnest(generate_series(.data$first_label, .data$last_label, !!step))
    ) |>
    # Clip each label's UTC range to the segment.
    mutate(
      next_label = .data$bin_start + !!step,
      cell_start = pmax(
        .data$bin_start - to_seconds(.data$offset_seconds),
        .data$seg_start,
        na.rm = TRUE
      ),
      cell_end = if_else(
        .data$include_end,
        pmin(
          .data$next_label - to_seconds(.data$offset_seconds),
          .data$seg_end + to_seconds(0.000001),
          na.rm = TRUE
        ),
        pmin(
          .data$next_label - to_seconds(.data$offset_seconds),
          .data$seg_end,
          na.rm = TRUE
        )
      )
    ) |>
    select(
      "participant_id",
      "sensor",
      "bin_start",
      "cell_start",
      "cell_end",
      "expected_seconds"
    )
}

# Bins that no offset segment covers (the spring-forward gap) still appear as
# empty cells. Only a forward offset jump within one episode creates them; a
# timezone change between episodes does not.
.coverage_gap_cells <- function(segments, settings) {
  fixed <- settings[!is.na(settings$seconds), , drop = FALSE]
  if (nrow(segments) == 0L || nrow(fixed) == 0L) {
    return(.coverage_empty_cells())
  }

  # A forward offset jump between consecutive segments leaves a local-time hole.
  jumps <- segments |>
    arrange(.data$participant_id, .data$episode_id, .data$seg_start) |>
    group_by(.data$participant_id, .data$episode_id) |>
    mutate(previous_offset = lag(.data$offset_seconds)) |>
    ungroup() |>
    filter(!is.na(.data$previous_offset), .data$offset_seconds > .data$previous_offset)

  if (nrow(jumps) == 0L) {
    return(.coverage_empty_cells())
  }

  # Fixed-width sensors get one degenerate cell per skipped label, capped at
  # the last observation's label so a gap after the last measurement does not
  # add a row. The offset applicable at the last observation gives its label.
  cap_offset <- segments |>
    filter(.data$seg_start <= .data$label_end) |>
    arrange(.data$participant_id, .data$seg_start) |>
    group_by(.data$participant_id) |>
    slice(n()) |>
    ungroup() |>
    transmute(
      .data$participant_id,
      cap_offset = .data$offset_seconds
    )

  skipped <- jumps |>
    cross_join(fixed) |>
    left_join(cap_offset, by = "participant_id") |>
    mutate(
      last_label = .coverage_gap_bucket_before(
        .data$seg_start,
        .data$previous_offset,
        .data$seconds
      ),
      first_label = .coverage_gap_bucket(
        .data$seg_start,
        .data$offset_seconds,
        .data$seconds
      ),
      count = floor(
        (as.numeric(.data$first_label) - as.numeric(.data$last_label)) /
          .data$seconds -
          1e-9
      ),
      cap = .coverage_gap_bucket(.data$label_end, .data$cap_offset, .data$seconds),
      count = pmin(
        .data$count,
        floor((as.numeric(.data$cap) - as.numeric(.data$last_label)) / .data$seconds + 1e-9)
      )
    ) |>
    filter(.data$count >= 1)

  if (nrow(skipped) == 0L) {
    return(.coverage_empty_cells())
  }

  # One row per skipped label: repeat each setting's row `count` times and step
  # through the intermediate labels.
  expanded <- skipped[rep(seq_len(nrow(skipped)), skipped$count), , drop = FALSE]
  expanded$bin_start <- as.POSIXct(
    as.numeric(expanded$last_label) + sequence(skipped$count) * expanded$seconds,
    origin = "1970-01-01",
    tz = "UTC"
  )

  expanded |>
    transmute(
      .data$participant_id,
      .data$sensor,
      .data$bin_start,
      cell_start = .data$seg_start,
      cell_end = .data$seg_start,
      .data$expected_seconds
    )
}

# Bucket start for a local instant, using DuckDB's time_bucket() origin
# (2000-01-03) so bins that do not divide a day stay on the same grid.
.coverage_gap_bucket <- function(time, offset_seconds, step) {
  origin <- .coverage_time_bucket_origin()
  local <- as.numeric(time) + offset_seconds - origin
  as.POSIXct(floor(local / step) * step + origin, origin = "1970-01-01", tz = "UTC")
}

# Bucket start strictly before the instant: the last label covered before a gap.
.coverage_gap_bucket_before <- function(time, offset_seconds, step) {
  origin <- .coverage_time_bucket_origin()
  local <- as.numeric(time) + offset_seconds
  bucket <- floor((local - origin) / step) * step + origin
  bucket <- if_else(abs(local - bucket) < 1e-6, bucket - step, bucket)
  as.POSIXct(bucket, origin = "1970-01-01", tz = "UTC")
}

# DuckDB's time_bucket() origin, shared by the R-side gap-label calculations.
.coverage_time_bucket_origin <- function() {
  as.numeric(as.POSIXct("2000-01-03 00:00:00", tz = "UTC"))
}

# Empty cell relation with the column structure callers expect.
.coverage_empty_cells <- function() {
  tibble(
    participant_id = integer(),
    sensor = character(),
    bin_start = as.POSIXct(character(), tz = "UTC"),
    cell_start = as.POSIXct(character(), tz = "UTC"),
    cell_end = as.POSIXct(character(), tz = "UTC"),
    expected_seconds = numeric()
  )
}

# -- count / frequency metric --------------------------------------------------

# Count metric: coverage is the number of distinct events per cell; the
# proportional variant divides by expected opportunities.
.coverage_assemble_count <- function(spec, context, proportional) {
  observations <- context$observations
  # The local episode query already returned the observation span. Reuse it
  # unless a date filter narrowed the observations after that query ran.
  spans <- context$spans
  if (is.null(spans) || !is.null(spec$start_date) || !is.null(spec$end_date)) {
    spans <- observations |>
      summarise(
        span_start = min(.data$time, na.rm = TRUE),
        span_end = max(.data$time, na.rm = TRUE),
        .by = "participant_id"
      ) |>
      collect()
  }

  segments <- .coverage_fit_segments(
    context$segments,
    spans,
    inclusive_end = TRUE
  )
  cells <- .coverage_bin_cells(spec, segments)

  # Count distinct observation instants per cell.
  counts <- observations |>
    select("participant_id", "sensor", event_time = "time") |>
    .coverage_count_cells(spec, segments, cells)

  # Frequency returns the numerator alone; proportional count divides by the
  # opportunity each cell offered.
  if (!proportional) {
    out <- .coverage_combine_cells(cells, counts)
  } else {
    denominators <- .coverage_count_opportunities(spec, cells, spans)
    out <- .coverage_combine_cells(cells, counts, denominators)
  }

  .coverage_mark_ios_na(out, spec)
}

# Assign each event to the bin for its local time. The join is on the computed
# bin label rather than on the cell ranges.
.coverage_count_cells <- function(events, spec, segments, cells) {
  labels <- cells |>
    distinct(.data$participant_id, .data$sensor, .data$bin_start)
  settings <- .coverage_settings(spec)

  # When output is UTC, or all offsets are zero, local bin labels equal UTC
  # labels, so the segment join and timezone conversion are unnecessary.
  wall_clock <- spec$local && any(segments$offset_seconds != 0)
  offset_segments <- if (wall_clock) .coverage_join_segments(spec$db, segments)

  # Bin events per sensor group, then count distinct instants per cell.
  settings |>
    .coverage_bin_groups() |>
    map(\(group) {
      .coverage_count_group(group, events, labels, spec$week_start, offset_segments)
    }) |>
    reduce(union_all)
}

# Count the events of one bin-width group per cell. `offset_segments` is NULL
# for UTC output, where events are bucketed without a segment join.
.coverage_count_group <- function(group, events, labels, week_start, offset_segments) {
  group_events <- events |> filter(.data$sensor %in% !!group$sensor)

  if (is.null(offset_segments)) {
    # UTC mode: bucket the event time directly.
    group_events <- group_events |>
      mutate(
        bin_start = !!(.coverage_bin_start_expr(
          .data$event_time,
          group$unit[[1]],
          group$seconds[[1]],
          week_start
        ))
      )
  } else {
    # Local mode: attach the event's offset segment and convert to local time.
    group_events <- group_events |>
      inner_join(
        offset_segments,
        by = join_by(
          participant_id,
          event_time >= seg_start,
          event_time < seg_end_match
        )
      ) |>
      mutate(
        local_time = to_local_time(
          .data$event_time + to_seconds(.data$seg_offset_seconds),
          "UTC"
        ),
        bin_start = !!(.coverage_bin_start_expr(
          .data$local_time,
          group$unit[[1]],
          group$seconds[[1]],
          week_start
        ))
      )
  }

  group_events |>
    inner_join(labels, by = c("participant_id", "sensor", "bin_start")) |>
    summarise(
      value = n_distinct(.data$event_time, na.rm = TRUE),
      .by = c("participant_id", "sensor", "bin_start")
    )
}

# Sum one opportunity per cell, floored at a full expected interval.
.coverage_count_opportunities <- function(spec, cells, spans) {
  cells |>
    inner_join(.coverage_inline(spec$db, spans), by = "participant_id") |>
    mutate(
      clipped_start = pmax(.data$cell_start, .data$span_start, na.rm = TRUE),
      clipped_end = pmin(.data$cell_end, .data$span_end, na.rm = TRUE),
      seconds = pmax(
        !!(.coverage_seconds_between_expr(.data$clipped_end, .data$clipped_start)),
        0,
        na.rm = TRUE
      ),
      opportunities = pmax(.data$seconds, .data$expected_seconds, na.rm = TRUE) /
        .data$expected_seconds
    ) |>
    summarise(
      value = sum(.data$opportunities, na.rm = TRUE),
      .by = c("participant_id", "sensor", "bin_start")
    )
}

# -- interval metric -----------------------------------------------------------

# Interval metric: covered seconds per cell divided by the cell's span seconds.
.coverage_assemble_interval <- function(spec, context) {
  observations <- context$observations

  expectations <- .coverage_settings(spec) |> select("sensor", "expected_seconds")

  spans <- observations |>
    inner_join(.coverage_inline(spec$db, expectations), by = "sensor") |>
    mutate(seg_end = .data$time + to_seconds(.data$expected_seconds)) |>
    summarise(
      span_start = min(.data$time, na.rm = TRUE),
      span_end = max(.data$seg_end, na.rm = TRUE),
      label_end = max(.data$time, na.rm = TRUE),
      .by = "participant_id"
    ) |>
    collect()
  segments <- .coverage_fit_segments(
    context$segments,
    spans,
    inclusive_end = FALSE
  )

  cells <- .coverage_bin_cells(spec, segments)

  # Merge overlapping and touching expected intervals, then split them at cells.
  merged <- .coverage_merge_intervals(observations, .coverage_inline(spec$db, expectations))
  covered <- .coverage_interval_pieces(merged, cells)
  # Compare covered seconds with the time each cell could cover.
  denominators <- .coverage_interval_opportunities(spec, cells, spans)
  out <- .coverage_combine_cells(cells, covered, denominators, na_when_zero = TRUE)

  .coverage_mark_ios_na(out, spec)
}

# Merge overlapping and touching [time, time + expected) intervals.
.coverage_merge_intervals <- function(observations, expectations) {
  observations |>
    inner_join(expectations, by = "sensor") |>
    mutate(
      seg_start = .data$time,
      seg_end = .data$time + to_seconds(.data$expected_seconds)
    ) |>
    # Track the running maximum end within each participant and sensor.
    group_by(.data$participant_id, .data$sensor) |>
    window_order(.data$seg_start, .data$seg_end) |>
    mutate(prior_end = lag(.data$seg_end)) |>
    mutate(prior_max = cummax(.data$prior_end)) |>
    mutate(
      is_new = if_else(
        is.na(.data$prior_max) | .data$seg_start > .data$prior_max,
        1L,
        0L
      )
    ) |>
    mutate(island = cumsum(.data$is_new)) |>
    ungroup() |>
    # Collapse each run of overlapping intervals to its outer bounds.
    group_by(.data$participant_id, .data$sensor, .data$island) |>
    summarise(
      seg_start = min(.data$seg_start, na.rm = TRUE),
      seg_end = max(.data$seg_end, na.rm = TRUE),
      .groups = "drop"
    ) |>
    select("participant_id", "sensor", "seg_start", "seg_end")
}

# Split the merged covered set at cell boundaries and sum overlap seconds.
.coverage_interval_pieces <- function(merged, cells) {
  merged |>
    inner_join(
      cells,
      by = join_by(participant_id, sensor, seg_start < cell_end, seg_end > cell_start)
    ) |>
    mutate(
      piece_start = pmax(.data$seg_start, .data$cell_start, na.rm = TRUE),
      piece_end = pmin(.data$seg_end, .data$cell_end, na.rm = TRUE)
    ) |>
    summarise(
      value = sum(
        pmax(
          !!(.coverage_seconds_between_expr(.data$piece_end, .data$piece_start)),
          0,
          na.rm = TRUE
        ),
        na.rm = TRUE
      ),
      .by = c("participant_id", "sensor", "bin_start")
    )
}

# Time each cell could cover: its extent clipped to the participant span.
.coverage_interval_opportunities <- function(spec, cells, spans) {
  cells |>
    inner_join(.coverage_inline(spec$db, spans), by = "participant_id") |>
    mutate(
      clipped_start = pmax(.data$cell_start, .data$span_start, na.rm = TRUE),
      clipped_end = pmin(.data$cell_end, .data$span_end, na.rm = TRUE)
    ) |>
    summarise(
      value = sum(
        pmax(
          !!(.coverage_seconds_between_expr(.data$clipped_end, .data$clipped_start)),
          0,
          na.rm = TRUE
        ),
        na.rm = TRUE
      ),
      .by = c("participant_id", "sensor", "bin_start")
    )
}

# -- bin metric ----------------------------------------------------------------

# Bin metric: occupied grid slots per cell divided by eligible grid slots.
.coverage_assemble_bin <- function(spec, context) {
  observations <- context$observations

  expectations <- .coverage_settings(spec) |> select("sensor", "expected_seconds")

  # The slot grid fixes the participant span and the denominator anchors.
  slot_grid <- .coverage_slot_grid(observations, expectations, spec$db)

  spans <- slot_grid$spans
  segments <- .coverage_fit_segments(
    context$segments,
    spans,
    inclusive_end = FALSE
  )

  cells <- .coverage_bin_cells(spec, segments)

  # Numerator: grid slot starts occupied by an observation.
  occupied <- .coverage_occupied_slots(slot_grid$slots, spec, segments, cells)
  # Denominator: grid slot starts the cell and span make eligible.
  eligible <- .coverage_eligible_slots(spec, cells, spans, slot_grid$anchors)

  # Ratio, keeping NA when a cell has no eligible slots.
  out <- .coverage_combine_cells(cells, occupied, eligible, na_when_zero = TRUE)

  .coverage_mark_ios_na(out, spec)
}

# Anchor the slot grid at each participant-sensor's first observation
# and span it to the end of the latest occupied slot.
.coverage_slot_grid <- function(observations, expectations, db) {
  expectations <- .coverage_inline(db, expectations)

  # Anchor the grid at each participant-sensor's first observation.
  anchors <- observations |>
    summarise(
      anchor = min(.data$time, na.rm = TRUE),
      .by = c("participant_id", "sensor")
    )
  # Occupied slot start for each observation, deduplicated.
  slots <- observations |>
    inner_join(anchors, by = c("participant_id", "sensor")) |>
    inner_join(expectations, by = "sensor") |>
    mutate(
      index = floor(
        !!(.coverage_seconds_between_expr(.data$time, .data$anchor)) /
          .data$expected_seconds
      ),
      slot_start = .data$anchor + to_seconds(.data$index * .data$expected_seconds)
    ) |>
    distinct(
      .data$participant_id,
      .data$sensor,
      .data$slot_start,
      .data$expected_seconds
    )

  list(
    anchors = collect(anchors),
    slots = slots,
    spans = .coverage_slot_spans(observations, expectations) |> collect()
  )
}

# The span ends at the last observation rounded up to the next slot boundary.
.coverage_slot_spans <- function(observations, expectations) {
  observations |>
    inner_join(expectations, by = "sensor") |>
    summarise(
      anchor = min(.data$time, na.rm = TRUE),
      latest = max(.data$time, na.rm = TRUE),
      .by = c("participant_id", "sensor", "expected_seconds")
    ) |>
    mutate(
      last_slot = .data$anchor +
        to_seconds(
          floor(
            !!(.coverage_seconds_between_expr(.data$latest, .data$anchor)) /
              .data$expected_seconds
          ) *
            .data$expected_seconds
        )
    ) |>
    summarise(
      span_start = min(.data$anchor, na.rm = TRUE),
      span_end = max(.data$last_slot + to_seconds(.data$expected_seconds), na.rm = TRUE),
      label_end = max(.data$latest, na.rm = TRUE),
      .by = "participant_id"
    )
}

# Count occupied grid slots with the same bin assignment as observations.
.coverage_occupied_slots <- function(slots, spec, segments, cells) {
  slots |>
    select("participant_id", "sensor", event_time = "slot_start") |>
    .coverage_count_cells(spec, segments, cells)
}

# Count expected slot starts in the half-open cell/span intersection.
.coverage_eligible_slots <- function(spec, cells, spans, anchors) {
  cells |>
    inner_join(.coverage_inline(spec$db, spans), by = "participant_id") |>
    left_join(
      .coverage_inline(spec$db, anchors),
      by = c("participant_id", "sensor")
    ) |>
    mutate(
      anchor = coalesce(.data$anchor, .data$span_start),
      clipped_start = pmax(.data$cell_start, .data$span_start, na.rm = TRUE),
      clipped_end = pmin(.data$cell_end, .data$span_end, na.rm = TRUE)
    ) |>
    mutate(
      slots = !!(.coverage_slot_count_expr(
        .data$anchor,
        .data$clipped_start,
        .data$clipped_end,
        .data$expected_seconds
      ))
    ) |>
    summarise(
      value = sum(.data$slots, na.rm = TRUE),
      .by = c("participant_id", "sensor", "bin_start")
    )
}

# -- assembly helpers ----------------------------------------------------------

# Assemble one row per cell: frequency coverage is the numerator alone, while
# proportional metrics divide the numerator by the denominator.
.coverage_combine_cells <- function(
  cells,
  numerator,
  denominator = NULL,
  na_when_zero = FALSE
) {
  out <- cells |>
    distinct(.data$participant_id, .data$sensor, .data$bin_start) |>
    left_join(
      numerator,
      by = c("participant_id", "sensor", "bin_start")
    )

  if (is.null(denominator)) {
    # Frequency: cells without a contribution are zero.
    out <- mutate(out, coverage = as.numeric(coalesce(.data$value, 0)))
  } else {
    out <- out |>
      left_join(
        denominator,
        by = c("participant_id", "sensor", "bin_start"),
        suffix = c(".numerator", ".denominator")
      )
    if (na_when_zero) {
      # Interval and bin metrics keep empty cells as NA.
      out <- out |>
        mutate(
          coverage = if_else(
            coalesce(.data$value.denominator, 0) == 0,
            NA_real_,
            coalesce(.data$value.numerator, 0) / .data$value.denominator
          )
        )
    } else {
      out <- mutate(
        out,
        coverage = coalesce(.data$value.numerator, 0) / .data$value.denominator
      )
    }
  }

  out |>
    select("participant_id", time = "bin_start", "sensor", "coverage")
}

# Copy a small R data frame into DuckDB with explicit column types, defaulting
# POSIXct to TIMESTAMPTZ and everything else to its closest DuckDB type.
.coverage_inline <- function(db, data, types = NULL) {
  all_types <- vapply(
    data,
    \(column) {
      if (inherits(column, "POSIXt")) {
        "TIMESTAMPTZ"
      } else if (is.logical(column)) {
        "BOOLEAN"
      } else if (is.integer(column)) {
        "INTEGER"
      } else if (is.numeric(column)) {
        "DOUBLE"
      } else {
        "VARCHAR"
      }
    },
    character(1)
  )

  if (!is.null(types)) {
    all_types[names(types)] <- unlist(types)
  }

  dbplyr::copy_inline(db, data, types = all_types)
}

# iOS participants do not expose the four iOS-only sensors, so their coverage
# is set to NA rather than zero.
.coverage_mark_ios_na <- function(out, spec) {
  if (!any(spec$sensors %in% .coverage_ios_sensors)) {
    return(out)
  }

  ios <- tbl(spec$db, "Device") |>
    filter(grepl("ios|iphone|ipad", tolower(.data$platform))) |>
    distinct(.data$participant_id) |>
    mutate(is_ios = TRUE)

  out |>
    left_join(ios, by = "participant_id") |>
    mutate(
      coverage = if_else(
        coalesce(.data$is_ios, FALSE) & .data$sensor %in% .coverage_ios_sensors,
        NA_real_,
        .data$coverage
      )
    ) |>
    select(-"is_ios")
}

# Add the `coverage` class and the metadata attributes carried to collection.
.coverage_table <- function(spec, coverage) {
  class(coverage) <- c("coverage", class(coverage))

  attr(coverage, "participant_id") <- spec$participant_id
  attr(coverage, "coverage_type") <- spec$coverage_type
  attr(coverage, "expected") <- spec$expected
  attr(coverage, "metric") <- spec$metric
  attr(coverage, "by") <- .coverage_compact_bin_spec(spec$bin_spec)
  attr(coverage, "week_start") <- spec$week_start
  attr(coverage, "local") <- spec$local_requested %||% spec$local

  coverage
}

# Warn for selected participants without a usable Device platform row; iOS
# marking cannot exclude their iOS-specific sensors.
.coverage_warn_missing_platforms <- function(db, participants, participant_id) {
  selected <- if (is.null(participant_id)) {
    as.character(participants)
  } else {
    unique(as.character(participant_id))
  }
  platforms <- tbl(db, "Device") |>
    filter(.data$participant_id %in% !!selected) |>
    select("participant_id", "platform") |>
    distinct() |>
    collect()
  known <- platforms |>
    filter(!is.na(.data$platform), nzchar(trimws(.data$platform))) |>
    pull(.data$participant_id) |>
    as.character() |>
    unique()
  unknown <- setdiff(selected, known)
  if (length(unknown) > 0L) {
    cli_warn(c(
      "Operating system is unknown for participant{?s} {.val {unknown}}.",
      i = "Device information was not collected; iOS-specific sensors cannot be excluded."
    ))
  }

  invisible(NULL)
}

# -- dbplyr expression helpers -------------------------------------------------

# These helpers build expressions that dbplyr translates. Unquote them in
# `mutate()` with `x = !!(.coverage_..._expr(...))`. DuckDB functions unknown to
# dbplyr (date_diff, epoch, to_seconds, generate_series, time_bucket) are passed
# through with their arguments escaped.
# Exact seconds between two instants, computed by DuckDB.
.coverage_seconds_between_expr <- function(later, earlier) {
  later <- enexpr(later)
  earlier <- enexpr(earlier)

  expr(date_diff("microsecond", !!earlier, !!later) / 1e6)
}

# Number of expected-grid starts in [start, end), counted with ceiling().
.coverage_slot_count_expr <- function(anchor, start, end, expected_seconds) {
  anchor <- enexpr(anchor)
  start <- enexpr(start)
  end <- enexpr(end)
  expected_seconds <- enexpr(expected_seconds)
  yes <- expr(date_diff("microsecond", !!anchor, !!end) / 1e6)
  no <- expr(date_diff("microsecond", !!anchor, !!start) / 1e6)

  expr(
    pmax(
      ceiling(!!yes / !!expected_seconds) - ceiling(!!no / !!expected_seconds),
      0,
      na.rm = TRUE
    )
  )
}

# One bin step: a fixed number of seconds or one calendar month.
.coverage_bin_step_expr <- function(unit, seconds) {
  if (identical(unit, "month")) {
    expr(to_months(1L))
  } else {
    expr(to_seconds(!!seconds))
  }
}

# Start of the bin containing `x`: time_bucket() for fixed widths, floor_date()
# for calendar units.
.coverage_bin_start_expr <- function(x, unit, seconds, week_start) {
  x <- enexpr(x)

  if (is.na(unit)) {
    return(expr(time_bucket(to_seconds(!!seconds), !!x)))
  }

  expr(
    as_datetime(
      floor_date(!!x, !!unit, week_start = !!week_start)
    )
  )
}

# -- plot helpers --------------------------------------------------------------

# Observed period length in seconds for cycle aggregation; calendar months and
# years vary, so use the actual ranges present in the data.
.coverage_period_seconds <- function(time, unit) {
  if (unit %in% c("month", "year")) {
    start <- unique(floor_date(time, unit))
    end <- ceiling_date(start, unit, change_on_boundary = TRUE)
    return(unique(as.numeric(difftime(end, start, units = "secs"))))
  }

  c(minute = 60, hour = 3600, day = 86400, week = 604800)[[unit]]
}

# TRUE when a requested cycle is not strictly coarser than every bin spec.
.coverage_cycle_is_not_coarser <- function(cycle, bin_spec, x) {
  specs <- if (is.list(bin_spec)) bin_spec else as.list(bin_spec)
  bin_rank <- c(minute = 1L, hour = 2L, day = 3L, week = 4L, month = 5L)
  cycle_rank <- c(hour = 2L, day = 3L, week = 4L, month = 5L, year = 6L)

  if (all(map_lgl(specs, is.character))) {
    return(any(map_lgl(
      specs,
      \(spec) bin_rank[[spec]] >= cycle_rank[[cycle]]
    )))
  }

  cycle_seconds <- min(.coverage_period_seconds(x$time, cycle))
  any(map_lgl(specs, \(spec) {
    if (is.character(spec)) {
      bin_rank[[spec]] >= cycle_rank[[cycle]]
    } else {
      spec >= cycle_seconds
    }
  }))
}

# Week-of-month or week-of-year index for a date, respecting `week_start`.
.coverage_week_number <- function(time, period, week_start) {
  day <- if (identical(period, "month")) {
    mday(time)
  } else {
    yday(time)
  }

  period_start <- floor_date(time, period)
  start_weekday <- wday(period_start, week_start = week_start)
  as.integer(floor((day + start_weekday - 2) / 7) + 1)
}

# Calendar position used to aggregate bins into the requested cycle. Finer
# bins are averaged into the same position rather than adding detail facets.
.coverage_cycle_position <- function(cycle, bin_spec) {
  specs <- if (is.list(bin_spec)) bin_spec else as.list(bin_spec)
  weekly_bins <- all(map_lgl(specs, \(spec) identical(spec, "week")))

  switch(
    cycle,
    hour = "minute",
    day = "hour",
    week = "day_of_week",
    month = if (weekly_bins) "week_of_month" else "day_of_month",
    year = if (weekly_bins) "week_of_year" else "month"
  )
}

# Scale each sensor's counts to its own global maximum so sensors with
# different sampling rates share one fill scale.
.coverage_add_scaled_coverage <- function(data) {
  data |>
    group_by(.data$sensor) |>
    mutate(max_coverage = max(coalesce(.data$coverage, 0))) |>
    mutate(
      max_coverage = if_else(
        !is.finite(.data$max_coverage) | .data$max_coverage == 0,
        1,
        .data$max_coverage
      ),
      scaled_coverage = .data$coverage / .data$max_coverage
    ) |>
    ungroup()
}

# Shared coverage fill scale for heatmaps and participant strips.
.coverage_fill_scale <- function(proportional, max = 1) {
  ggplot2::scale_fill_gradientn(
    colours = c("#d70525", "#645a6c", "#3F7F93"),
    breaks = c(0, 0.5, 1),
    labels = if (proportional) c(0, 0.5, 1) else c("low", "medium", "high"),
    limits = c(0, max),
    name = "coverage"
  )
}
