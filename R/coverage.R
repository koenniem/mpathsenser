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
    cli_abort("All {.fn coverage_expected} intervals must be positive, finite numbers.")
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
#'   (the bin start), `measure`, and `coverage` (the absolute measurement count).
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
#'   dplyr::group_by(participant_id, measure, date, hour) |>
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
  .coverage_build(
    db = db,
    participant_id = participant_id,
    sensor = sensor,
    expected = NULL,
    metric = NULL,
    by = by,
    local = local,
    week_start = week_start,
    start_date = start_date,
    end_date = end_date,
    coverage_type = "frequency"
  )
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
#' or `"month"` for
#' calendar-aligned bins, or a positive number of seconds for fixed-width bins.
#' When an explicit `by` is shorter than a sensor's expected interval, that
#' sensor's bin width is raised to the expected interval and a warning reports
#' the adjustment; other sensors keep the requested width.
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
#' latest occupied slot's end. For iOS participants, `AppUsage`, `Light`,
#' `Memory`, and `Screen` are marked `NA`; a warning is issued when `Device`
#' platform information is missing.
#'
#' `expected` is required, but it may be supplied directly or constructed with
#' [coverage_expected()].
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
#'   sensor's expected interval separately. If an explicit width is smaller
#'   than a sensor's expected interval, it is raised to that interval for that
#'   sensor and a warning is issued. A calendar-month width is compared using
#'   the shortest month (28 days); an adjusted month bin becomes fixed-width.
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
#'   (the bin start), `measure`, and `coverage`. Bins with no observations are
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
  .coverage_build(
    db = db,
    participant_id = participant_id,
    sensor = sensor,
    expected = expected,
    metric = metric,
    by = by,
    local = local,
    week_start = week_start,
    start_date = start_date,
    end_date = end_date,
    coverage_type = "proportional"
  )
}

#' Materialise a coverage result
#'
#' @param x A lazy coverage table created by [coverage_frequency()] or
#'   [coverage_proportional()].
#' @param ... Passed on to the underlying `collect()` method.
#'
#' @returns A tibble with rounded coverage values and factorised `measure`.
#' @keywords internal
#' @exportS3Method dplyr::collect
collect.coverage <- function(x, ...) {
  requested <- attr(x, "participant_id")
  out <- NextMethod()

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

  out$coverage <- round(out$coverage, 2)
  out$measure <- factor(
    out$measure,
    levels = rev(sort(unique(as.character(out$measure))))
  )

  class(out) <- c("coverage", class(out))
  for (nm in c(
    "participant_id",
    "coverage_type",
    "expected",
    "metric",
    "by",
    "week_start",
    "local"
  )) {
    attr(out, nm) <- attr(x, nm)
  }
  out
}

#' Plot a coverage overview
#'
#' @param x A coverage result from [coverage_frequency()] or
#'   [coverage_proportional()], either lazy or already collected.
#' @param digits Number of digits to show in heatmap labels. Defaults to 2.
#' @param type The plot type. `"auto"` (the default) uses a line graph when
#'   `cycle = NULL` and a heatmap otherwise. Use `"line"` or `"heatmap"` to
#'   force one of the two.
#' @param cycle The calendar cycle used to average the values for plotting:
#'   `NULL`, `"hour"`, `"day"`, `"week"`, `"month"`, or `"year"`. Defaults to
#'   `NULL`, which plots the full time series. This only changes the plot; use
#'   dplyr `summarise()` on the returned table for other summaries.
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
  type = c("auto", "line", "heatmap"),
  cycle = NULL,
  label = TRUE,
  week_start = NULL,
  ...
) {
  ensure_suggested_package("ggplot2")
  type <- match.arg(type)
  cycle <- if (is.null(cycle)) {
    NULL
  } else {
    match.arg(cycle, c("hour", "day", "week", "month", "year"))
  }
  check_arg(label, "logical", n = 1)

  x <- collect(x)
  if (is.null(week_start)) {
    week_start <- attr(x, "week_start")
  }
  if (is.null(week_start)) {
    week_start <- getOption("lubridate.week.start", 7)
  }
  week_start <- .coverage_validate_week_start(week_start)
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

  title <- if (is.null(participant_id)) {
    "Coverage for all participants"
  } else if (length(participant_id) == 1) {
    paste0("Coverage for participant ", participant_id)
  } else {
    "Coverage per participant"
  }

  if (is.null(cycle)) {
    position_cols <- character(0)
    x_col <- "time"
  } else {
    position_cols <- .coverage_cycle_positions(cycle, by_spec, x)
    x <- x |>
      mutate(
        month = lubridate::month(.data$time),
        day_of_month = lubridate::mday(.data$time),
        day_of_week = lubridate::wday(
          .data$time,
          label = label,
          week_start = week_start
        ),
        hour = lubridate::hour(.data$time),
        minute = lubridate::minute(.data$time),
        second = lubridate::second(.data$time),
        week_of_month = .coverage_week_number(.data$time, "month", week_start),
        week_of_year = .coverage_week_number(.data$time, "year", week_start)
      ) |>
      group_by(across(all_of(c("participant_id", "measure", position_cols)))) |>
      summarise(
        coverage = mean(.data$coverage, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        coverage = if_else(is.nan(.data$coverage), NA_real_, .data$coverage)
      )
    x_col <- position_cols[length(position_cols)]
  }

  multiple_participants <- length(unique(x$participant_id)) > 1
  add_facets <- function(plot) {
    facet_cols <- position_cols[-length(position_cols)]
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
    return(add_facets(plot))
  }

  if (!is_proportional) {
    x <- x |>
      group_by(.data$measure) |>
      mutate(max_coverage = max(dplyr::coalesce(.data$coverage, 0))) |>
      mutate(
        max_coverage = if_else(
          !is.finite(.data$max_coverage) | .data$max_coverage == 0,
          1,
          .data$max_coverage
        ),
        scaled_coverage = .data$coverage / .data$max_coverage
      ) |>
      ungroup()
    fill_col <- "scaled_coverage"
  } else {
    fill_col <- "coverage"
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

  if (is_proportional) {
    scale_max <- if (identical(metric, "count")) {
      max(c(1, x$coverage), na.rm = TRUE)
    } else {
      1
    }
    plot <- plot +
      ggplot2::scale_fill_gradientn(
        colours = c("#d70525", "#645a6c", "#3F7F93"),
        breaks = c(0, 0.5, 1),
        labels = c(0, 0.5, 1),
        limits = c(0, scale_max),
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

.coverage_ios_sensors <- c("AppUsage", "Light", "Memory", "Screen")

.coverage_build <- function(
  db,
  participant_id,
  sensor,
  expected,
  metric,
  by,
  local,
  week_start,
  start_date,
  end_date,
  coverage_type
) {
  check_db(db)
  check_arg(
    participant_id,
    type = c("character", "integerish", "numeric"),
    allow_null = TRUE
  )
  check_sensors(sensor, allow_null = TRUE)
  check_arg(local, "logical", n = 1)

  participants <- get_participants(db)$participant_id
  if (length(participants) == 0) {
    cli_abort("The database does not contain any participants.")
  }
  if (!is.null(participant_id)) {
    unknown <- setdiff(as.character(participant_id), as.character(participants))
    if (length(unknown) > 0) {
      cli_abort(
        "Participant{?s} {.val {unknown}} could not be found in the {.pkg mpathsenser} database."
      )
    }
  }

  sensor_explicit <- !is.null(sensor)
  if (is.null(sensor)) {
    sensor <- sensors
  }
  sensor <- .physical_sensor(sensor)
  sensor <- unique(sensor[tolower(sensor) %in% tolower(sensors)])
  if (length(sensor) == 0) {
    cli_abort("No valid sensors to compute coverage for.")
  }

  if (identical(coverage_type, "proportional")) {
    check_arg(expected, type = "numeric")
    if (
      is.null(names(expected)) ||
        length(expected) == 0 ||
        anyNA(names(expected)) ||
        any(!nzchar(names(expected)))
    ) {
      cli_abort("{.arg expected} must be a named numeric vector of sensor intervals.")
    }
    if (anyDuplicated(names(expected))) {
      cli_abort("{.arg expected} must not contain duplicate sensor names.")
    }
    if (any(!is.finite(expected)) || any(expected < 0.000001)) {
      cli_abort("{.arg expected} must contain finite intervals of at least one microsecond.")
    }

    keep <- sensor[sensor %in% names(expected)]
    dropped <- setdiff(sensor, keep)
    if (length(dropped) > 0 && sensor_explicit) {
      cli_warn(c(
        "Dropping sensor{?s} without an expected interval: {.val {dropped}}.",
        i = "Add them to {.arg expected} to compute proportional coverage."
      ))
    }
    sensor <- keep
    if (length(sensor) == 0) {
      cli_abort("No sensors left after filtering by {.arg expected}.")
    }
  }

  week_start <- .coverage_validate_week_start(week_start)
  if (is.null(by)) {
    if (identical(coverage_type, "frequency")) {
      cli_abort("{.arg by} cannot be {.val NULL} for {.fn coverage_frequency}.")
    }
  } else if (is.character(by)) {
    by <- match.arg(by, c("minute", "hour", "day", "week", "month"))
  } else {
    check_arg(by, type = c("numeric", "integerish"), n = 1)
    if (!is.finite(by) || by < 0.000001) {
      cli_abort("{.arg by} must be a positive, finite number of seconds.")
    }
    by <- as.numeric(by)
  }

  if (identical(coverage_type, "proportional")) {
    metric <- match.arg(metric, c("count", "interval", "bin"))
  }

  if (!.coverage_is_date(start_date) || !.coverage_is_date(end_date)) {
    cli_abort(
      "{.arg start_date} and {.arg end_date} must be {.val NULL}, a date string, or a {.cls Date}."
    )
  }

  bin_spec <- if (identical(coverage_type, "proportional")) {
    .coverage_proportional_bins(by, sensor, expected)
  } else {
    stats::setNames(rep(by, length(sensor)), sensor)
  }

  if (length(intersect(sensor, .coverage_ios_sensors)) > 0) {
    .coverage_warn_unknown_platforms(db, participants, participant_id)
  }

  relations <- purrr::map(
    sensor,
    \(sensor_name) {
      if (identical(metric, "interval") || identical(metric, "bin")) {
        .coverage_instants_branch(
          db = db,
          sensor = sensor_name,
          participant_id = participant_id,
          local = local,
          start_date = start_date,
          end_date = end_date
        )
      } else {
        .coverage_counts_branch(
          db = db,
          sensor = sensor_name,
          participant_id = participant_id,
          by = bin_spec[[sensor_name]],
          week_start = week_start,
          local = local,
          start_date = start_date,
          end_date = end_date
        )
      }
    }
  )
  relation <- purrr::reduce(relations, dplyr::union_all)
  relation_sql <- as.character(dbplyr::sql_render(relation))
  settings_sql <- .coverage_sql_settings(sensor, bin_spec, expected, week_start)
  query <- .coverage_sql(
    relation_sql = relation_sql,
    settings_sql = settings_sql,
    metric = metric,
    coverage_type = coverage_type
  )

  out <- tbl(db, sql(query))
  if (length(intersect(sensor, .coverage_ios_sensors)) > 0) {
    ios_participants <- tbl(db, "Device") |>
      filter(grepl("ios|iphone|ipad", tolower(.data$platform))) |>
      distinct(.data$participant_id) |>
      mutate(is_ios = TRUE)
    out <- out |>
      left_join(ios_participants, by = "participant_id") |>
      mutate(
        coverage = if_else(
          dplyr::coalesce(.data$is_ios, FALSE) & .data$measure %in% .coverage_ios_sensors,
          NA_real_,
          .data$coverage
        )
      ) |>
      select(-"is_ios")
  }

  class(out) <- c("coverage", class(out))
  attr(out, "participant_id") <- participant_id
  attr(out, "coverage_type") <- coverage_type
  attr(out, "expected") <- expected
  attr(out, "metric") <- metric
  attr(out, "by") <- .coverage_compact_bin_spec(bin_spec)
  attr(out, "week_start") <- week_start
  attr(out, "local") <- local
  out
}

.coverage_is_date <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  }
  if (!inherits(x, "Date") && !is.character(x)) {
    return(FALSE)
  }
  date <- try(as.Date(x), silent = TRUE)
  if (!inherits(date, "Date") || length(date) != 1 || is.na(date)) {
    return(FALSE)
  }
  TRUE
}

.coverage_warn_unknown_platforms <- function(db, participants, participant_id) {
  selected <- if (is.null(participant_id)) {
    as.character(participants)
  } else {
    unique(as.character(participant_id))
  }
  platforms <- tbl(db, "Device") |>
    select("participant_id", "platform") |>
    distinct() |>
    collect()
  known <- unique(as.character(platforms$participant_id[
    as.character(platforms$participant_id) %in%
      selected &
      !is.na(platforms$platform) &
      nzchar(trimws(platforms$platform))
  ]))
  unknown <- setdiff(selected, known)
  if (length(unknown) > 0) {
    cli_warn(c(
      "Operating system is unknown for participant{?s} {.val {unknown}}.",
      i = "Device information was not collected; iOS-specific sensors cannot be excluded."
    ))
  }
  invisible(NULL)
}

.coverage_counts_branch <- function(
  db,
  sensor,
  participant_id,
  by,
  week_start,
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
  bin_time <- if (local) "time_local" else "time"
  out <- mutate(
    out,
    bin = sql(.coverage_sql_bin(bin_time, by, week_start)),
    bin_time = sql(bin_time)
  ) |>
    summarise(
      n = dplyr::n_distinct(.data$time),
      bin_first = min(.data$bin_time, na.rm = TRUE),
      bin_last = max(.data$bin_time, na.rm = TRUE),
      .by = c("participant_id", "bin")
    ) |>
    mutate(measure = sensor)

  out
}

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
    out <- summarise(
      out,
      bin_time = min(.data$time_local, na.rm = TRUE),
      .by = c("participant_id", "time")
    )
  } else {
    out <- select(out, "participant_id", "time") |>
      distinct() |>
      mutate(bin_time = .data$time)
  }

  mutate(out, measure = sensor)
}

.coverage_filter_branch <- function(
  db,
  sensor,
  participant_id,
  local,
  start_date,
  end_date
) {
  view <- if (local) paste0(sensor, "_with_local") else sensor
  out <- tbl(db, view)

  if (!is.null(participant_id)) {
    out <- filter(out, .data$participant_id %in% !!as.character(participant_id))
  }
  if (identical(sensor, "Heartbeat")) {
    out <- filter(
      out,
      is.na(.data$device_role_name) | !grepl("^Secondary", .data$device_role_name)
    )
  }
  if (!is.null(start_date)) {
    out <- filter(out, .data$time >= !!as.Date(start_date))
  }
  if (!is.null(end_date)) {
    out <- filter(out, .data$time <= !!(as.Date(end_date) + 1))
  }

  out
}

# Keep every proportional bin wide enough for an expected opportunity.
.coverage_proportional_bins <- function(by, sensor, expected) {
  bin_spec <- if (is.null(by)) {
    expected[sensor]
  } else {
    stats::setNames(rep(by, length(sensor)), sensor)
  }
  if (is.null(by)) {
    return(bin_spec)
  }

  too_small <- sensor[expected[sensor] > .coverage_min_bin_seconds(by)]
  if (length(too_small) == 0) {
    return(bin_spec)
  }

  requested_width <- .coverage_bin_width_label(by)
  adjustments <- purrr::map_chr(too_small, \(measure) {
    expected_width <- .coverage_seconds_label(expected[[measure]])
    paste0(
      measure, ": ", requested_width, " is shorter than ", expected_width,
      "; using ", expected_width
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

  bin_spec <- as.list(bin_spec)
  bin_spec[too_small] <- as.list(expected[too_small])
  bin_spec
}

# Compare calendar bins using their shortest possible duration.
.coverage_min_bin_seconds <- function(by) {
  if (!is.character(by)) {
    return(as.numeric(by))
  }

  c(
    minute = 60,
    hour = 3600,
    day = 86400,
    week = 604800,
    month = 28 * 86400
  )[[by]]
}

# Show exact durations in the bin-adjustment warning.
.coverage_seconds_label <- function(seconds) {
  paste0(format(seconds, trim = TRUE, scientific = FALSE), " seconds")
}

# Describe requested calendar or fixed-width bins in warnings.
.coverage_bin_width_label <- function(by) {
  if (identical(by, "month")) {
    return("month (shortest month: 28 days)")
  }
  if (is.character(by)) {
    return(paste0(
      by,
      " (",
      .coverage_seconds_label(.coverage_min_bin_seconds(by)),
      ")"
    ))
  }
  .coverage_seconds_label(by)
}

# Preserve vector attrs unless per-sensor bins need different types.
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

# Let cycle helpers inspect mixed calendar and fixed-width bins uniformly.
.coverage_bin_specs <- function(bin_spec) {
  if (is.list(bin_spec)) bin_spec else as.list(bin_spec)
}

# Preserve calendar bins as intervals; numeric bins are fixed durations.
.coverage_sql_interval <- function(by) {
  if (is.character(by)) {
    sprintf("INTERVAL '1 %s'", by)
  } else {
    sprintf("TO_SECONDS(%.15g)", by)
  }
}

.coverage_sql_bin <- function(column, by, week_start) {
  interval <- .coverage_sql_interval(by)
  if (identical(by, "week")) {
    offset_seconds <- (week_start - 1L) * 86400
    return(sprintf(
      "time_bucket(%s, %s - TO_SECONDS(%d)) + TO_SECONDS(%d)",
      interval,
      column,
      offset_seconds,
      offset_seconds
    ))
  }
  sprintf("time_bucket(%s, %s)", interval, column)
}

.coverage_sql_settings <- function(sensor, bin_spec, expected, week_start) {
  rows <- vapply(
    sensor,
    \(measure) {
      by <- bin_spec[[measure]]
      values <- c(
        sprintf("'%s'", gsub("'", "''", measure)),
        .coverage_sql_interval(by),
        sprintf("%d", if (identical(by, "week")) (week_start - 1L) * 86400 else 0L)
      )
      if (!is.null(expected)) {
        values <- c(values, sprintf("%.15g", expected[[measure]]))
      }
      sprintf("(%s)", paste(values, collapse = ", "))
    },
    character(1)
  )
  columns <- if (is.null(expected)) {
    "measure, bin_interval, week_offset_seconds"
  } else {
    "measure, bin_interval, week_offset_seconds, expected_seconds"
  }
  sprintf(
    "settings AS (SELECT * FROM (VALUES %s) AS s(%s))",
    paste(rows, collapse = ", "),
    columns
  )
}

.coverage_sql <- function(
  relation_sql,
  settings_sql,
  metric,
  coverage_type
) {
  is_interval <- identical(metric, "interval")
  is_bin_metric <- identical(metric, "bin")
  post_span_ctes <- ""

  if (is_interval) {
    data_ctes <- .coverage_sql_intervals(relation_sql)
    span_sql <- paste(
      "SELECT participant_id, MIN(seg_start) AS first_time,",
      "       MAX(seg_end) AS last_time",
      "FROM intervals GROUP BY participant_id"
    )
    last_boundary <- "sp.last_time - INTERVAL 1 MICROSECOND"
    value_join <- paste(
      "LEFT JOIN bin_coverage bc",
      "  ON bc.participant_id = s.participant_id",
      " AND bc.measure = s.measure",
      " AND bc.bin = s.bin",
      sep = "\n"
    )
    covered_seconds <- paste(
      "GREATEST(DATE_DIFF('microsecond', GREATEST(s.bin, sp.first_time),",
      "  LEAST(s.bin + s.bin_interval, sp.last_time)) / 1000000.0, 0) AS covered_seconds"
    )
    joined_values <- paste(
      "CAST(COALESCE(bc.covered, 0) AS DOUBLE) AS covered",
      covered_seconds,
      sep = ",\n         "
    )
    value <- "LEAST(covered / GREATEST(covered_seconds, 0.000001), 1)"
  } else if (is_bin_metric) {
    data_ctes <- .coverage_sql_slots(relation_sql)
    span_sql <- paste(
      "SELECT participant_id, MIN(bin_time) AS first_time,",
      "       MAX(slot_start + TO_SECONDS(expected_seconds)) AS last_time",
      "FROM observed_slots GROUP BY participant_id"
    )
    post_span_ctes <- paste0(",\n", .coverage_sql_slot_bins())
    last_boundary <- "sp.last_time - INTERVAL 1 MICROSECOND"
    slot_anchor <- "COALESCE(a.slot_anchor, sp.first_time)"
    joined_values <- paste(
      "CAST(COALESCE(sb.n, 0) AS DOUBLE) AS n",
      .coverage_sql_expected_slot_count(
        anchor = slot_anchor,
        start = "GREATEST(s.bin, sp.first_time)",
        end = "LEAST(s.bin + s.bin_interval, sp.last_time)",
        expected_seconds = "s.expected_seconds"
      ),
      sep = ",\n         "
    )
    value_join <- paste(
      "LEFT JOIN slot_bins sb",
      "  ON sb.participant_id = s.participant_id",
      " AND sb.measure = s.measure",
      " AND sb.bin = s.bin",
      "LEFT JOIN slot_anchors a",
      "  ON a.participant_id = s.participant_id",
      " AND a.measure = s.measure",
      sep = "\n"
    )
    value <- "CASE WHEN expected_slots = 0 THEN NULL ELSE n / expected_slots END"
  } else {
    data_ctes <- sprintf("counts AS (\n%s\n)", relation_sql)
    span_sql <- paste(
      "SELECT participant_id, MIN(bin_first) AS first_time,",
      "       MAX(bin_last) AS last_time",
      "FROM counts GROUP BY participant_id"
    )
    last_boundary <- "sp.last_time"
    value_join <- paste(
      "LEFT JOIN counts c",
      "  ON c.participant_id = s.participant_id",
      " AND c.measure = s.measure",
      " AND c.bin = s.bin",
      sep = "\n"
    )
    covered_seconds <- paste(
      "GREATEST(DATE_DIFF('microsecond', GREATEST(s.bin, sp.first_time),",
      "  LEAST(s.bin + s.bin_interval, sp.last_time)) / 1000000.0, 0) AS covered_seconds"
    )
    joined_values <- paste(
      "CAST(COALESCE(c.n, 0) AS DOUBLE) AS n",
      covered_seconds,
      sep = ",\n         "
    )
    value <- if (identical(coverage_type, "proportional")) {
      paste(
        "n / (GREATEST(covered_seconds, expected_seconds)",
        "/ expected_seconds)"
      )
    } else {
      "n"
    }
  }

  expected_select_spine <- if (identical(coverage_type, "proportional")) {
    ", sn.expected_seconds"
  } else {
    ""
  }
  expected_select_joined <- if (identical(coverage_type, "proportional")) {
    ", s.expected_seconds"
  } else {
    ""
  }

  sprintf(
    paste(
      "WITH %s,",
      "%s,",
      "spans AS (%s)%s,",
      "spine AS (",
      "  SELECT sp.participant_id, sn.measure, sn.bin_interval, sn.week_offset_seconds%s,",
      "         unnest(generate_series(",
      "           time_bucket(sn.bin_interval, sp.first_time - TO_SECONDS(sn.week_offset_seconds))",
      "             + TO_SECONDS(sn.week_offset_seconds),",
      "           time_bucket(sn.bin_interval, %s - TO_SECONDS(sn.week_offset_seconds))",
      "             + TO_SECONDS(sn.week_offset_seconds),",
      "           sn.bin_interval",
      "         )) AS bin",
      "  FROM spans sp CROSS JOIN settings sn",
      "),",
      "joined AS (",
      "  SELECT s.participant_id, s.measure, s.bin, s.bin_interval%s,",
      "         %s",
      "  FROM spine s",
      "  JOIN spans sp ON sp.participant_id = s.participant_id",
      "  %s",
      ")",
      "SELECT participant_id, bin AS time, measure, %s AS coverage",
      "FROM joined",
      "ORDER BY participant_id, time, measure"
    ),
    settings_sql,
    data_ctes,
    span_sql,
    post_span_ctes,
    expected_select_spine,
    last_boundary,
    expected_select_joined,
    joined_values,
    value_join,
    value
  )
}

# Anchor by chronology; local wall-clock values can move backwards.
.coverage_sql_slots <- function(relation_sql) {
  sprintf(
    paste(
      "instants AS (\n%s\n),",
      "slot_anchors AS (",
      "  SELECT participant_id, measure, arg_min(bin_time, time) AS slot_anchor",
      "  FROM instants GROUP BY participant_id, measure",
      "),",
      "observed_slots AS (",
      "  SELECT i.participant_id, i.measure, i.bin_time, s.expected_seconds,",
      "         a.slot_anchor + TO_SECONDS(FLOOR(",
      "           DATE_DIFF('microsecond', a.slot_anchor, i.bin_time)",
      "             / (1000000.0 * s.expected_seconds)",
      "         ) * s.expected_seconds) AS slot_start",
      "  FROM instants i",
      "  JOIN slot_anchors a USING (participant_id, measure)",
      "  JOIN settings s USING (measure)",
      "),",
      "occupied_slots AS (",
      "  SELECT participant_id, measure, slot_start, expected_seconds",
      "  FROM observed_slots",
      "  GROUP BY participant_id, measure, slot_start, expected_seconds",
      ")",
      sep = "\n"
    ),
    relation_sql
  )
}

# Assign occupied slots by their starts without expanding the expected grid.
.coverage_sql_slot_bins <- function() {
  paste(
    "slot_bins AS (",
    "  SELECT o.participant_id, o.measure,",
    "         time_bucket(s.bin_interval,",
    "           o.slot_start - TO_SECONDS(s.week_offset_seconds))",
    "           + TO_SECONDS(s.week_offset_seconds) AS bin,",
    "         COUNT(*) AS n",
    "  FROM occupied_slots o",
    "  JOIN settings s USING (measure)",
    "  JOIN spans p USING (participant_id)",
    "  WHERE o.slot_start >= p.first_time AND o.slot_start < p.last_time",
    "  GROUP BY o.participant_id, o.measure, bin",
    ")",
    sep = "\n"
  )
}

# Count grid starts in a half-open range without materializing every slot.
.coverage_sql_expected_slot_count <- function(
  anchor,
  start,
  end,
  expected_seconds
) {
  sprintf(
    paste(
      "GREATEST(",
      "  CEIL(DATE_DIFF('microsecond', %s, %s)",
      "    / (1000000.0 * %s)) -",
      "  CEIL(DATE_DIFF('microsecond', %s, %s)",
      "    / (1000000.0 * %s)),",
      "  0) AS expected_slots"
    ),
    anchor,
    end,
    expected_seconds,
    anchor,
    start,
    expected_seconds
  )
}

.coverage_sql_intervals <- function(relation_sql) {
  islands_sql <- .coverage_sql_islands()
  sprintf(
    paste(
      "instants AS (\n%s\n),",
      "intervals AS (",
      "  SELECT i.participant_id, i.measure, i.time,",
      "         i.bin_time AS seg_start,",
      "         i.bin_time + TO_SECONDS(s.expected_seconds) AS seg_end",
      "  FROM instants i JOIN settings s ON s.measure = i.measure",
      "),",
      "%s,",
      "bin_coverage AS (",
      "  SELECT i.participant_id, i.measure, b.bin,",
      "         SUM(GREATEST(DATE_DIFF('microsecond', GREATEST(i.seg_start, b.bin),",
      "                       LEAST(i.seg_end, b.bin + s.bin_interval)), 0) / 1000000.0) AS covered",
      "  FROM islands i JOIN settings s ON s.measure = i.measure",
      "  CROSS JOIN LATERAL UNNEST(generate_series(",
      "    time_bucket(s.bin_interval, i.seg_start - TO_SECONDS(s.week_offset_seconds))",
      "      + TO_SECONDS(s.week_offset_seconds),",
      "    time_bucket(s.bin_interval, i.seg_end - INTERVAL 1 MICROSECOND - TO_SECONDS(s.week_offset_seconds))",
      "      + TO_SECONDS(s.week_offset_seconds),",
      "    s.bin_interval",
      "  )) AS b(bin)",
      "  GROUP BY i.participant_id, i.measure, b.bin",
      ")",
      sep = "\n"
    ),
    relation_sql,
    islands_sql
  )
}

# Merge ordered half-open intervals with a running maximum, so nested intervals
# are handled correctly and overlapping samples count only once.
.coverage_sql_islands <- function() {
  paste(
    "prior AS (",
    "  SELECT *, MAX(seg_end) OVER (PARTITION BY participant_id, measure",
    "    ORDER BY seg_start, seg_end",
    "    ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS prior_end",
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
    "      ORDER BY seg_start, seg_end ROWS UNBOUNDED PRECEDING) AS grp",
    "    FROM marked",
    "  ) GROUP BY participant_id, measure, grp",
    ")",
    sep = "\n"
  )
}

.coverage_validate_week_start <- function(week_start) {
  check_arg(week_start, type = c("numeric", "integerish"), n = 1)
  if (!rlang::is_integerish(week_start) || week_start < 1 || week_start > 7) {
    cli_abort("{.arg week_start} must be a whole number between 1 (Monday) and 7 (Sunday).")
  }
  as.integer(week_start)
}

# Derive real calendar durations for validating custom-second bins against cycles.
.coverage_period_seconds <- function(time, unit) {
  if (unit %in% c("month", "year")) {
    start <- unique(lubridate::floor_date(time, unit))
    end <- lubridate::ceiling_date(start, unit, change_on_boundary = TRUE)
    return(unique(as.numeric(difftime(end, start, units = "secs"))))
  }
  c(minute = 60, hour = 3600, day = 86400, week = 604800)[[unit]]
}

.coverage_bin_is_finer_than <- function(bin_spec, unit, x) {
  specs <- .coverage_bin_specs(bin_spec)
  rank <- c(minute = 1L, hour = 2L, day = 3L, week = 4L, month = 5L)
  if (all(purrr::map_lgl(specs, is.character))) {
    return(any(purrr::map_lgl(specs, \(spec) rank[[spec]] < rank[[unit]])))
  }

  period_seconds <- .coverage_period_seconds(x$time, unit)
  any(purrr::map_lgl(specs, \(spec) {
    if (is.character(spec)) {
      rank[[spec]] < rank[[unit]]
    } else {
      any(spec < period_seconds | spec %% period_seconds != 0)
    }
  }))
}

.coverage_cycle_is_not_coarser <- function(cycle, bin_spec, x) {
  specs <- .coverage_bin_specs(bin_spec)
  bin_rank <- c(minute = 1L, hour = 2L, day = 3L, week = 4L, month = 5L)
  cycle_rank <- c(hour = 2L, day = 3L, week = 4L, month = 5L, year = 6L)
  if (all(purrr::map_lgl(specs, is.character))) {
    return(any(purrr::map_lgl(
      specs,
      \(spec) bin_rank[[spec]] >= cycle_rank[[cycle]]
    )))
  }

  cycle_seconds <- min(.coverage_period_seconds(x$time, cycle))
  any(purrr::map_lgl(specs, \(spec) {
    if (is.character(spec)) {
      bin_rank[[spec]] >= cycle_rank[[cycle]]
    } else {
      spec >= cycle_seconds
    }
  }))
}

.coverage_week_number <- function(time, period, week_start) {
  day <- if (identical(period, "month")) {
    lubridate::mday(time)
  } else {
    lubridate::yday(time)
  }
  period_start <- lubridate::floor_date(time, period)
  start_weekday <- lubridate::wday(period_start, week_start = week_start)
  as.integer(floor((day + start_weekday - 2) / 7) + 1)
}

.coverage_cycle_positions <- function(cycle, bin_spec, x) {
  specs <- .coverage_bin_specs(bin_spec)
  weekly_bins <- all(purrr::map_lgl(specs, \(spec) identical(spec, "week")))
  positions <- switch(
    cycle,
    hour = "minute",
    day = "hour",
    week = "day_of_week",
    month = if (weekly_bins) "week_of_month" else "day_of_month",
    year = if (weekly_bins) "week_of_year" else "month"
  )

  if (identical(cycle, "year") && !weekly_bins &&
    .coverage_bin_is_finer_than(bin_spec, "month", x)) {
    positions <- c(positions, "day_of_month")
  }
  if (cycle %in% c("day", "week", "month", "year") &&
    .coverage_bin_is_finer_than(bin_spec, "day", x)) {
    positions <- c(positions, "hour")
  }
  if (cycle %in% c("day", "week", "month", "year") &&
    .coverage_bin_is_finer_than(bin_spec, "hour", x)) {
    positions <- c(positions, "minute")
  }
  if (any(purrr::map_lgl(specs, \(spec) {
    !is.character(spec) && any(spec < 60 | spec %% 60 != 0)
  }))) {
    positions <- c(positions, "second")
  }
  positions
}
