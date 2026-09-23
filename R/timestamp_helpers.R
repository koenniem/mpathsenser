# Timestamp conversion and correction helpers for m-Path Sense data.

# Columns that, for m-Path Sense versions <= .source_timestamp_version, are
# stored as local wall-clock values rather than UTC instants. The workaround is
# needed only while reading files; the local-time views and to_local_time() do
# not need to know about it (see inst/extdata/views.sql).
.source_timestamp_fixes <- list(
  AppUsage = c("period_start", "period_end", "last_foreground"),
  Bluetooth = c("start_scan", "end_scan"),
  Location = "time",
  Weather = c("time", "sunrise", "sunset")
)

# The m-Path Sense version up to which the columns above are local wall-clock
# values. Everything after stores them as true UTC instants.
.source_timestamp_version <- 6L

# Returns TRUE when `column` of `sensor` is a legacy local wall-clock column.
.source_timestamp_legacy <- function(sensor, column) {
  column %in% (.source_timestamp_fixes[[sensor]] %||% character(0))
}

# SQL fragment that converts a source timestamp while preserving historical
# local wall-clock values. The value is a UTC instant expression (e.g.
# to_timestamp(epoch) or a JSON string cast to TIMESTAMPTZ); for legacy columns
# the timezone offset is stripped so the stored wall-clock component keeps its
# historical value. The package forces the session timezone to UTC, so
# CAST(value AS TIMESTAMP) reads that wall-clock component correctly.
.source_timestamp_import_sql <- function(value, sensor, column, version_alias = "m") {
  if (.source_timestamp_legacy(sensor, column)) {
    sprintf(
      "CASE WHEN %s.sense_version <= %d THEN CAST(CAST(%s AS TIMESTAMP) AS TIMESTAMPTZ) ELSE CAST(%s AS TIMESTAMPTZ) END",
      version_alias,
      .source_timestamp_version,
      value,
      value
    )
  } else {
    sprintf("CAST(%s AS TIMESTAMPTZ)", value)
  }
}

# TRUE for the objects dbplyr passes to a registered translation function:
# column references (`ident`) and already-built SQL expressions (`sql`). Such
# inputs are evaluated by the database, so their values cannot be checked in R.
.is_sql_input <- function(x) {
  inherits(x, c("ident", "sql"))
}

# Validate a timestamp argument for the R path. `x` may be POSIXt already or
# anything as.POSIXct() can turn into one; character values are read as UTC.
.check_timestamp <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(x)
  }

  class_x <- class(x)
  x <- try(as.POSIXct(x, tz = "UTC"), silent = TRUE)
  if (!inherits(x, "POSIXt")) {
    cli_abort(c(
      "{.var x} must be a vector of class POSIXt or a character coercible to POSIXt.",
      "x" = "You've supplied a vector of class {.cls {class_x}}"
    ))
  }

  x
}

# Validate a timezone argument for both paths. A database column (or SQL
# expression) is left to DuckDB; NULL and NA mean "unknown timezone" and fall
# back to UTC, matching COALESCE(tz, 'UTC') in the to_local_time() SQL macro.
# Timezone *names* are validated by whichever engine applies them: R and DuckDB
# ship different tz databases, so checking names in R could reject values DuckDB
# can handle (and vice versa).
.check_timezone <- function(
  timezone,
  arg = rlang::caller_arg(timezone),
  call = rlang::caller_env()
) {
  if (.is_sql_input(timezone) || is.null(timezone)) {
    return(invisible(timezone))
  }
  if (is.logical(timezone) && all(is.na(timezone))) {
    return(invisible(timezone))
  }

  check_arg(timezone, "character", arg = arg, call = call)
  invisible(timezone)
}

# Convert a vector of absolute timestamps into participant-local wall-clock
# values. Each input instant is re-interpreted in `timezone` and the resulting
# clock value is stored with a UTC attribute (R vectors can only carry one
# timezone). Legacy columns are never passed here: the views and the import SQL
# handle them separately, so this function assumes clean UTC input.
.to_local_time_r <- function(x, timezone) {
  .check_timezone(timezone)
  if (is.null(timezone) || (is.logical(timezone) && all(is.na(timezone)))) {
    timezone <- NA_character_
  }

  x <- .check_timestamp(x)

  timezone <- rep(timezone, length.out = length(x))
  out <- x
  local <- !is.na(x) & !is.na(timezone)
  # with_tz() does not recycle a vector of timezones, so convert each distinct
  # timezone in one pass rather than looping over observations.
  for (tz in unique(timezone[local])) {
    idx <- local & timezone == tz
    out[idx] <- lubridate::force_tz(lubridate::with_tz(x[idx], tz), "UTC")
  }
  out
}

# SQL counterpart of .to_local_time_r(), used when to_local_time() runs inside
# a lazy dbplyr query. Argument literals are checked in R with the same helpers
# as the R path; columns are checked by DuckDB while the query runs. The macro
# (inst/extdata/views.sql) receives a TIMESTAMPTZ and returns the local
# wall-clock TIMESTAMP, so a collected vector mixed into a lazy query is
# inlined as an epoch (to_timestamp) rather than a naive timestamp literal,
# which `AT TIME ZONE` would interpret in the opposite direction.
.to_local_time_sql <- function(x, timezone) {
  .check_timezone(timezone)

  if (!.is_sql_input(x)) {
    x <- dbplyr::sql_prefix("to_timestamp", 1)(as.numeric(.check_timestamp(x)))
  }

  dbplyr::sql_prefix("to_local_time", 2)(x, timezone)
}

# dbplyr translation for to_local_time(). duckdb's own sql_translation() builds
# the translation variant; injecting to_local_time() into its scalar functions
# makes dbplyr call the exported function while building a lazy query instead
# of passing the call through untouched.
sql_translation.duckdb_connection <- function(con) {
  duckdb_translation <- get0(
    "sql_translation.duckdb_connection",
    envir = asNamespace("duckdb"),
    mode = "function",
    inherits = FALSE
  )
  if (is.null(duckdb_translation)) {
    cli_abort("The duckdb package no longer provides its dbplyr translation.")
  }

  variant <- duckdb_translation(con)
  variant$scalar$to_local_time <- to_local_time
  variant
}

#' Convert canonical timestamps to participant-local wall-clock values
#'
#' `to_local_time()` converts absolute timestamps into the wall-clock time of
#' the timezone in which they were measured. It works both on timestamps that
#' have already been collected into R and inside a lazy [dbplyr] query, where
#' dbplyr calls the same function while building the query and DuckDB applies
#' the `to_local_time()` macro. Argument checks run in R in both cases.
#'
#' @param x A vector of class POSIXt, or a character vector coercible to POSIXt.
#'   Inside a lazy query this is a database column.
#' @param timezone A character vector of IANA timezone names, one per
#'   observation or a single value recycled over `x`. `NULL` or `NA` means the
#'   timezone is unknown and the value is interpreted as UTC.
#'
#' @returns A POSIXct vector when `x` is a collected vector, or a SQL expression
#'   inside a lazy query. The clock values represent the local time in
#'   `timezone`, but the returned vector has the technical timezone `UTC` so
#'   that R does not shift them again; the `timezone` argument carries the
#'   interpretation.
#'
#' @details
#' Inside a lazy query, `mutate()` after `arrange()` that overwrites the ordered
#' column does not keep the arranged row order: the generated SQL's `ORDER BY`
#' refers to the new column, so the rows are ordered by the converted values. A
#' local `dplyr` pipeline keeps the arranged order instead. Convert into a new
#' column, or call `arrange()` after the conversion, when the order matters.
#'
#' @export
#'
#' @examples
#' x <- as.POSIXct("2025-05-10 12:00:00", tz = "UTC")
#' to_local_time(x, "Europe/Brussels")
to_local_time <- function(x, timezone) {
  if (.is_sql_input(x) || .is_sql_input(timezone)) {
    return(.to_local_time_sql(x, timezone))
  }

  .to_local_time_r(x, timezone)
}

#' Collect a lazy table with participant-local timestamp values
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' `collect_local()` is a convenience wrapper around [dplyr::collect()] that
#' also converts every timestamp column of the table to participant-local
#' wall-clock values. It is equivalent to collecting the corresponding
#' `_local` view, and is useful when the same data is needed both as absolute
#' instants and as local clock values.
#'
#' @param x A lazy dbplyr table, typically returned by [get_data()].
#' @param ... Arguments passed on to [dplyr::collect()].
#'
#' @returns A data frame with the same columns as `x`, where every POSIXt
#'   column has been converted using [to_local_time()] and the `timezone`
#'   column.
#' @export
collect_local <- function(x, ...) {
  out <- collect(x, ...)

  # Only physical sensor tables carry a timezone and need conversion. Tables
  # that already went through a `_local`/`_with_local` view are left untouched.
  sensor <- attr(x, "mpathsenser_sensor")
  if (is.null(sensor) || grepl("_(local|with_local)$", sensor)) {
    return(out)
  }

  if (!"timezone" %in% names(out)) {
    return(out)
  }

  posix_columns <- names(out)[vapply(out, inherits, logical(1), what = "POSIXt")]
  for (column in posix_columns) {
    out[[column]] <- to_local_time(out[[column]], out$timezone)
  }
  out
}
