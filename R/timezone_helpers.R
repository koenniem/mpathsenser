#' Assign timezones to canonical measurements in an m-Path Sense database
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function uses the `Timezone` table to assign a timezone to all other
#'   sensor tables. For each observation in a sensor table, it finds the
#'   timezone interval that matches the observation's start time and updates
#'   the `timezone` column accordingly. Observations that occurred before the
#'   first timezone measurement of a participant are assigned the first known
#'   timezone.
#'
#' @details
#' The timezone is assigned based on the start time of a measurement, so a
#' measurement that crosses a timezone change is assigned the timezone at its
#' start.
#'
#' The timezone intervals are derived once from the `Timezone` table and then
#' applied to every sensor table with a single equality join, rather than running
#' an ASOF join per sensor. This is much faster when there are many sensors or a
#' large `Timezone` table, and it guarantees that all sensor tables receive the
#' identical timezone for a given `(participant_id, time)`.
#'
#' Consecutive timezone events that share the same timezone are collapsed into a
#' single interval before the join. Timezone sampling is far more frequent than
#' actual timezone changes, so this reduces the join to a handful of intervals per
#' participant instead of matching each measurement against every sampled event.
#'
#' Observations that already carry a `timezone` value are left untouched: only
#' `NULL` timezone cells are populated. This makes rerunning the function
#' cheap (it only fills gaps) and safe, since a previously assigned timezone is
#' never overwritten. To recompute a timezone that has already been set, clear
#' it first (e.g. `UPDATE ... SET timezone = NULL`) or re-import the data.
#'
#' @param db A database connection, typically created by [open_db()].
#' @param sensors A character vector of sensor table names to update. Defaults to `NULL` for all supported
#'   sensors.
#' @param .progress Logical; whether to show a progress bar during processing. Defaults to `TRUE`.
#'
#' @return Invisibly returns `TRUE` if all updates complete successfully.
#'
#' @examples
#' \dontrun{
#' # Connect to an m-Path Sense database
#' db <- open_db("path/to/db")
#'
#' # Add timezone information to all tables
#' add_timezones_to_db(db)
#'
#' # Disconnect when done
#' close_db(db)
#' }
#'
#' @export
add_timezones_to_db <- function(db, sensors = NULL, .progress = TRUE) {
  check_db(db)
  check_sensors(sensors, allow_null = TRUE)

  sensors <- sensors %||% mpathsenser::sensors
  sensors <- .physical_sensor(sensors)

  # Do not add the timezone to the timezone table itself to avoid confusion
  sensors <- sensors[tolower(sensors) != "timezone"]

  # Check that the table timezone exists
  if (!DBI::dbExistsTable(db, "Timezone", schema = "main")) {
    cli::cli_abort(
      c(
        "The table `Timezone` does not exist in the database.",
        i = "Check whether timezone measurements appear in your source data.",
        i = "If there are, something went wrong when reading in the data.",
        i = "Otherwise, data may have been collected with an older version of m-Path Sense \\
             that did not support timezones."
      )
    )
  }

  # Make sure the timezone column exists in all sensor tables
  for (sensor in sensors) {
    if (!"timezone" %in% DBI::dbListFields(db, sensor, schema = "main")) {
      DBI::dbExecute(db, sprintf("ALTER TABLE %s ADD COLUMN timezone TEXT", sensor))
    }
  }

  # Build the timezone intervals once, then apply them to every sensor with a
  # single equality join. Consecutive timezone events with the same timezone are
  # collapsed into a single interval first, so the join matches each measurement
  # against a handful of intervals rather than every sampled event. This matters
  # because timezone sampling is far more frequent than actual timezone changes.
  #
  # Each interval starts at its timezone event and ends at the next event for the
  # same participant (open-ended for the last one). The first interval is opened
  # at '-infinity' so that observations before the first timezone measurement
  # inherit the first known timezone.
  #
  # After the preceding deduplication there is at most one Timezone row per
  # (participant_id, time), so ordering by time alone is deterministic and no
  # `rowid` tie-break is needed. `IS NOT DISTINCT FROM` keeps a run of NULL
  # timezones together; `any_value()` is safe because a run is homogeneous.
  DBI::dbExecute(
    db,
    "CREATE OR REPLACE TEMP TABLE temp_tz_intervals AS
     WITH run_boundaries AS (
       SELECT participant_id, time, timezone,
              CASE WHEN timezone IS NOT DISTINCT FROM
                        LAG(timezone) OVER (PARTITION BY participant_id ORDER BY time)
                   THEN 0 ELSE 1 END AS is_start
       FROM Timezone
     ),
     with_grp AS (
       SELECT participant_id, time, timezone,
              SUM(is_start) OVER (PARTITION BY participant_id ORDER BY time) AS grp
       FROM run_boundaries
     ),
     compressed AS (
       SELECT participant_id, MIN(time) AS start_time,
              any_value(timezone) AS timezone
       FROM with_grp
       GROUP BY participant_id, grp
     )
     SELECT participant_id,
            CASE WHEN ROW_NUMBER() OVER (PARTITION BY participant_id ORDER BY start_time) = 1
                 THEN TIMESTAMPTZ '-infinity' ELSE start_time END AS start_time,
            LEAD(start_time) OVER (PARTITION BY participant_id ORDER BY start_time) AS end_time,
            timezone
     FROM compressed"
  )
  on.exit(
    DBI::dbExecute(db, "DROP TABLE IF EXISTS temp_tz_intervals"),
    add = TRUE
  )

  # Start a progress bar
  if (.progress) {
    pb <- cli::cli_progress_bar(
      "Adding timezones...",
      total = length(sensors),
      clear = FALSE
    )
  }

  for (sensor in sensors) {
    # Only fill NULL timezone cells: observations that already have a timezone
    # are preserved. A measurement matching a participant with no timezone
    # events is not matched (the interval join is an INNER join), so its
    # timezone stays NULL, exactly as with the ASOF version.
    DBI::dbExecute(
      db,
      sprintf(
        "UPDATE %s s
         SET timezone = t.timezone
         FROM temp_tz_intervals t
         WHERE s.participant_id = t.participant_id
           AND s.time >= t.start_time
           AND (t.end_time IS NULL OR s.time < t.end_time)
           AND s.timezone IS NULL",
        sensor
      )
    )

    # Update progress bar
    if (.progress) {
      cli::cli_progress_update()
    }
  }

  if (.progress) {
    cli::cli_progress_done()
  }

  invisible(TRUE)
}
