# Internal helpers shared by the read_mpath_sense() pipeline.

# Print a message when debug = TRUE. The glue expressions in the message are
# evaluated in the calling environment, so callers can refer to their own
# variables. The message is printed immediately: cli_progress_step renders
# its "done" state lazily at the next progress event, which under a tight
# loop can emit stale or duplicated lines (e.g. the last sensor of a batch
# printed twice, with the previous sensor's message text).
.read_debug <- function(debug, ..., .envir = parent.frame()) {
  if (debug) {
    # The message is diagnostic output only: a message that fails to render
    # (e.g. unresolvable glue) must never take down the import, so any error
    # is swallowed here.
    tryCatch(cli::cli_inform(..., .envir = .envir), error = function(e) NULL)
  }
  invisible(NULL)
}

# Run code and, when debug = TRUE, report the message together with the time
# the code took. The code is evaluated in the calling environment, so
# assignments it makes are visible to the caller. The in-progress message is
# shown before the code runs and replaced in place by the done message (which
# may refer to results of the code, e.g. row counts) once it finishes.
#
# cli_progress_step is not used here: its done message cannot reliably render
# variables that are assigned after the step is created (the format is glued
# in a different environment), so the messages are evaluated with
# cli::format_inline in the calling environment and written directly, with a
# carriage return replacing the in-progress line.
.read_debug_time <- function(debug, msg, msg_done, code, .envir = parent.frame()) {
  if (isFALSE(debug)) {
    return(eval(substitute(code), envir = .envir))
  }

  t0 <- proc.time()
  tryCatch(
    cat(cli::format_inline(paste0("{cli::symbol$info} ", msg, "..."), .envir = .envir)),
    error = function(e) NULL
  )
  out <- eval(substitute(code), envir = .envir)
  ms <- round((proc.time() - t0)[["elapsed"]] * 1000)
  done <- tryCatch(
    cli::format_inline(
      paste0("{cli::symbol$tick} ", msg_done, " [", ms, "ms]"),
      .envir = .envir
    ),
    error = function(e) NULL
  )
  if (is.null(done)) {
    # The done message could not be rendered (e.g. unresolvable glue): end
    # the in-progress line so the output stays readable. A failed debug
    # message must never take down the import.
    cat("\n")
  } else {
    cat("\r", done, "\n", sep = "")
  }
  out
}

# Keep only the files that were not imported before, based on the
# ProcessedFiles table. A file is considered processed when a record with the
# same (name, size, modification time) already exists: unchanged re-uploads
# are cheap to detect, while corrected files (same name, different content)
# have a different size or modification time and are imported again. Files
# with duplicate content under a different name are imported as well; the
# data-level deduplication afterwards (newest file wins per measurement key)
# keeps the sensor tables clean.
.read_filter_new_files <- function(db, file_meta) {
  key_new <- paste(
    file_meta$file_name,
    file_meta$file_size_bytes,
    as.numeric(file_meta$modified_at)
  )

  processed <- DBI::dbGetQuery(
    db,
    "SELECT file_name, file_size_bytes, modified_at FROM ProcessedFiles"
  )
  if (nrow(processed) > 0) {
    key_old <- paste(
      processed$file_name,
      processed$file_size_bytes,
      as.numeric(as.POSIXct(processed$modified_at, tz = "UTC"))
    )
    keep <- !key_new %in% key_old
    file_meta <- file_meta[keep, , drop = FALSE]
    key_new <- key_new[keep]
  }

  # Drop intra-run duplicates (the same name, size, and modification time in
  # the same run): the first occurrence is imported, later ones add nothing.
  file_meta[!duplicated(key_new), , drop = FALSE]
}

# Register empty (0-byte) files as processed. Because empty files contain no
# mpathinfo, the participant and study are parsed from the file name (legacy
# m-Path Sense naming convention). Returns the relative paths of files that
# could not be attributed and were skipped.
.read_register_empty_files <- function(db, empty_meta) {
  meta <- .read_meta_from_file_name(empty_meta$file_name)
  # Empty files have no mpathinfo, so the participant is parsed from the file
  # name (legacy m-Path Sense naming convention). Valid participants are
  # numeric and non-missing.
  valid <- !is.na(meta$participant_id) & meta$participant_id != "N/A"
  if (!any(valid)) {
    cli_warn(c(
      "Skipped {length(empty_meta$rel_path)} empty file{?s} with an unrecognised file name.",
      i = "Empty files are registered as processed when their name follows the m-Path Sense convention."
    ))
    return(empty_meta$rel_path)
  }

  meta <- meta[valid, ]
  meta$participant_id <- suppressWarnings(as.numeric(meta$participant_id))
  # A participant id that is not numeric cannot be stored as UINTEGER, so drop
  # those files (reporting them as skipped) while still registering the rest.
  non_numeric <- !is.finite(meta$participant_id)
  skipped_non_numeric <- empty_meta$rel_path[valid][non_numeric]
  if (any(non_numeric)) {
    cli_warn(c(
      "Skipped {sum(non_numeric)} empty file{?s} with a non-numeric participant id.",
      i = "Participant ids are stored as unsigned integers."
    ))
    meta <- meta[!non_numeric, ]
  }
  if (nrow(meta) == 0) {
    return(c(empty_meta$rel_path[!valid], skipped_non_numeric))
  }
  meta <- cbind(
    meta,
    empty_meta[valid, c("file_name", "rel_path", "file_size_bytes", "modified_at")]
  )

  empty_tbl <- data.frame(
    file_name = meta$file_name,
    participant_id = meta$participant_id,
    file_size_bytes = as.numeric(meta$file_size_bytes),
    modified_at = as.POSIXct(meta$modified_at, tz = "UTC"),
    stringsAsFactors = FALSE
  )

  .read_db_transaction(db, {
    DBI::dbExecute(
      db,
      "INSERT INTO Study (study_id, data_format) VALUES ($1, 'CARP JSON') ON CONFLICT DO NOTHING",
      params = list(meta$study_id)
    )
    DBI::dbExecute(
      db,
      "INSERT INTO Participant (participant_id, study_id) VALUES ($1, $2) ON CONFLICT DO NOTHING",
      params = list(meta$participant_id, meta$study_id)
    )
    DBI::dbWriteTable(
      db,
      name = "empty_meta",
      value = empty_tbl,
      temporary = TRUE,
      overwrite = TRUE,
      row.names = FALSE
    )
    DBI::dbExecute(
      db,
      paste(
        "INSERT INTO ProcessedFiles (file_name, participant_id, sense_version, file_size_bytes, modified_at)",
        "SELECT file_name, participant_id, NULL,",
        "CAST(file_size_bytes AS UBIGINT), CAST(modified_at AS TIMESTAMPTZ)",
        "FROM empty_meta",
        "ON CONFLICT DO NOTHING"
      )
    )
    DBI::dbExecute(db, "DROP TABLE IF EXISTS empty_meta")
  })

  skipped_non_numeric
}

# Parse participant and study metadata from m-Path Sense file names. The file
# name is structured as:
# therapistid_study_id_participantid_m_Path_sense_yyyy-mm-dd_HH-MM-SS%OS6.json
# Note that the study_id may itself contain underscores.
.read_meta_from_file_name <- function(file_name) {
  valid_names <- grepl("m_Path_sense", file_name)

  invalid_names <- tibble::tibble(
    study_id = NA_character_,
    participant_id = NA_character_,
    file_name = file_name[!valid_names]
  )

  file_name <- file_name[valid_names]
  if (length(file_name) == 0) {
    return(invalid_names)
  }

  split_file_name <- strsplit(file_name, "_")
  study_id <- purrr::map(split_file_name, \(x) {
    x[-c(1, seq.int(length(x) - 5, length(x)))]
  })
  study_id <- purrr::map_chr(study_id, \(x) paste0(x, collapse = "_"))
  participant_id <- purrr::map_chr(split_file_name, \(x) x[length(x) - 5])

  out <- tibble::tibble(
    study_id = study_id,
    participant_id = participant_id,
    file_name = file_name
  )

  if (nrow(invalid_names) > 0) {
    out <- rbind(out, invalid_names)
    out <- out[match(file_name, out$file_name), ]
  }

  out
}

# Format a character vector of paths as a SQL array literal
.read_sql_array <- function(paths) {
  escaped <- gsub("'", "''", paths)
  paste0("[", paste0("'", escaped, "'", collapse = ", "), "]")
}

# SQL fragment that safely unnests a JSON array (or single value, or NULL)
# into one row per element, tolerating missing keys. The array is transformed
# directly to a list of typed STRUCTs (schema), which uses far less memory than
# keeping the elements as JSON values: DuckDB's parsed JSON representation
# costs roughly 1.5-2 KB per element, which makes ingesting large arrays (e.g.
# Garmin logs with tens of thousands of values per entry) run out of memory.
# With a typed schema the transform costs only tens of bytes per element.
.read_json_array_typed <- function(expr, schema, key = NULL) {
  obj_schema <- gsub("^\\[|\\]$", "", schema)
  if (is.null(key)) {
    paste0(
      "CASE WHEN json_type(",
      expr,
      ") = 'ARRAY' THEN json_transform(",
      expr,
      ", '",
      schema,
      "')",
      " WHEN (",
      expr,
      ") IS NULL THEN []",
      " ELSE [json_transform(",
      expr,
      ", '",
      obj_schema,
      "')] END"
    )
  } else {
    # Single-parse variant: expr is the payload object, key the array field.
    # Transforming the whole payload once avoids the double parse of
    # expr->'key' followed by json_transform. Missing keys yield NULL (and
    # thus no rows) rather than an error.
    paste0(
      "CASE WHEN json_type(",
      expr,
      ") = 'ARRAY' THEN json_transform(",
      expr,
      ", '",
      schema,
      "')",
      " WHEN (",
      expr,
      ") IS NULL THEN []",
      " ELSE json_transform(",
      expr,
      ", '{\"",
      key,
      "\": ",
      schema,
      "}').",
      key,
      " END"
    )
  }
}

# SQL fragment: cast a possibly-missing numeric value, turning negative
# sentinels (m-Path Sense uses -1 for missing Garmin values) into NULL
.read_null_neg <- function(expr, type = "BIGINT") {
  sprintf("NULLIF(TRY_CAST(%s AS %s), -1)", expr, type)
}

# SQL filter selecting the rows of a given senseVersion (NULL when unknown)
.read_version_filter <- function(sense_version) {
  if (is.na(sense_version)) {
    "m.sense_version IS NULL"
  } else {
    sprintf("m.sense_version = %d", as.integer(sense_version))
  }
}

# Resolve the requested sensors to registry names
.read_resolve_sensors <- function(sensors) {
  registry_names <- names(sensor_registry[["default"]])
  if (is.null(sensors)) {
    registry_names
  } else {
    registry_names[tolower(registry_names) %in% tolower(sensors)]
  }
}

# All payload types handled by the registry, plus mpathinfo
.read_known_types <- function() {
  types <- unlist(
    lapply(sensor_registry, \(reg) vapply(reg, \(x) x[["type"]], character(1))),
    use.names = FALSE
  )
  unique(c(types, "dk.cachet.carp.mpathinfo"))
}

# Aggregate unknown types across batches into a named count vector
.read_aggregate_types <- function(unknown_types) {
  keep <- vapply(unknown_types, \(x) nrow(x) > 0, logical(1))
  unknown_types <- unknown_types[keep]
  if (length(unknown_types) == 0) {
    return(character(0))
  }
  df <- do.call(rbind, unknown_types)
  agg <- stats::aggregate(n ~ payload_type, data = df, FUN = sum)
  out <- agg$n
  names(out) <- agg$payload_type
  out
}

# Combine a list of per-file payload type counts into a single data frame
.read_combine_types <- function(unknown_types) {
  unknown_types <- unknown_types[vapply(unknown_types, nrow, integer(1)) > 0]
  if (length(unknown_types) == 0) {
    data.frame(payload_type = character(0), n = integer(0))
  } else {
    do.call(rbind, unknown_types)
  }
}

# Execute code within a database transaction that is guaranteed to be rolled
# back on any exit path other than a clean commit. Unlike DBI::dbWithTransaction,
# this also handles user interrupts: an interrupted import rolls back the batch
# it was working on instead of leaving an active transaction behind.
.read_db_transaction <- function(db, code) {
  DBI::dbBegin(db)
  committed <- FALSE
  on.exit({
    if (!committed && DBI::dbIsValid(db)) {
      try(DBI::dbRollback(db), silent = TRUE)
    }
  })
  out <- force(code)
  DBI::dbCommit(db)
  committed <- TRUE
  out
}

# Execute one sensor ingest statement inside the current batch transaction.
.read_ingest <- function(db, sql) {
  DBI::dbExecute(db, sql)
}

# Deduplicate the sensor tables: per measurement key (participant_id, time,
# plus table-specific extras) only one row is kept — the most recent one. "Most
# recent" is resolved as the row of the newest file (highest source_file_id),
# and within that file the last recorded row (highest rowid). Every sensor
# therefore uses the same last-wins tie-break, which is an upsert
# (INSERT OR REPLACE) semantics: a later measurement overwrites an earlier one
# regardless of whether the duplicate originates from the same file or a
# different file. This means that:
# - rows that were removed in a corrected re-imported file are preserved;
# - new rows in the corrected file are kept;
# - conflicting rows are replaced by the values of the newest file;
# - a measurement whose values were revised (e.g. an interval sensor reporting
#   the same start time twice with a later end time, or a Garmin recalculated
#   point) keeps the most recent values.
#
# When `file_ids` is given (the newly imported files of a run), only the key
# groups that those new rows participate in are examined: the question becomes
# "are the rows we just inserted a duplicate of themselves or of already
# existing rows?" rather than "is there duplicate data anywhere in the
# database?". This avoids globally grouping and resolving every key group in
# the table, which is particularly beneficial for small imports into large
# databases. Existing rows outside the new key groups are guaranteed to be
# duplicate-free (they were deduplicated when they were imported), so they are
# never modified. When `file_ids` is NULL, every duplicated key group in the
# table is resolved instead; that is the full-table behaviour used by the
# exported `deduplicate_db()`, which also cleans up duplicates left behind by
# an interrupted import.
#
# Only candidate key groups are touched: the key groups that are *duplicated*
# (occur more than once), restricted when `file_ids` is given to the groups
# that involve a newly imported row. This is important: building the candidate
# set from the distinct keys of the new rows alone would make it as large as
# the number of newly imported rows even when none of them duplicate anything,
# and the joins, window functions, and deletes below scale with the candidate
# size. Restricting genuinely duplicated groups keeps the work proportional to
# the number of real duplicates; a bulk import of largely distinct data (the
# common no-reimport case) short-circuits cheaply on zero candidates. The
# duplicated groups involving a new row are found by a grouped count over a
# NULL-safe semi-join of the whole sensor table against the new rows' keys,
# so the search itself still scales with the amount of new data rather than
# the table size. Each sensor is deduplicated in its own transaction, so an
# interrupted run rolls back cleanly instead of leaving a sensor half
# deduplicated.
.read_dedup <- function(db, sensors, debug = FALSE, file_ids = NULL) {
  removed <- integer(0)
  for (sensor in sensors) {
    keys <- read_dedup_keys[[sensor]] %||% c("participant_id", "time")
    key_list <- paste0(keys, collapse = ", ")
    key_list_t <- paste0("t.", keys, collapse = ", ")

    # Join conditions between the candidate table d and the sensor table.
    join_t <- paste0(
      sprintf("d.%s IS NOT DISTINCT FROM t.%s", keys, keys),
      collapse = " AND "
    )
    # Candidate finding for the file_ids branch: join a set of the newly
    # imported keys (alias nk) against the sensor table (alias b). The base
    # key columns are NOT NULL so equality is safe there, but the table-specific
    # extras (e.g. package_name, uuid) are nullable, so the match must be
    # NULL-safe (IS NOT DISTINCT FROM) to keep the same key semantics as the
    # window/delete joins below.
    b_key_list <- paste0("b.", keys, collapse = ", ")
    b_grp <- paste0("b.", keys, collapse = ", ")
    join_nk <- paste0(
      sprintf("nk.%s IS NOT DISTINCT FROM b.%s", keys, keys),
      collapse = " AND "
    )
    # The candidate table holds the key groups worth examining: the duplicated
    # groups of the whole table (file_ids = NULL) or the duplicated key groups
    # involving a row newly imported in this run (file_ids given).
    cand_table <- if (is.null(file_ids)) "dedup_touched" else "dedup_cand"

    .read_debug_time(
      debug,
      "Deduplicating {sensor}",
      "Deduplicated {sensor}: {n_removed} duplicate row{?s} removed.",
      n_removed <- .read_db_transaction(db, {
        if (is.null(file_ids)) {
          # Key groups that occur more than once anywhere in the table
          DBI::dbExecute(
            db,
            sprintf(
              "CREATE OR REPLACE TEMP TABLE dedup_touched AS
               SELECT %s FROM %s GROUP BY %s HAVING COUNT(*) > 1",
              key_list,
              sensor,
              key_list
            )
          )
          n_dupes <- DBI::dbGetQuery(db, "SELECT COUNT(*) AS n FROM dedup_touched")[[1]]
        } else {
          # Key groups of the rows just inserted in this run — but only those
          # that are actually duplicated somewhere in the table (which includes
          # duplicates against pre-existing rows). Doing SELECT DISTINCT on the
          # new rows alone would make the candidate set as large as the number
          # of newly imported rows even when there are no duplicates at all,
          # and the window/delete passes below scale with that candidate size.
          # A bulk import of largely distinct data would therefore pay the full
          # window+delete cost over millions of non-duplicate keys. Restricting
          # candidates to genuinely duplicated groups keeps the work proportional
          # to the number of real duplicates, for zero candidates it short-circuits
          # cheaply (the common no-reimport case), and the duplicated groups are
          # found from the new source_file_ids only.
          DBI::dbExecute(
            db,
            sprintf(
              "CREATE OR REPLACE TEMP TABLE dedup_newkeys AS
               SELECT DISTINCT %s FROM %s WHERE source_file_id IN (%s)",
              key_list,
              sensor,
              paste0(file_ids, collapse = ", ")
            )
          )
          DBI::dbExecute(
            db,
            sprintf(
              "CREATE OR REPLACE TEMP TABLE dedup_cand AS
               SELECT %s FROM %s b JOIN dedup_newkeys nk ON %s
               GROUP BY %s HAVING COUNT(*) > 1",
              b_key_list,
              sensor,
              join_nk,
              b_grp
            )
          )
          n_dupes <- DBI::dbGetQuery(db, "SELECT COUNT(*) AS n FROM dedup_cand")[[1]]
        }

        # No candidate groups in this sensor: nothing to do.
        if (n_dupes == 0L) {
          # file_ids branch dedup_newkeys was created as well, so drop both.
          DBI::dbExecute(db, "DROP TABLE IF EXISTS dedup_newkeys")
          DBI::dbExecute(db, sprintf("DROP TABLE IF EXISTS %s", cand_table))
          0L
        } else {
          # The winner per candidate key group: the row of the newest file, and
          # within that file the last recorded row (highest rowid). This is the
          # same last-wins/upsert rule for every sensor. Join to the candidate
          # table so only candidate key groups are considered.
          DBI::dbExecute(
            db,
            sprintf(
              "CREATE OR REPLACE TEMP TABLE dedup_keep AS
               SELECT rid FROM (
                 SELECT t.rowid AS rid,
                        ROW_NUMBER() OVER (PARTITION BY %s ORDER BY t.source_file_id DESC, t.rowid DESC) AS rn
                 FROM %s t
                 JOIN %s d ON %s
               ) WHERE rn = 1",
              key_list_t,
              sensor,
              cand_table,
              join_t
            )
          )
          # Remove all non-winning rows of the candidate key groups. The join to
          # the candidate table is essential: rows whose key group is not a
          # candidate are left untouched, also when they were imported by an
          # earlier run.
          n_removed <- DBI::dbExecute(
            db,
            sprintf(
              "DELETE FROM %s AS t USING %s d
               WHERE %s
                 AND t.rowid NOT IN (SELECT rid FROM dedup_keep)",
              sensor,
              cand_table,
              join_t
            )
          )
          DBI::dbExecute(db, "DROP TABLE IF EXISTS dedup_keep")
          DBI::dbExecute(db, "DROP TABLE IF EXISTS dedup_newkeys")
          DBI::dbExecute(db, sprintf("DROP TABLE IF EXISTS %s", cand_table))
          n_removed
        }
      })
    )
    removed <- c(removed, setNames(n_removed, sensor))
  }

  removed
}

# Deduplication keys per sensor table. The default is (participant_id, time).
# The tie-break within a duplicated key group is always last-wins (upsert):
# keep the row of the newest file, and within that file the last recorded row.
read_dedup_keys <- list(
  AppUsage = c("participant_id", "time", "package_name"),
  Bluetooth = c("participant_id", "time", "bluetooth_device_id"),
  BluetoothBeacon = c("participant_id", "time", "uuid", "region"),
  GarminActigraphy = c("participant_id", "time", "instance"),
  Heartbeat = c("participant_id", "time", "device_type")
)
