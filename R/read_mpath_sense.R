# Read m-Path Sense data into a DuckDB database.
#
# The JSON files are staged directly inside DuckDB (read_json) and SQL ingest
# functions move the data into the sensor tables. All files of a batch are
# staged together, which lets DuckDB parallelise reading and writing across the
# whole batch. DuckDB's own memory usage is controlled through the `threads`,
# `memory_limit`, and `temp_directory` arguments of [create_db()] and [open_db()].
#
# The sensor ingest functions are organised in a versioned registry, keyed by
# the `senseVersion` attribute found in the mpathinfo entry that starts every
# m-Path Sense JSON file. If a file reports a version that is not (yet)
# registered, the `default` parser set is used and a warning is issued.

#' Import m-Path Sense data into a DuckDB database
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Import JSON files from m-Path Sense into a DuckDB database. This function
#'   is the bread and butter of this package, as it populates the database with
#'   the data that most of the other functions in this package use.
#'
#' @details Files are processed in batches of `batch_size` files. Each batch is
#'   imported in a single transaction: if a file in the batch causes an error,
#'   the batch is rolled back and each file of the batch is retried
#'   individually, so that only the files that truly fail to import are
#'   reported. This keeps the import function safe to run unattended (e.g. in a
#'   loop on a server).
#'
#'   Within a batch, all files are staged together and sensor data is ingested
#'   in the same transaction. This lets DuckDB parallelise reading and writing
#'   across the whole batch while preserving atomic batch-level rollback.
#'
#'   Files are tracked in the `ProcessedFiles` table. A file is skipped when a
#'   file with the same name, size, and modification time was already imported.
#'   Files with the same name but different content (e.g. a corrected file that
#'   was re-uploaded) are imported again. Files with duplicate content under a
#'   different name are imported as well; on deduplication, the data of the
#'   newest file then wins per measurement key, so corrected data replaces old
#'   values while measurements that were removed from the new file are
#'   preserved. Deduplication examines only the key groups of the rows that
#'   this run imported (newly inserted rows plus any existing rows sharing a
#'   key with them), so its cost scales with the amount of new data rather than
#'   the size of the whole database. Use [deduplicate_db()] to clean up
#'   duplicates that predate this run, e.g. after an interrupted import.
#'
#'   Timestamps are stored as UTC instants in DuckDB. When timezone
#'   measurements are available, the import also assigns each observation its
#'   IANA timezone using [add_timezones_to_db()]'s interval-matching rules.
#'   Sensor tables always contain both the absolute timestamp and the
#'   observation's timezone.
#'
#' @section Parallel: This function does not support parallel processing via
#'   \pkg{future}, as DuckDB already parallelises reading and writing across its
#'   threads. Since a DuckDB database allows only a single writer at a time, do
#'   not run multiple imports into the same database concurrently.
#'
#' @param path The path to the file directory.
#' @param db Valid database connection, typically created by [create_db()].
#' @param sensors Select one or multiple sensors as in
#'   \code{\link[mpathsenser]{sensors}}. Leave NULL to extract all sensor data.
#' @param batch_size The number of files that are to be processed in a single
#'   batch. All files of a batch are staged and ingested together; larger
#'   batches import faster but use more memory.
#' @param recursive Should the listing recurse into directories?
#' @param .progress Whether to display a progress bar.
#' @param debug Whether suppressed warnings and errors should be shown for
#'   debugging purposes. When `TRUE`, a message is shown for every staged batch
#'   and for every sensor that is ingested.
#'
#' @returns A message indicating how many files were imported. If all files
#'   were imported successfully, this function returns an empty string
#'   invisibly. Otherwise the file names of the files that were not imported are
#'   returned visibly.
#'
#' @seealso [create_db()] for creating a database for this function to use;
#'   [close_db()] for closing this database; [optimize_db()] to re-order the
#'   data in the sensor tables for faster future processing;
#'   [add_timezones_to_db()] to assign a timezone to each measurement.
#'
#' @export
read_mpath_sense <- function(
  path = getwd(),
  db,
  sensors = NULL,
  batch_size = 1000,
  recursive = TRUE,
  .progress = TRUE,
  debug = FALSE
) {
  # Check arguments
  check_arg(path, type = "character", n = 1)
  check_db(db)
  check_sensors(sensors, allow_null = TRUE)
  check_arg(batch_size, "integerish", n = 1)
  check_arg(recursive, "logical", n = 1)
  check_arg(.progress, "logical", n = 1)
  check_arg(debug, "logical", n = 1)

  # Roll back any unfinished transaction (e.g. left behind by an interrupted
  # run), so that this import can proceed.
  .read_rollback_stale_transaction(db)

  # Check if the directory exists
  if (!dir.exists(path)) {
    cli_abort(c(
      "Directory {.path {path}} does not exist.",
      i = "Did you make a typo in the path name?"
    ))
  }

  # Retrieve all JSON files and their file metadata
  files <- list.files(path = path, pattern = "*.json$", recursive = recursive)

  # Process the files oldest to newest, based on the timestamp embedded in the
  # file name (m-Path Sense names carry the export time). This stores the data
  # roughly chronologically in the sensor tables, which improves compression
  # and zonemap pruning, and makes the batch order deterministic. Files
  # without a parseable timestamp (e.g. renamed files) are processed last.
  time_pat <- "m_Path_sense_([0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2})"
  stamp <- ifelse(
    grepl(time_pat, files),
    sub(paste0(".*", time_pat, ".*"), "\\1", files),
    NA_character_
  )
  files <- files[order(stamp, files, na.last = TRUE)]

  if (length(files) == 0) {
    cli_abort(c(
      "Can't find any JSON files in {.path {path}}.",
      i = "Did you put the JSON files in the correct directory?"
    ))
  }

  .read_debug(debug, "Found {length(files)} file{?s} to process.")

  # Register meta data of the file to track provenance
  full_paths <- normalizePath(file.path(path, files), mustWork = FALSE)
  file_info <- file.info(full_paths, extra_cols = FALSE)
  file_meta <- tibble::tibble(
    source_file = full_paths,
    file_name = basename(full_paths),
    rel_path = files,
    file_size_bytes = file_info$size,
    modified_at = as.POSIXct(file_info$mtime, tz = "UTC")
  )

  # Empty files contain no mpathinfo and cannot be staged with read_json.
  # Register them as processed (if their name allows attributing them to a
  # participant) so they are not reported on every run.
  is_empty <- file_meta$file_size_bytes == 0
  skipped_empty <- character(0)
  if (any(is_empty)) {
    skipped_empty <- .read_register_empty_files(db, file_meta[is_empty, ])
  }
  file_meta <- file_meta[!is_empty, ]

  # Resolve which sensor ingest functions to run
  target_sensors <- .read_resolve_sensors(sensors)

  # Keep only files that were not yet (successfully) imported
  .read_debug_time(
    debug,
    "Checking for already imported files",
    "Found {length(files) - nrow(file_meta)} duplicate file{?s}. Continuing with {nrow(file_meta)} file{?s}.",
    file_meta <- .read_filter_new_files(db, file_meta)
  )

  if (nrow(file_meta) == 0) {
    cli_inform("No new files to process.")
    return(invisible(""))
  }

  # Split the files into batches
  batches <- split(seq_len(nrow(file_meta)), ceiling(seq_len(nrow(file_meta)) / batch_size))

  # Set up a progress bar
  if (.progress) {
    pb <- cli::cli_progress_bar(
      total = length(batches),
      format = "Importing data... {cli::pb_bar} {cli::pb_current}/{cli::pb_total} batch{?es} \\
      [{cli::pb_percent}] | {cli::pb_eta_str}"
    )
    cli::cli_progress_update(inc = 0, force = TRUE)
  }

  # Bookkeeping across batches
  run_file_ids <- integer(0)
  skipped_files <- character(0) # no mpathinfo, no participant id, or not registered
  failed_files <- character(0) # files that error even when retried individually
  unknown_types <- list()
  unknown_versions <- character(0)
  active_sensors <- character(0) # sensors for which data was actually found

  for (batch_idx in seq_along(batches)) {
    if (debug) {
      len_batches <- length(batches)
      cli::cli_rule(
        left = "Starting work on {.field batch {batch_idx}} out of {len_batches}."
      )
    }

    batch_files <- file_meta[batches[[batch_idx]], , drop = FALSE]
    res <- .read_mpath_sense_batch(db, batch_files, target_sensors, debug)

    run_file_ids <- c(run_file_ids, res$file_ids)
    skipped_files <- c(skipped_files, res$skipped)
    failed_files <- c(failed_files, res$failed)
    unknown_types <- c(unknown_types, list(res$unknown_types))
    unknown_versions <- unique(c(unknown_versions, res$unknown_versions))
    active_sensors <- unique(c(active_sensors, res$active_sensors))

    # Update progress bar
    if (.progress) {
      cli::cli_progress_update(inc = 1, force = TRUE)
    }
  }

  # Deduplicate the sensor data. Because the sensor tables have no unique
  # constraints, duplicate measurements (e.g. the same file imported under a
  # different name) are removed afterwards, per measurement key, with the
  # newest file winning. Only the key groups of the rows that this run just
  # imported are examined (the rows we inserted, plus any existing rows that
  # share a key with them); rows that were already in the database before this
  # run were deduplicated when they were imported and are left untouched. This
  # keeps the cost proportional to the amount of new data instead of the whole
  # table size, which matters for small imports into large databases. Use
  # deduplicate_db() to also clean up duplicates left behind by interrupted
  # imports. Sensors without candidate groups cost a single grouped scan.
  if (length(run_file_ids) > 0) {
    .read_dedup(db, active_sensors, debug = debug, file_ids = run_file_ids)

    # Optimize the database before adding timezones
    .read_debug_time(
      debug,
      msg = "Optimizing the database...",
      msg_done = "Database optimized.",
      optimize_db(db, sensors = active_sensors, .progress = FALSE)
    )

    # Add the observation timezone after all files have been ingested and
    # deduplicated. This keeps the canonical timestamp as TIMESTAMPTZ while
    # making newly imported databases immediately usable.
    has_timezones <- DBI::dbGetQuery(
      db,
      "SELECT COUNT(*) AS n FROM Timezone"
    )
    has_timezones <- has_timezones$n > 0
    if (has_timezones) {
      .read_debug_time(
        debug,
        msg = "Adding timezones to measurements...",
        msg_done = "Added timezones to database.",
        add_timezones_to_db(db, .progress = FALSE)
      )
    }
  }

  # Close the progress bar
  cli::cli_progress_done()

  # Aggregated warnings (never per file, as there may be hundreds of thousands)
  if (length(unknown_versions) > 0) {
    cli_warn(c(
      "Unknown senseVersion{?s} {.val {unknown_versions}} found in the data.",
      i = "Data was imported using the default parser. Please contact the package maintainer."
    ))
  }
  unknown_types <- .read_aggregate_types(unknown_types)
  if (length(unknown_types) > 0) {
    n_types <- paste0(names(unknown_types), " (", unname(unknown_types), "x)")
    cli_warn(c(
      "Unknown sensor type{?s} {.val {names(unknown_types)}} found in the data.",
      i = "Data from these sensors is not imported: {n_types}.",
      i = "New sensor types may indicate a new senseVersion; please contact the package maintainer."
    ))
  }
  if (length(skipped_files) > 0) {
    cli_warn(c(
      "Skipped {length(skipped_files)} file{?s} that could not be attributed to a participant (no mpathinfo entry).",
      i = "These files are reported as unprocessed and can be inspected manually."
    ))
  }
  problems <- unique(c(skipped_files, skipped_empty, failed_files))

  if (length(problems) == 0) {
    cli_inform(
      "All {nrow(file_meta) + sum(is_empty)} file{?s} {?was/were} successfully written to the database."
    )
    return(invisible(""))
  } else {
    cli_warn("Some files could not be written to the database.")
    return(problems)
  }
}

# Roll back an unfinished transaction, if any. An interrupted import can leave
# an open transaction on the connection; without this, the next import would
# fail with "there is still an active transaction".
.read_rollback_stale_transaction <- function(db) {
  started <- tryCatch(
    {
      DBI::dbBegin(db)
      TRUE
    },
    error = function(e) FALSE
  )
  if (started) {
    # There was no unfinished transaction: roll back the freshly started one
    DBI::dbRollback(db)
  } else {
    DBI::dbRollback(db)
    cli_inform("Rolled back an unfinished transaction left by a previous import.")
  }
  invisible(NULL)
}

# Process a batch of files within a single transaction. When the batch fails,
# each file of the batch is retried individually so that only the files that
# also fail on their own are reported as unprocessed.
.read_mpath_sense_batch <- function(db, batch_meta, target_sensors, debug) {
  res <- tryCatch(
    .read_db_transaction(
      db,
      .read_mpath_sense_loop(db, batch_meta, target_sensors, debug)
    ),
    error = function(e) {
      .read_debug(debug, conditionMessage(e))
      NULL
    }
  )

  if (!is.null(res)) {
    return(res)
  }

  out <- list(
    file_ids = integer(0),
    skipped = character(0),
    failed = character(0),
    unknown_types = list(),
    unknown_versions = character(0),
    active_sensors = character(0)
  )
  for (i in seq_len(nrow(batch_meta))) {
    res1 <- tryCatch(
      .read_db_transaction(
        db,
        .read_mpath_sense_loop(
          db,
          batch_meta[i, , drop = FALSE],
          target_sensors,
          debug
        )
      ),
      error = function(e) {
        .read_debug(debug, conditionMessage(e))
        NULL
      }
    )
    if (is.null(res1)) {
      out$failed <- c(out$failed, batch_meta$rel_path[i])
    } else {
      out$file_ids <- c(out$file_ids, res1$file_ids)
      out$skipped <- c(out$skipped, res1$skipped)
      out$unknown_types <- c(out$unknown_types, list(res1$unknown_types))
      out$unknown_versions <- unique(c(out$unknown_versions, res1$unknown_versions))
      out$active_sensors <- unique(c(out$active_sensors, res1$active_sensors))
    }
  }
  out$unknown_types <- .read_combine_types(out$unknown_types)
  out
}

# Process a batch of files: stage the raw JSON payloads of the whole batch,
# register the studies, participants, and processed files, and dispatch the
# sensor ingest functions. Staging the whole batch together lets DuckDB
# parallelise the work. Returns a list with:
# - file_ids: the file_id values of the ProcessedFiles records created
# - skipped: relative paths of files in the batch that were skipped
# - unknown_types: data.frame of payload types not handled by any parser
# - unknown_versions: senseVersion values not found in the registry
# - active_sensors: sensors for which data was found in the batch
.read_mpath_sense_loop <- function(db, batch_meta, target_sensors, debug) {
  # Clean up temporary tables, also when an error occurs outside of a
  # transaction (e.g. a failed batch that was rolled back).
  on.exit(
    try(
      DBI::dbExecute(
        db,
        "DROP TABLE IF EXISTS raw_staging; DROP TABLE IF EXISTS mpathinfo_map;
       DROP TABLE IF EXISTS file_metadata_map; DROP TABLE IF EXISTS file_id_map"
      ),
      silent = TRUE
    ),
    add = TRUE
  )

  batch_paths <- batch_meta$source_file

  # Stage the raw JSON payloads. read_json reads all files of the batch at
  # once; filename adds the full path, which links each row to its file.
  #
  # The format is given explicitly: m-Path Sense files are JSON arrays, and
  # format = 'auto' reads the file twice (once to detect the format), which
  # roughly doubles the memory needed to stage large files. The payload type
  # is extracted with a regular expression on the serialized JSON instead of
  # data->>'__type', and the data is kept as VARCHAR rather than JSON, because
  # parsing the JSON at staging time keeps staging memory bounded; typed
  # transformations are applied only by the sensor queries that need them.
  stage_query <- function(format) {
    paste0(
      "CREATE OR REPLACE TEMP TABLE raw_staging AS ",
      "SELECT sensorStartTime, sensorEndTime, ",
      "regexp_extract(data, '\"__type\"\\s*:\\s*\"([^\"]+)\"', 1) AS payload_type, ",
      "filename AS source_file, data ",
      "FROM read_json(",
      .read_sql_array(batch_paths),
      ", ",
      "format = '",
      format,
      "', filename = true, ",
      "columns = {'sensorStartTime': 'BIGINT', 'sensorEndTime': 'BIGINT', 'data': 'VARCHAR'})"
    )
  }
  staged <- .read_debug_time(
    debug,
    "Staging {length(batch_paths)} file{?s}",
    "Staging {length(batch_paths)} file{?s}.",
    tryCatch(
      {
        DBI::dbExecute(db, stage_query("array"))
        TRUE
      },
      error = function(e) FALSE
    )
  )
  if (!staged) {
    # Fall back to auto-detection for files that are not JSON arrays
    DBI::dbExecute(db, stage_query("auto"))
  }

  # Unknown sensor types: collect for an aggregated warning at the end of the
  # import. Known but useless types (see ignored_sensor_types) are skipped
  # silently. New types often signal a new senseVersion.
  types <- DBI::dbGetQuery(
    db,
    "SELECT payload_type, COUNT(*) AS n FROM raw_staging WHERE payload_type IS NOT NULL GROUP BY payload_type"
  )
  known_types <- c(.read_known_types(), ignored_sensor_types)
  unknown_types <- types[!types$payload_type %in% known_types, , drop = FALSE]

  # Extract the mpathinfo entry of each file (one row per file). The type is
  # matched without parsing the JSON, so that large payloads (e.g. Garmin
  # logs) are not parsed at this stage; the mpathinfo fields themselves are
  # only parsed for the matching rows.
  .read_debug_time(
    debug,
    "Extracting mpathinfo metadata",
    "Extracting mpathinfo metadata.",
    {
      DBI::dbExecute(
        db,
        "CREATE OR REPLACE TEMP TABLE mpathinfo_map AS
         SELECT
           source_file,
           TRY_CAST(data->>'connectionId' AS UINTEGER) AS participant_id,
           COALESCE(NULLIF(data->>'studyName', ''), 'Unknown_Study') AS study_id,
           TRY_CAST(data->>'senseVersion' AS INTEGER) AS sense_version
         FROM raw_staging
         WHERE regexp_extract(data, '\"__type\"\\s*:\\s*\"([^\"]+)\"', 1) = 'dk.cachet.carp.mpathinfo'
         QUALIFY ROW_NUMBER() OVER (PARTITION BY source_file ORDER BY sensorStartTime) = 1"
      )
      mpath <- DBI::dbGetQuery(
        db,
        "SELECT source_file, participant_id, study_id, sense_version FROM mpathinfo_map"
      )
    }
  )

  # Files without mpathinfo cannot be attributed to a participant
  skipped <- setdiff(batch_paths, mpath$source_file)

  # Merge the mpathinfo metadata with the R-side file metadata. The participant
  # id is kept as a numeric (UINTEGER) value, matching the database column.
  meta <- merge(mpath, batch_meta, by = "source_file", all.x = TRUE, sort = FALSE)
  meta$participant_id <- as.numeric(meta$participant_id)

  # Files with a missing participant id cannot satisfy the NOT NULL
  # constraints of the database
  no_pid <- meta$source_file[is.na(meta$participant_id)]
  meta <- meta[!is.na(meta$participant_id), ]
  skipped <- unique(c(skipped, no_pid))

  if (nrow(meta) == 0) {
    return(list(
      file_ids = integer(0),
      skipped = batch_meta$rel_path[batch_meta$source_file %in% skipped],
      unknown_types = unknown_types,
      unknown_versions = character(0),
      active_sensors = character(0)
    ))
  }

  # Write the file metadata map (a temp table) for SQL-side joins. The batch
  # order is carried along as an explicit column: file_id values are assigned
  # in this order below, and file_id ordering decides which file wins on
  # deduplication (newest file wins), so the assignment must be deterministic.
  meta$batch_order <- match(meta$source_file, batch_meta$source_file)
  DBI::dbWriteTable(
    db,
    name = "file_metadata_map",
    value = meta,
    temporary = TRUE,
    overwrite = TRUE,
    row.names = FALSE
  )

  # Enforce the cascade hierarchy (Study, Participant, ProcessedFiles) and map
  # each file to its file_id (and sense_version for the version dispatch).
  # The file_ids are assigned with nextval() in deterministic batch order
  # (never assume the output of an unordered nextval() follows input order),
  # and the same batch-local mapping is used for the ProcessedFiles insert and
  # for the sensor ingest statements, so metadata and sensor rows always agree
  # on the file_id. ProcessedFiles is inserted with a plain INSERT (no ON
  # CONFLICT): .read_filter_new_files() already removed previously processed
  # files and intra-run duplicates, so every remaining row is new. DuckDB
  # implements ON CONFLICT DO NOTHING as a MERGE_INTO that scans the whole
  # ProcessedFiles table for every batch, which made registration grow
  # linearly with the database size; a plain INSERT appends at constant cost.
  .read_debug_time(
    debug,
    "Registering Study, Participant, and ProcessedFiles",
    "Registering Study, Participant, and ProcessedFiles.",
    {
      DBI::dbExecute(
        db,
        "INSERT INTO Study (study_id, data_format)
       SELECT DISTINCT study_id, 'CARP JSON' FROM file_metadata_map
       ON CONFLICT DO NOTHING"
      )
      DBI::dbExecute(
        db,
        "INSERT INTO Participant (participant_id, study_id)
       SELECT DISTINCT participant_id, study_id FROM file_metadata_map
       ON CONFLICT DO NOTHING"
      )
      DBI::dbExecute(
        db,
        "CREATE OR REPLACE TEMP TABLE file_id_map AS
       SELECT m.source_file, m.participant_id, m.study_id, m.sense_version,
              nextval('processed_files_seq') AS file_id
       FROM (SELECT source_file, participant_id, study_id, sense_version, batch_order
             FROM file_metadata_map
             ORDER BY batch_order) m"
      )
      DBI::dbExecute(
        db,
        "INSERT INTO ProcessedFiles (file_id, file_name, participant_id, sense_version, file_size_bytes, modified_at)
       SELECT m.file_id, f.file_name, f.participant_id, f.sense_version,
              CAST(f.file_size_bytes AS UBIGINT), CAST(f.modified_at AS TIMESTAMPTZ)
       FROM file_id_map m
       JOIN file_metadata_map f ON f.source_file = m.source_file"
      )
      fid <- DBI::dbGetQuery(
        db,
        "SELECT source_file, participant_id, study_id, file_id, sense_version FROM file_id_map"
      )
    }
  )

  # Files that could not be registered (e.g. duplicate content in the same
  # batch) are skipped and reported. Every file in the batch is assigned a
  # file_id above, so this is normally empty; it guards against files that
  # failed to register for any other reason.
  not_registered <- setdiff(meta$source_file, fid$source_file)
  skipped <- unique(c(skipped, not_registered))

  # Only run the ingest functions of sensors whose payload type occurs in the
  # staged data; the other queries would scan the staging table for nothing.
  .read_debug_time(
    debug,
    "Ingesting sensor data",
    "Ingesting sensor data.",
    {
      registry_default <- sensor_registry[["default"]]
      type_map <- vapply(registry_default, \(x) x[["type"]], character(1))
      active <- intersect(target_sensors, names(type_map)[type_map %in% types$payload_type])
    }
  )

  # Dynamically trigger the targeted ingest functions, per senseVersion
  unknown_versions <- character(0)
  for (v in unique(meta$sense_version)) {
    vkey <- if (is.na(v)) "unknown" else as.character(v)
    registry <- sensor_registry[[vkey]]
    if (is.null(registry)) {
      unknown_versions <- c(unknown_versions, vkey)
      registry <- registry_default
    }
    for (sensor_name in active) {
      sql <- registry[[sensor_name]]$fun(v)
      .read_debug_time(
        debug,
        "Ingesting data for {sensor_name}",
        "Ingested {n_rows} row{?s} into {sensor_name}.",
        n_rows <- .read_ingest(db, sql)
      )
    }
  }

  list(
    file_ids = fid$file_id,
    skipped = batch_meta$rel_path[batch_meta$source_file %in% skipped],
    unknown_types = unknown_types,
    unknown_versions = unknown_versions,
    active_sensors = active
  )
}
