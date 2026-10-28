# Shared coverage fixtures.

# Hand-computed fixture: participant 1 timezone move, 2 fall-back fold,
# 3 spring-forward gap, 4 missing timezone (leading and mid-stream).
timezone_fixture_db <- function() {
  db <- fresh_test_db()
  DBI::dbExecute(db, "INSERT INTO Study (study_id, data_format) VALUES ('fix', NULL)")
  DBI::dbExecute(db, "INSERT INTO Participant SELECT i, 'fix' FROM range(1, 5) t(i)")
  DBI::dbExecute(
    db,
    "
    INSERT INTO raw.Accelerometer
      (participant_id, time, timezone, source_file_id, source_row_id, source_measurement_id)
    VALUES
      (1, TIMESTAMPTZ '2024-01-15 09:00:00+00', 'Europe/Amsterdam', 1, 1, 1),
      (1, TIMESTAMPTZ '2024-01-15 11:00:00+00', 'America/New_York', 1, 2, 1),
      (2, TIMESTAMPTZ '2024-10-27 00:30:00+00', 'Europe/Amsterdam', 1, 3, 1),
      (2, TIMESTAMPTZ '2024-10-27 01:30:00+00', 'Europe/Amsterdam', 1, 4, 1),
      (3, TIMESTAMPTZ '2024-03-31 00:30:00+00', 'Europe/Amsterdam', 1, 5, 1),
      (3, TIMESTAMPTZ '2024-03-31 01:30:00+00', 'Europe/Amsterdam', 1, 6, 1),
      (4, TIMESTAMPTZ '2024-01-15 08:00:00+00', NULL, 1, 7, 1),
      (4, TIMESTAMPTZ '2024-01-15 09:00:00+00', 'Europe/Amsterdam', 1, 8, 1),
      (4, TIMESTAMPTZ '2024-01-15 10:00:00+00', NULL, 1, 9, 1),
      (4, TIMESTAMPTZ '2024-01-15 11:00:00+00', NULL, 1, 10, 1)
  "
  )
  db
}

coverage_db <- function(participants = "12345") {
  db <- fresh_test_db()
  DBI::dbExecute(
    db,
    "INSERT INTO Study (study_id, data_format) VALUES ('foo', NULL)"
  )
  for (participant in participants) {
    DBI::dbExecute(
      db,
      sprintf(
        "INSERT INTO Participant (participant_id, study_id) VALUES (%s, 'foo')",
        as.character(participant)
      )
    )
  }
  db
}

insert_measurements <- function(db, sensor, participant_id, time, ...) {
  n <- length(time)
  extra <- list(...)
  data <- data.frame(
    participant_id = rep(participant_id, length.out = n),
    time = as.POSIXct(time, tz = "UTC"),
    timezone = NA_character_,
    source_file_id = 1,
    source_row_id = seq_len(n),
    source_measurement_id = 1
  )
  for (name in names(extra)) {
    data[[name]] <- extra[[name]]
  }
  DBI::dbWriteTable(
    db,
    DBI::Id(schema = "raw", table = sensor),
    data,
    append = TRUE
  )
  invisible(db)
}
