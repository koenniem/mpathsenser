utils::globalVariables(c(
  "overlaps", # dplyr join_by helper
  "epoch",
  # DuckDB functions reached through dbplyr's pass-through translation.
  "make_timestamptz",
  "epoch_us",
  ".gaps_prev_time",
  "to_seconds",
  "time_bucket",
  "date_diff",
  "generate_series",
  "to_months",
  # Bare column names in join_by() expressions, where .data$ is not allowed.
  "participant_id",
  "measure",
  "time",
  "from",
  "to",
  "event_time",
  "seg_start",
  "seg_end",
  "seg_end_match",
  "cell_start",
  "cell_end"
))
