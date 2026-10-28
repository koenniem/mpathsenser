# Build the simulated ESM fixture used in the get-started vignette.
#
# The responses are simulated, but the participant, beep times, and step counts
# come from the Android example capture in inst/extdata/example. The file uses
# the m-Path CSV layout and mpathr's example metadata, so it can be read with
# mpathr::read_mpath(). Run from the package root:
#
#   Rscript data-raw/build_example_esm.R
#
# The generated CSV is committed, so re-run this script when the fixture or
# the simulated values change.

if (!"mpathsenser" %in% loadedNamespaces()) {
  pkgload::load_all(".", quiet = TRUE)
}

if (!requireNamespace("mpathr", quietly = TRUE)) {
  cli::cli_abort("The mpathr package is needed to build the ESM fixture.")
}

suppressPackageStartupMessages(library(dplyr))

participant_id <- 372780
timezone_offset <- 7200L
output_dir <- "inst/extdata/example-esm"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# The raw export from mpathr is the template for the column layout and the
# metadata. Reading it keeps every column that read_mpath() expects, including
# the list columns that are easy to get wrong by hand.
raw_basic <- system.file("extdata", "example_basic.csv", package = "mpathr")
raw_meta <- system.file("extdata", "example_meta.csv", package = "mpathr")
raw_example <- mpathr::read_mpath(raw_basic, raw_meta)

# Beep times are participant-local wall-clock times. They sit in the dense
# parts of the pedometer coverage, so every 30-minute window has readings.
beep_local <- as.POSIXct(
  c(
    "2026-09-30 08:47:00",
    "2026-09-30 09:17:00",
    "2026-09-30 11:17:00",
    "2026-09-30 12:17:00",
    "2026-09-30 13:12:00",
    "2026-09-30 15:22:00",
    "2026-09-30 16:17:00",
    "2026-09-30 17:52:00",
    "2026-10-01 08:22:00",
    "2026-10-01 09:12:00",
    "2026-10-01 10:17:00",
    "2026-10-01 12:27:00",
    "2026-10-01 13:12:00",
    "2026-10-01 13:47:00",
    "2026-10-01 14:47:00",
    "2026-10-01 18:22:00"
  ),
  tz = "UTC"
)
answered <- rep(TRUE, length(beep_local))
answered[c(4, 11, 14)] <- FALSE

n_beeps <- length(beep_local)
start_delay <- 30L
answer_duration <- 90L
sent_offset <- 2L
template <- raw_example |>
  filter(questionListName == "main_questionnaire") |>
  slice_head(n = n_beeps)
if (nrow(template) != n_beeps) {
  stop("The mpathr example export does not have enough questionnaire rows.")
}

# Raw m-Path timestamps are local wall-clock seconds, while the database stores
# true UTC instants. Subtract the offset to link against the Pedometer table.
beep_utc <- as.POSIXct(
  as.numeric(beep_local) - timezone_offset,
  origin = "1970-01-01",
  tz = "UTC"
)
beep_start_utc <- beep_utc + sent_offset + start_delay

# Steps in the 30 minutes before each beep -----------------------------------

db <- example_db()

pedometer <- get_data(db, "Pedometer", participant_id = participant_id) |>
  collect()

close_db(db)

beeps <- tibble(participant_id = participant_id, time = beep_start_utc)
linked <- link(
  x = beeps,
  y = pedometer,
  by = "participant_id",
  time = time,
  y_time = time,
  offset_before = "30 minutes",
  add_before = TRUE,
  name = "pedometer"
)

# `add_before` inserts a baseline row when the window itself has no readings.
# The fixture should not confuse that baseline with real coverage, so require
# at least one measurement inside every window.
has_window_reading <- vapply(
  linked$pedometer,
  function(x) any(is.na(x$original_time)),
  logical(1)
)
if (!all(has_window_reading)) {
  stop(
    "Some beep windows have no pedometer readings: ",
    paste(which(!has_window_reading), collapse = ", ")
  )
}

steps_before <- vapply(
  linked$pedometer,
  function(x) max(x$step_count) - min(x$step_count),
  numeric(1)
)

# A modest negative association with fixed noise. The values are simulated for
# the vignette; they are not measurements from the participant.
set.seed(20261027)
noise <- sample(-12:12, n_beeps, replace = TRUE)
anxiety <- as.integer(pmax(pmin(55 - steps_before / 120 + noise, 100), 0))
anxiety[!answered] <- NA_integer_

# Build the ESM export --------------------------------------------------------

base_columns <- c(
  "connectionId",
  "legacyCode",
  "code",
  "alias",
  "initials",
  "accountCode",
  "scheduledBeepId",
  "sentBeepId",
  "reminderForOriginalSentBeepId",
  "questionListName",
  "questionListLabel",
  "fromProtocolName",
  "timeStampScheduled",
  "timeStampSent",
  "timeStampStart",
  "timeStampStop",
  "originalTimeStampSent",
  "timeZoneOffset",
  "deltaUTC"
)
question_columns <- setdiff(
  names(template),
  c(base_columns, "slider_anxious_sliderNegPos")
)

esm <- template |>
  # Only the anxiety rating is used in the vignette; clear the other answers
  # from the template so the fixture contains no unrelated responses.
  mutate(across(all_of(question_columns), ~NA)) |>
  mutate(
    connectionId = participant_id,
    legacyCode = "409460__372780",
    code = "example",
    alias = "participant",
    initials = "P",
    accountCode = "example",
    scheduledBeepId = row_number(),
    sentBeepId = 1000L + row_number(),
    reminderForOriginalSentBeepId = 0L,
    timeStampScheduled = as.integer(as.numeric(beep_local)),
    timeStampSent = as.integer(as.numeric(beep_local) + sent_offset),
    timeStampStart = if_else(
      answered,
      as.integer(as.numeric(beep_local) + sent_offset + start_delay),
      NA_integer_
    ),
    timeStampStop = if_else(
      answered,
      as.integer(as.numeric(beep_local) + sent_offset + start_delay + answer_duration),
      NA_integer_
    ),
    originalTimeStampSent = timeStampSent,
    timeZoneOffset = timezone_offset,
    deltaUTC = 0,
    slider_anxious_sliderNegPos = anxiety
  )

output_csv <- file.path(output_dir, "example_esm.csv")
mpathr::write_mpath(esm, output_csv, .progress = FALSE)

# Read the file back with mpathr's raw metadata, so the committed fixture is
# validated against the same reader the vignette uses.
check <- mpathr::read_mpath(output_csv, raw_meta)
decoded <- mpathr::timestamps_to_datetime(
  check$timeStampSent,
  tz_offset = check$timeZoneOffset
)
if (!identical(check$slider_anxious_sliderNegPos, anxiety)) {
  stop("The stress values did not survive the round trip through write_mpath().")
}
if (!isTRUE(all.equal(as.numeric(decoded), as.numeric(beep_utc)))) {
  stop("The beep timestamps did not survive the round trip through write_mpath().")
}

cat(
  "Wrote",
  nrow(esm),
  "beeps for participant",
  participant_id,
  "to",
  output_csv,
  "\n"
)
cat(
  "Answered:",
  sum(answered),
  "of",
  length(answered),
  "| steps range:",
  paste(range(steps_before), collapse = "-"),
  "| anxiety range:",
  paste(range(anxiety, na.rm = TRUE), collapse = "-"),
  "\n"
)
