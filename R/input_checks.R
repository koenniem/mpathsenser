# Function for testing if a package in 'suggested' is installed, before running it. This function
# needs to be at the top of the file to make sure it is skipped when calculating coverage.
ensure_suggested_package <- function(name, call = caller_env()) {
  if (!requireNamespace(name, quietly = TRUE)) {
    cli_abort(
      c(
        "Package {.pkg {name}} is needed for this function to work.",
        i = "Please install it using {.code install.packages('{name}')}"
      ),
      call = call
    )
  }
  invisible(TRUE)
}

check_db <- function(
  db,
  allow_null = FALSE,
  arg = caller_arg(db),
  call = caller_env()
) {
  rlang::check_required(db, arg = arg, call = call)

  if (allow_null && is_null(db)) {
    return(invisible(TRUE))
  }

  if (is.null(db)) {
    msg <- paste0("Database connection {.arg ", arg, "} must not be NULL.")
    cli_abort(msg, arg = arg, call = call)
  }

  if (!inherits(db, "DBIConnection")) {
    msg <- c(
      paste0("Argument {.arg ", arg, "} is not a database connection."),
      x = paste0("You supplied ", with_article(utils::tail(class(db), 1)), ".")
    )
    cli_abort(msg, arg = arg, call = call)
  }

  if (inherits(db, "SQLiteConnection")) {
    msg <- c(
      x = "You provided an SQLite database,",
      x = "Support for SQLite was dropped in {.pkg mpathsenser} 2.0.0 in favour of DuckDB.",
      i = "Please import your data to a new database using the latest version of {.pkg mpathsenser}.",
      i = "Or use an older version of {.pkg mpathsenser}."
    )
    cli_abort(msg, arg = arg, call = call)
  }

  if (!dbIsValid(db)) {
    msg <- c(
      paste0("Database connection {.arg ", arg, "} is not valid."),
      i = "Did you forget to open the connection or save it to a variable?"
    )
    cli_abort(msg, arg = arg, call = call)
  }

  # Unlike the sensor check, the version check is never skippable: an
  # unsupported layout can produce wrong results without an error.
  version <- .read_database_version(db)
  if (!.database_version_supported(version)) {
    .abort_unsupported_database_version(version, call = call)
  }

  # Check that all sensors are present in the database. Physical sensor
  # tables live in the raw schema; the main schema holds a read-only view per
  # sensor. Both layers are required: a missing raw table means the data is
  # gone, and a missing view means queries through main.<sensor> fail.
  if (isTRUE(getOption("mpathsenser.check_missing_sensors", TRUE))) {
    raw_tables <- dbGetQuery(
      db,
      "SELECT table_name FROM information_schema.tables
       WHERE table_schema = 'raw' AND table_type = 'BASE TABLE'
         AND NOT ends_with(table_name, '_optimize_tmp')"
    )$table_name
    main_tables <- dbGetQuery(
      db,
      "SELECT table_name FROM information_schema.tables
       WHERE table_schema = 'main' AND table_type IN ('BASE TABLE', 'VIEW')"
    )$table_name
    missing_sensors <- sensors[
      !(tolower(sensors) %in% tolower(raw_tables)) |
        !(tolower(sensors) %in% tolower(main_tables))
    ]
    if (length(missing_sensors) > 0) {
      msg <- c(
        "The following sensor{?s} {?is/are} missing from the database: {.val {missing_sensors}}.",
        x = "The database is likely corrupt.",
        i = "Try re-opening the database or create a new one using {.help create_db}.",
        i = "To skip this check in this session, run \\
        {.code options(mpathsenser.check_missing_sensors = FALSE)}."
      )
      cli_abort(msg, arg = arg, call = call)
    }
  }

  return(invisible(TRUE))
}

.is_lazy_tbl <- function(x) inherits(x, "tbl_sql")

check_arg <- function(
  x,
  type,
  n = NULL,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  rlang::check_required(x, arg = arg, call = call)

  if (allow_null && is_null(x)) {
    return(invisible(TRUE))
  }

  type <- match.arg(
    type,
    c(
      "character",
      "integer",
      "double",
      "logical",
      "integerish",
      "numeric",
      "factor",
      "POSIXt",
      "Date",
      "data.frame",
      "tbl_sql",
      "list"
    ),
    several.ok = TRUE
  )

  res <- lapply(type, function(y) {
    switch(
      y,
      character = rlang::is_character(x, n),
      integer = rlang::is_integer(x, n),
      double = rlang::is_double(x, n),
      logical = rlang::is_logical(x, n),
      integerish = rlang::is_integerish(x, n),
      numeric = rlang::is_double(x, n) || rlang::is_integer(x, n),
      factor = is.factor(x) && (is.null(n) || length(x) == n),
      POSIXt = inherits(x, "POSIXt") && (is.null(n) || length(x) == n),
      Date = inherits(x, "Date") && (is.null(n) || length(x) == n),
      data.frame = is.data.frame(x),
      tbl_sql = .is_lazy_tbl(x),
      list = rlang::is_list(x, n)
    )
  })
  res <- unlist(res)

  if (!any(res)) {
    n_provided <- NULL
    if (!is.null(n)) {
      n <- paste(" of length", n)
      n_provided <- paste(" of length", length(x))
    }

    # Internal class names are not what users see in the documentation.
    type_display <- type
    type_display[type_display == "tbl_sql"] <- "lazy table"

    msg <- c(
      paste0("Argument {.arg ", arg, "} must be ", with_article(type_display), n, "."),
      x = paste0(
        "You supplied ",
        with_article(utils::tail(class(x), 1)),
        n_provided,
        "."
      )
    )
    cli_abort(msg, arg = arg, call = call)
  }

  return(invisible(TRUE))
}

with_article <- function(x) {
  article <- dplyr::if_else(grepl("^[aeiouAEIOU]", x), "an", "a")
  paste(article, x, collapse = " or ")
}

check_sensors <- function(
  x,
  n = NULL,
  allow_null = FALSE,
  include_views = TRUE,
  resolve = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_arg(x, type = "character", allow_null = allow_null, n = n, arg = arg, call = call)
  check_arg(include_views, "logical", n = 1)
  check_arg(resolve, "logical", n = 1)
  valid_sensors <- sensors
  if (include_views) {
    valid_sensors <- c(
      valid_sensors,
      paste0(sensors, "_local"),
      paste0(sensors, "_with_local")
    )
  }
  missing <- x[!(tolower(x) %in% tolower(valid_sensors))]

  if (length(missing) > 0) {
    msg <- c(
      "Sensor{?s} {.arg {missing}} could not be found.",
      i = "See {.code mpathsenser::sensors} for the full list of available sensors."
    )
    cli_abort(msg, arg = arg, call = call)
  }

  if (!resolve) {
    return(invisible(TRUE))
  }

  # `resolve = TRUE` returns canonical base names instead: NULL means every
  # sensor, view suffixes are stripped, and duplicates collapse.
  if (is_null(x)) {
    return(sensors)
  }
  resolved <- unique(.standard_sensor_names(.physical_sensor(x)))
  if (length(resolved) == 0L) {
    cli_abort("No valid sensors to compute coverage for.", arg = arg, call = call)
  }
  resolved
}

.physical_sensor <- function(x) {
  sub("_(local|with_local)$", "", x, ignore.case = TRUE)
}

# Convert sensor names to the package's spelling, leaving unknown names unchanged.
.standard_sensor_names <- function(x) {
  lookup <- stats::setNames(sensors, tolower(sensors))
  standard <- unname(lookup[tolower(x)])
  standard[is.na(standard)] <- x[is.na(standard)]
  standard
}

# Return the database's participants and reject unknown requested IDs.
check_participants <- function(db, participant_id) {
  participants <- as.character(get_participants(db)$participant_id)
  if (length(participants) == 0L) {
    cli_abort("The database does not contain any participants.")
  }
  if (!is.null(participant_id)) {
    unknown <- setdiff(as.character(participant_id), participants)
    if (length(unknown) > 0L) {
      cli_abort(
        "Participant{?s} {.val {unknown}} could not be found in the {.pkg mpathsenser} database."
      )
    }
  }
  participants
}

# Validate the optional date window and normalise it to Date or NULL.
check_dates <- function(start_date, end_date) {
  if (!.is_date(start_date) || !.is_date(end_date)) {
    cli_abort(
      "{.arg start_date} and {.arg end_date} must be {.val NULL}, a date string, or a {.cls Date}."
    )
  }
  list(
    start_date = if (is.null(start_date)) NULL else as.Date(start_date),
    end_date = if (is.null(end_date)) NULL else as.Date(end_date)
  )
}

# TRUE for NULL, or for a length-one value coercible to a non-NA Date.
.is_date <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  }
  if (!inherits(x, "Date") && !is.character(x)) {
    return(FALSE)
  }
  date <- try(as.Date(x), silent = TRUE)
  if (!inherits(date, "Date") || length(date) != 1L || is.na(date)) {
    return(FALSE)
  }
  TRUE
}

# Validate the week start as a whole number from 1 (Monday) to 7 (Sunday).
check_week_start <- function(week_start) {
  check_arg(week_start, type = c("numeric", "integerish"), n = 1)
  if (!rlang::is_integerish(week_start) || week_start < 1 || week_start > 7) {
    cli_abort("{.arg week_start} must be a whole number between 1 (Monday) and 7 (Sunday).")
  }
  as.integer(week_start)
}

check_offset <- function(offset_before, offset_after, call = caller_env()) {
  if (
    (is.null(offset_before) || all(offset_before == 0)) &&
      (is.null(offset_after) || all(offset_after == 0))
  ) {
    cli_abort(
      "{.arg offset_before} and {.arg offset_after} cannot both be 0 or NULL.",
      call = call
    )
  }
  if (
    !is.null(offset_before) &&
      !(is.character(offset_before) ||
        lubridate::is.period(offset_before) ||
        is.numeric(offset_before))
  ) {
    cli_abort(
      "{.arg offset_before} must be a character, numeric, or period.",
      call = call
    )
  }
  if (
    !is.null(offset_after) &&
      !(is.character(offset_after) ||
        lubridate::is.period(offset_after) ||
        is.numeric(offset_after))
  ) {
    cli_abort(
      "{.arg offset_after} must be a character, numeric, or period.",
      call = call
    )
  }

  # Convert offset_before to integer time
  if (is.character(offset_before) || is.numeric(offset_before)) {
    offset_before <- lubridate::as.period(offset_before)
    offset_before <- as.integer(as.double(offset_before))
  }

  # Convert offset_after to integer time
  if (is.character(offset_after) || is.numeric(offset_after)) {
    offset_after <- lubridate::as.period(offset_after)
    offset_after <- as.integer(as.double(offset_after))
  }
  if (is.na(offset_before) || is.na(offset_after)) {
    cli_abort(
      c(
        "Invalid offset specified.",
        i = "Try something like {.val 30 minutes}, {.code lubridate::minutes(30)}, or {.code 1800}."
      ),
      call = call
    )
  }
  if (!is.null(offset_before) && offset_before < 0) {
    offset_before <- abs(offset_before)
    cli_warn(
      c(
        "`offset_before` must be a positive period (i.e. greater than 0).",
        i = "Taking the absolute value."
      ),
      call = call
    )
  }
  if (!is.null(offset_after) && offset_after < 0) {
    offset_after <- abs(offset_after)
    cli_warn(
      c(
        "`offset_after` must be a positive period (i.e. greater than 0).",
        i = "Taking the absolute value."
      ),
      call = call
    )
  }

  return(list(offset_before = offset_before, offset_after = offset_after))
}
