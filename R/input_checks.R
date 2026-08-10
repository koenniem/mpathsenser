# Function for testing if a package in 'suggested' is installed, before running it. This function
# needs to be at the top of the file to make sure it is skipped when calculating coverage.
ensure_suggested_package <- function(name, call = rlang::caller_env()) {
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
  arg = rlang::caller_arg(db),
  call = rlang::caller_env()
) {
  rlang::check_required(db, arg = arg, call = call)

  if (allow_null && rlang::is_null(db)) {
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
      x = "You provded an SQLite database,",
      x = "Support for SQLite was dropped in {.pkg mpathsenser} 1.2.4 in favour of duckdb.",
      i = "Please import your data to a new database using the latest version of {.pkg mpathsenser}.",
      i = "Or use an older version of {.pkg mpathsenser}."
    )
    cli::cli_abort(msg, arg = arg, call = call)
  }

  if (!dbIsValid(db)) {
    msg <- c(
      paste0("Database connection {.arg ", arg, "} is not valid."),
      i = "Did you forget to open the connection or save it to a variable?"
    )
    cli_abort(msg, arg = arg, call = call)
  }

  return(invisible(TRUE))
}

check_arg <- function(
  x,
  type,
  n = NULL,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  rlang::check_required(x, arg = arg, call = call)

  if (allow_null && rlang::is_null(x)) {
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
      "data.frame",
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
      data.frame = is.data.frame(x),
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

    msg <- c(
      paste0("Argument {.arg ", arg, "} must be ", with_article(type), n, "."),
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
  article <- lapply(x, function(y) {
    if (any(grepl("^[aeiouAEIOU]", y))) {
      return("an")
    } else {
      return("a")
    }
  })
  article <- unlist(article)
  paste(article, x, collapse = " or ")
}

check_sensors <- function(
  x,
  n = NULL,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_arg(x, type = "character", allow_null = allow_null, n = n, arg = arg, call = call)
  missing <- x[!(tolower(x) %in% tolower(sensors))]

  if (length(missing) > 0) {
    msg <- c(
      "Sensor{?s} {.arg {missing}} could not be found.",
      i = "See {.code mpathsenser::sensors} for the full list of available sensors."
    )
    cli_abort(msg, arg = arg, call = call)
  }

  return(invisible(TRUE))
}

check_offset <- function(offset_before, offset_after, call = rlang::caller_env()) {
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
