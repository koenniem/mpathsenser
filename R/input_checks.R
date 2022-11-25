# Function for testing if a package in 'suggested' is installed, before running it. This function
# needs to be at the top of the file to make sure it is skipped when calculating coverage.
ensure_suggested_package <- function(name, call = rlang::caller_env()) {
  if (!requireNamespace(name, quietly = TRUE)) {
    abort(c(
      paste0("Package `", name, "` is needed for this function to work."),
      i = paste0("Please install it using `install.packages(\"", name, "\")`")
    ), call = call)
  }
}

check_db <- function(db,
                     allow_null = FALSE,
                     arg = rlang::caller_arg(db),
                     call = rlang::caller_env()) {

  rlang::check_required(db, arg = arg, call = call)

  if (allow_null && rlang::is_null(db)) {
    return(invisible(TRUE))
  }

  if (is.null(db)) {
    msg <- paste0("Database connection `", arg, "` must not be NULL.")
    abort(msg, arg = arg, call = call)
  }

  if (!inherits(db, "DBIConnection")) {
    msg <- c(paste0("Argument `", arg, "` is not a database connection."),
      x = paste0("You supplied ", with_article(utils::tail(class(db), 1)), ".")
    )
    abort(msg, arg = arg, call = call)
  }

  if (!dbIsValid(db)) {
    msg <- c(paste0("Database connection `", arg, "` is not valid."),
      i = "Did you forget to open the connection or save it to a variable?"
    )
    abort(msg, arg = arg, call = call)
  }

  return(invisible(TRUE))
}

check_arg <- function(x,
                      type,
                      n = NULL,
                      allow_null = FALSE,
                      arg = rlang::caller_arg(x),
                      call = rlang::caller_env()) {

  rlang::check_required(x, arg = arg, call = call)

  if (allow_null && rlang::is_null(x)) {
    return(invisible(TRUE))
  }

  type <- match.arg(type,
    c(
      "character", "integer", "double", "logical", "integerish", "numeric",
      "factor", "POSIXt", "data.frame", "list"
    ),
    several.ok = TRUE
  )

  res <- lapply(type, function(y) {
    switch(y,
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

    msg <- c(paste0("Argument `", arg, "` must be ", with_article(type), n, "."),
      x = paste0(
        "You supplied ", with_article(utils::tail(class(x), 1)),
        n_provided, "."
      )
    )
    abort(msg, arg = arg, call = call)
  }

  return(invisible(TRUE))
}

with_article <- function(x) {
  article <- lapply(x, function(y) {
    if (any(substr(y, 1, 1) == c("a", "e", "h", "i", "o", "u"))) {
      return("an")
    } else {
      return("a")
    }
  })
  article <- unlist(article)
  paste(article, x, collapse = " or ")
}

check_sensors <- function(x,
                          n = NULL,
                          allow_null = FALSE,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  check_arg(x, type = "character", allow_null = allow_null, n = n, arg = arg, call = call)
  missing <- x[!(tolower(x) %in% tolower(sensors))]

  if (length(missing) > 0) {
    msg <- c(
      paste0("Sensor(s) ", paste0("\"", missing, "\"", collapse = ", "), " could not be found."),
      i = "See `mpathsenser::sensors` for the full list of available sensors."
    )
    abort(msg, arg = arg, call = call)
  }

  return(invisible(TRUE))
}

check_offset <- function(offset_before, offset_after, call = rlang::caller_env()) {
  if ((is.null(offset_before) || all(offset_before == 0)) &&
      (is.null(offset_after) || all(offset_after == 0))) {
    return(abort("`offset_before` and `offset_after` cannot be 0 or NULL at the same time.",
          call = call))
  }
  if (!is.null(offset_before) && !(is.character(offset_before) ||
                                   lubridate::is.period(offset_before) ||
                                   is.numeric(offset_before))) {
    return(abort("`offset_before` must be a character vector, numeric vector, or a period.",
          call = call))
  }
  if (!is.null(offset_after) && !(is.character(offset_after) ||
                                  lubridate::is.period(offset_after) ||
                                  is.numeric(offset_after))) {
    return(abort("`offset_after` must be a character vector, numeric vector, or a period.",
          call = call))
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
    return(abort(c(
      "Invalid offset specified.",
      i = "Try something like '30 minutes', lubridate::minutes(30), or 1800."
    ), call = call))
  }
  if (!is.null(offset_before) && offset_before < 0) {
    offset_before <- abs(offset_before)
    warn(c(
      "`offset_before` must be a positive period (i.e. greater than 0).",
      i = "Taking the absolute value."
    ), call = call)
  }
  if (!is.null(offset_after) && offset_after < 0) {
    offset_after <- abs(offset_after)
    warn(c(
      "`offset_after` must be a positive period (i.e. greater than 0).",
      i = "Taking the absolute value."
    ), call = call)
  }

  return(list(offset_before = offset_before, offset_after = offset_after))
}
