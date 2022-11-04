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
