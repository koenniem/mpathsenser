# nocov start
.onLoad <- function(libname, pkgname) {
  # Set options for mpathsenser
  op <- options()
  op_mpathsenser <- list(
    mpathsenser.show_progress = TRUE,
    mpathsenser.check_missing_sensors = TRUE
  )
  toset <- !(names(op_mpathsenser) %in% names(op))
  if (any(toset)) {
    options(op_mpathsenser[toset])
  }

  rlang::run_on_load()

  # to_local_time() also works inside a lazy dbplyr query, with the same
  # R-side argument checks. dbplyr passes unknown functions through by name
  # without calling them, so register the exported function as the DuckDB
  # translation. The method wraps duckdb's own translation, leaving every other
  # translation untouched. Registering programmatically (rather than through
  # NAMESPACE) avoids a load-time "S3 method overwritten" message.
  registerS3method(
    "sql_translation",
    "duckdb_connection",
    sql_translation.duckdb_connection,
    envir = asNamespace("dbplyr")
  )

  invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
  # Empty for now...
}

.onDetach <- function(libpath) {
  # Empty for now...
}
# nocov end
