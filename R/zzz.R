# nocov start
.onLoad <- function(libname, pkgname) {
  # Set progress default
  op <- options()
  op_mpathsenser <- list(
    mpathsenser.show_progress = TRUE
  )
  toset <- !(names(op_mpathsenser) %in% names(op))
  if (any(toset)) {
    options(op_mpathsenser[toset])
  }

  # blue code cli formatting class
  cli::cli_div(
    theme = list(
      span.bcode = list(color = "blue"),
      "span.bcode" = list(before = "`"),
      "span.bcode" = list(after = "`")
    )
  )

  rlang::run_on_load()

  invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
  # Empty for now...
}

.onDetach <- function(libpath) {
  # Empty for now...
}
# nocov end
