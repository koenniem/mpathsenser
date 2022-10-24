.onLoad <- function(libname, pkgname) {

  # Set progress default
  op <- options()
  op.mpathsenser <- list(
    mpathsenser.show_progress = TRUE
  )
  toset <- !(names(op.mpathsenser) %in% names(op))
  if (any(toset)) options(op.mpathsenser[toset])

  rlang::run_on_load()

  invisible()
}

.onAttach <- function(libname, pkgname) {
  # Empty for now...
}

.onDetach <- function(libpath) {
  # Empty for now...
}
