test_that("lint_free", {
  skip_on_cran()
  skip_if_not_installed("lintr")

  pkgname <- "mpathsenser"
  lint_path <- path.expand(normalizePath(getwd(), winslash = "/"))

  find_root <- function(path, pkgname) {
    if ("DESCRIPTION" %in% dir(path)) {
      return(path.expand(normalizePath(path, winslash = "/")))
    } else if ("00_pkg_src" %in% dir(path)) {
      path <- file.path(path, "00_pkg_src", pkgname)
      return(path.expand(normalizePath(path, winslash = "/")))
    } else {
      find_root(dirname(path), pkgname)
    }
  }

  lint_path <- find_root(lint_path, pkgname)

  lintr::expect_lint_free(
    filename = lint_path,
    linters = list(
      line_length_linter = lintr::line_length_linter(100),
      cyclocomp_linter = NULL
    ))
})
