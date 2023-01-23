#' Purge redundant rows that are preceded and/or succeeded by rows with the same values
#'
#' @description `r lifecycle::badge("stable")` In a time series, especially those stemming from
#'   sensor data, it may occur that many subsequent values are the same. When calculating the
#'   duration of these events (e.g. [activity_duration()]), these subsequent values carry no value
#'   when summing to the total of one type of event. To this end, this function is useful for
#'   cleaning time series data by removing rows that contain consecutive identical values in a
#'   selection of columns. It allows you to specify whether to check for identical values preceding
#'   each row, succeeding each row, or both.
#'
#' @details Normally, comparing anything to `NA` results in `NA`. However, this function deviates
#' from this base `R` convention so that, for example, comparing `1` to `NA` is indeed not equal.
#'
#' @param .data A data frame containing the data to be compressed
#' @param .cols A selection of columns in which to check for consecutive identical values. If none
#'   are specified, all columns will be used.
#' @param .before A logical value indicating whether to check for identical values preceding each
#'   row. Defaults to TRUE.
#' @param .after A logical value indicating whether to check for identical values succeeding each
#'   row. Defaults to TRUE.
#'
#' @return A data frame with rows containing consecutive identical values in the specified columns
#'   removed.
#'
#' @examples
#' # Define some data
#' # The duration of each event is 1 day. However, we might as well
#' # remove the middle event c at 2022-01-05, as this will make event c
#' # at 2022-01-04 last 2 days. The total duration for c will not change
#' # while we can remove the computation burden.
#' data <- data.frame(time = c("2022-01-01", "2022-01-02", "2022-01-03",
#'                            "2022-01-04", "2022-01-05", "2022-01-06"),
#'                    x = c("a", "b", "c", "c", "c", "d"))
#' purge_redundant(data, c(x, y))
purge_redundant <- function(.data, .cols = NULL, .before = TRUE, .after = TRUE) {
if (!.before && !.after) {
    return(.data)
  }

  # Use all columns if none are specified
  # Do not rely on `if_any` default `.cols` value as it will be omitted in a future version
  if (missing(.cols)) {
    .cols <- rlang::expr(all_of(!!colnames(.data)))
  }

  # Only keep rows where at least one columns differs from the previous and/or next value
  .data |>
    filter(if_any(.cols = {{ .cols }},
                  .fns = \(col) !is_surrounded(col, .before, .after)))
}

# Whether a value is "unique" in a time series, i.e. not preceded and/or succeeded by the same
# value
# Also, always keep the first and/or last value, as these will have NAs in the preceding and/or
# successive rows respectively
is_surrounded <- function(.vec, .before, .after) {
  # By default, there are no differences
  is_first <- is_last <- diff_lag <- diff_lead <- logical(length(.vec))

  # Whether the lag is different
  if (.before) {
    is_first[1] <- TRUE
    diff_lag <- !equals(.vec, lag(.vec))
  }

  # Whether the lead is different
  if (.after) {
    is_last[length(.vec)] <- TRUE
    diff_lead <- !equals(.vec, lead(.vec))
  }

  # Return TRUE if it is different or if it is the first or last row (only if
  # .before or .after is TRUE)
  res <- is_first | is_last | diff_lead | diff_lag
  return(!res)
}

# Methods to test for equality, including `NA`.
# Unlike base R, this function produce `FALSE` when comparing values to NA.
equals <- function(x, y, ...) {
  if (is.null(x) || is.null(y)) {
    return(FALSE)
  }

  UseMethod("equals", object = x)
}

equals.default <- function(x, y) {
  same <- (x == y) | (is.na(x) & is.na(y))
  same[is.na(same)] <- FALSE
  return(same)
}

equals.numeric <- function(x, y, tolerance = sqrt(.Machine$double.eps)) {
  same <- (abs(x - y) < tolerance) | (is.na(x) & is.na(y))
  same[is.na(same)] <- FALSE
  return(same)
}

equals.list <- function(x, y) {
  res <- mapply(identical, x, y)
  names_res <- mapply(equals, names(x), names(y))
  res & names_res
}

