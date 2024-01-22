#' @description
#' Overcomes one of the major challenges in mobile (passive) sensing, namely
#' being able to pre-process the raw data that comes from a mobile sensing app,
#' specifically "m-Path Sense" <https://m-path.io>. The main task of 'mpathsenser' is
#' therefore to read "m-Path Sense" JSON files into a database and provide several
#' convenience functions to aid in data processing.
#' @keywords internal
#' @importFrom rlang .data
## usethis namespace: start
#' @importFrom DBI dbConnect
#' @importFrom DBI dbDisconnect
#' @importFrom DBI dbExecute
#' @importFrom DBI dbIsValid
#' @importFrom dbplyr window_order
#' @importFrom dplyr across
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr collect
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr lag
#' @importFrom dplyr lead
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom lifecycle deprecated
#' @importFrom purrr map
#' @importFrom rlang .env
#' @importFrom rlang :=
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom rlang warn
#' @importFrom tidyr complete
#' @importFrom tidyr drop_na
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
## usethis namespace: end
"_PACKAGE"

rlang::on_load(rlang::local_use_cli())
