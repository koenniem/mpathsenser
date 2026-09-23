#' @description
#' Overcomes one of the major challenges in mobile (passive) sensing, namely
#' being able to pre-process the raw data that comes from a mobile sensing app,
#' specifically "m-Path Sense" <https://m-path.io>. The main task of 'mpathsenser' is
#' therefore to read "m-Path Sense" JSON files into a database and provide several
#' convenience functions to aid in data processing.
#' @keywords internal
#' @importFrom rlang .data
## usethis namespace: start
#' @importFrom cli cli_abort
#' @importFrom cli cli_inform
#' @importFrom cli cli_progress_bar
#' @importFrom cli cli_progress_done
#' @importFrom cli cli_progress_update
#' @importFrom cli cli_warn
#' @importFrom DBI dbConnect
#' @importFrom DBI dbDisconnect
#' @importFrom DBI dbExecute
#' @importFrom DBI dbExistsTable
#' @importFrom DBI dbGetQuery
#' @importFrom DBI dbIsValid
#' @importFrom DBI dbQuoteIdentifier
#' @importFrom DBI dbReadTable
#' @importFrom DBI dbRollback
#' @importFrom DBI dbWriteTable
#' @importFrom DBI Id
#' @importFrom dbplyr sql
#' @importFrom dbplyr window_order
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr any_of
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr collect
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr if_else
#' @importFrom dplyr lag
#' @importFrom dplyr lead
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr row_number
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr summarise
#' @importFrom dplyr tbl
#' @importFrom dplyr ungroup
#' @importFrom duckdb duckdb
#' @importFrom lifecycle deprecated
#' @importFrom lubridate as_datetime
#' @importFrom lubridate tz
#' @importFrom purrr map
#' @importFrom rlang .env
#' @importFrom rlang :=
#' @importFrom rlang %||%
#' @importFrom rlang caller_arg
#' @importFrom rlang caller_env
#' @importFrom rlang ensym
#' @importFrom rlang is_null
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom tidyr complete
#' @importFrom tidyr drop_na
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
## usethis namespace: end
"_PACKAGE"

rlang::on_load(rlang::local_use_cli())
