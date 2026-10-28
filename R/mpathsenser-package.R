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
#' @importFrom DBI dbWithTransaction
#' @importFrom DBI Id
#' @importFrom dbplyr sql
#' @importFrom dbplyr window_order
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr any_of
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr coalesce
#' @importFrom dplyr collect
#' @importFrom dplyr cross_join
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr if_else
#' @importFrom dplyr inner_join
#' @importFrom dplyr join_by
#' @importFrom dplyr lag
#' @importFrom dplyr lead
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr n_distinct
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr row_number
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr summarise
#' @importFrom dplyr tbl
#' @importFrom dplyr transmute
#' @importFrom dplyr ungroup
#' @importFrom dplyr union_all
#' @importFrom duckdb duckdb
#' @importFrom lifecycle deprecated
#' @importFrom lubridate as_datetime
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate floor_date
#' @importFrom lubridate hour
#' @importFrom lubridate mday
#' @importFrom lubridate minute
#' @importFrom lubridate month
#' @importFrom lubridate second
#' @importFrom lubridate tz
#' @importFrom lubridate wday
#' @importFrom lubridate yday
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom purrr map_lgl
#' @importFrom purrr reduce
#' @importFrom rlang .env
#' @importFrom rlang :=
#' @importFrom rlang %||%
#' @importFrom rlang caller_arg
#' @importFrom rlang caller_env
#' @importFrom rlang enexpr
#' @importFrom rlang ensym
#' @importFrom rlang expr
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
