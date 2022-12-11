# mpathsenser 1.1.2
## Major changes
* `link()` gained 3 new arguments:
    - `time`: The name of the column containing the timestamps in `x`.
    - `end_time`: Optionally, the name of the column containing the end time in `x`. 
    - `y_time`: The name of the column containing the timestamps in `y`.
    - `name`: The name of the nested `y` data, defaulting to `"data"`.      
  Using `end_time`, it is now possible to specify custom time intervals instead of only fixed 
intervals through `offset_before` or `offset_after`. Note that these two functionality cannot
be specified at the same time.
* `time` and `y_time` in `link()` must now be explicitly named, though for the time being default
to 'time' with a warning.
* Added `continue` argument to `add_gaps()` that controls whether the last measurement(s) should be 
continued after a gap.
* `link_db()` is now soft deprecated as it provides only marginal added functionality compared to 
`link()`.
* `decrypt_gps()` now takes a vector of encrypted GPS coordinates instead of a whole data frame 
with fixed variables names (`latitude` and `longitude`). This allows more flexibility in its use. 
Also, parallelisation has been added similar to other functions in this package (i.e. by setting
a [future plan](https://rdrr.io/cran/future/), e.g.`future::plan("multisession")`).

## Deprecations
The following functions are now made defunctional and internal:

* `activity_duration()`
* `app_usage()`
* `n_screen_on()`
* `n_screen_unlocks()`
* `screen_duration()`, 
* `step_count()` 

These functions delivered incorrect output and only allowed summaries by a fixed time frame, e.g.
by hour or day. These functions will be reimplemented (some with a different name) in mpathsenser 
2.0.0.

## Minor changes
* When `add_before` or `add_after` is `TRUE` in `link()`, no extra row is added if there already is 
a row with a timestamp exactly equal to the start of the interval (for `add_before = TRUE`) or to 
the end of the interval ⁠`(add_after = TRUE)`.
* `moving_average()` now allows a lazy tibble to allow further computations in-database after 
having called `moving_average()`.
* `identify_gaps()` is now slightly more efficient.
* `get_data()` is now case insensitive. In a future update, all sensor names throughout all 
functions will be made case insensitive.
* When using `add_before = TRUE`, `link()` no longer adds an extra measurement if the first 
measurement in the interval equals the start time of the interval exactly. 
* `get_data()` now allows multiple `participant_id`s to be used.
* `external_time` has been added as an argument to `link_db()`, to be able to specify the time 
column in `external_data` in accordance with the change in `link()` above. 

## Bugfixes
* `link()` now correctly handles natural joins (when `by = NULL`) and cross joins (when 
`by = character()`).
* The column `original_time` was not added for any other nested data row except the first one,
if `add_before` or `add_after` was true.
* `link()` no longer suffers from `future`'s max object restriction (500MB by default).
* When `x` and `y` use different time zones in `link()` and `add_before = TRUE`, `link()` now 
correctly leaves all time zones equal to the input. 
* `link()` incorrectly assigned the time zone of `x` to the nested data of `y`, if `add_before` or
`add_after` was true. This is now changed to the time zone of `y`, to ensure consistency. Note that
if the time zones of `x` and `y` are different, matching will be correct but the nested data may
seem off as it will keep `y`'s input time zone. 

# mpathsenser 1.1.1
## Major changes
* `identify_gaps()` now allows multiple sensors to be used. This is particularly useful when there 
are no sensors with high frequency sampling (like accelerometer and gyroscope) or to ensure there 
can be no measurements within the gaps from any sensor.
* Changed the arguments names of `copy_db()` `from_db` and `to_db` to `source_db` and `target_db`
respectively.
* Set `activity_duration()`, `screen_duration()`, `n_screen_on()`, `n_screen_unlocks()`, and 
`step_count()` to internal until it is clear how these functions should behave and, more 
importantly, what their output should be.
* Reworked `moving_average()` to work correctly on multiple participants.

## Deprecations
* Deprecated functionality for on-the-fly database creation in several functions. This disentangles 
the functionalities of `create_db()` and the other functions, where the latter implicitly depended 
on the former. The following arguments are thereby rendered disabled:
  - `dbname` and `overwrite_db` arguments in `import()`
  - `path` and `db_name` in `copy_db()`
* Deprecated the `parallel` argument in several functions. If you wish to process in parallel, you
must now specify this beforehand using a [future plan](https://rdrr.io/cran/future/), e.g.
`future::plan("multisession")`. As a consequence, the package `future` is no longer a dependency 
(but `furrr` is).
* Deprecated the `plot` argument in `coverage()`. To plot a coverage chart, you can now use the 
default `plot()` function with the output from `coverage()`.

## Minor changes
* All functions gained basic argument checking, ensuring that input arguments have at least the 
proper type.
* The package now provides more nicely formatted errors, warnings, and messages through 
[`rlang::abort`, `rlang::warn`, and `rlang::inform`](https://rlang.r-lib.org/reference/abort.html).
* Partially rewrote `import()` to be more manageable in code. As a consequence, the dependency on 
`rjson` and `dbx` can be dropped in favour of `jsonlite` and native SQL. 
* Added `lifecycle` as a dependency for deprecating arguments.
* Added a warning section in `identify_gaps()` and friends to inform the user of a possible 
inconsistency when identifying gaps. 
* Switched `identify_gaps()` from using the lag of each measurements towards using the lead. This
makes no difference in the output but is a little easier to read.

## Bug fixes
* Fixed a note when first running `link()` or `link_gaps()` in a session, stating that using 
external vectors `dplyr::select()` is ambiguous.
* `bin_data()` now correctly includes measurements in bins that do not have a stop time. This was in 
particular a problem with the last measurement of a series.
* Fixed a non-working example in `bin_data()`.
* Fixed a bug in `add_gaps()` where multiple gaps in succession (i.e. without other data in between)
were incorrectly handled.
* Fixed `app_category()` not  being able to find the exact app name in the search results, thereby 
defaulting to the `n`th result (default 1).

# mpathsenser 1.1.0
## Major changes
* Added several functions:
  - `link_gaps()`: For linking gap data to other data, i.e. how many gaps occur within an interval.
  - `add_gaps()`: To interleave gaps with other data.
  - `bin_data()`: To subdivide data into bins, e.g. all measurements within an hour or day.
* Added lifecycle badge to signal the state of functions.
* `link()` has been revised and expanded:
  - Replaced `offset` with `offset_before` and `offset_after`, allowing both to be specified at the 
  same time (#3).
  - Added new `add_before` and `add_after` argument to allow the last row before the measurement 
  and first row after the measurement respectively to be added to the data.
  - Added a new `split` argument, allowing computation to be split among many parts thereby
  lowering computational burden.
* `app_category()` is now case insensitive and gained the new argument `exact` to be able to match the
package name exactly based on a partial match.
* Added a (start of a) vignette to further highlight the package use.

## Minor changes
* Changed `get_activity()` to `activity_duration()`.
* Changed `link2()` to `link_db()`.

## Bug fixes
* Fixed issue where `link()` runs out of memory when there are too many matches (#2). `link()` is 
now much more memory efficient and slightly faster.
* Fixed issue in `get_data()` which allowed multiple sensors to be requested from one function call, 
sometimes leading to crashes (#4).
* Fixed issue in `link()` where column `original_time` is missing if no records before or after the 
interval are found (#6).
* Fixed a bug in `import()` where sensor data not present in first file of the batch are dropped 
for the other files well.
* Fixed `app_category()` to work with the updated Google Play website.

# mpathsenser 1.0.3
* Fix final tests not yet using TMPDIR

# mpathsenser 1.0.2
* Changed some unit to use TMPDIR

# mpathsenser 1.0.1
* Fixed floating point differences when testing on MacOS

# mpathsenser 1.0.0
* Initial release on CRAN
* Changed the name to mpathsenser
