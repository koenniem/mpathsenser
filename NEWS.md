# mpathsenser 1.2.0

## Major changes
`mpathsenser` now supports the new data format as of m-Path Sense 4.2.6. This comes with a large 
number of changes. Most importantly, this means that `import()` had to be updated to handle the new
data format. Both the old and new data format are now supported by this package. With the new data
format there are some changes to the database.

First, some fields have been removed:

* The `x`, `y`, and `z` fields from `Accelerometer` have been removed from `import()` and all subsequent
functions. These fields were only used when m-Path Sense still collected continuous data, and for 
some time now only summary data is collected. No continuous data has ever been collected outside of
pilot testing, and hence these fields have been removed.
* The `x_mean`, `y_mean`, `z_mean`, `x_mean_sq`, `y_mean_sq`, `z_mean_sq`, and `n` fields from
`Gyroscope` have been removed as m-Path Sense will currently collect continuous data. These fields
were implemented in anticipation of this change but instead, for now, gyroscopic information has 
been removed from the app altogether. Thus, these fields are removed from simplicity and clarity.
* The `timezone` field has been removed from all sensor tables. This field was once added in 
m-Path Sense but this never made it to the final version. It has been removed from the database
and all subsequent functions.

Second, some fields have been added:

* The `Accelerometer` has gained many new data fields:
    - `end_time` is the time at which the sample of the data ended, where `time` denotes the start 
    time.
    - `n`, the number of samples, was already present but has been moved in the ordering of the 
    fields.
    - `x_mean`, `y_mean`, and `z_mean` are the mean values of the accelerometer data. These were 
    already present in the data and remain unchanged.
    - `x_median`, `y_median`, and `z_median` are the median values of the accelerometer data.
    - `x_std`, `y_std`, and `z_std` are the standard deviations of the accelerometer data.
    - `x_aad`, `y_aad`, and `z_aad` are the average absolute deviations of the accelerometer data.
    - `x_min`, `y_min`, and `z_min` are the minimum values of the accelerometer data.
    - `x_max`, `y_max`, and `z_max` are the maximum values of the accelerometer data.
    - `x_max_min_diff`, `y_max_min_diff`, and `z_max_min_diff` are the differences between the
    maximum and minimum values of the accelerometer data.
    - `x_mad`, `y_mad`, and `z_mad` are the median absolute deviations of the accelerometer data.
    - `x_iqr`, `y_iqr`, and `z_iqr` are the interquartile ranges of the accelerometer data.
    - `x_neg_n`, `y_neg_n`, and `z_neg_n` are the number of negative values of the accelerometer 
    data.
    - `x_pos_n`, `y_pos_n`, and `z_pos_n` are the number of positive values of the accelerometer 
    data.
    - `x_above_mean`, `y_above_mean`, and `z_above_mean` are the number of values above the mean of
    the accelerometer data.
    - `x_energy`, `y_energy`, and `z_energy` are similar to `x_mean_sq`, `y_mean_sq`, and
    `z_mean_sq`, being the average sum of squares.
    - `avg_res_acc` is the average resultant acceleration, being average of the square roots of the 
    values in each of the three axis squared and added together.
    - `sma` is the signal magnitude area, being the sum of absolute values of the three axis 
    averaged over a window.
* The `AppUsage` table has gained 2 new fields:
    - `package_name` is the full application package name.
    - `last_foreground` is the time at which the application was last in the foreground. If the app
    had not yet been in the foreground, this is `NA`.
* The `Bluetooth` table has gained 2 new fields:
    - `start_scan` is the time at which the scan started.
    - `end_scan` is the time at which the scan ended.
* The `Device` table has gained 2 new fields:
    - `operating_system_version` is the version of the operating system.
    - `sdk` is the version of the Android SDK or the iOS kernel.
* A new sensor `Heartbeat` has been added to the data. This table has the following fields:
    - `measurement_id`, `participant_id`, `date`, and `time` like every other sensor.
    - `period` denotes the time period over which the a heartbeat should be registered, in minutes.
    - `device_type` denotes the type of device of this heartbeat.
    - `device_role_name` is the role name of the device in the protocol.
* The `Light` table has gained 1 new field:
    - `end_time` is the time at which the sample of the data ended, where `time` denotes the start
    time.
* `Location` has gained 3 new fields:
    - `vertical_accuracy` is the estimated vertical accuracy of this location, in meters.
    - `heading_accuracy` is the estimated bearing accuracy of this location, in degrees. Only 
    available on Android. 
    - `is_mock` is a boolean indicating whether this location was mocked or not. Always `FALSE` on
    iOS. Moreover, because SQLite does not support booleans, this is stored as an integer.
* `Timezone` has been added a separate sensor. This table has the following fields:
    - `measurement_id`, `participant_id`, `date`, and `time` like every other sensor.
    - `timezone` is the time zone of the device at the time of the measurement.
    
Data collected with previous version of m-Path Sense (henceforth referred to as legacy data) can 
still be read by `import()` and subsequent functions, but all new fields will have missing values.

## Minor changes
* `mpathsenser::sensors` now holds 27 sensors, being updated with `Heartbeat` and `Timezone`
* Added the correct citation for this package.
* Coverage plots with absolute numbers `coverage(relative = FALSE)` now show correct colours. The 
colours are now based on the relative values within each sensor, such that the highest sample is
fully red and zero being fully blue. 
* `vacuum_db()` is a newly exported function within this package. Once called upon a database, it
shrinks the database to its minimal size by cleaning up remnants from `import()`.
* The `maggrittr` package has been dropped as a dependency, favouring `R`'s native pipe `|>` over 
the `maggrittr` pipe `%>%`.
* Added a vignette 'Data overview' to clarify which fields are available in the database and what 
they mean.
* Added a `format` argument to `geocode_rev()` to allow for different output formats from 
Nominatim's API.
* `geocode_rev()` now returns `NA` if the client or API is offline, as per CRAN guidelines.

## Deprecations


## Bugfixes
* Fixed an issue in `fix_jsons()` where files with illegal ASCII characters could be not fixed 
because the file was still locked from reading.
* A new case has been added to `fix_jsons()` where JSON files could incorrectly end with `}},` 
followed by a closing bracket `]` on a new line. This trailing comma is now removed by 
`fix_jsons()`.
* If `recursive = TRUE` in `unzip_data()` and `to = NULL`, the output path of the JSON files will 
be the local directories through which the recursive path is traversed rather than the main 
directory.

# mpathsenser 1.1.3
This is a release with breaking changes due to removal of deprecated arguments. Please review 
carefully before updating.

This release also supports changes from the new release of m-Path Sense (01/02/2023). Most notably,
the accelerometer and gyroscope are no longer samples of a continuous stream, but rather summaries
of these streams. Old versions are still supported by all functions.

## Major changes
* Thanks to a new version of m-Path Sense, accelerometer and gyroscope have gained extra columns:
    - `x_mean`: The average acceleration or gyroscopic value along the `x` axis within a sample;
    - `y_mean`: The average acceleration or gyroscopic value along the `y` axis within a sample;
    - `z_mean`: The average acceleration or gyroscopic value along the `z` axis within a sample;
    - `x_mean_sq`: The mean of the squared `x` values within the sample;
    - `y_mean_sq`: The mean of the squared `y` values within the sample;
    - `z_mean_sq`: The mean of the squared `z` values within the sample;
  From these values, one could calculate the `L1 norm` and `L2 norm` like before.
* Added a new value `timezone` to all sensor data. Confusingly, this is _not_ the timezone of the 
data itself (as this is always in UTC), but rather the timezone the participant was in at the time
of the measurement.

## Deprecations
* Removed deprecated `parallel` argument in `fix_jsons()`, `test_jsons()`, `unzip_data()`, and
`import()`.
* Removed deprecated `overwrite_db` and `dbname` arguments from `import()`.
* Removed deprecated  `path` and `db_name` arguments from `copy_db()`.

## Minor changes
* Provided support for dplyr 1.1.0.
* `link()` no longer adds an extra row before (if `add_before = TRUE`) or after (if 
`add_after = TRUE`) if the first or last measurement equals the start or end time respectively.
* Changed `link_db()` lifecycle status to deprecated as `link_db()` depends on `link()`. Eventually,
`link()` might see changes in its functionality that will cause `link_db()` to break, so it is 
better to deprecate it already to motivate users to stop using this function.

## Bugfixes
* Fix cross-reference to undeclared package ‘future’ in documentation.
* Fixed bug #8 where `bin_data()` incorrectly handled days occurring after DST change.

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
