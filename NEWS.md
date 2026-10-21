# mpathsenser 2.0.0

mpathsenser 2.0.0 is here, and it is a big one. The package now runs on
[DuckDB](https://duckdb.org), a database built for exactly the kind of data
m-Path Sense produces, instead of dragging everything through SQLite and R.
Imports are faster, need far less memory, and stay quick as a study grows.
Timestamps are exact moments now, with each participant's timezone alongside
them, so days, nights, and travels between timezones finally line up.

It is also a breaking release. Old databases cannot be opened, several
functions are gone, and some columns and arguments changed. The
[Migration guide](https://koenniem.github.io/mpathsenser/articles/migration-guide-2.0.0.html)
takes you from 1.2.4 to 2.0.0 step by step, with before-and-after code
examples. All the changes are below.

## Breaking changes

Read this section first: these changes will stop existing scripts or
databases from working unchanged.

* **DuckDB replaces SQLite.** Databases are DuckDB files now, and an existing
  SQLite database cannot be opened or converted. Your JSON files are still
  the source of truth, so create a fresh database with `create_db()` and
  import them again with `read_mpath_sense()`.
* **`import()` has become `read_mpath_sense()`.** The familiar `path`,
  `sensors`, and `recursive` arguments are still there. `backend` is gone,
  the default `batch_size` is 1000, and the new `deduplicate` and `optimize`
  arguments (both `TRUE`) decide whether the import cleans up duplicates and
  tidies the tables afterwards. Imports no longer run in parallel: DuckDB
  uses the available cores itself, and a database accepts one writer at a
  time.
* **`coverage()` has been split in two.** `coverage_frequency()` answers "how
  many measurements were recorded?", while `coverage_proportional()` answers
  "how does that compare with what was expected?", whether you care about
  counts, covered time, or filled time slots. `coverage_expected()` supplies
  the default expected intervals that used to live in `freq`, and the
  defaults can be overridden per sensor.
* **A long list of helpers is gone, each with a replacement.** `ccopy()` →
  `base::file.copy()`. `fix_jsons()` and `test_jsons()` →
  `jsonlite::validate()`, with `unzip_data()` or `utils::unzip()` for
  archives. `first_date()` and `last_date()` → `get_data()` with
  `summarise()`. `with_localtime()` → `to_local_time()` or
  `collect_local()`. `link_db()` → `link()`. `index_db()` → nothing, because
  DuckDB manages its own statistics. `vacuum_db()` → `optimize_db()`.
  `decrypt_gps()` → decrypt before importing.
* **`link()` no longer guesses how to match rows.** `by = NULL` used to match
  on every column the two data sets shared. Now it pairs every row of `x`
  with every row of `y` before the interval filter, just like
  `by = character()`. Pass the keys yourself, usually
  `by = "participant_id"`, or measurements from different participants can
  be linked to one another. The `split` argument is deprecated and ignored.
* **The database layout changed.** Sensor data now lives in internal tables
  with read-only views in front of them. `measurement_id` and `date` are gone
  from every sensor, participant ids are stored as numbers, and
  `get_processed_files()` no longer carries `study_id`. Identify a
  measurement by `participant_id` and `time`, plus sensor-specific fields
  such as the app in `AppUsage`, and select days with the `start_date` and
  `end_date` arguments of `get_data()`. Every database also records the
  layout version it was created with, and `open_db()` refuses a version it
  does not recognise instead of reading it incorrectly.
* **Timestamps are real date-times now.** `time` is a proper date-time
  (`POSIXct`, in UTC), so sorting, comparing, and doing arithmetic finally
  work as you would expect. Code that compared timestamps as text needs to
  switch to date-time comparisons. When the imported files contain `Timezone`
  measurements, each observation is given the timezone that applied when it
  started, stored in the `timezone` column. Read a `_local` view, or use
  `to_local_time()` or `collect_local()`, to see the participant's clock
  time. Date filters in `get_data()` are strict and half-open: a `Date`
  selects a whole day, a `POSIXt` selects an exact moment, and a malformed
  date is an error instead of a filter that quietly matches nothing. Old
  files from m-Path Sense 6 and earlier, which stored some times as local
  clock values, are now read correctly, so old data gets better times than
  before.
* **`identify_gaps()` returns a lazy table for a connection.** It now
  dispatches on its input: a database connection or lazy table returns a
  lazy table, a data frame returns a tibble. Add `collect()` before using
  the result in R. `sensor = NULL` (the default) now scans every sensor
  rather than only the accelerometer.
* **A few arguments changed shape.** `create_db()` no longer creates
  `sense.db` from a directory on its own, and `open_db()` requires a path.
  Both accept a directory plus a file name, and both take `threads`,
  `memory_limit`, and `temp_directory` to control how DuckDB uses your
  machine. `open_db()` also gained `read_only` for dashboards and other
  readers. `copy_db()` and `get_nrows()` want `sensor = NULL` instead of
  `"All"`, and `copy_db()` requires a file as its target rather than an
  in-memory database. `unzip_data()` now requires `path`. `moving_average()`,
  still experimental, replaced `n`, a number of rows, with `window`, an
  amount of time, so `n = 60` becomes `window = 60` or
  `window = "1 minute"`. The window is centered on each measurement, and
  every measurement inside it counts once.
* **Sensors and columns came and went.** Ten sensors that m-Path Sense no
  longer records are no longer imported: `AirQuality`, `Calendar`,
  `Geofence`, `Gyroscope`, `InstalledApps`, `Keyboard`, `Mobility`, `Noise`,
  `PhoneLog`, and `TextMessage`. Installed apps are still available through
  `installed_apps()`, which now reads them from `AppUsage`. `AppUsage`
  renamed `start` and `end` to `period_start` and `period_end`. `Bluetooth`
  lost `bluetooth_device_type`. `Device` lost `sdk`, and `device_info()` no
  longer returns the raw payload. Garmin's `zeroCrossing` data now uses the
  corrected column name `deadband`.

## Major changes

The good news: this is what you get in return.

* **An example database ships with the package.** `example_db()` opens a
  small, ready-to-use database built from an example capture of 27 zipped
  JSON files from an Android participant. It is the nicest way to try the
  new workflow without touching your own data.
* **New sensors: `BluetoothBeacon` and the Garmin family.** Bluetooth Beacon
  scans are imported alongside the existing Bluetooth data, and m-Path
  Sense's Garmin sensors are supported too: fourteen sensors covering
  accelerometer, actigraphy, heart rate, respiration, skin temperature,
  SpO2, steps, stress, wrist status, and more.
* **Importing is faster and gentler on memory.** Files are read into DuckDB in
  batches instead of being held in R, so a study no longer has to fit in
  memory. A file that fails is isolated and the rest of the batch continues,
  an interrupted import is rolled back cleanly, and `.debug = TRUE` narrates
  what the importer is doing, timing included. Garmin's large nested arrays
  are imported without ballooning memory.
* **Duplicates are resolved by a clear rule.** A file is recognised by its
  name, size, and modification time, so unchanged files are skipped without
  being read and a renamed copy of an old file is imported again. When a
  measurement occurs more than once, the copy from the newest file wins, and
  within that file the last row as written wins. Measurements that occur
  only once are never touched, so a corrected re-upload cannot delete data
  the correction left out. Files are processed oldest to newest, and the
  newest file wins. Run `deduplicate_db()` to apply the same cleanup to a
  whole database, for example after an interrupted import.
* **Housekeeping on demand.** `optimize_db()` reorders each sensor table by
  participant and time, which keeps filtering fast and the database small.
  `read_mpath_sense()` does this automatically for the sensors it imports;
  turn it off with `optimize = FALSE` if you would rather run it later. For
  the British at heart, it also goes by `optimise_db()`.
* **Timezones and local time, done properly.** When your files contain
  `Timezone` measurements, every observation gets the timezone that applied
  at that moment during import. `add_timezones_to_db()` fills in any gaps
  later, without overwriting timezones that are already there, so it is safe
  to run again and again. `to_local_time()`, `collect_local()`, and the
  `_local` and `_with_local` views show the clock time a participant
  actually experienced.
* **Coverage, clearer and faster.** The two coverage functions do their work
  inside the database and stay there until you `collect()`, and `plot()`
  draws the result as a line chart or a heatmap. Expected intervals can
  differ per sensor, calendar units keep their real boundaries, and local
  bins follow the timezone that applied at each observation, including
  daylight-saving folds and travel between timezones. The last bin always
  contains the last observation rather than the end of a trailing expected
  window. For the sensors iOS does not provide (`AppUsage`, `Light`,
  `Memory`, and `Screen`), coverage is `NA` for iOS participants, with a
  warning when the device platform is unknown.
* **Linking and gap-filling got friendlier.** `link()` supports named x-to-y
  keys and custom names for the nested data, so linking sensors with
  different column names is painless. `add_gaps()` handles multiple grouping
  columns and keeps the type of fill columns, date-times included. It also
  accepts lazy tables now: a lazy `data` returns a lazy table, mixed
  local/lazy inputs need `copy = TRUE`, and `by = NULL` joins on the columns
  the two inputs share instead of producing broken output. Both gap
  functions are substantially faster.

## Minor changes

Smaller improvements worth knowing about.

* `sensors` is in alphabetical order.
* `open_db()` now says so when a database is already open for writing
  elsewhere, and lists the database files it found when you point it at a
  directory by mistake.
* `create_db()` and `open_db()` accept `duckdb_home` and `shared_home` for
  machines that keep DuckDB's extension files in a shared location.
* The dependency list got shorter: `furrr`, `sodium`, `progressr`, and
  `vroom` are no longer dependencies, and DuckDB took SQLite's place.
* Databases created by development versions before database versioning open
  with a warning and are treated as the current version. Create a fresh
  database if you want the real thing.

## Bugfixes

* A temporary database connection is closed even when an error occurs.
* `add_timezones_to_db()` no longer trips over the `start` column in
  `AppUsage`.
* `app_category()` skips missing app names and only rate-limits actual
  requests.

# mpathsenser 1.2.4
## Major changes
* `link_db()` is now defunct. Please use `link()` instead.
* Bump minimum R requirement to 4.2.0 as the native pipe `|>` is used in this package.
* Added `add_timezone_to_db()` function to add the timezone "measurements" to each measurement of
sensor data. This allows you to more easily take into account which timezone a participant was in
instead of only relying on UTC.
* Added `to_local_time()` function to easily add the relevant timezone to a timestamp, even if
multiple timezones are present in the data. Note that this does not work in-database.

## Minor changes
* `import()` now supports the new output format for `connectivity` where `connectivity_status` can
be a list of statuses as well.
* `mpathsenser` no longer gives a warning for unimplemented 'mpathinfo' meta data.
* `bin_data()` gained a `.name` argument to specify the name of the column containing the binned
data.
* The use of `progressr::progress()` has been removed from the documentation is this function is
now defunct.

## Bugfixes
* Fixed a bug in `import()` where files could not be read in correctly if they did not contain any
measurement with a `sensorEndTime`.
* Fixed `unzip_data()` not correctly detecting which files not to overwrite if `overwrite = FALSE`
and the zip files have a non-standard name.
* `import()` now correctly reads `study_id` if it contains an underscore `_`.

# mpathsenser 1.2.3
This is a hotfix release to fix a bug in `import()`.

## Minor changes
* Added a contribution statement to the project. The previous contribution statement was actually
a code of conduct which has now been appropriately assigned as such.

## Bugfixes
* Fixed bugs in the import functions of `AppUsage`, `Device`, and `Location` that may have prevented
files from being imported.

# mpathsenser 1.2.2
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
    - `end_time` is the time at which the sample of the data ended, where `time` denotes the start
    time. Note that this timestamp may vary slightly from the `end` field in the data.
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
* The `Noise` table has gained 1 new field:
    - `end_time` is the time at which the sample of the data ended, where `time` denotes the start
    time.
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
* `geocode_rev()` and `app_category()` now return `NA` if the client or API is offline, as per CRAN
guidelines.

## Bugfixes
* Fixed an issue in `fix_jsons()` where files with illegal ASCII characters could be not fixed
because the file was still locked from reading.
* A new case has been added to `fix_jsons()` where JSON files could incorrectly end with `}},`
followed by a closing bracket `]` on a new line. This trailing comma is now removed by
`fix_jsons()`.
* If `recursive = TRUE` in `unzip_data()` and `to = NULL`, the output path of the JSON files will
be the local directories through which the recursive path is traversed rather than the main
directory.
* Replaced double quotation marks with single quotation marks in the description, per CRAN
guidelines.

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
