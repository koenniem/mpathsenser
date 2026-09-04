
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mpathsenser <a href='https://koenniem.github.io/mpathsenser/index.html'><img src='logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/mpathsenser)](https://cran.r-project.org/package=mpathsenser)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/koenniem/mpathsenser/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/koenniem/mpathsenser/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/koenniem/mpathsenser/graph/badge.svg)](https://app.codecov.io/gh/koenniem/mpathsenser)
<!-- badges: end -->

`mpathsenser` reads the JSON files exported by the [m-Path
Sense](https://m-path.io) mobile sensing app into a
[DuckDB](https://duckdb.org) database, and provides a set of convenience
functions to inspect, process, and analyse the resulting data.

## Installation

You can install the latest release from CRAN:

``` r
install.packages("mpathsenser")
```

Or the development version from GitHub:

``` r
remotes::install_github("koenniem/mpathsenser")
```

## Importing m-Path Sense data

Importing data always follows the same three steps: point to the folder
with the JSON files, create a database, and read the files into it. This
package ships with a small example dataset (two participants, three days
of data) that mimics what m-Path Sense exports, which we use throughout
this README. Your own data works exactly the same: simply set `path` to
the folder that contains the files exported by the app.

``` r
# The folder with the JSON files exported by m-Path Sense
path <- system.file("extdata", "example", package = "mpathsenser")

# Create a new database (an in-memory database is fine for experiments)
db <- create_db(NULL, file.path(tempdir(), "study.db"))

# Import the data
read_mpath_sense(path = path, db = db)
```

`read_mpath_sense()` returns a message once all files were written to
the database, or the names of the files that could not be imported.
Files are imported in batches (`batch_size`, default 100 files at a
time) within transactions, so a file that fails to import does not
affect the others. If a file fails even on its own, it is reported and
the rest of the batch is imported normally.

Re-running `read_mpath_sense()` on the same folder skips the files that
were already imported, and only processes the new ones. Files are
tracked in the `ProcessedFiles` table by their name, size, and
modification time, so corrected files that were re-uploaded (same name,
different content) are imported again. Duplicate measurements are then
removed automatically, keeping the row of the newest file per
measurement — see the [Get started
vignette](https://koenniem.github.io/mpathsenser/articles/mpathsenser.html)
for details.

## Inspecting the database

The database holds the sensor data in separate tables (one per sensor)
together with metadata about the study, participants, and processed
files.

``` r
get_participants(db)
#>   participant_id       study_id
#> 1          67890 cravings_study
#> 2          12345 cravings_study
```

``` r
# Number of rows per sensor table
get_nrows(db)
#>         Accelerometer              Activity              AppUsage               Battery             Bluetooth       BluetoothBeacon          Connectivity                Device 
#>                    14                    10                    38                    10                     0                     0                    10                     9 
#>                 Error   GarminAccelerometer      GarminActigraphy             GarminBBI     GarminEnhancedBBI       GarminGyroscope       GarminHeartRate            GarminMeta 
#>                     0                     0                     0                     0                     0                     0                    90                     3 
#>     GarminRespiration GarminSkinTemperature            GarminSPO2           GarminSteps          GarminStress     GarminWristStatus    GarminZeroCrossing             Heartbeat 
#>                     0                     0                     0                     0                    30                     0                     0                    10 
#>                 Light              Location                Memory             Pedometer                Screen              Timezone               Weather                  Wifi 
#>                    10                    10                    10                    10                    10                    10                    10                    10
```

## Extracting data

Data is extracted with `get_data()`, which returns a lazy
[dbplyr](https://dbplyr.tidyverse.org) table that can be queried further
with `dplyr`. You can select a participant and/or a time window, or
leave those arguments empty for everything.

``` r
library(dplyr)

get_data(db, sensor = "Pedometer", participant_id = "12345") |>
  collect()
#> # A tibble: 9 × 4
#>   participant_id time                step_count source_file_id
#>   <chr>          <dttm>                   <dbl>          <dbl>
#> 1 12345          2025-12-13 08:00:00         65              6
#> 2 12345          2025-12-13 14:00:00        459              8
#> 3 12345          2025-12-13 20:00:00        409              2
#> 4 12345          2025-12-14 08:00:00       1093             10
#> 5 12345          2025-12-14 14:00:00        298              5
#> 6 12345          2025-12-14 20:00:00       1125              1
#> # ℹ 3 more rows
```

``` r
# Average battery level per participant
get_data(db, sensor = "Battery") |>
  group_by(participant_id) |>
  summarise(battery_level = mean(battery_level, na.rm = TRUE)) |>
  collect()
#> # A tibble: 2 × 2
#>   participant_id battery_level
#>   <chr>                  <dbl>
#> 1 67890                   42  
#> 2 12345                   64.6
```

## Coverage chart

The `coverage()` function computes how many samples per hour were
collected for each sensor, either in absolute numbers or relative to the
expected sampling rate (see `mpathsenser::freq`). The resulting coverage
chart is a quick way to spot participants or sensors with poor data
collection.

``` r
cov <- coverage(
  db = db,
  participant_id = "12345",
  sensor = c("Activity", "Battery", "Screen", "Wifi", "Location", "Pedometer"),
  relative = FALSE
)
plot(cov)
```

<img src="man/figures/coverage-1.png" alt="" width="100%" style="display: block; margin: auto;" />

## Learn more

- The [Get started
  vignette](https://koenniem.github.io/mpathsenser/articles/mpathsenser.html)
  walks through the full workflow: importing data, deduplication,
  optimising the database, assigning timezones, and creating coverage
  charts.
- The [data overview
  article](https://koenniem.github.io/mpathsenser/articles/data-overview.html)
  documents the database schema.
- The [reference
  site](https://koenniem.github.io/mpathsenser/reference/index.html)
  lists all functions.

## Getting help

If you encounter a clear bug or need help getting a function to run,
please file an issue with a minimal reproducible example on
[GitHub](https://github.com/koenniem/mpathsenser/issues).

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://koenniem.github.io/mpathsenser/CODE_OF_CONDUCT.html).
By participating in this project you agree to abide by its terms.
