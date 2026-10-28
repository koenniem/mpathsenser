---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# mpathsenser <a href='https://koenniem.github.io/mpathsenser/index.html'><img src='logo.png' align="right" height="139" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/mpathsenser)](https://cran.r-project.org/package=mpathsenser)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/koenniem/mpathsenser/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/koenniem/mpathsenser/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/koenniem/mpathsenser/graph/badge.svg)](https://app.codecov.io/gh/koenniem/mpathsenser)
<!-- badges: end -->

`mpathsenser` reads the JSON files exported by the [m-Path Sense](https://m-path.io) mobile sensing app into a [DuckDB](https://duckdb.org) database, and provides a set of convenience functions to inspect, process, and analyse the resulting data.

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

Importing data always follows the same steps: unpack the archives exported by m-Path Sense, point to the folder with the JSON files, create a database, and read the files into it. This package ships with a small example capture (one Android participant, about a day and a half of data) that we use throughout this README. Your own data works exactly the same: extract the `.zip` files with `unzip_data()` and set `path` to the folder that contains the extracted JSON files.




``` r
# The folder with the ZIP archives transferred by m-Path Sense
zip_dir <- system.file("extdata", "example", package = "mpathsenser")

# Extract the JSON files to a normal folder
path <- file.path(tempdir(), "mpathsenser-example")
unzip_data(zip_dir, to = path)

# Create a new database (an in-memory database is fine for experiments)
db <- create_db(NULL, file.path(tempdir(), "study.db"))

# Import the data
read_mpath_sense(path = path, db = db)
```

`read_mpath_sense()` returns a message once all files were written to the database, or the names of the files that could not be imported. Files are imported in batches (`batch_size`, default 1000 files at a time) within transactions, so a file that fails to import does not affect the others. If a file fails even on its own, it is reported and the rest of the batch is imported normally.

Re-running `read_mpath_sense()` on the same folder skips the files that were already imported, and only processes the new ones. Files are tracked in the `ProcessedFiles` table by their name, size, and modification time, so corrected files that were re-uploaded (same name, different content) are imported again. Duplicate measurements are then removed automatically, keeping the row of the newest file per measurement — see the [Get started vignette](https://koenniem.github.io/mpathsenser/articles/mpathsenser.html) for details.

If you only want to explore the package before importing your own data, `example_db()` opens the same capture as a ready-to-use in-memory database:


``` r
db <- example_db()
```

## Inspecting the database

The database holds the sensor data in separate tables (one per sensor) together with metadata about the study, participants, and processed files.


``` r
get_participants(db)
#>   participant_id  study_id
#> 1         372780 studyName
```


``` r
# Number of rows per sensor table
get_nrows(db)
#>         Accelerometer              Activity              AppUsage 
#>                   500                   568                  4187 
#>               Battery             Bluetooth       BluetoothBeacon 
#>                   242                     0                     0 
#>          Connectivity                Device                 Error 
#>                    35                    15                    27 
#>   GarminAccelerometer      GarminActigraphy             GarminBBI 
#>                     0                     0                     0 
#>     GarminEnhancedBBI       GarminGyroscope       GarminHeartRate 
#>                     0                     0                     0 
#>            GarminMeta     GarminRespiration GarminSkinTemperature 
#>                     0                     0                     0 
#>            GarminSPO2           GarminSteps          GarminStress 
#>                     0                     0                     0 
#>     GarminWristStatus    GarminZeroCrossing             Heartbeat 
#>                     0                     0                   499 
#>                 Light              Location                Memory 
#>                   500                   156                   375 
#>             Pedometer                Screen              Timezone 
#>                  8002                   316                   513 
#>               Weather                  Wifi 
#>                    22                   500
```

## Extracting data

Data is extracted with `get_data()`, which returns a lazy [dbplyr](https://dbplyr.tidyverse.org) table that can be queried further with `dplyr`. You can select a participant and/or a time window, or leave those arguments empty for everything.


``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following object is masked from 'package:mpathsenser':
#> 
#>     sql
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

get_data(db, sensor = "Pedometer", participant_id = "372780") |>
  collect()
#> # A tibble: 8,002 × 4
#>   participant_id time                step_count timezone        
#>            <dbl> <dttm>                   <dbl> <chr>           
#> 1         372780 2026-09-30 06:17:15     223688 Europe/Amsterdam
#> 2         372780 2026-09-30 06:19:59     223692 Europe/Amsterdam
#> 3         372780 2026-09-30 06:20:01     223694 Europe/Amsterdam
#> 4         372780 2026-09-30 06:20:11     223695 Europe/Amsterdam
#> 5         372780 2026-09-30 06:20:11     223697 Europe/Amsterdam
#> 6         372780 2026-09-30 06:20:11     223699 Europe/Amsterdam
#> # ℹ 7,996 more rows
```


``` r
# Average battery level per participant
get_data(db, sensor = "Battery") |>
  group_by(participant_id) |>
  summarise(battery_level = mean(battery_level, na.rm = TRUE)) |>
  collect()
#> # A tibble: 1 × 2
#>   participant_id battery_level
#>            <dbl>         <dbl>
#> 1         372780          67.0
```

## Coverage chart

Use `coverage_frequency()` for absolute counts of distinct measurement times and `coverage_proportional()` for proportions relative to expected sampling intervals. `coverage_proportional()` requires a named `expected` vector; `coverage_expected()` provides the defaults. Its `metric` can be `"count"` (distinct measurements), `"interval"` (the union of `[time, time + expected)` intervals), or `"bin"` (expected-interval slots with at least one observation). `by` accepts calendar bins (`"minute"`, `"hour"`, `"day"`, `"week"`, or `"month"`) or a custom width in seconds. Monthly bins follow calendar months; numeric widths are fixed durations. `coverage_frequency()` defaults to `"hour"`, while `coverage_proportional()` defaults to each sensor's expected interval. If an explicit `by` is shorter than a sensor's expected interval, that sensor's bins are widened to the expected interval and a warning is issued; other sensors keep the requested width. Both functions return lazy, participant-specific series with gaps zero-filled. `plot()` defaults to the full time series; pass `cycle = "hour"`, `"day"`, `"week"`, `"month"`, or `"year"` to plot an average calendar profile. The cycle changes the plot only. On iOS, `AppUsage`, `Light`, `Memory`, and `Screen` coverage is `NA`; missing device-platform information triggers a warning.


``` r
cov <- coverage_frequency(
  db = db,
  participant_id = "372780",
  sensor = c("Activity", "Battery", "Screen", "Wifi", "Location", "Pedometer"),
  by = "hour"
)
plot(cov, cycle = "day")
```

<div class="figure" style="text-align: center">
<img src="man/figures/coverage-1.png" alt="plot of chunk coverage" width="100%" />
<p class="caption">plot of chunk coverage</p>
</div>

## Learn more

- The [Get started vignette](https://koenniem.github.io/mpathsenser/articles/mpathsenser.html) walks through the full workflow: importing data, deduplication, optimising the database, assigning timezones, and creating coverage charts.
- The [data overview article](https://koenniem.github.io/mpathsenser/articles/data-overview.html) documents the database schema.
- The [reference site](https://koenniem.github.io/mpathsenser/reference/index.html) lists all functions.

## Getting help

If you encounter a clear bug or need help getting a function to run, please file an issue with a minimal reproducible example on [GitHub](https://github.com/koenniem/mpathsenser/issues).

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](https://koenniem.github.io/mpathsenser/CODE_OF_CONDUCT.html). By participating in this project you agree to abide by its terms.


