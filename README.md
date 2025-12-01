
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mpathsenser <a href='https://ppw-okpiv.pages.gitlab.kuleuven.be/researchers/u0134047/mpathsenser/index.html'><img src='man/figures/logo.png' align="right" height="139" /></a>

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

## Installing the package

You can install the latest version of mpathsenser from CRAN:

``` r
install.packages("mpathsenser")
```

Or the development version from Github:

``` r
remotes::install_github("koenniem/mpathsenser")
```

## Importing files

Specify a path variable to wherever you put the JSON files. Make sure to
use `/` and not a backslash.

``` r
path <- "~/Mobile Sensing Study/Data"
```

If you haven’t done so, unzip all files.

``` r
unzip_data(path = path)
#> Unzipped 37 files.
```

In m-Path Sense, data is written to JSON files as it comes in. In the
JSON file format, every file starts with `[` and ends with `]`. If the
app is killed, JSON files may not be properly closed and hence cannot be
read by JSON parsers. So, we must first test if all files are in a valid
JSON format and fix those that are not.

``` r
# Test JSONs for problems. Output is a character vector containing bad files (if any).
to_fix <- test_jsons(path = path)
#> Warning: There were issues in some files

# Fix JSON files if there are any.
# Note that test_jsons() returns the full path names, so a path directory is not necessary.
if (length(to_fix) > 0) {
  fix_jsons(path = NULL, files = to_fix)
}
#> Fixed 12 files
```

To import data, first create a database.

``` r
db <- create_db(path = path, db_name = "some_db.db")
```

Then, call `import()` to start reading in the files.

``` r
import(path = path, db = db)
#> All files were successfully written to the database.
```

If everything went correctly, there should be a message that all files
were successfully written to the database. Otherwise `import()` return a
character vector containing the files that failed to be imported. Note
that files only need to be imported once, and that new files can be
added to the database by calling `import()` again using the same
database. Files that were processed previously will be skipped.

## Extracting data from the database

Once files are imported, you can establish a database connection with
`open_db()`. Don’t forget to save it to a variable!

``` r
db <- open_db(
  path = path, 
  db_name = "some_db.db"
)
```

To find out which participants are in the database (or rather their
participant numbers):

``` r
get_participants(db)
#>   participant_id     study_id
#> 1           2784 Study_Merijn
#> 2            N/A           -1
```

We can also check what device they are using (which can be found in the
Device table of the database).

``` r
device_info(db = db)
#> # A tibble: 1 × 10
#>   participant_id device_id    hardware device_name device_manufacturer device_model operating_system
#>   <chr>          <chr>        <chr>    <chr>       <chr>               <chr>        <chr>           
#> 1 2784           SP1A.210812… qcom     r8q         samsung             SM-G780G     REL             
#> # ℹ 3 more variables: platform <chr>, operating_system_version <chr>, sdk <chr>
```

To find out how much data there is in this database, look at the number
of rows as an indication. Note that this operation may be slow for large
databases, as every tables in the database needs to be queried.

``` r
get_nrows(db)
#> Accelerometer    AirQuality      Activity      AppUsage       Battery     Bluetooth      Calendar 
#>         75680             0             2           386             0          1103            98 
#>  Connectivity        Device         Error      Geofence     Gyroscope     Heartbeat InstalledApps 
#>            40            12             1             0         26509             0          1236 
#>      Keyboard         Light      Location        Memory      Mobility         Noise     Pedometer 
#>             0           538            37            84             0             0          4099 
#>      PhoneLog        Screen   TextMessage      Timezone       Weather          Wifi 
#>             0           358             0             0            35            84
```

Now let’s find out how to actually retrieve data from the database.
There is a simple function for this, which is called `get_data()`. With
this function you can extract any kind of data you want. Make sure you
also run `?get_data` for an overview of how to use this (or any other)
function. In most functions, you can also leave arguments empty to
retrieve all data (e.g. not in a specific time window).

``` r
get_data(
  db = db, # the ACTIVE database connection, open with open_db AND save to a variable
  sensor = "Pedometer", # A sensor name, see mpathsenser::sensors for the full list
  participant_id = "2784", # A participant ID, see get_participants
  start_date = "2022-06-14", # An optional start date, in the format YYYY-MM-DD
  end_date = "2022-06-15" # An optional end date, in the format YYYY-MM-DD
)
#> # Source:   SQL [?? x 5]
#> # Database: sqlite 3.51.0 [C:\Users\u0134047\AppData\Local\Temp\RtmpsbzfUd\readme\some_db.db]
#>   measurement_id                       participant_id date       time     step_count
#>   <chr>                                <chr>          <chr>      <chr>         <int>
#> 1 ce16d410-ebc5-11ec-a276-bfb1e065589a 2784           2022-06-14 09:38:54     119131
#> 2 ce659050-ebc5-11ec-a235-b1fd6433d9e2 2784           2022-06-14 09:38:54     119132
#> 3 ceb64860-ebc5-11ec-8f07-93c1927f71b2 2784           2022-06-14 09:38:55     119133
#> 4 cf133570-ebc5-11ec-bf61-85f33d53f14d 2784           2022-06-14 09:38:55     119134
#> 5 cfb47e80-ebc5-11ec-b17b-85bcc3b36c13 2784           2022-06-14 09:38:56     119136
#> # ℹ more rows
```

A more comprehensive guide is provided in the [Get Started
vignette](https://ppw-okpiv.pages.gitlab.kuleuven.be/researchers/u0134047/mpathsenser/articles/mpathsenser.html).

## Reference

For an overview of all functions in this package, see the [mpathsenser
Reference
Site](https://ppw-okpiv.pages.gitlab.kuleuven.be/researchers/u0134047/mpathsenser/reference/index.html).
The database schema used in this package can be found
[here](https://gitlab.kuleuven.be/ppw-okpiv/researchers/u0134047/mpathsenser/-/blob/master/inst/extdata/mpathsenser_db.png).

## Getting help

If you encounter a clear bug or need help getting a function to run,
please file an issue with a minimal reproducible example on
[Gitlab](https://gitlab.kuleuven.be/ppw-okpiv/researchers/u0134047/mpathsenser/-/issues).

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://ppw-okpiv.pages.gitlab.kuleuven.be/researchers/u0134047/mpathsenser/CODE_OF_CONDUCT.html).
By participating in this project you agree to abide by its terms.
