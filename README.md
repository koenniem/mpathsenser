
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mpathsenser <a href='https://ppw-okpiv.pages.gitlab.kuleuven.be/researchers/u0134047/mpathsenser/index.html'><img src='man/figures/logo.png' align="right" height="139" /></a>

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN
status](https://www.r-pkg.org/badges/version/mpathsenser)](https://cran.r-project.org/package=mpathsenser)

## Installing the package

You can install the latest version of `mpathsenser` from CRAN:

``` r
install.packages("mpathsenser")
```

Alternatively, you can install the development version from my Gitlab
repo. First, make sure you have
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) (Windows,
Linux) or XCode installed. For XCode, register as an [Apple
Developer](https://developer.apple.com/) (don’t worry, it’s free) and
then run `xcode-select --install` in a terminal. Then, run the following
code in R:

``` r
devtools::install_git("https://gitlab.kuleuven.be/ppw-okpiv/researchers/u0134047/mpathsenser")
```

## Importing files

Specify a path variable to wherever you put the JSON files. Make sure to
use / and not a backslash.

``` r
path <- "~/Mobile Sensing Study/Data"
```

If you haven’t done so, unzip all files.

``` r
unzip_data(path = path, parallel = TRUE)
```

In m-Path Sense, data is written to JSON files as it comes in. In the
JSON file format, every file starts with \[ and ends with \]. If the app
is killed, JSON files are not properly closed and hence cannot be read
by JSON readers. So, we must first test if all files are in a valid JSON
format and fix those that are not.

``` r
to_fix <- test_jsons(path = path)

# Fix JSON files if there are any
if (to_fix != "") {
  fix_jsons(path = path, files = to_fix, parallel = TRUE)
}
```

If you want to put the database in a different directory than the files,
create it first and then pass it to `import`.

``` r
db <- create_db("path/for/you/new/database/", "some_db.db")
```

If not, you can simply call `import` to start reading in the files.

``` r
import(path = path, dbname = "some_db.db")

# In the case of an existing database or database in another location:
# import(path = path, db = db)
```

Note that `import` always closes the database connection.

## Extracting data from the database

To establish a database connection, use `open_db`. Don’t forget to save
it to a variable!

``` r
db <- open_db(path = path, db_name = "some_db.db")
```

To find out which participants are in the database (or rather their
participant numbers):

``` r
get_participants(db)
```

We can also check what device they are using (which can be found in the
Device table of the database).

``` r
device_info(db = db)
```

To find out how much data there is in this database, look at the number
of rows as an indication. You may notice from running this function that
it’s not exactly fast. That’s because it is a lot of data. So be careful
when computing things, or you’ll be stuck for a long time (especially
when working with accelerometer and gyroscope)

``` r
get_nrows(db)
```

Now let’s find out how to actually retrieve data from the database.
There is a simple function for this, which is called `get_data`. With
this function you can extract any kind of data you want. Make sure you
also run `?get_data` for an overview of how to use this (or any other)
function. In most functions, you can also leave arguments empty to
retrieve all data (e.g. not in a specific time window).

``` r
get_data(
  db = db, # the ACTIVE database connection, open with open_db AND save to a variable
  sensor = "Activity", # A sensor name, see mpathsenser::sensors for the full list
  participant_id = "12345", # A participant ID, see get_participants
  start_date = "2021-11-14", # An optional start date, in the format YYYY-MM-DD
  end_date = "2021-11-15" # An optional end date, in the format YYYY-MM-DD
)
```

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
Conduct](https://gitlab.kuleuven.be/ppw-okpiv/researchers/u0134047/mpathsenser/-/blob/master/CONTRIBUTING.md).
By participating in this project you agree to abide by its terms.
