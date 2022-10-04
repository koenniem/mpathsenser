# mpathsenser 1.0.4 (in development)
## Major changes
* Added several functions:
  - `link_gaps`
  - `add_gaps`
  - `bin_data`: To subdivide data into bins, e.g. 
* Added lifecycle badge to signal the state of functions.
* `link` has been revised and expanded:
  - Replaced `offset` with `offset_before` and `offset_after`, allowing both to be specified at the 
  same time.
  - Added new `add_before` and `add_after` argument to allow the last row before the measurement 
  and first row after the measurement respectively to be added to the data.
  - Added a new `split` argument, allowing computation to be split among many parts thereby
  lowering computational burden.
* `app_category` is now case insensitive and gained the new argument "exact" to be able to match the
package name exactly based on a partial match.
* Added a (start of a) vignette to further highlight the package use.

## Minor changes
* Changed `get_activity` to `activity_duration`.
* Changed `link2` to `link_db`.

## Bug fixes
* Fixed issue #2
* Fixed issue #3
* Fixed issue in `get_data` which allowed multiple sensors to be requested from one function call, 
sometimes leading to crashes (#4).
* Fixed issue in `link` where column `original_time` is missing if no records before or after the 
interval are found (#6).
* Fixed a bug in import where sensor data not present in first file of the batch are dropped for the
other files well.
* Fixed app_category to work with the updated Google Play website.

# mpathsenser 1.0.3
* Fix final tests not yet using TMPDIR

# mpathsenser 1.0.2
* Changed some unit to use TMPDIR

# mpathsenser 1.0.1
* Fixed floating point differences when testing on MacOS

# mpathsenser 1.0.0
* Initial release on CRAN
* Changed the name to mpathsenser
