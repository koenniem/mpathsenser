## Resubmission
This is a resubmission. In this version I have:
* Replaced the double quotation marks with single quotation marks in the description to denote package, software,
and API names.

## Test environments
* local Windows 10 install, R 4.3.2
* Fedora Linux (on R-hub) R-devel
* Ubuntu Linux (devel and release)
* Mac OSX M1 (devel and release)
* Windows (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs, but there was 1 NOTE:
* Possibly misspelled words in DESCRIPTION:
  JSON (15:5)
  pre (12:35)

JSON is a commonly and well-known file format in the community and I believe should not be spelled in full. The prefix
'pre' belongs to 'pre-processing', a common variant of 'preprocessing', which also triggers a spelling error. This word is 
commonplace and should be accepted.

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

Best wishes,

Koen Niemeijer  
Package Maintainer
