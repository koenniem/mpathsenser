## Resubmission
This is a resubmission. In this version I have:

* Changed the URL in the description in the BugReports field to comply with CRAN policies.

## Test environments
* local Windows 10 install, R 4.2.0
* Fedora Linux (on R-hub) R-devel
* Mac OSX M1 (devel and release)
* Windows (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

```
Found the following (possibly) invalid URLs:
  URL: https://gitlab.kuleuven.be/ppw-okpiv/researchers/u0134047/mpathsenser/issues (moved to https://gitlab.kuleuven.be/ppw-okpiv/researchers/u0134047/mpathsenser/-/issues)
    From: DESCRIPTION
          man/mpathsenser-package.Rd
    Status: 200
    Message: OK
```
This message is erroneous as the link is functional. Using the redirected URL would also result in a NOTE as this is not in canonical form.

There is 1 NOTE that is only found on Windows (Server 2022, R-devel 64-bit): 

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

Best wishes,

Koen Niemeijer\
Package Maintainer
