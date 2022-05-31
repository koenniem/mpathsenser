## Test environments
* local Windows 10 install, R 4.2.0
* Fedora Linux (on R-hub) R-devel
* Mac OSX M1 (devel)
* Windows (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs, or NOTES.

There is 1 NOTE that is only found on Windows (Server 2022, R-devel 64-bit): 

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

## Reverse dependencies
There were no strong reverse dependencies to be checked.

--- 

This version attempts to rectify last version which, despite our best efforts, still wrote to the package library in violation of CRAN policy (thanks to Kurt Hornik and Tomas Kalibera for pointing this out). We changed some small matters which could possibly be the cause of this but we could not reproduce the issue of the read-only file system and hence are unsure of whether the problem has been fixed before submission. If the problem still occurs, we would very much appreciate to hear where the issue resides (i.e. more details) or how we can replicate CRAN's environment.

Best wishes,

Koen Niemeijer
Package Maintainer