# Version 1.2

* Corrected some parts of the internal C function that computes CAPE and CIN that were executed when upToTop is TRUE. There is no need to change calling programs. The structure of return values hasn't changed, either.

* Some parts of the manual have been corrected.

# Version 1.1

* The TTindex function was updated after an error found in the code (reported by the CRAN team).