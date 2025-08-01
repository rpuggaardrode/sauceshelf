## Version 0.1.4

* Added `removeExisting` argument to `sauce2ssff()`, which removes any existing SSFF files with the `.sauce` extension before adding new ones to the EMU database.
* Following `R_CMD_CHECK` recommendations, testing class membership is now done with `inherits()` instead of comparing the output of `class()` to a string.
* Added check to `sauce2ssff()` to test if `emuDBhandle` actually corresponds to a loaded EMU database.

## Version 0.1.3

* When running `praatsauce()` and `sauce_hirst2pass()` over an EMU database and saving `.Pitch` and `.Formant` objects, the directory structure of the EMU database is now emulated in the directory where those objects are written to disk. This is so they can potentially be used for re-running PraatSauce over the same EMU database.

## Version 0.1.2

* Fixed breaking bug in `sauce_hirst2pass()` when *not* processing EMU database, introduced in version 0.1.1.

## Version 0.1.1

* Fix to `sauce_hirst2pass()` to make sure that the `file` column includes full path when bulk processing an EMU database.

## Version 0.1.0

* Added new function `sauce_hirst2pass()` for using the by-speaker two-pass procedure for estimating solid pitch floor and ceiling values before computing other measures which may rely on F0. The function is designed to work with both `praatsauce()` and `mixedsauce()`, but for now it only works for `praatsauce()`. 
* Minor fixes to `mixedsauce()` to improve versatility.
* Fixes to `sauce2ssff()`, including support for adding strength-of-excitation measures to databases which was inexplicably missing before, and somewhat improved handling of session names (thanks to Angelo Dian!)

## Version 0.0.6

* Check to ensure that all relevant columns are numeric when calling `sauce2ssff()`.

## Version 0.0.5

* Attempt to fix bug whereby the time column of PraatSauce output is sometimes interpreted as a string, causing breaking downstream issues with `mixedsauce()`, `emusauce()` and `sauce2ssff()`.
* Added check to ensure that different session naming conventions in EMU databases are supported.

## Version 0.0.4

* Copying over [fixes to the PraatSauce code](https://github.com/kirbyj/praatsauce/commit/1bf33f7252897e6f3c7fbd3b1d5be38ae3922a30)

## Version 0.0.3

* Copying over [fixes and enhancements to the PraatSauce code](https://github.com/kirbyj/praatsauce/commit/559e0f43af69751dc27bdf82477a44edce7d0566), but not the new `verbose` in PraatSauce as it would not render correctly in R

## Version 0.0.2

* Copying over [fixes to the PraatSauce code](https://github.com/kirbyj/praatsauce/commit/973bf5975f55152a0106e87799b52f1b0bb3313d)
* Added check to make sure that PraatSauce output that uses TextGrid labels are loaded in correctly.
