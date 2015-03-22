# For 1.0

TODO
----

- read from and write to Excel files
- devtools::test() runs testthat tests, how to run *other* tests?
- `as.genalex(..., force=TRUE, verbose=TRUE)`
- `is.genalex(..., force=TRUE)` to check for consistency
- `cbind.genalex`?  `insertLocus`?
- what happens to `as.genalex(as.data.frame(x1))`?
- what happens to `as.genalex(as.data.frame(x1, complete = TRUE))`?
- Add used papers to the data description?
- Functions for converting from/to some other genetic formats, attaching them to `as.genalex`
- If I am converting from those other formats, do I need to have the specific packages loaded?
- check on any implicit version dependencies and document them
- Initial `dispersalDiversity` package should use readGenalex 1.0

### R Extension rules for using types from other packages

<quote>
(*Regarding the DESCRIPTION file*) Finally, the ‘Enhances’ field lists packages “enhanced” by the package at hand, e.g., by providing methods for classes from these packages, or ways to handle objects from these packages (so several packages have ‘Enhances: chron’ because they can handle datetime objects from chron even though they prefer R’s native datetime functions). Version requirements can be specified, but are currently not used. Such packages cannot be required to check the package: any tests which use them must be conditional on the presence of the package. (If your tests use e.g. a dataset from another package it should be in ‘Suggests’ and not ‘Enhances’.)
</quote>


### internal for reading Excel files

* what is the current state of the art?

### internal for force=TRUE stuff

* straighten up names code, too much duplication
* straighten up implied attributes code, also too much duplication
* keep calling genalex() to do the lifting, but perhaps there is some additional that can be done there, perhaps splitting genalex() into an internal and external function
* perhaps makes sense to start on importing/exporting data iwth another package to see what additional issues may come up


Completed
---------

* Straightened up `checkNullAllele` code, added return of matrices indicating possible null alleles with mode="locus" and mode="column", and added `checkNullAllele()`-specific test suite
* Renamed `putLocus` to the much more semantically consistent `replaceLocus`
* Expanded `as.genalex.data.frame` to convert a suitably-formatted data frame to class `genalex` by determining suitable attributes
* Added method `rbind.genalex`
* Added tests for all package functions
* Renamed `createGenalex()` to `genalex`, it is a constructor
* Moved deprecated functions to `R/readGenalex-deprecated.Rd`
* Moved data documentation to `R/readGenalex-data.Rd`
* Switched to full-generic interface, e.g., replaced `reduceGenalexPloidy` with `reducePloidy` generic and `reducePloidy.genalex` method, and deprecated old functions
* Added `createGenalex()`, for creating a de-novo class `'genalex'` data frame
* drawing on writeGenalex documentation about differences when writing, be more explicit about what happens with extra columns and with extra column names on input
* straighten out test for writeGenalex with and without quote=
* GenAlEx says all genotype data is numeric, so code it as numeric after reading
* Is `as` a generic and `as.genalex` a method?
* Add summary.genalex method
* Update README for new data sets
* Fix WARNING during make check, probably because of the following:
* Split data up into two separate RData
* implement makefile rule for building RData in data/
* exchange fake data for real data from DataDryad
* how to add citation instructions to data?  see data/datasource for that info
* create an S3 class "genalex" based on data.frame and modify is.genalex to check for that (or could we then use the generic?)  I ended up creating as.genalex and using this internally where the class will be returned.
* Implementing class "genalex" needs to bump version to 1.0, since there would be API changes.
* For 1.0, recognise pre-1.0 genalex data frames and convert to 1.0 class (via `as.genalex`)
* implement `quote=` for `writeGenalex`
* Moved `checkNullAlleles` from popgen repository to this package

