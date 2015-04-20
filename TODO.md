# For 1.0

Completed
---------

* Clarified what happens when `'data.frame'` and `'genalex'` items are mixed to `rbind` or `cbind`
* Added `ploidy.genalex` to return the ploidy of an object of class `'genalex'`
* Added `getPopulation.genalex` to return genotypes for specific populations in an object of class `'genalex'`
* Added `as.genalex.loci` to convert class `'loci'` to class `'genalex'`
* Added `joinGenotypes` to join `101` `107` to `101/107`, and a phased version, and added several tests
* Added `splitGenotypes` to split `101/107` into separate columns of a data frame, and added several tests for it
* Added `as.genetics.genalex` method to convert a (diploid-only) class `genalex` object to a data frame with allele columns for each locus converted to single-column genotypes of class `genotype` or class `haplotype`, as defined by package `genetics`.  We accept but ignore with warning a `sep =` argument for specifying the separation of the alleles within the resulting genotype.  This is inconsistently applied by package `genetics`, e.g., `makeGenotypes` uses it for column names but always uses `"/"` for genotypes and haplotypes, so we cannot reliably apply it ourselves.
* Check added for duplicate column names involving the loci and the sample and population columns
* Added `writeGenepop` to write Genepop-format data files (Rousset 2008 *Molecular Ecology Resources*, <http://kimura.univ-montp2.fr/~rousset>)
* Added `addLocus.genalex` for directly adding genotype data
* Added `cbind.genalex` method to combind class `genalex` objects
* Added extra columns checks and `skip.extra` option to `is.genalex`
* Added `extra` as a shortcut for extracting extra columns from a class `genalex` object, and `extra<-` as a shortcut for assigning them.  The latter does a bit of error checking and sets their rownames to be the sample names of the class `genalex` object.
* Sample names are checked for duplication more thoroughly
* The `locus.columns` attribute now has names that match the `locus.names` attribute, and tests have been added to verify it works as it should
* `writeGenalexExcel()` will write to an Excel file, using the `XLConnect` package
* `readGenalexExcel()` will read from an Excel file, using the `XLConnect` package
* In most cases, both `as.genalex(as.data.frame(x1))` and `as.genalex(as.data.frame(x1, complete = TRUE))` will result in identical class `'genalex'` objects, except for the `"data.file.name"` attribute.  If the column names are not well-formed so that it is difficult to tell the ploidy *de novo*, then perhaps there may be some issues.
* `is.genalex(...)` is a shallow `inherits()` check
* `is.genalex(..., force=TRUE)` is a deeper comparison that rederives attributes from the data and compares the ones safe to compare, returning `FALSE` if the attributes are not consistent
* `is.genalex(..., force=TRUE, verbose=TRUE)` does what `is.genalex(..., force=TRUE)` does, but adds the printing of which attributes differed, via `message()`
* `as.genalex(..., force=TRUE)` will rederive attributes from the data and will reset them so that data and attributes are again consistent.  It does not report which attributes differed, for that use `is.genalex(..., force=TRUE, verbose=TRUE)`
* Other tests are run during CRAN check, so test that way
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
* The documentation is more explicit about how extra columns are handled when writing
* straighten out test for writeGenalex with and without quote=
* GenAlEx says all genotype data is numeric, so code it as numeric after reading
* S3 methods for `as.genalex` append to the base method name `as.genalex`, e.g., `as.genalex.genalex`
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

