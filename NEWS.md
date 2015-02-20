# readGenalex 0.9.9000 (class_genalex development branch)

* NOT YET: Extended `checkNullAlleles` to return a matrix of potential null alleles, with a couple of indexing modes.  More extensions needed.
* Added `Qagr_adult_genotypes`and `Qagr_peric_genotypes` data sets and modified examples to use them
* Added test for `checkNullAlleles` and an example
* Rolled `checkNullAlleles` from my popgen repository into this package
* New `as.genalex` function to convert a pre-class `'genalex'` data frame.  This is used wherever a class `'genalex'` data frame is returned, so conversion from old to new format happens automatically.
* Added `na.strings` argument to both `writeGenalex` and `readGenalex`, with defaults that reflect GenAlEx's own expectations for missing data
* `readGenalex` now has `...` in its argument list, for additional arguments to be passed to `scan()` while reading data
* Extensive documentation updates reflecting use of class `'genalex'`
* Created `inst/extdata` for data in non-traditional formats.  More to be done there.
* Added tests for `writeGenalex`

# readGenalex 0.4.1.9000 (current development version)

* Moved documentation from separate *.Rd files to Roxygen2 comments in R source using the really helpful Rd2roxygen package.
* Implemented `writeGenalex`, to write a data.frame created by this package to a GenAlEx-compatible text file, and documented differences between output of `writeGenalex` and input to `readGenalex`
* Fixed bug in `is.genalex` when `genetic.data.format` attribute doesn't exist
* Slight expansion of documentation for `reorderGenalexLoci`
* Removed Date from DESCRIPTION, generated automatically during package creation
* DESCRIPTION uses Authors@R
* Marginally more clever Makefile

# readGenalex 0.4.1

* First release available on CRAN
* First release available as a `devtools`-installable github repository (so development releases are always available via `devtools::install_packages("douglasgscofield/readGenalex")`)
* Restructured into R package directory structure, with individual exported functions split into separate files, and internal functions into `readGenalex-internal.R`
* Moved and expanded documentation following R package guidelines
* Added `example_genotypes` toy dataset.  After loading the package with `library(readGenalex)` do `data(example_genotypes)` to load `example_genotypes` into your envirnment.  The same dataset is also available here in text form as `data/example_genotypes.txt`.
* Added `.Rbuildignore`
* Improve error messages for sample size mismatches
* Handle lack of extra columns more gracefully, by completely leaving off the `extra.columns` attribute
* Fix `getGenalexLocus()` when given a `pop=` argument
* Fix `printGenalexGenotype()` to print sample name and population name using `as.character()` since population names are stored as factors
* Remove the `data.column.names` attribute, which duplicated the values in `names`

# readGenalex 0.4

* Downconvert ploidy using `reduceGenalexPloidy()`. Currently only handles 2n to 1n, and selects only the first allele of each locus.

# readGenalex 0.3

* Improve error messages when mismatch between group names in the header and in the data
* Handle the presence of extra columns to the right of the genotype columns.  These are loaded into a separate `extra.columns` attribute.

