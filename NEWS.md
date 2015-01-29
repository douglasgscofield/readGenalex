# readGenalex 0.4.1.9000 (current development version)

* Implemented `writeGenalex()`, to write a data.frame created by this package to a GenAlEx-compatible text file, and documented differences between output of `writeGenalex()` and input to `readGenalex()`
* Fixed bug in `is.genalex()` when `genetic.data.format` attribute doesn't exist
* Slight expansion of documentation for `reorderGenalexLoci()`
* Removed Date from DESCRIPTION, generated automatically during package creation
* TODO: drawing on writeGenalex documentation about differences when writing, be more explicit about what happens with extra columns and with extra column names on input
* TODO: read from and write to Excel files
* TODO: add more tests
* TODO: move existing .Rd files to Roxygen2 comments using package Rd2roxygen
* TODO: create an S3 class "genalex" based on data.frame and modify is.genalex to check for that (or could we then use the generic?)
* TODO: `createGenalex()`, for creating a de-novo `is.genalex()`-able data.frame.  This requires a createGenalexLocus or some generalisation of the current locus-handling functions, to add a new locus to the "end".
* TODO: functions for converting from/to some other genetic formats
* What will be enough for 0.5?  1.0?  If I implement class "genalex", that would seem to be 1.0.  When I do the dispersalDiversity package, since that will require readGenalex, should that be based on a 1.0-type readGenalex?  It seems to make the most sense if it is.

# readGenalex 0.4.1

* First release available on CRAN
* First release available as a `devtools`-installable github repository (`devtools::install_packages("douglasgscofield/readGenalex")`)
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

