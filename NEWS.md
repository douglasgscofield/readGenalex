# readGenalex 0.9.9000 (development version)

* Creates and manipulates data frames using the new class `'genalex'`.  The `"genetic.data.format"` attribute present in earlier versions of this package is removed.
* Extensive documentation updates reflecting use of class `'genalex'`
* Several functions have been deprecated and replaced with generics recognising class `'genalex'`: `printGenalexGenotype` is replaced by `printGenotype`, `computeGenalexColumns` is replaced by `getLocusColumns`, `reorderGenalexLoci` is replaced by `reorderLoci`, `getGenalexLocus` is replaced by `getLocus`, `putGenalexLocus` is replaced by `replaceLocus`, `dropGenalexLoci` is replaced by `dropLocus`, and `reduceGenalexPloidy` is replaced by `reducePloidy`
* Extended `is.genalex` to have shallow (default) or deep (`force = TRUE`) checking of class `'genalex'` consistency.  With deep checking, attributes are inferred from the structure of the data frame and compared to the actual attributes.  Checking of string attributes (`dataset.title`, `data.file.name`) and extra data columns may skipped with options.
* New `as.genalex` generic with `as.genalex.genalex` and `as.genalex.data.frame`.  The latter will convert a pre-class `'genalex'` data frame to class `'genalex'`, and is used wherever a data frame is returned so conversion from old to new format happens automatically.  It will also convert a suitably-formatted data frame to class `'genalex'` after determining suitable attributes
* There is a `summary` method for class `'genalex'` which prints a few lines describing the data set, followed by a summary of the data frame contents and a summary of the extra columns, if present
* New `as.data.frame` method to convert class `'genalex'` to class `'data.frame'`, optionally clearing all class `'genalex'`-specific attributes
* New `as.genetics` method to convert class `'genalex'` to a data frame with genotypes for each locus encoded using class `'genotype'` or class `'haplotype'` from package `genetics`
* New `splitGenotypes` function to split `genotype`-encoded genotypes into separate columns of a data frame
* New `joinGenotypes` function to join class `'genalex'`-style allele columns into a single column per locus
* New `as.loci.genalex` method for the `as.loci` generic from package `pegas` to convert a class `'genalex'` object to class `'loci'`
* New `genalex` function to create a class `'genalex'` object from constituent data
* New `rbind` method to combine samples from multiple class `'genalex'` objects
* New `cbind` method to combine loci and extra data columns from multiple class `'genalex'` objects
* New `addLocus` method to directly add genotype data to the right side of an existing class `'genalex'` object
* New `extra` and `extra<-` methods to get and set the extra columns of a class `'genalex'` object.  The latter coerces its `value` object to a data frame, if it is not already, does a bit of error checking, and sets the row names to be the sample names of the object.
* `readGenalex` now has `...` in its argument list, for additional arguments to be passed to `scan()` while reading data
* Added `na.strings` argument to `readGenalex`, with defaults that reflect GenAlEx's own expectations for missing data
* Added `na` and `na.character` arguments to `writeGenalex`, with values to use for NA in genotype/numeric/logical columns and character columns, respectively
* Added `readGenalexExcel` and `writeGenalexExcel` which use the `XLConnect` package to read and write to Excel files of both `.xls` and `.xlsx` formats.  These functions verify the consistency of their annotations and data prior to writing; this check can be removed on option.
* Added `writeGenepop` to write data in a class `'genalex'` object to a [Genepop](http://kimura.univ-montp2.fr/~rousset)-format text file
* Clarified typing of sample, population, genotype, and extra columns.  Sample and population columns are stored as character (population was previously a factor), genotype columns are stored as numeric, and extra columns are stored as their natural type as determined by `type.convert(..., as.is=TRUE)`, so that character columns are not converted to factors.
* As a result of the clarified typing, `writeGenalex(..., quote=TRUE)` will only quote names in the GenAlEx header, in the first two columns, and in any character-valued extra columns
* There is now more thorough checking for duplicate sample names, which should always produce an error
* There are also checks for duplication among the locus names and the sample and population columns
* The `locus.columns` attribute now has names which match the corresponding loci
* Added tests for all functions and found a few bugs along the way
* Rolled `checkNullAlleles` from my popgen repository into this package
* Extended `checkNullAlleles` to return a matrix of potential null alleles, with a couple of indexing modes
* Added `quiet=` option to `checkNullAlleles`, and pass through some arguments to `printGenotype` when `quiet=FALSE`
* Added `Qagr_adult_genotypes`and `Qagr_pericarp_genotypes` data sets and modified examples to use them, and removed the `example_genotypes` data set
* Created `inst/extdata` to hold data in non-R formats (simple tables), which of course includes GenAlEx-format data
* DESCRIPTION now has an explicit collation order and bug report URL
* Added 'data' target to Makefile to build specified RData files

# readGenalex 0.4.1.9000 (development version)

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

