# readGenalex 0.4.1.9000 (current development version)

* Slight expansion of documentation for `reorderGenalexLoci()`
* Removed Date from DESCRIPTION, generated automatically during package creation
* TODO: check that removing Date was OK :-)
* TODO: read from Excel files
* TODO: implement writeGenalex, as tab-separated text and if possible to an Excel file

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

