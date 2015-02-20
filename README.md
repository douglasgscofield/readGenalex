readGenalex
===========

An R package for reading data files in GenAlEx format, as exported from Excel
as a delimited text file, into an annotated `data.frame` of class `'genalex'`,
and manipulate it in that form.  Several functions are provided for accessing
and printing this data.  GenAlEx and its documentation are available at
<http://biology-assets.anu.edu.au/GenAlEx>.

## Development version

This branch (**class_genalex**) is a development branch to implement the
functionality of this package by defining an S3 class `'genalex'` that is
shared with `data.frame`.  This functionality will be present in the 1.0
release of this package.  In the meantime, the code in this particular branch
is not likely to be functional until further notice.

## Using the package

`readGenalex` 0.4.1 is available on CRAN:

```R
> install.packages("readGenalex")
```

The development version 0.9.9000 on this branch (`class_genalex`) is hosted on
Github and can always be installed via:

```R
> install.packages("devtools")
> devtools::install_github("douglasgscofield/readGenalex", ref = "class_genalex")
```

To use:

```R
> library(readGenalex)
> refgt <- readGenalex("reference_genotypes.txt")
> refgt
    id Site loc1 loc1.2 loc2 loc2.2 loc3 loc3.2 loc4 loc4.2 loc5 loc5.2
1 ref1    1    3      3    2      3    2      2    3      3    4      3
2 ref2    1    2      3    1      1    2      4    3      3    6      1
3 ref3    1    3      3    2      3    2      2    3      1    4      2
4 ref4    1    3      3    2      1    2      2    3      1    2      3
5 ref5    1    1      1    1      3    2      5    3      3    6      2
6 ref6    1    1      1    2      1    2      5    2      3    3      1
> attributes(refgt)
$names
 [1] "id"     "Site"   "loc1"   "loc1.2" "loc2"   "loc2.2" "loc3"   ...
$row.names
[1] 1 2 3 4 5 6

$class
[1] "data.frame"

$n.loci
[1] 5

$ploidy
[1] 2

$n.samples
[1] 6

...
```

It only reads the number of samples specified by the GenAlEX header, and only
treats as genotypes the number of genotype columns implied by the GenAlEx
header in concert with the stated ploidy level.

It also tries to ignore extra TAB characters that tools such as Excel can
insert when exporting TAB-delimited text, otherwise these could imply both
additional columns and additional rows.  Hopefully the latter is avoided by
only reading the number of samples specified by the header.

If there are additional **named** columns to the right of the genotypes, these
are read and stored in a dataframe attached to the attribute `extra.columns`.
The first column of the `extra.columns` dataframe is the sample name (leftmost
column from the genotypes, e.g., the `id` column from the above example).  It
attempts to ignore additional unnamed columns scattered amongst the named extra
columns.

There are other functions supplied for manipulating population genetic data
produced by `readGenalex()`:

`writeGenalex()`
: Write a GenAlEx-format text file from a `readGenalex`-format data frame.

`is.genalex()`
: Checks whether the `genetic.data.format` attribute is set to `genalex`.

`reduceGenalexPloidy()`
: Reduce the ploidy to 1 by selecting the first allele of each locus.

`dropGenalexLoci()`
: Drop named loci from the data.

`printGenalexGenotype()`
: Print genotypes of specific rows.

`reorderGenalexLoci()`
: Reorder loci into a given order.

`computeGenalexColumns()`
: Return a vector of column numbers for specified loci.

`putGenalexLocus()`
: Replace genotypes of specified locus.

`getGenalexLocus()`
: Return genotypes of specified locus, optionally only for specific populations.

`checkNullAlleles()`
: Compare genotypes against a set of reference genotypes to check for potential null (nonamplifying) alleles.

