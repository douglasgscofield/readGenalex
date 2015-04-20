readGenalex
===========

An R package for reading data files in GenAlEx format, as exported from Excel
as a delimited text file, into an annotated `data.frame` of class `'genalex'`,
and manipulate it in that form.  Several functions are provided for accessing,
manipulating, and printing this data.  GenAlEx and its documentation are
available at <http://biology-assets.anu.edu.au/GenAlEx>.



## Development version 0.9.9000

This is a development version which implements the functionality of this
package by defining an S3 class `'genalex'` that is shared with `data.frame`.
This functionality will be released to CRAN in the 1.0 version of this package.
In the meantime, the code in this master branch may have odd little bugs.  I
will not make a commit to the master branch that does not pass the test suite,
but please use caution.



## Using the package

`readGenalex` 0.4.1 is available on CRAN:

```R
> install.packages("readGenalex")
```

The development version here (**0.9.9000**) is hosted on Github and can always
be installed via:

```R
> install.packages("devtools")
> devtools::install_github("douglasgscofield/readGenalex")
```

Class `genalex` is an annotated data frame with attributes containing
additional information specified by the user:

~~~~
> library(readGenalex)
> data(Qagr_adult_genotypes)
> head(Qagr_adult_genotypes)
  AdultMomFamily     Site 0c11 0c11.2 0c19 0c19.2 Oe09 Oe09.2 0i01 0i01.2 0m05
1           2201 Sedgwick  215    215  226    244  190    192  204    206  210
2           2202 Sedgwick  213    217  238    238  190    192  204    204  210
3           2203 Sedgwick  213    215  234    240  190    192  196    206  216
4           2204 Sedgwick  213    213  234    234  186    192  196    204  210
5           2205 Sedgwick  213    217  222    238  190    194  204    204  210
6           2206 Sedgwick  213    213  226    240  194    194  196    204  202
  0m05.2 0m07 0m07.2 1c06 1c06.2 1c08 1c08.2 1f02 1f02.2 1g13 1g13.2
1    210  199    199  238    242  273    273  190    190  189    191
2    222  199    205  244    244  273    273  190    190  185    187
3    220  199    199  234    234  273    273  190    190  189    189
4    212  199    201  234    236  269    273  190    190  187    191
5    216  201    205  236    242  273    273  180    190  185    187
6    206  201    203  240    242  271    273  190    190  185    185
> class(Qagr_adult_genotypes)
[1] "genalex"    "data.frame"
> attributes(Qagr_adult_genotypes)
$names
 [1] "AdultMomFamily" "Site"           "0c11"           "0c11.2"
 [5] "0c19"           "0c19.2"         "Oe09"           "Oe09.2"
 [9] "0i01"           "0i01.2"         "0m05"           "0m05.2"
[13] "0m07"           "0m07.2"         "1c06"           "1c06.2"
[17] "1c08"           "1c08.2"         "1f02"           "1f02.2"
[21] "1g13"           "1g13.2"

$row.names
  [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
 [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
 [37]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
 [55]  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
 [73]  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
 [91]  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
[109] 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
[127] 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
[145] 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162
[163] 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
[181] 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198
[199] 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216
[217] 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234
[235] 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252
[253] 253 254 255 256 257 258 259 260 261 262

$class
[1] "genalex"    "data.frame"

$n.loci
[1] 10

$ploidy
[1] 2

$n.samples
[1] 262

$n.pops
[1] 1

...
~~~~

`readGenalex` reads GenAlEx-format data from a text file, and
`readGenalexExcel` reads data from a specified worksheet of an Excel workbook
of `.xls` or `.xlsx` format using the `XLConnect` package.  Both functions read
only the number of samples specified by the GenAlEX header, and only treat as
genotypes the number of genotype columns implied by the GenAlEx header in
concert with the stated ploidy level.

Because GenAlEx is an Excel plugin, `readGenalex` attempts to deal with various
issues that may arise when exporting text files from Excel.  `readGenalex` also
tries to ignore extra tab characters that tools such as Excel can insert when
exporting tab-delimited text, otherwise these could imply both additional
columns and additional rows.  Hopefully the latter is avoided by only reading
the number of samples specified by the header.

If there are additional **named** columns to the right of the genotypes, these
are read and stored in a data frame attached to the attribute `extra.columns`.
The first column of the `extra.columns` data frame is the sample name (leftmost
column from the genotypes, e.g., the `id` column from the above example).  It
attempts to ignore additional unnamed columns scattered amongst the named extra
columns.

There are also corresponding `writeGenalex` and `writeGenalexExcel` functions.

Functions:

Function | Description
-------- | -----------
`readGenalex()` | Read GenAlEx-format data from a text file
`readGenalexExcel()` | Read GenAlEx-format data from a worksheet of an Excel workbook
`genalex()` | Create a class `'genalex'` object from constituent data
`as.genalex()` | Generic function which converts a pre-1.0-style `readGenalex` data frame to class `'genalex'`, or converts a suitably-formatted data frame to class `'genalex'`.  Optionally it can determine structure implied by the data and update attributes to reflect that structure.
`writeGenalex()` | Write a GenAlEx-format text file
`writeGenalexExcel()` | Write a GenAlEx-format worksheet to an Excel workbook
`summary()` | Prints a summary of the data set, a summary of the genotype data frame, and a summary of the extra columns, if any
`is.genalex()` | Checks whether the object is class `'genalex'`, optionally does a deeper check to determine whether the structure described in the attributes matches the structure implied by the data
`as.data.frame()` | Method to convert class `'genalex'` to class `'data.frame'`, optionally all class `'genalex'`-specific attributes are removed
`reducePloidy()` | Reduce the ploidy to 1 by selecting the first allele of each locus
`dropLocus()` | Drop named loci
`getPopulation()` | Return genotypes of specific populations in object of class `'genalex'` 
`printGenotype()` | Print genotypes of specific rows
`reorderLoci()` | Reorder loci into a given order
`computeLocusColumns()` | Return a vector of column numbers for specified loci
`replaceLocus()` | Replace genotypes of specified locus
`getLocus()` | Return genotypes of specified locus, optionally only for specific populations
`addLocus()` | Add genotypes to an object of class `'genalex'`
`extra()` | Get or set `extra.columns` attribute
`ploidy()` | Get `ploidy` attribute
`cbind()` | Merge loci and extra data columns from two or more class `'genalex'` objects
`rbind()` | Merge samples from two or more class `'genalex'` objects
`checkNullAlleles()` | Compare genotypes against a set of reference genotypes to check for potential null (nonamplifying) alleles
`writeGenepop()`| Write class `'genalex'` object in [Genepop](http://kimura.univ-montp2.fr/~rousset) format
`as.genetics()` | Convert class `'genalex'` object to a data frame with genotypes encoded using class `genotype` (unphased) or class `haplotype` (phased) from package `genetics`
`splitGenotypes()` | Split genotypes encoded as `101/107` into separate columns of a data frame, suitable for further use with `genalex()`
`joinGenotypes()` | Join class `'genalex'` (or other class) genotypes into a single column
`as.loci.genalex` | Extends the `as.loci` generic from the `pegas` package to convert class `'genalex'` to class `'loci'`
`as.genalex.loci` | Converts an object of class `'loci'` from the `pegas` package to class `'genalex'`


## Datasets

The package also provides two data sets as class `'genalex'` objects that can be loaded with `data`:

* `Qagr_adult_genotypes`: Coast live oak (*Quercus agrifolia*) adult microsatellite genotypes, holding 10-locus diploid microsatellite genotypes of 262 adult coast live oak trees from Sedgwick Reserve, Santa Barbara County, California, USA
* `Qagr_pericarp_genotypes`: Coast live oak (*Quercus agrifolia*) pericarp microsatellite genotypes, holding 10-locus diploid microsatellite genotypes of 568 pericarps (outer seed coats) from coast live oak acorns collected from 17 acorn woodpecker (*Melanerpes formicivorus*) granaries at Sedgwick Reserve, Santa Barbara County, California, USA

These data sets are also available at the Dryad Data Repository, <http://datadryad.org/resource/doi:10.5061/dryad.40kq7>.

Several papers have been published with these data.  If using them, please cite the original paper as well as the data:

Scofield DG, Smouse PE, Karubian J, Sork VL (2012) Use of alpha, beta, and gamma diversity measures to characterize seed dispersal by animals. *The American Naturalist* 180(6): 719-732.  <http://dx.doi.org/10.1086/668202>

Scofield DG, Smouse PE, Karubian J, Sork VL (2012) Data from: Use of alpha, beta, and gamma diversity measures to characterize seed dispersal by animals.  Dryad Digital Repository. <http://dx.doi.org/10.5061/dryad.40kq7>
