checkNullAlleles
================

Compare genotypes against a set of reference genotypes and look for genotypes
that differ in a way consistent with the occurrence of a null allele, that is,
heterozygous in the reference and homozygous matching one of the alleles in
the heterozygous reference.

The function simply reports such cases; whether it represents an actual null
allele is for you to decide. You can check a single dataset for internal
consistency by using the same dataset as reference and comparison genotype.

Set `quiet=TRUE` if you don't want `checkNullAlleles` to print informational 
messages in addition to the null allele reports.

The reference and compare genotypes are in GenAlEx format, and are either read
in within `checkNullAlleles()` or are presented as dataframes read by
`readGenalex()`.  `readGenalex()` and its associated functions must be available,
and can be found at <https://github.com/douglasgscofield/readGenalex>.

Expected output is the following, with the loci having potental null alleles
surrounded by asterisks.

```R
source("readGenalex.R") 
source("checkNullAlleles.2.R")
checkNullAlleles("reference_genotypes.txt", "compare_genotypes.txt")
```

produces

~~~~
checkNullAlleles 0.2: 6 reference genotypes
checkNullAlleles 0.2: 16 comparison genotypes

comp8	1 compare	1/1	2/1	2/5	*3/3*	3/1 
ref6	1 ref	        1/1	2/1	2/5	*2/3*	3/1 

comp10	1 compare	2/3	1/1	*4/4*	3/3	6/1 
ref2	1 ref	        2/3	1/1	*2/4*	3/3	6/1 

comp12	1 compare	3/3	2/1	2/2	3/1	*3/3* 
ref4	1 ref	        3/3	2/1	2/2	3/1	*2/3* 
~~~~
