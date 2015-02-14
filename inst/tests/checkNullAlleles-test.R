# following example in
#
# http://stackoverflow.com/questions/8898469/is-it-possible-to-use-r-package-data-in-testthat-tests-or-run-examples
#
reffile <- system.file("extdata", "ref_genotypes.txt", 
                       package="readGenalex")
checkfile <- system.file("extdata", "check_genotypes.txt", 
                         package="readGenalex")
res1 <- checkNullAlleles(reffile, checkfile)
ref <- readGenalex(reffile)
check <- readGenalex(checkfile)
res2 <- checkNullAlleles(reffile, checkfile)
stopifnot(identical(res1, res2))
