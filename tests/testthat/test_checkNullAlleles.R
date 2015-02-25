library(readGenalex)

#########################################
context("Testing checkNullAlleles() loading data")

reffile <- system.file("extdata", "ref_genotypes.txt", package="readGenalex")
checkfile <- system.file("extdata", "check_genotypes.txt", package="readGenalex")
ref <- readGenalex(reffile)
check <- readGenalex(checkfile)

test_that("we can find and load the files", {
    expect_match(reffile, "ref_genotypes.txt", fixed = TRUE)
    expect_match(checkfile, "check_genotypes.txt", fixed = TRUE)
    expect_equal(attr(ref, "locus.names"), attr(check, "locus.names"))
    expect_equal(attr(ref, "dataset.title"), "reference genotypes")
    expect_equal(attr(check, "dataset.title"), "Compare genotypes")
})

#########################################
context("Testing checkNullAlleles() correct callouts")

test_that("Null values identified in output and args passed through to printGenotype()", {
    expect_message(checkNullAlleles(ref, check), "6 reference genotypes")
    expect_message(checkNullAlleles(ref, check), "6 check genotypes")
    expect_output(checkNullAlleles(ref, check), "comp8\t1\tcheck\t1/1\t2/1\t2/5\t*3/3*\t3/1", fixed=TRUE)
    # label=
    expect_output(checkNullAlleles(ref, check), "comp8\t1\tcheck\t1/1\t2/1\t2/5\t*3/3*\t3/1", fixed=TRUE)
    # sep= should be passed through to printGenotype()
    expect_output(checkNullAlleles(ref, check, sep=" "), "comp10 1 check 2/3 1/1 *4/4* 3/3 6/1", fixed=TRUE)
    # other args should be passed through to printGenotype()
    expect_output(checkNullAlleles(ref, check, allele.sep="|", callout.char="#"), 
                  "comp12\t1\tcheck\t3|3\t2|1\t2|2\t3|1\t#3|3#", fixed=TRUE)
})

x <- checkNullAlleles(ref, check, quiet=TRUE)
y <- checkNullAlleles(ref, check, quiet=TRUE, mode="locus")

test_that("Null values identified in correct matrix with default mode='locus'", {
    expect_is(x, "matrix")
    expect_is(y, "matrix")
    expect_true(is.logical(x))
    expect_true(is.logical(y))
    expect_equal(x, y)
    expect_equal(attr(check, "n.samples"), nrow(x))
    expect_equal(attr(check, "n.loci"), ncol(x))
    expect_equal(sum(x), 3)
    expect_equal(as.vector(table(apply(x, 1, sum))), c(13, 3))
})

z <- checkNullAlleles(ref, check, quiet=TRUE, mode="column")

test_that("Null values identified in matrix with mode='column'", {
    expect_is(z, "matrix")
    expect_true(! is.logical(z))
    expect_true(is.integer(z))
    expect_equal(isTRUE(all.equal(x, z)), FALSE)
    expect_equal(dim(check), dim(z))
    expect_equal(dimnames(check), dimnames(z))
    expect_equal(sum(z), 6)
    expect_equal(as.vector(table(apply(z, 1, sum))), c(13, 3))
})

#########################################
context("Testing checkNullAlleles() quiet= and invisible return")

test_that("Return is invisible with quiet=FALSE", {
    # expect_output will capture the return value if quiet=TRUE, this is its header
    expect_output(checkNullAlleles(ref, check, quiet=TRUE), "        loc1  loc2  loc3  loc4  loc5\n")
    expect_equal(dim(checkNullAlleles(ref, check, quiet=TRUE)), dim(x))
    expect_equal(dimnames(checkNullAlleles(ref, check, quiet=TRUE)), dimnames(x))
    expect_equal(dim(checkNullAlleles(ref, check, quiet=TRUE, mode="column")), dim(z))
    expect_equal(dimnames(checkNullAlleles(ref, check, quiet=TRUE, mode="column")), dimnames(z))
})


