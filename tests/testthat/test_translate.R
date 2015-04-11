library(readGenalex)


#########################################
context("Setting up some test data for testing translations")

g1 <- data.frame(a = 11:13, a.2 = 14:16, b = 101:103, b.2 = 104:106)
x1 <- genalex(1:3, "snurf", g1)

g2 <- data.frame(a = 21:23, a.2 = 24:26, b = 201:203, b.2 = 204:206)
x2 <- genalex(4:6, "snirf", g2)

test_that("data set up correctly", {
    expect_is(x1, "genalex")
    expect_is(x2, "genalex")
})

context("Testing writeGenepop()")

test_that("writeGenepop() generates correct errors", {
    expect_error(writeGenepop(g1, file = ""), "g1 must be class 'genalex'")
    xx1 <- x1; names(xx1)[3] <- "x"
    expect_error(writeGenepop(xx1, file = ""), "xx1 class 'genalex' annotations are inconsistent, not writing")
    attr(xx1, "ploidy") <- 4
    xx1 <- as.genalex(xx1, force = TRUE)
    expect_error(writeGenepop(xx1, file = ""), "Genepop format can only encode haploid or diploid data")
    xx1 <- x1
    xx1[2, 3] <- xx1[2, 3] * 100
    expect_error(writeGenepop(xx1, file = ""), "One or more xx1 alleles require more than three characters to represent, which is not consistent with Genepop format")
})

test_that("writeGenalex() output content correct", {
    # dataset title, is first line
    expect_output(writeGenepop(x1, file = ""), "genalex")
    # locus names
    expect_output(writeGenepop(x1, file = ""), "a, *b\n")
    # population name, is third line
    expect_output(writeGenepop(x1, file = ""), "Pop snurf\n")
    expect_output(writeGenepop(x2, file = ""), "Pop snirf\n")
    # individual identifiers and genotypes
    expect_output(writeGenepop(x1, file = ""), "\n1 , 011014 101104\n")
    # genotypes
    expect_output(writeGenepop(x1, file = ""), " 012015 102105\n")
    # more than one population
    cb <- rbind(x1, x2)
    expect_output(writeGenepop(cb, file = ""), "Pop snurf\n")
    expect_output(writeGenepop(cb, file = ""), "Pop snirf\n")
    expect_output(writeGenepop(cb, file = ""), "\n5 , 022025 202205\n")
    # missing data
    cbna <- cb
    cbna[2, 3] <- NA
    expect_output(writeGenepop(cbna, file = ""), "2 , 000015 102105\n")
    cbna <- cb
    cbna[2, 4] <- NA
    expect_output(writeGenepop(cbna, file = ""), "2 , 012000 102105\n")
    cbna[2, 3] <- NA
    expect_output(writeGenepop(cbna, file = ""), "2 , 000000 102105\n")
})


test_that("writeGenepop() output alignment more-or-less correct", {
    # locus names
    expect_output(writeGenepop(x1, file = ""), "\n    a,     b\n")

    # 4 loci
    x3 <- x1
    names(x3)[3:6] <- c("c","c.2","d","d.2")
    x3 <- as.genalex(x3, force=TRUE)
    cbb <- cbind(x1, x3)
    expect_output(writeGenepop(cbb, file = ""), "\n    a,     b,     c,     d\n")
    expect_output(writeGenepop(cbb, file = ""), "\n1 , 011014 101104 011014 101104\n")

    # make one locus name longer than the default column width
    cb5 <- cbb
    names(cb5)[5] <- "longloc"
    cb5 <- as.genalex(cb5, force=TRUE)
    expect_output(writeGenepop(cb5, file = ""), "\n    a,     longloc,c,    d\n")
    expect_output(writeGenepop(cb5, file = ""), "\n1 , 011014 101104 011014 101104\n")
})


#########################################
if (suppressPackageStartupMessages(require("genetics", character.only = TRUE, 
                                           quietly = TRUE, warn.conflicts = FALSE))) {
    # only if genetics package is available

    #####################################
    context("Testing as.genetics.genalex()")
    expect_output(as.genetics(x1), "104/101", fixed = TRUE)
    expect_output(as.genetics(x1), "14/11", fixed = TRUE)
    expect_output(as.genetics(x1, phased = FALSE), "104/101", fixed = TRUE)
    expect_output(as.genetics(x1, phased = FALSE), "14/11", fixed = TRUE)
    expect_output(as.genetics(x1, phased = TRUE), "101/104", fixed = TRUE)
    expect_output(as.genetics(x1, phased = TRUE), "11/14", fixed = TRUE)
    # attributes updated
    xx1 <- x1; attr(xx1, "data.file.name") <- "placeholder"
    expect_match(attr(as.genetics(xx1), "data.file.name"), "^as\\.genetics\\(placeholder\\)$")
    expect_equal(attr(as.genetics(x1), "ploidy"), 2)
    expect_equal(attr(as.genetics(x1), "n.loci"), 2)
    expect_equal(attr(as.genetics(x1), "locus.columns"), setNames(3:4, c("a", "b")))
    expect_equal(attr(as.genetics(x1), "names"), c("sample", "pop", "a", "b"))
    # properly ignore sep= option
    expect_warning(as.genetics(x1, sep = "|"), "'sep' is applied inconsistently by package 'genetics' so is ignored here")
    expect_output(as.genetics(x1, sep = "/"), "2 snurf 15/12 105/102", fixed = TRUE)
    expect_output(as.genetics(x1, phased = TRUE, sep = "/"), "2 snurf 12/15 102/105", fixed = TRUE)
    # correct classes on converted columns
    expect_is(as.genetics(x1)[["a"]], "genotype")
    expect_is(as.genetics(x1)[["b"]], "genotype")
    expect_true(! inherits(as.genetics(x1)[["a"]], "haplotype"))
    expect_true(! inherits(as.genetics(x1)[["b"]], "haplotype"))
    expect_is(as.genetics(x1, phased = TRUE)[["a"]], "haplotype")
    expect_is(as.genetics(x1, phased = TRUE)[["b"]], "haplotype")
    # errors
    xx1 <- x1; attr(xx1, "ploidy") <- 4; xx1 <- as.genalex(xx1, force = TRUE)
    expect_error(as.genetics(xx1), "class 'genotype' can only encode diploid data")
    xx1 <- x1; attr(xx1, "ploidy") <- 1; xx1 <- as.genalex(xx1, force = TRUE)
    expect_error(as.genetics(xx1), "class 'genotype' can only encode diploid data")
    xx1 <- x1; attr(xx1, "locus.names") <- c("x","y")
    expect_error(as.genetics(xx1), "class 'genalex' annotations are inconsistent, not converting")
    # missing data
    xx1 <- x1; xx1[2, 4] <- NA
    expect_true(is.na(as.genetics(xx1)[2, 3]))
    xx1 <- x1; xx1[2, 3] <- NA
    expect_true(is.na(as.genetics(xx1)[2, 3]))

    #########################################
    context("Testing splitGenotypes()")

    xx1 <- as.genetics(x1)[, 3:4]
    expect_output(splitGenotypes(xx1), "a a.2")
    expect_output(splitGenotypes(xx1), "b b.2")
    expect_output(splitGenotypes(xx1), "2 15  12 105 102")
    expect_is(splitGenotypes(xx1), "data.frame")
    xx2 <- as.data.frame(lapply(lapply(xx1, as.character), 
                                sub, pattern = "/",replacement = "_"))
    expect_error(splitGenotypes(xx2), "loci not diploid: a b")
    expect_output(splitGenotypes(xx2, sep = "_"), "a a.2")
    expect_output(splitGenotypes(xx2, sep = "_"), "2 15  12 105 102")
}


#########################################
if (suppressPackageStartupMessages(require("pegas", character.only = TRUE, 
                                           quietly = TRUE, warn.conflicts = FALSE))) {
    # only if pegas package is available

    #########################################
    context("Testing joinGenotypes()")

    expect_output(joinGenotypes(x1), "11/14 101/104")
    expect_output(joinGenotypes(x1, sep = "_"), "11_14 101_104")
    expect_is(joinGenotypes(x1), "data.frame")
    expect_true(! inherits(joinGenotypes(x1), "genalex"))

    expect_error(joinGenotypes(as.data.frame(x1)), "both 'loci' and 'ploidy' must be supplied")
    expect_error(joinGenotypes(as.data.frame(x1), ploidy = 2), "both 'loci' and 'ploidy' must be supplied")
    expect_error(joinGenotypes(as.data.frame(x1), loci = 2), "both 'loci' and 'ploidy' must be supplied")
    expect_output(joinGenotypes(as.data.frame(x1), loci = c(3,5), ploidy = 2), "11/14 101/104")
    expect_output(joinGenotypes(as.data.frame(x1), loci = c(3,5), ploidy = 2, sep = "_"), "11_14 101_104")
    expect_is(joinGenotypes(as.data.frame(x1), loci = c(3,5), ploidy = 2), "data.frame")
    expect_true(! inherits(joinGenotypes(as.data.frame(x1), loci = c(3,5), ploidy = 2), "genalex"))

    #########################################
    context("Testing as.loci.genalex()")

    expect_output(as.loci(x1), "Allelic data frame: 3 individuals")
    expect_output(as.loci(x1), "2 loci")  # a and b
    expect_output(as.loci(x1), "1 additional variable")  # population
    expect_equal(rownames(as.loci(x1)), x1[, 1])  # sample names now rownames
    expect_equal(ncol(as.loci(x1)), 3)  # sample dropped
    expect_equal(names(as.loci(x1))[1], "population")  # sample dropped
    # pop column from character to factor and renamed population
    expect_match(paste(collapse = " ", names(x1)), " pop ")
    expect_is(x1[, 2], "character")
    expect_is(as.loci(x1)[, 1], "factor")
    expect_output(as.loci(x1)[1], "population")
    expect_match(paste(collapse = " ", names(as.loci(x1))), "population ")
    expect_match(attr(as.loci(x1), "pop.title"), "pop")
    # additional attribute for class 'loci' and updated data.file.name
    expect_equal(unname(attr(as.loci(x1), "locicol")), 2:3)
    expect_equal(attr(as.loci(x1), "locus.columns"), attr(as.loci(x1), "locicol"))
    xx1 <- x1; attr(xx1, "data.file.name") <- "placeholder"
    expect_match(attr(as.loci(xx1), "data.file.name"), "^as\\.loci\\(placeholder\\)$")
    # genotype columns are factors
    expect_is(as.loci(x1)[,2], "factor")
    expect_true(all(levels(as.loci(x1)[,2]) == c("11/14", "12/15", "13/16")))
    expect_is(as.loci(x1)[,3], "factor")
    expect_true(all(levels(as.loci(x1)[,3]) == c("101/104", "102/105", "103/106")))
}

