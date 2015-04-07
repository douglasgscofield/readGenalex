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
    # strange that 'genetics' reverses the loci
    expect_output(as.genetics(x1), "104/101", fixed = TRUE)
    expect_output(as.genetics(x1), "14/11", fixed = TRUE)
    expect_match(attr(as.genetics(x1), "data.file.name"), "as.genetics(", fixed = TRUE)
}
