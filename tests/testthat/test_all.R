library(readGenalex)


#########################################
context("Setting up some test data and testing genalex()")

g1 <- data.frame(a = 11:13, a.2 = 14:16, b = 101:103, b.2 = 104:106)
x1 <- genalex(1:3, "snurf", g1)

g2 <- data.frame(a = 21:23, a.2 = 24:26, b = 201:203, b.2 = 204:206)
x2 <- genalex(4:6, "snirf", g2)
g2.reord <- data.frame(b = 201:203, b.2 = 204:206, a = 21:23, a.2 = 24:26) 
x2.reord <- genalex(4:6, "snirf", g2.reord)

x1.x <- x1; extra(x1.x) <- as.data.frame(x2, complete = TRUE)
x2.x <- x2; extra(x2.x) <- as.data.frame(x1, complete = TRUE)
x2.reord.x <- x2.reord; extra(x2.reord.x) <- as.data.frame(x1, complete = TRUE)

test_that("genalex() works", {
    expect_true(nrow(x1) == attr(x1, "n.samples"))
    expect_true(ncol(x1) == 2 + attr(x1, "ploidy") * attr(x1, "n.loci"))
    expect_true(attr(x1, "pop.labels") == "snurf")
    expect_true(attr(x1, "n.loci") == attr(x2, "n.loci"))
    expect_true(attr(x2, "pop.labels") == "snirf")
    expect_true(attr(x1, "sample.title") == attr(x2, "sample.title"))
    expect_true(all(x1$sample == 1:3))
})


#########################################
context("Testing extra.genalex()")

test_that("extra.genalex() works", {
    expect_true(is.null(extra(x1)))
    expect_equal(extra(x1), attr(x1, "extra.columns"))
    expect_true(! is.null(extra(x1.x)))
    expect_equal(extra(x1.x), attr(x1.x, "extra.columns"))
})

test_that("extra<-.genalex() works", {
    xx <- x1; extra(xx) <- as.data.frame(x2, complete = TRUE)
    expect_true(! is.null(extra(xx)))
    expect_equal(extra(xx), attr(xx, "extra.columns"))
    expect_equal(rownames(extra(xx)), xx[, 1])
    # extra columns should have row names that match corresponding sample names
    expect_equal(rownames(attr(x1.x, "extra.columns")), x1.x[, 1])
    expect_equal(rownames(attr(x2.x, "extra.columns")), x2.x[, 1])
    expect_equal(rownames(attr(x2.reord.x, "extra.columns")), x2.reord.x[, 1])
    # extra columns should not be converted to factors
    expect_true(! any(sapply(attr(x1.x, "extra.columns"), is.factor)))
})


#########################################
context("Testing readGenalex()")

fa <- system.file("extdata/Qagr_adult_genotypes.txt", package = "readGenalex")
ga <- readGenalex(fa)
fp <- system.file("extdata/Qagr_pericarp_genotypes.txt", package = "readGenalex")
gp <- readGenalex(fp)

test_that("readGenalex() works", {
    # adult genotypes
    expect_true(attr(ga, "n.samples") == 262)
    expect_true(attr(ga, "n.pops") == 1)
    expect_true(attr(ga, "dataset.title") == "Q.agrifolia adults at Sedgwick")
    expect_true(all(attr(ga, "pop.labels") == c("Sedgwick")))
    expect_true(all(attr(ga, "pop.sizes") == c(262)))
    expect_match(attr(ga, "data.file.name"), "Qagr_adult_genotypes.txt", fixed = TRUE)
    # pericarp genotypes
    expect_true(attr(gp, "n.samples") == 568)
    expect_true(attr(gp, "n.pops") == 17)
    expect_true(attr(gp, "dataset.title") == "Sedgwick Q.agrifolia 2006 granary pericarps")
    expect_true(all(attr(gp, "pop.labels") == c("L0010", "L0031", "L0039", "L0048", "L0107",
                                                "L0108", "L0140", "L0151", "L0152", "L0162",
                                                "L0163", "L0922", "L0931", "L0938", "L0942",
                                                "L0990", "L0991")))
    expect_true(all(attr(gp, "pop.sizes") == c(12, 12, 49, 50, 50, 12, 12, 50, 50, 12, 50,
                                               7, 11, 50, 47, 50, 44)))
    expect_match(attr(gp, "data.file.name"), "Qagr_pericarp_genotypes.txt", fixed = TRUE)
})


#########################################
context("Testing is.genalex()")

test_that("is.genalex() works", {
    expect_true(is.genalex(x1))
    expect_true(is.genalex(x2.reord))
    expect_true(is.genalex(x2.reord.x))
    expect_equal(is.genalex(g1), FALSE)
    expect_equal(is.genalex(as.data.frame(x2)), FALSE)
    expect_equal(is.genalex(as.data.frame(x2, complete = FALSE)), FALSE)
})

test_that("is.genalex() 'force =' works", {
    x0 <- x1
    expect_true(is.genalex(x0))
    expect_true(is.genalex(x0, force = TRUE))
    expect_true(is.genalex(x0, force = TRUE, verbose = TRUE))
    # create inconsistent n.loci annotation
    attr(x0, "n.loci") <- attr(x0, "n.loci") + 2
    expect_true(! is.genalex(x0, force = TRUE))
    expect_match(capture.output(r <- is.genalex(x0, force = TRUE, verbose = TRUE)), "x and inferred attributes for x attributes do not match : n.loci")
    # create inconsistent annotation throughout
    attr(x0, "n.samples") <- attr(x0, "n.samples") - 1
    attr(x0, "sample.title") <- "silly sample title"
    attr(x0, "locus.columns") <- attr(x0, "locus.columns") + 1
    attr(x0, "locus.names") <- "my test population"
    attr(x0, "n.pops") <- attr(x0, "n.pops") + 10
    attr(x0, "pop.title") <- "my test population"
    attr(x0, "pop.labels") <- "Zyzyx"
    attr(x0, "pop.sizes") <- setNames(5, "Zyzyx")
    attr(x0, "dataset.title") <- "my test dataset"
    attr(x0, "data.file.name") <- "blahblah"
    expect_true(is.genalex(x0))
    expect_true(! is.genalex(x0, force = TRUE))
    expect_match(capture.output(r <- is.genalex(x0, force = TRUE, verbose = TRUE)), 
                "x and inferred attributes for x attributes do not match : n.samples sample.title n.loci locus.columns locus.names n.pops pop.labels pop.sizes pop.title")
    # create inconsistent ploidy
    x0 <- x1
    attr(x0, "ploidy") <- 3
    expect_true(is.genalex(x0))
    expect_error(is.genalex(x0, force = TRUE), "ploidy 3 inconsistent with apparent number of locus columns 4")
})



#########################################
context("Testing writeGenalex()")

test_that("writeGenalex() obeys options", {
    # sep= separator
    expect_output(writeGenalex(x1, file = ""), "\n2\tsnurf\t12\t15\t")
    expect_output(writeGenalex(x1, file = "", sep = ","), "\n1,snurf,11,14,")
    expect_output(writeGenalex(x1, file = "", sep = " "), "\n3 snurf 13 16 ")
    # eol= end of line
    expect_output(writeGenalex(x1, file = ""), "\t102\t105\n")
    expect_match(capture.output(writeGenalex(x1, file = "", eol = "\r")), "\t101\t104\r")
    expect_output(writeGenalex(x1, file = "", eol = " "), "\t101\t104 2\tsnurf\t")
    # na= na.character= NA
    x1.na <- x1; x1.na[2, 4] <- NA
    expect_output(writeGenalex(x1.na, file = ""), "\n2\tsnurf\t12\t0\t102\t")
    expect_output(writeGenalex(x1.na, file = "", na = "NA"), "\n2\tsnurf\t12\tNA\t102\t")
    expect_output(writeGenalex(x1.na, file = "", na = "."), "\n2\tsnurf\t12\t.\t102\t")
    x1.na[2, 2] <- NA
    # we expect population sizes not to match the annotation, because we set a pop to NA
    expect_error(writeGenalex(x1.na, file = ""), "x1.na class 'genalex' annotations are inconsistent, not writing")
    expect_output(writeGenalex(x1.na, file = "", check.annotation = FALSE), "\n2\t\t12\t0\t102\t")
    expect_output(writeGenalex(x1.na, file = "", na.character = "-missing-", check.annotation = FALSE), "\n2\t-missing-\t12\t0\t102\t")
    expect_output(writeGenalex(x1.na, file = "", na = "NA", na.character = ".", check.annotation = FALSE), "\n2\t.\t12\tNA\t102\t")
    # quote= quote character values
    expect_output(writeGenalex(x1, file = "", quote = TRUE), "\n\"2\"\t\"snurf\"\t12\t15\t")
    # still missing extra.columns stuff
})


#########################################
if (suppressPackageStartupMessages(require("XLConnect", character.only = TRUE, 
                                           quietly = TRUE, warn.conflicts = FALSE))) {
    # only of XLConnect package is available

    #####################################
    context("Testing readGenalexExcel()")

    fx <- system.file("extdata/Qagr_genotypes.xlsx", package = "readGenalex")
    xp <- readGenalexExcel(fx, worksheet = 1)
    xa <- readGenalexExcel(fx, worksheet = "Qagr_adult_genotypes")

    test_that("readGenalexExcel() works", {
        expect_match(attr(xp, "data.file.name"), "Qagr_genotypes.xlsx(1)", fixed = TRUE)
        expect_match(attr(xa, "data.file.name"), "Qagr_genotypes.xlsx(Qagr_adult_genotypes)", fixed = TRUE)
        lxp <- xp; lxa <- xa; lgp <- gp; lga <- ga
        attr(lxp, "data.file.name") <- attr(lxa, "data.file.name") <- 
            attr(lgp, "data.file.name") <- attr(lga, "data.file.name") <- ""
        expect_equal(lxp, lgp)
        expect_equal(lxa, lga)
    })
    test_that("readGenalexExcel() adds names to locus.columns", {
        expect_equal(attr(xp, "locus.names"), names(attr(xp, "locus.columns")))
    })

    #####################################
    context("Testing writeGenalexExcel()")

    xlfile <- "test.xlsx"
    unlink(xlfile)

    test_that("writeGenalexExcel() obeys options", {
        sheet <- "sheet1"
        writeGenalexExcel(x1, xlfile, sheet)
        x1.in <- readGenalexExcel(xlfile, sheet)
        df.x1 <- attr(x1, "data.file.name"); df.x1.in <- attr(x1.in, "data.file.name")
        attr(x1, "data.file.name") <- attr(x1.in, "data.file.name") <- ""
        expect_equal(x1, x1.in)
        # we expect not to be able to overwrite the sheet
        expect_error(writeGenalexExcel(x1, xlfile, sheet), "worksheet sheet1 already exists in workbook test.xlsx, will not overwrite")
        # we expect population sizes not to match the annotation, because we set a pop to NA
        x1.na <- x1; x1.na[2, 4] <- NA
        x1.na[2, 2] <- NA
        expect_error(writeGenalexExcel(x1.na, xlfile, sheet), "x1.na class 'genalex' annotations are inconsistent, not writing")
        expect_error(writeGenalexExcel(x1.na, xlfile, sheet, check.annotation = FALSE), "worksheet sheet1 already exists in workbook test.xlsx, will not overwrite")
        # write a proper NA
        x1.na <- x1; x1.na[2, 4] <- NA
        expect_error(writeGenalexExcel(x1.na, xlfile, sheet, na = "", overwrite = TRUE), "should be one of ")
        writeGenalexExcel(x1.na, xlfile, sheet, na = "0", overwrite = TRUE)
        # do we read this back in correctly?
        x1.na.in <- readGenalexExcel(xlfile, sheet)
        attr(x1.na, "data.file.name") <- attr(x1.na.in, "data.file.name") <- ""
        expect_equal(x1.na, x1.na.in)
        # multiple sheets
        writeGenalexExcel(x1, xlfile, sheet, overwrite = TRUE)
        sheet <- "sheet2"
        writeGenalexExcel(x1, xlfile, sheet)
        writeGenalexExcel(x1.na, xlfile, "sheet3")
        x.1 <- readGenalexExcel(xlfile, 1)
        x.2 <- readGenalexExcel(xlfile, 2)
        attr(x.1, "data.file.name") <- attr(x.2, "data.file.name") <- ""
        expect_equal(x.1, x.2)
    })

}

unlink(xlfile)

#########################################
context("Testing summary.genalex()")

test_that("summary.genalex summarises pops etc., genotypes, and extra data", {
    s <- capture.output(summary(x1))
    expect_output(s, "Number of samples: 3")
    expect_output(s, "Number of loci: 2")
    expect_output(s, "Min.   :104.0")
    expect_that(s, not(prints_text("Summary of extra.columns data frame:")))
    s.x <- capture.output(summary(x1.x))
    expect_output(s.x, "Summary of extra.columns data frame:")
    expect_output(s.x, "Min.   :204.0")
})


#########################################
context("Testing printGenotype()")

test_that("printGenotype prints selected lines and calls out genotypes", {
    s <- capture.output(printGenotype(x1, rows = 3, callout.locus = "b"))
    expect_equal(s, "3 snurf 13/16 *103/106*")
    #expect_match(s, "3 snurf 13/16 *103/106*", fixed = TRUE)
    s2 <- capture.output(printGenotype(x2, rows = 1, callout.locus = "a", allele.sep = "|"))
    expect_equal(s2, "4 snirf *21|24* 201|204")
    s3 <- paste(collapse = ":", capture.output(printGenotype(x2, allele.sep = "|")))
    expect_equal(s3, "4 snirf 21|24 201|204:5 snirf 22|25 202|205:6 snirf 23|26 203|206")
})


#########################################
context("Testing getLocusColumns()")

test_that("getLocusColumns works with 1 or more loci and regardless of ploidy", {
    expect_equal(getLocusColumns(x1, c("a","b")), getLocusColumns(x2, c("a","b")))
    expect_equal(getLocusColumns(x2, "b"), 5:6)
    expect_equal(getLocusColumns(x2.reord, "b"), 3:4)
})


#########################################
context("Testing reorderLoci()")

test_that("reorderLoci checks for same numbers of loci and handles several permutations", {
    locn <- attr(x1, "locus.names")
    expect_is(reorderLoci(x1, locn), "genalex")
    expect_is(reorderLoci(x1, locn), "data.frame")
    r1 <- reorderLoci(x1, rev(locn))
    expect_is(reorderLoci(r1, locn), "genalex")
    expect_is(reorderLoci(r1, locn), "data.frame")
    expect_equal(x1, reorderLoci(r1, locn))
    expect_equal(x1, reorderLoci(reorderLoci(x1, rev(locn)), locn))
    expect_error(reorderLoci(x1, c(locn, locn)), 
        "loci must appear only once")
    expect_error(reorderLoci(x1, c(locn, "xxx")),
        "reorder list must contain all existing loci")
    expect_error(reorderLoci(x1, c("xxx", locn)),
        "reorder list must contain all existing loci")
})


#########################################
context("Testing replaceLocus()")

pl.1 <- replaceLocus(x1, "b", data.frame(d=101:103, d.2=104:106))
pl.2 <- replaceLocus(x1, "b", matrix(101:106, byrow=FALSE, nrow=3, ncol=2))

test_that("replaceLocus()", {
    # replace with its own data
    expect_is(pl.1, "genalex")
    expect_is(pl.1, "data.frame")
    expect_equal(pl.1, x1)
    expect_is(pl.2, "genalex")
    expect_is(pl.2, "data.frame")
    expect_equal(pl.2, x1)
})


#########################################
context("Testing getLocus()")

test_that("getLocus()", {
    # return is a data.frame
    l1 <- getLocus(pl.1, "a")
    expect_equal(class(getLocus(pl.1, "a")), "data.frame")
    expect_equal(class(l1), "data.frame")
    l2 <- getLocus(pl.1, "b")
    expect_equal(l2, data.frame(b=101:103, b.2=104:106))
})


#########################################
context("Testing addLocus()")

l1 <- data.frame(y=1001:1003, y.2=104:106)
l2 <- data.frame(z=1001:1003, z.2=104:106)
l3 <- data.frame(z=1001:1003, z.2=104:106, z.3=595:597)
l4 <- data.frame(y=101:103, y.2=c("a","b","c"))
l5 <- data.frame(y=c("a","b","c"), y.2=101:103)
l6 <- data.frame(a=1001:1003, a.2=104:106)
l7 <- data.frame(b=301:303, b.2=404:406)

test_that("addLocus()", {
    # return is class genalex
    expect_is(addLocus(x1, l1), "genalex")
    expect_is(addLocus(x1, l1), "data.frame")
    expect_true(is.genalex(addLocus(x1, l1)))
    expect_true(is.genalex(addLocus(x1, l1), force = TRUE))
    # error if wrong number of rows
    expect_error(addLocus(x1, l1[1:2, ]), "must have the same number of samples")
    # error if not numeric
    expect_error(addLocus(x1, l4), "genotype data must be numeric")
    expect_error(addLocus(x1, l5), "genotype data must be numeric")
    # error if ploidy apparently wrong
    expect_error(addLocus(x1, l3), "appear not to match ploidy")
    # error if locus already exists
    expect_error(addLocus(x1, l6), "already contains loci: a")
    expect_error(addLocus(x1, l7), "already contains loci: b")
    expect_error(addLocus(x1, cbind(l6, l7)), "already contains loci: a b")
    # adds one locus
    a1 <- addLocus(x1, l1)
    expect_equal(attr(a1, "locus.names"), c("a", "b", "y"))
    # adds multiple loci
    a2 <- addLocus(x1, cbind(l1, l2))
    expect_equal(attr(a2, "locus.names"), c("a", "b", "y", "z"))
    a3 <- addLocus(x1, l1)
    a3 <- addLocus(a3, l2)
    attr(a2, "data.file.name") <- attr(a3, "data.file.name") <- ""
    expect_equal(a2, a3)
    # drop and add locus ??
    expect_error(addLocus(x1, getLocus(x1, "b")), "already contains loci: b")
    a4 <- x1
    a4 <- addLocus(dropLocus(x1, "a"), getLocus(x1, "a"))
    a4 <- addLocus(dropLocus(x1, "b"), getLocus(x1, "b"))
    attr(a4, "data.file.name") <- attr(x1, "data.file.name") <- ""
    expect_equal(a4, x1)
})


#########################################
context("Testing dropLocus()")


test_that("dropLocus()", {
    p1 <- dropLocus(x1, "a")
    expect_error(dropLocus(x1, c("xxx")),
        "locus not present")
    expect_equal(dropLocus(x1, c("xxx", "a"), quiet = TRUE), p1)
    expect_error(dropLocus(x1, c("xxx", "a")),
        "locus not present")
    expect_error(dropLocus(x1, c("a","xxx"), quiet = FALSE),
        "locus not present")
    expect_is(p1, "genalex")
    p1.2 <- genalex(1:3, "snurf", g1[, 3:4], ploidy = 2)
    attr(p1, "data.file.name") <- attr(p1.2, "data.file.name") <- ""
    expect_equal(p1, p1.2)
    expect_equal(names(p1), c("sample", "pop", "b", "b.2"))
    expect_equal(attr(p1, "n.loci"), 1)
    expect_equal(attr(p1, "ploidy"), 2)
    expect_equal(attr(p1, "locus.columns"), setNames(3, "b"))
})


#########################################
context("Testing getPopulation()")

test_that("getPopulation()", {
    # return is class genalex
    l1 <- getPopulation(x1, "snurf")
    expect_equal(nrow(x1), nrow(l1))
    expect_is(l1, 'genalex')
    expect_is(l1, 'data.frame')
    expect_match(attr(l1, 'data.file.name'), 'pop==', fixed = TRUE)

    xx <- rbind(x1, x2)
    l1 <- getPopulation(xx, "snirf")
    l2 <- getPopulation(xx, "snurf")
    expect_equal(nrow(xx), nrow(l1) + nrow(l2))
    expect_is(l1, 'genalex')
    expect_is(l1, 'data.frame')
})


#########################################
context("Testing ploidy()")

test_that("ploidy()", {
    # return is a data.frame
    expect_equal(ploidy(x1), 2)
})


#########################################
context("Testing reducePloidy()")

test_that("reducePloidy()", {
    p1 <- reducePloidy(x1, 1)
    expect_is(p1, "genalex")
    expect_equal(reducePloidy(x1, 2), x1)
    expect_error(reducePloidy(x1, 3),
        "greater than existing ploidy")
    p1.4 <- genalex(1:3, "snurf", g1, ploidy = 4)
    expect_error(reducePloidy(p1.4, 2),
        "can't currently handle new.ploidy other than 1, existing ploidy other than 2")
    p1.2 <- genalex(1:3, "snurf", g1[, c(1,3)], ploidy = 1)
    attr(p1, "data.file.name") <- attr(p1.2, "data.file.name") <- ""
    expect_equal(p1, p1.2)
    expect_equal(names(p1), c("sample", "pop", "a", "b"))
    expect_equal(attr(p1, "ploidy"), 1)
    expect_equal(attr(p1, "locus.columns"), setNames(3:4, c("a", "b")))
})

#########################################
context("Testing genalex()")

test_that("genalex() object identities correct", {
    expect_is(x1,        "genalex")
    expect_is(x1,        "data.frame")
    expect_is(x1.x,      "genalex")
    expect_is(x1.x,      "data.frame")
})

nnn <- list(title = "ttt", sample = "sss", pop = "ppp")
rb.n <- rbind(x1, x2, names = nnn)

test_that("genalex correctly applies names", {
    # defaults
    expect_equal(attr(x1, "dataset.title"), "genalex")
    expect_equal(attr(x1, "sample.title"), "sample")
    expect_equal(attr(x1, "pop.title"), "pop")
    # names argument
    xn <- genalex(1:3, "snoof", g1, names = nnn)
    expect_equal(attr(xn, "dataset.title"), "ttt")
    expect_equal(attr(xn, "sample.title"), "sss")
    expect_equal(attr(xn, "pop.title"), "ppp")
    # partial
    pnn <- nnn; pnn$title <- pnn$sample <- NULL
    xpn <- genalex(1:3, "snoff", g1, names = pnn)
    expect_equal(attr(xpn, "dataset.title"), "genalex")
    expect_equal(attr(xpn, "sample.title"), "sample")
    expect_equal(attr(xpn, "pop.title"), "ppp")
})


test_that("genalex() generates errors for data inconsistencies", {
    expect_error(rbind(x1, x1),
        "sample names must be unique")
    expect_error(genalex(7:8, "shortsample", g1),
        "'samples' and 'genotypes' must have the same length")
    ga <- data.frame(a=11:13, a.2=14:16, b=c("a","b","c"), b.2=rep("d",3))
    expect_error(genalex(1:3, "nonnumeric", ga),
        "genotype data must be numeric")
    g3 <- data.frame(a = 21:23, a.2 = 24:26, b = 201:203)
    expect_error(genalex(7:9, "missing1", g3, ploidy = 2),
        "'genotypes' must have a number of columns divisible by ploidy")
    g4 <- data.frame(a=21:23, a.2=24:26, b=201:203, b.2=204:206, c=1:3)
    expect_error(genalex(7:9, "added1", g4, ploidy = 2),
        "'genotypes' must have a number of columns divisible by ploidy")
})



#########################################
context("Testing as.genalex()")

g1 <- data.frame(a = 11:13, a.2 = 14:16, b = 101:103, b.2 = 104:106)
df1 <- cbind(sampxx = 1:3, popyy = c("a","a","b"), g1)
gdf1 <- as.genalex(df1)
gdf1.n <- as.genalex(df1, names = nnn)
gdf1.p1 <- as.genalex(df1, ploidy = 1)

test_that("as.genalex object identities correct", {
    expect_is(gdf1,    "genalex")
    expect_is(gdf1,    "data.frame")
    expect_is(gdf1.n,  "genalex")
    expect_is(gdf1.n,  "data.frame")
    expect_is(gdf1.p1, "genalex")
    expect_is(gdf1.p1, "data.frame")
})

test_that("as.genalex correctly returns when given class 'genalex'", {
    expect_equal(as.genalex(gdf1), gdf1)
    expect_equal(as.genalex(gdf1.n), gdf1.n)
    expect_equal(as.genalex(gdf1.p1), gdf1.p1)
})

test_that("as.genalex correctly determines default attributes", {
    expect_equal(attr(gdf1, "n.pops"), 2)
    expect_equal(attr(gdf1, "pop.sizes"), setNames(c(2, 1), c("a", "b")))
    expect_equal(attr(gdf1, "locus.names"), c("a", "b"))
    expect_equal(attr(gdf1, "locus.columns"), setNames(c(3, 5), c("a", "b")))
    expect_equal(attr(gdf1, "ploidy"), 2)
    expect_equal(attr(gdf1, "n.loci"), 2)
    # ploidy = 1
    expect_equal(attr(gdf1.p1, "ploidy"), 1)
    expect_equal(attr(gdf1.p1, "n.loci"), 4)
    expect_equal(attr(gdf1, "pop.sizes"), setNames(c(2, 1), c("a", "b")))
    expect_equal(attr(gdf1.p1, "locus.names"), c("a", "a.2", "b", "b.2"))
    expect_equal(attr(gdf1.p1, "locus.columns"), setNames(3:6, c("a", "a.2", "b", "b.2")))

    old <- as.data.frame(gdf1)
    attr(old, "genetic.data.format") <- "genalex"
    expect_warning(old.1 <- as.genalex(old, ploidy = 1),
        "args ignored, converting pre-1.0 readGenalex data frame")
    expect_warning(old.2 <- as.genalex(old, ploidy = 2),
        "args ignored, converting pre-1.0 readGenalex data frame")
    expect_equal(old.1, old.2)
    expect_true(! isTRUE(all.equal(old, old.2, check.attributes = TRUE)))
    expect_true(isTRUE(all.equal(old, old.2, check.attributes = FALSE)))

    expect_error(as.genalex(df1[, 1:2]),
        "not enough columns for class 'genalex'")
    expect_error(as.genalex(cbind(df1[, 1:2], loc=rep("a",3)), ploidy = 1),
        "genotype data must be numeric")
})

test_that("as.genalex correctly applies names", {
    # defaults and column names
    expect_equal(attr(gdf1, "dataset.title"), "genalex")
    expect_equal(attr(gdf1, "sample.title"), "sampxx")
    expect_equal(attr(gdf1, "pop.title"), "popyy")
    # names argument
    expect_equal(attr(gdf1.n, "dataset.title"), "ttt")
    expect_equal(attr(gdf1.n, "sample.title"), "sss")
    expect_equal(attr(gdf1.n, "pop.title"), "ppp")
    # partial
    pnn <- nnn; pnn$title <- pnn$sample <- NULL
    gdf1.p <- as.genalex(df1, names = pnn)
    expect_equal(attr(gdf1.p, "dataset.title"), "genalex")
    expect_equal(attr(gdf1.p, "sample.title"), "sampxx")
    expect_equal(attr(gdf1.p, "pop.title"), "ppp")
})

test_that("as.genalex(..., force = TRUE) works", {
    gdf1.n <- as.genalex(gdf1.n)
    x0 <- as.genalex(gdf1.n, force = TRUE)
    expect_equal(gdf1.n, x0)
    attr(x0, "n.samples") <- attr(x0, "n.samples") - 1
    attr(x0, "sample.title") <- "sillier sample title"
    attr(x0, "n.loci") <- attr(x0, "n.loci") + 2
    attr(x0, "locus.columns") <- attr(x0, "locus.columns") + 1
    attr(x0, "locus.names") <- "my test population"
    attr(x0, "pop.title") <- "my test population"
    attr(x0, "pop.sizes") <- setNames(5, "Zyzyx")
    expect_true(is.genalex(x0))
    expect_true(! is.genalex(x0, force = TRUE))
    # assign r here to capture the FALSE so expect_match doesn't see it
    expect_match(capture.output(r <- is.genalex(x0, force = TRUE, verbose = TRUE)),
                 "n.samples sample.title n.loci locus.columns locus.names pop.sizes pop.title")
    expect_that(gdf1.n, not(equals(x0)))
    # now restore it
    x0 <- as.genalex(x0, force = TRUE)
    expect_equal(gdf1.n, x0)
})

test_that("as.genalex.data.frame", {
    xy <- as.genalex(as.data.frame(x1))
    expect_equal(attr(xy, "data.file.name"),
                 "as.genalex.data.frame(as.data.frame(x1))") 
    xz <- as.genalex(as.data.frame(x1, complete = TRUE))
    expect_equal(attr(xz, "data.file.name"),
                 "as.genalex.data.frame(as.data.frame(x1, complete = TRUE))") 
    # reset mismatched attribute to check everything else
    attr(xy, "data.file.name") <- attr(xz, "data.file.name") <- ""
    expect_equal(xy, xz)
})

#########################################
context("Testing rbind.genalex()")

rb <- rbind(x1, x2)
rb.reord <- rbind(x1, x2.reord)
rbx <- rbind(x1.x, x2.x)
rbx.reord <- rbind(x1.x, x2.reord.x)

test_that("rbind.genalex() object identities correct", {
    expect_is(rb,        "genalex")
    expect_is(rb,        "data.frame")
    expect_is(rb.reord,  "genalex")
    expect_is(rb.reord,  "data.frame")
    expect_is(rbx,       "genalex")
    expect_is(rbx,       "data.frame")
    expect_is(rbx.reord, "genalex")
    expect_is(rbx.reord, "data.frame")
})

test_that("rbind dispatches to rbind.data.frame if one is data.frame", {
    x1.df <- as.data.frame(x1)
    expect_false("genalex" %in% class(rbind(x1.df, x2)))
    expect_false("genalex" %in% class(rbind(x2, x1.df)))
})

test_that("rbind.genalex locus ordering and reordering", {
    attr(rb, "data.file.name") <- attr(rb.reord, "data.file.name") <- "placeholder"
    expect_equal(rb, rb.reord)
    attr(rbx, "data.file.name") <- attr(rbx.reord, "data.file.name") <- "placeholder"
    expect_equal(rbx, rbx.reord)
})

test_that("rbind.genalex generates errors for data inconsistencies", {
    expect_error(rbind(x1, x1),
        "sample names must be unique")
    expect_error(rbind(x1.x, x2),
        "all arguments must either have or lack extra columns")
    expect_error(rbind(x2, x1.x),
        "all arguments must either have or lack extra columns")
    x1.alt <- matrix(1, ncol = ncol(x2), nrow = 3)
    expect_error(rbind(x1.alt, x2),
        "all arguments must be class 'genalex'")
    expect_error(rbind(x2, x1.alt),
        "all arguments must be class 'genalex'")
    x1.rp <- reducePloidy(x1, 1)
    expect_error(rbind(x1.rp, x2),
        "all arguments must have the same ploidy")
    expect_error(rbind(x2, x1.rp),
        "all arguments must have the same ploidy")
    x1.dl <- dropLocus(x1, "b")
    expect_error(rbind(x1.dl, x2),
        "all arguments must contain the same loci")
    expect_error(rbind(x2, x1.dl),
        "all arguments must contain the same loci")
})

nnn <- list(title = "ttt", sample = "sss", pop = "ppp")
rb.n <- rbind(x1, x2, names = nnn)

test_that("rbind.genalex correctly applies names", {
    expect_equal(attr(rb, "dataset.title"), attr(x1, "dataset.title"))
    expect_equal(attr(rb, "sample.title"), attr(x1, "sample.title"))
    expect_equal(attr(rb, "pop.title"), attr(x1, "pop.title"))
    expect_equal(attr(rb.reord, "dataset.title"), attr(x1, "dataset.title"))
    expect_equal(attr(rb.reord, "sample.title"), attr(x1, "sample.title"))
    expect_equal(attr(rb.reord, "pop.title"), attr(x1, "pop.title"))
    expect_equal(attr(rb.n, "dataset.title"), "ttt")
    expect_equal(attr(rb.n, "sample.title"), "sss")
    expect_equal(attr(rb.n, "pop.title"), "ppp")
    expect_equal(attr(rb, "data.file.name"), "rbind(x1, x2)")
    expect_equal(attr(rb.n, "data.file.name"), "rbind(x1, x2, names = nnn)")
})

#########################################
context("Testing cbind.genalex()")

test_that("cbind.genalex() generates errors about mismatches", {
    expect_error(cbind(x1, x2), "all arguments must contain the same samples")
    x2.2 <- x2; x2.2[,1] <- x1[,1]; x2.2 <- as.genalex(x2.2, force = TRUE)
    expect_error(cbind(x1, x2.2), "population membership for samples in argument 2 do not match those in the first argument")
    x2.2[,2] <- x1[,2]; x2.2 <- as.genalex(x2.2, force = TRUE)
    expect_error(cbind(x1, x2.2), "genotypes for duplicated locus a in argument 2 do not match those in argument 1")
})

# complete duplicate of x1
x5 <- x1
# duplicate one column, first position
g6 <- data.frame(a = 11:13, a.2 = 14:16, x = 101:103, x.2 = 104:106)
x6 <- genalex(1:3, "snurf", g6)
# duplicate one column, second position
g7 <- data.frame(x = 11:13, x.2 = 14:16, b = 101:103, b.2 = 104:106)
x7 <- genalex(1:3, "snurf", g7)
# duplicate one column, second position in x1 but first in x8
g8 <- data.frame(b = 101:103, b.2 = 104:106, x = 11:13, x.2 = 14:16) 
x8 <- genalex(1:3, "snurf", g8)
# duplicate one column, only column in x9
g9 <- data.frame(b = 101:103, b.2 = 104:106)
x9 <- genalex(1:3, "snurf", g9)
attr(x1, "data.file.name") <- attr(x5, "data.file.name") <- attr(x6, "data.file.name") <- attr(x7, "data.file.name") <- attr(x8, "data.file.name") <- attr(x9, "data.file.name") <- "placeholder"


# all new columns
ga <- data.frame(c = 21:23, c.2 = 24:26, d = 201:203, d.2 = 204:206)
xa <- genalex(1:3, "snurf", ga)


test_that("cbind.genalex() assembles values and ignores duplicate columns correctly", {
    cb <- cbind(x1, x5)
    attr(cb, "data.file.name") <- "placeholder"
    expect_equal(x1, cb)
    expect_equal(x5, cb)
    cb <- cbind(x1, x6)
    attr(cb, "data.file.name") <- "placeholder"
    expect_true(is.genalex(cb, force = TRUE))
    expect_is(cb, "data.frame")
    expect_equal(attr(cb, "locus.names"), c("a", "b", "x"))
    cb <- cbind(x1, x7)
    attr(cb, "data.file.name") <- "placeholder"
    expect_true(is.genalex(cb, force = TRUE))
    expect_is(cb, "data.frame")
    expect_equal(attr(cb, "locus.names"), c("a", "b", "x"))
    cb <- cbind(x1, x8)
    attr(cb, "data.file.name") <- "placeholder"
    expect_true(is.genalex(cb, force = TRUE))
    expect_is(cb, "data.frame")
    expect_equal(attr(cb, "locus.names"), c("a", "b", "x"))
    cb <- cbind(x1, x9)
    attr(cb, "data.file.name") <- "placeholder"
    expect_true(is.genalex(cb, force = TRUE))
    expect_is(cb, "data.frame")
    expect_equal(attr(cb, "locus.names"), c("a", "b"))
    cb <- cbind(x1, xa)
    attr(cb, "data.file.name") <- "placeholder"
    expect_true(is.genalex(cb, force = TRUE))
    expect_is(cb, "data.frame")
    expect_equal(attr(cb, "locus.names"), c("a", "b", "c", "d"))
    cb <- cbind(x9, x1, xa, x6)
    attr(cb, "data.file.name") <- "placeholder"
    expect_true(is.genalex(cb, force = TRUE))
    expect_is(cb, "data.frame")
    expect_equal(attr(cb, "locus.names"), c("b", "a", "c", "d", "x"))
})

test_that("cbind.genalex() reorders sample rows correctly", {
    x5 <- x5[c(2,3,1), ]
    cb <- cbind(x1, x5)
    attr(cb, "data.file.name") <- "placeholder"
    expect_equal(x1, cb)
    xa <- xa[c(3,2,1), ]
    cb <- cbind(x1, xa)
    attr(cb, "data.file.name") <- "placeholder"
    expect_true(is.genalex(cb, force = TRUE))
    expect_is(cb, "data.frame")
    expect_equal(attr(cb, "locus.names"), c("a", "b", "c", "d"))
})

#x1.x <- x1; attr(x1.x, "extra.columns") <- as.data.frame(x2, complete = TRUE)
#x2.x <- x2; attr(x2.x, "extra.columns") <- as.data.frame(x1, complete = TRUE)
#x2.reord.x <- x2.reord; attr(x2.reord.x, "extra.columns") <- as.data.frame(x1, complete = TRUE)
x5.x <- x1.x[c(2,3,1), ]
extra(x5.x) <- attr(x5.x, "extra.columns")[c(2,3,1), ]
x8.x <- x8
extra(x8.x) <- data.frame(s = x8[, 1], xxxx = 1:3 * 1017)

attr(x1.x, "data.file.name") <- attr(x5.x, "data.file.name") <- "placeholder"

test_that("cbind.genalex() handles extra columns correctly", {
    cb <- cbind(x1.x, x5.x)
    attr(cb, "data.file.name") <- "placeholder"
    expect_equal(x1.x, cb)
    expect_true(is.genalex(cb, force = TRUE))
    expect_is(cb, "data.frame")
    expect_equal(attr(cb, "locus.names"), c("a", "b"))
    # row names of extra columns should match sample names
    expect_equal(rownames(attr(cb, "extra.columns")), x1.x[, 1])

    cb <- cbind(x5.x, x1.x)
    attr(cb, "data.file.name") <- "placeholder"
    expect_equal(x5.x, cb)
    expect_true(is.genalex(cb, force = TRUE))
    expect_is(cb, "data.frame")
    expect_equal(attr(cb, "locus.names"), c("a", "b"))
    expect_equal(names(attr(cb, "extra.columns")), c("sample", "pop", "a", "a.2", "b", "b.2"))
    # row names of extra columns should match sample names
    expect_equal(rownames(attr(cb, "extra.columns")), x5.x[, 1])

    # adds extra columns correctly if second arg doesn't have them
    cb <- cbind(x1.x, x5)
    attr(cb, "data.file.name") <- "placeholder"
    expect_equal(x1.x, cb)
    expect_true(is.genalex(cb, force = TRUE))
    expect_is(cb, "data.frame")
    expect_equal(attr(cb, "locus.names"), c("a", "b"))

    # adds extra columns correctly if first arg doesn't have them
    cb <- cbind(x5, x1.x)
    attr(cb, "data.file.name") <- "placeholder"
    expect_equal(x1.x, cb)
    expect_true(is.genalex(cb, force = TRUE))
    expect_is(cb, "data.frame")
    expect_equal(attr(cb, "locus.names"), c("a", "b"))
    names(attr(x5.x, "extra.columns"))[1] <- "sampsamp"
    cb <- cbind(x1.x, x5.x)
    attr(cb, "data.file.name") <- "placeholder"
    expect_equal(names(attr(cb, "extra.columns"))[1], "sample")
    expect_equal(names(extra(cb))[1], "sample")
    expect_equal(names(extra(cb))[7], "sampsamp")
    cb <- cbind(x5.x, x1.x)
    attr(cb, "data.file.name") <- "placeholder"
    expect_equal(names(attr(cb, "extra.columns"))[1], "sampsamp")

    # drops same-name columns and doesn't inspect contents
    attr(x5.x, "extra.columns")[, 2] <- "foo"
    cb <- cbind(x1.x, x5.x)
    attr(cb, "data.file.name") <- "placeholder"
    #expect_equal(x1.x, cb)  # not true, rows in different order
    expect_true(is.genalex(cb, force = TRUE))

    # presents total at end correctly
    cb <- cbind(x1.x, x8.x)
    nm <- names(attr(cb, "extra.columns"))
    expect_equal(nm, c("sample", "pop", "a", "a.2", "b", "b.2", "s", "xxxx"))
})
