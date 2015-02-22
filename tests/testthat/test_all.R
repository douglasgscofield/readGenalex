library(readGenalex)

#########################################
context("Setting up some test data")

g1 <- data.frame(a = 11:13, a.2 = 14:16, b = 101:103, b.2 = 104:106)
x1 <- genalex(1:3, "snurf", g1)

g2 <- data.frame(a = 21:23, a.2 = 24:26, b = 201:203, b.2 = 204:206)
x2 <- genalex(4:6, "snirf", g2)
g2.reord <- data.frame(b = 201:203, b.2 = 204:206, a = 21:23, a.2 = 24:26) 
x2.reord <- genalex(4:6, "snirf", g2.reord)

x1.x <- x1; attr(x1.x, "extra.columns") <- x2
x2.x <- x2; attr(x2.x, "extra.columns") <- x1
x2.reord.x <- x2.reord; attr(x2.reord.x, "extra.columns") <- x1


#########################################
context("Correctness and errors with writeGenalex()")

test_that("writeGenalex() obeys sep", {
})


#########################################
context("Correctness and errors with summary.genalex()")

test_that("summary.genalex summarises pops etc., genotypes, and extra data", {
})


#########################################
context("Correctness and errors with printGenotype()")

test_that("printGenotype prints selected lines and calls out genotypes", {
})


#########################################
context("Correctness and errors with getLocusColumns()")

test_that("getLocusColumns works with 1 or more loci and regardless of ploidy", {
})


#########################################
context("Correctness and errors with reorderLoci()")

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
context("Correctness and errors with putLocus()")

pl.1 <- putLocus(x1, "b", data.frame(d=101:103, d.2=104:106))
pl.2 <- putLocus(x1, "b", matrix(101:106, byrow=FALSE, nrow=3, ncol=2))

test_that("putLocus()", {
    # replace with its own data
    expect_is(pl.1, "genalex")
    expect_is(pl.1, "data.frame")
    expect_equal(pl.1, x1)
    expect_is(pl.2, "genalex")
    expect_is(pl.2, "data.frame")
    expect_equal(pl.2, x1)
})


#########################################
context("Correctness and errors with getLocus()")

test_that("getLocus()", {
    # return is a data.frame
    l1 <- getLocus(pl.1, "a")
    expect_equal(class(getLocus(pl.1, "a")), "data.frame")
    expect_equal(class(l1), "data.frame")
    l2 <- getLocus(pl.1, "b")
    expect_equal(l2, data.frame(b=101:103, b.2=104:106))
})


#########################################
context("Correctness and errors with dropLocus()")


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
    attr(p1, "data.file.name") <- attr(p1.2, "data.file.name") <- NULL
    expect_equal(p1, p1.2)
    expect_equal(names(p1), c("sample", "pop", "b", "b.2"))
    expect_equal(attr(p1, "n.loci"), 1)
    expect_equal(attr(p1, "ploidy"), 2)
    expect_equal(attr(p1, "locus.columns"), c(3))
})


#########################################
context("Correctness and errors with reducePloidy()")

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
    attr(p1, "data.file.name") <- attr(p1.2, "data.file.name") <- NULL
    expect_equal(p1, p1.2)
    expect_equal(names(p1), c("sample", "pop", "a", "b"))
    expect_equal(attr(p1, "ploidy"), 1)
    expect_equal(attr(p1, "locus.columns"), 3:4)
})

#########################################
context("Correctness and errors with genalex()")

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
        "'genotypes' must have a number of columns dividable by ploidy")
    g4 <- data.frame(a=21:23, a.2=24:26, b=201:203, b.2=204:206, c=1:3)
    expect_error(genalex(7:9, "added1", g4, ploidy = 2),
        "'genotypes' must have a number of columns dividable by ploidy")
})



#########################################
context("Correctness and errors with as.genalex()")

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
    expect_equal(attr(gdf1, "locus.columns"), c(3, 5))
    expect_equal(attr(gdf1, "ploidy"), 2)
    expect_equal(attr(gdf1, "n.loci"), 2)
    # ploidy = 1
    expect_equal(attr(gdf1.p1, "ploidy"), 1)
    expect_equal(attr(gdf1.p1, "n.loci"), 4)
    expect_equal(attr(gdf1, "pop.sizes"), setNames(c(2, 1), c("a", "b")))
    expect_equal(attr(gdf1.p1, "locus.names"), c("a", "a.2", "b", "b.2"))
    expect_equal(attr(gdf1.p1, "locus.columns"), 3:6)

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



#########################################
context("Correctness and errors with rbind.genalex()")

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

