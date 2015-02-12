# Copyright (c) 2011 Douglas G. Scofield, Umeå Plant Science Centre, Umeå, Sweden
#
# douglas.scofield@plantphys.umu.se
# douglasgscofield@gmail.com
#
# Compare genotypes against a set of reference genotypes and look for genotypes
# that differ in a way consistent with the occurrence of a null allele, that is,
# heterozygous in the reference and homozygous matching one of the alleles in
# the heterozygous reference.
#
# The reference and compare genotypes are in GenAlEx format, and are either read
# in within checkNullAlleles() or are presented as dataframes read by
# readGenalex().  readGenalex() and its associated functions must be available,
# and can be found at http://sites.google.com/site/douglasgscofield
#
# CHANGELOG
#
# 0.2: A few corrections
# 0.1: First public release
#
.checkNullAllelesVersion <- "0.2"

# Function for checking for null alleles in comparegt, when compared against
# refgt.  refgt and comparegt may be filenames (files must be in GenAlEx format)
# or they may be a GenAlEx dataframe loaded earlier using the read.genalex()
# function (included in a separate file.)
#
# Set quiet=TRUE if you don't want checkNullAlleles to print informational 
# messages in addition to the null allele reports.
#
# Use as you see fit.  No warranty regarding this code is implied nor should be
# assumed.  Send bug reports etc. to one of the above email addresses.

checkNullAlleles <- function(refgt, comparegt, quiet=FALSE)
{
    if (is.character(refgt))
        refgt <- readGenalex(refgt)
    else if (! is.genalex(refgt))
        stop("reference genotypes must be read by readGenalex()")
    if (! quiet)
        cat(sep="", "checkNullAlleles ", .checkNullAllelesVersion, ": ", attr(refgt, "n.samples"), " reference genotypes\n")
    if (is.character(comparegt))
        comparegt <- readGenalex(comparegt)
    else if (! is.genalex(comparegt))
        stop("comparison genotypes must be read by readGenalex()")
    if (! quiet)
        cat(sep="", "checkNullAlleles ", .checkNullAllelesVersion, ": ", attr(comparegt, "n.samples"), " comparison genotypes\n\n")
    if (attr(refgt, "ploidy") != 2) 
        stop("ploidy of refgt must be 2")
    if (attr(comparegt, "ploidy") != 2) 
        stop("ploidy of comparegt must be 2")
    loci <- attr(comparegt, "locus.names")
    n.loci <- attr(comparegt, "n.loci")
    locus.columns <- attr(comparegt, "locus.columns")
    comparegt.names <- comparegt[, 1]
    n.comparegt <- length(comparegt.names)
    refgt.names <- refgt[, 1]
    n.refgt <- length(refgt.names)

    ans <- list()
    n.potential.null.loci <- name.potential.null.loci <- array(0,
        dim=c(n.comparegt, n.refgt), 
        dimnames=list(comparegt.names, refgt.names))
    name.potential.null.loci[] <- ""

    for (j in 1:n.comparegt) {
        comparegt.printed <- FALSE
        for (k in 1:n.refgt) {
            potential.null.loci <- c()
            match.pmatch <- 0
            for (locus in 1:n.loci) {
                l1 = locus.columns[locus]
                l2 = l1 + 1
                m1 = refgt[k, l1]
                m2 = refgt[k, l2]
                p1 = comparegt[j, l1]
                p2 = comparegt[j, l2]

                # See if we match at this locus.  If we do, continue.  If we
                # don't, then check for potential null in the comparegt, which I
                # assume can occur when the comparegt is a homozygote, the refgt
                # is a heterozygote, and the comparegt matches one of the refgt
                # alleles.  If that's not true, we definitely don't match, so
                # stop comparing to this refgt.

                if (all(sort(c(m1, m2)) == sort(c(p1, p2)))) {
                    match.pmatch <- match.pmatch + 1
                    next
                } else if (p1 == p2 && m1 != m2 && (p1 == m1 || p1 == m2)) {
                    match.pmatch <- match.pmatch + 1
                    potential.null.loci <- c(potential.null.loci, loci[locus])
                } else {
                    break
                }
            }
            if (match.pmatch == n.loci) {
                n.potential.null.loci[j, k] <- length(potential.null.loci)
                name.potential.null.loci[j, k] <- paste(collapse=",", 
                                                       potential.null.loci)
                if (length(potential.null.loci) > 0) {
                    if (! comparegt.printed) {
                        comparegt.printed <- TRUE
                        printGenalexGenotype(comparegt, j, label="compare",
                                             callout.locus=potential.null.loci, 
                                             sep="\t")
                    }
                    printGenalexGenotype(refgt, k, label="ref",
                                         callout.locus=potential.null.loci, 
                                         sep="\t")
                }
            }
        }
        if (sum(n.potential.null.loci[j, ])) 
            cat("\n")
        if (! quiet && (j %% 50) == 0) 
            cat(sep="", "\n", "checkNullAlleles ", .checkNullAllelesVersion, 
                ": finished null-allele check through comparison genotype ",
                j, "\n\n")
    }
}


