#' @include readGenalex.R
NULL



#' Check for potential null (non-amplifying) alleles
#' 
#' Check genotypes against a set of reference genotypes and look for
#' loci that differ in a way consistent with the occurrence of a null
#' allele, that is, heterozygous in the reference and homozygous matching
#' one of the alleles in the heterozygous reference.
#'
#' The reference and check genotypes are in GenAlEx format, and are
#' names of files to be read with \code{readGenalex} or are class
#' \code{'genalex'} data frames.
#'
#' @note  The reference genotypes are assumed to be genotyped correctly,
#'        without null alleles of their own.  The check for null alleles is
#'        only as complete as the set of reference genotypes.
#' 
#' @note  Only genotypes with ploidy of 2 are currently checked.
#' 
#' @param ref    Object of class \code{'genalex'}, or a character filename
#'               of a file that can be read by \code{readGenalex}, containing
#'               reference genotypes.
#' 
#' @param check  Object of class \code{'genalex'}, or a character filename
#'               of a file that can be read by \code{readGenalex}, containing
#'               genotypes to check against \code{ref} for possible null
#'               alleles.
#' 
#' @param mode   \code{"column"} or \code{"locus"}.  Determines how each locus
#'               containing a potential null allele is indicated in the second
#'               index of each element of the return value.  If
#'               \code{"column"}, then the value is the column index within
#'               the class \code{'genalex'} data frame.  If \code{"locus"},
#'               the value is the locus index, starting from \code{1}.
#' 
#' @param quiet  \code{TRUE} or \code{FALSE}, if \code{FALSE} then messages 
#'               are printed during the check for null alleles in
#'               \code{check}.  If \code{TRUE} then null allele information
#'               is available in the return value.
#' 
#' @param ...    Additional arguments to pass to \code{readGenalex} when
#'               reading \code{ref} or \code{check}, if either was a filename.
#' 
#' @return A list of coordinates within \code{check} that contain potential
#'         null alleles.  Each element is an integer vector with the first
#'         value being the row in \code{check} and the second value being the
#'         column at which the genotype with the potential null allele is
#'         found.  If \code{quiet = FALSE}, informational messages are also
#'         printed during execution.
#' 
#' @author Douglas G. Scofield
#' 
#' @examples
#' 
#' reffile <- system.file("extdata", "ref_genotypes.txt", 
#'                        package="readGenalex")
#' checkfile <- system.file("extdata", "check_genotypes.txt", 
#'                          package="readGenalex")
#' res1 <- checkNullAlleles(reffile, checkfile)
#' ref <- readGenalex(reffile)
#' check <- readGenalex(checkfile)
#' res2 <- checkNullAlleles(reffile, checkfile)
#' stopifnot(identical(res1, res2))
#' 
#' @export checkNullAlleles
#' 
# TODO: implement mode locus
#
checkNullAlleles <- function(ref, check, mode = c("column", "locus"),
                             quiet = FALSE, ...) {
    if (is.character(ref))
        ref <- readGenalex(ref, ...)
    else if (! is.genalex(ref))
        stop(deparse(substitute(ref)), " must be class 'readGenalex'")
    if (! quiet)
        message("checkNullAlleles: ", attr(ref, "n.samples"), 
                " reference genotypes")
    if (is.character(check))
        check <- readGenalex(check, ...)
    else if (! is.genalex(check))
        stop(deparse(substitute(check)), " must be class 'readGenalex'")
    if (! quiet)
        message("checkNullAlleles: ", attr(ref, "n.samples"), 
                " check genotypes")
    if (attr(ref, "ploidy") != 2 || attr(check, "ploidy") != 2) 
        stop("ploidy of ref and check must both be 2")
    mode <- match.arg(mode)
    loci <- attr(check, "locus.names")
    n.loci <- attr(check, "n.loci")
    locus.columns <- attr(check, "locus.columns")
    check.names <- check[, 1]
    n.check <- length(check.names)
    ref.names <- ref[, 1]
    n.ref <- length(ref.names)
    # array containing potential null loci
    ans <- list()
    n.potential.null.loci <- name.potential.null.loci <- array(0,
        dim=c(n.check, n.ref), 
        dimnames=list(check.names, ref.names))
    name.potential.null.loci[] <- ""
    for (j in 1:n.check) {
        check.printed <- FALSE
        for (k in 1:n.ref) {
            potential.null.loci <- c()
            match.pmatch <- 0
            for (locus in 1:n.loci) {
                l1 <- locus.columns[locus]
                l2 <- l1 + 1
                m1 <- ref[k, l1]
                m2 <- ref[k, l2]
                p1 <- check[j, l1]
                p2 <- check[j, l2]
                # See if we match at this locus.  If we do, continue.  If we
                # don't, then check for potential null in the check, which I
                # assume can occur when the check is a homozygote, the ref
                # is a heterozygote, and the check matches one of the ref
                # alleles.  If that's not true, we definitely don't match, so
                # stop comparing to this ref.
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
                    if (! check.printed) {
                        check.printed <- TRUE
                        printGenalexGenotype(check, j, label="check",
                                             callout.locus=potential.null.loci, 
                                             sep="\t")
                    }
                    printGenalexGenotype(ref, k, label="ref",
                                         callout.locus=potential.null.loci, 
                                         sep="\t")
                }
            }
        }
        if (sum(n.potential.null.loci[j, ])) 
            cat("\n")
    }
}

