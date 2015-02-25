#' @include readGenalex.R
# For collation, load after the above
NULL



#' Check for potential null (non-amplifying) alleles
#'
#' Check genotypes against a set of reference genotypes and look for
#' loci that differ in a way consistent with the occurrence of a null
#' allele, that is, heterozygous in the reference and homozygous matching
#' one of the alleles in the heterozygous reference.  A report of genotypes
#' with potential null is generated, unless \code{quiet = TRUE}, and
#' a matrix is returned indicating the positions of potential null alleles
#' within the check genotypes.  The configuration of this matrix depends
#' on the \code{mode} argument.
#'
#' @note  The reference genotypes are assumed to be genotyped correctly,
#'        without null alleles of their own.  The check for null alleles is
#'        only as complete as the set of reference genotypes.
#'
#' @note  Only genotypes with ploidy of 2 are currently supported.
#'
#' @param ref    Object of class \code{'genalex'} containing reference 
#'               genotypes
#'
#' @param check  Object of class \code{'genalex'} containing genotypes to 
#'               check against \code{ref} for possible null alleles
#'
#' @param mode   \code{"locus"} or \code{"column"}.  Determines 
#'               containing a potential null allele is indicated in the second
#'               index of each element of the return value.  If \code{"locus"},
#'               the value is the locus index, starting from \code{1}.
#'               If \code{"column"}, then the value is the column index within
#'               the class \code{'genalex'} data frame.  
#'
#' @param quiet  Logical.  If \code{FALSE}, then messages
#'               are printed during the check for null alleles in
#'               \code{check}, and the matrix of potential null allele
#'               positions is returned invisibly.  If \code{TRUE}, then no
#'               messages or genotypes are printed, and the matrix of 
#'               potential null allele positions is returned visibly.
#'
#' @param sep    Separator between loci in output, passed to 
#'               \code{\link{printGenotype}}
#'
#' @param \dots  Additional parameters passed to \code{\link{printGenotype}}
#'
#' @return A matrix referencing positions within \code{check} that contain 
#'         potential, with its specific format dependent upon the value of
#'         \code{mode}:
#'         \itemize{
#'           \item \code{"locus"}: a logical matrix having dimensions
#'                 \code{(attr(check,"n.samples"),attr(check,"n.loci"))}
#'                 and dimnames
#'                 \code{list(check[,1],attr(check,"locus.names"))} with
#'                 \code{TRUE} for each locus in \code{check} that might
#'                 contain a potential null allele
#'           \item \code{"column"}: an integer matrix having dimensions
#'                 \code{dim(check)} and dimnames \code{dimnames(check))}
#'                 with nonzero values for each position within \code{check}
#'                 that might contain a potential null allele
#'         }
#'         If \code{quiet = FALSE}, this value is returned invisibly.
#'
#' If \code{quiet = FALSE} (the default), output is also written in the
#' form of two messages indicating the number of reference and check 
#' genotypes, as well as a listing of specific check and matching reference
#' genotypes with the loci containing the potential null alleles called out.
#' The check and reference genotypes are printed together with a label
#' equal to the respective argument names to indicate their origins.  This
#' listing is produced using \code{printGenotype}, and arguments can be 
#' passed through to modify its  output.
#'
#'
#' @author Douglas G. Scofield
#'
#' @seealso \code{\link{printGenotype}}
#'
#' @examples
#'
#' reffile <- system.file("extdata", "ref_genotypes.txt",
#'                        package="readGenalex")
#' checkfile <- system.file("extdata", "check_genotypes.txt",
#'                          package="readGenalex")
#' ref <- readGenalex(reffile)
#' check <- readGenalex(checkfile)
#' checkNullAlleles(ref, check)
#'
#' @export checkNullAlleles
#'
checkNullAlleles <- function(ref, check, mode = c("locus", "column"),
                             quiet = FALSE, sep = "\t", ...) {
    ref.name <- deparse(substitute(ref))
    if (! is.genalex(ref))
        stop(ref.name, " must be class 'genalex'")
    if (! quiet) message(attr(ref, "n.samples"), " reference genotypes")
    check.name <- deparse(substitute(check))
    if (! is.genalex(check))
        stop(check.name, " must be class 'genalex'")
    if (! quiet) message(attr(ref, "n.samples"), " check genotypes")
    if (attr(ref, "ploidy") != 2 || attr(check, "ploidy") != 2)
        stop("ploidy other than 2 not implemented")
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
    n.potential.null.loci <- name.potential.null.loci <-
        array(0, dim=c(n.check, n.ref), dimnames=list(check.names, ref.names))
    null.loci <- array(FALSE, dim = c(n.check, n.loci), 
                       dimnames = list(check.names, loci))
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
                } else if (p1 == p2 && m1 != m2 && (p1 == m1 || p1 == m2)) {
                    match.pmatch <- match.pmatch + 1
                    potential.null.loci <- c(potential.null.loci, loci[locus])
                    # remember the location in check
                } else break
            }
            if (match.pmatch == n.loci) {
                n.potential.null.loci[j, k] <- length(potential.null.loci)
                name.potential.null.loci[j, k] <- paste(collapse = ",",
                                                        potential.null.loci)
                if (length(potential.null.loci) > 0) {
                    null.loci[j, potential.null.loci] <- TRUE
                    if (! quiet) {
                        # print it out
                        if (! check.printed) {
                            check.printed <- TRUE
                            printGenotype(check, j, label = check.name, sep = sep,
                                          callout.locus = potential.null.loci,
                                          ...)
                        }
                        printGenotype(ref, k, label = ref.name, sep = sep,
                                      callout.locus = potential.null.loci,
                                      ...)
                    }
                }
            }
        }
        if (! quiet && sum(n.potential.null.loci[j, ]))
            cat("\n")
    }
    if (mode == "locus") {
        if (quiet) return(null.loci) else invisible(null.loci)
    } else if (mode == "column") {
        # translate to column locations, first replicate for ploidy
        null.loci <- matrix(do.call(rbind, 
                                    replicate(attr(check, "ploidy"), 
                                              null.loci, simplify = FALSE)),
                                 n.check, 2 * n.loci)
        # now add two left columns and names, promotes to integer too
        null.loci <- cbind(integer(n.check), integer(n.check), null.loci)
        dimnames(null.loci) <- dimnames(check)
        if (quiet) return(null.loci) else invisible(null.loci)
    } else stop("unimplemented mode")
}

