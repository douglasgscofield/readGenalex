#' Reorder a \code{readGenalex}-format \code{data.frame}
#' 
#' Reorder the genotype columns of a \code{readGenalex}-format
#' \code{data.frame} by locus.
#' 
#' 
#' @param dat An annotated \code{data.frame} created by \code{readGenalex()}
#' @param loci The names of loci found in \code{dat}, in the desired new order.
#' All loci in \code{dat} must be named.  The order of the alleles within each
#' locus is preserved.
#' @return A \code{data.frame} containing the same genotype data from
#' \code{dat} reordered according to \code{loci}.
#' @author Douglas G. Scofield
#' @examples
#' 
#' data(example_genotypes)
#' # reverse loci
#' reord = reorderGenalexLoci(example_genotypes, rev(attr(example_genotypes, "locus.names")))
#' 
#' @export reorderGenalexLoci
reorderGenalexLoci <-
function(dat, loci)
{
    existing.loci <- attr(dat, "locus.names")
    if (! all(existing.loci %in% loci)) 
        stop("not all existing loci in reorder list")
    newdata <- dat[,1:2]
    for (locus in loci) {
        newdata <- cbind(newdata, getGenalexLocus(dat, locus))
    }
    names.newdata <- names(newdata)
    attributes(newdata) <- attributes(dat)
    names(newdata) <- names.newdata
    attr(newdata,"locus.names") <- loci
    newdata
}
