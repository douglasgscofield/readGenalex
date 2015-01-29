#' Return genotype data for specified loci in the \code{readGenalex}-format
#' \code{data.frame}
#' 
#' Return genotype data for specified loci in the \code{readGenalex}-format
#' \code{data.frame}, optionally restricted to samples from specific
#' populations.
#' 
#' 
#' @param dat An annotated \code{data.frame} created by \code{readGenalex()}
#' @param locus The names of one or more loci found in \code{dat}
#' @param pop If supplied, return only data for samples from the specified
#' populations
#' @return A \code{data.frame} containing genotype data from \code{dat} for
#' loci specified in \code{code}, optionally restricted to samples from
#' populations specified in \code{pop}.
#' @author Douglas G. Scofield
#' @examples
#' 
#' data(example_genotypes)
#' nm = attr(example_genotypes, "locus.names")
#' loc1 = getGenalexLocus(example_genotypes, nm[1])
#' po = attr(example_genotypes, "pop.labels")
#' loc2.pop2 = getGenalexLocus(example_genotypes, nm[2], po[2])
#' 
#' @export getGenalexLocus
getGenalexLocus <-
function(dat, locus, pop=NULL)
{
    is.genalex(dat)
    cols = computeGenalexColumns(dat,locus)
    if (! is.null(pop)) {
        pop.column <- attr(dat, "pop.title")
        dat <- subset(dat, dat[[pop.column]] %in% pop)
    }
    dat[, cols]
}
