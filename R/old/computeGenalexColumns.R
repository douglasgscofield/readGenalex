#' Determine numeric column positions occupied by named loci
#' 
#' Determine the numeric column positions occupied by named loci in a
#' \code{date.frame} produced by \code{readGenalex()}.  This is mostly used as
#' a utility routine by other functions in the \code{readGenalex} package.
#' 
#' 
#' @param dat An annotated \code{data.frame} created by \code{readGenalex()}
#' @param locus The names of one or more loci found in \code{dat}
#' @param ploidy Ploidy of data in \code{dat}, if not supplied is extracted
#' from the \code{ploidy} attribute of \code{dat}
#' @return A vector of column positions occupied by genotype data for loci
#' named in \code{locus}.
#' @author Douglas G. Scofield
#' @examples
#' 
#' data(example_genotypes)
#' computeGenalexColumns(example_genotypes, c("loc2","loc4"))
#' 
#' @export computeGenalexColumns
computeGenalexColumns <-
function(dat, locus, ploidy=NULL)
{
    if (is.null(ploidy)) ploidy <- attr(dat,"ploidy")
    as.vector(sapply(attr(dat, "locus.columns")[attr(dat, "locus.names") %in% locus],
                     function(x) x:(x+ploidy-1)))
}
