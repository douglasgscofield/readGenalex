#' Reduce the ploidy of a \code{readGenalex}-format \code{data.frame}
#' 
#' Reduce the ploidy of a \code{readGenalex}-format \code{data.frame}.
#' Currently restricted to reducing the ploidy of diploid data to haploid by
#' selecting only the first allele of each locus.
#' 
#' This function reduced the ploidy of a \code{readGenalex}-format
#' \code{data.frame} by selecting the first allele of each locus.  Occasionally
#' haploid data is encoded in GenAlEx datasets by using homozygous diploid
#' loci, and this is a useful function for making these truly haploid.
#' 
#' @param dat An annotated \code{data.frame} created by \code{readGenalex()}
#' @param new.ploidy The desired new ploidy.  Currently, the only usefully
#' accepted value is 1, with ploidy of \code{dat} being 2; a ploidy matching
#' the current ploidy of \code{dat} silently returns \code{dat}. %% ~~Describe
#' \code{new.ploidy} here~~
#' @return A \code{data.frame} containing genotype data from \code{dat} reduced
#' to the specified \code{new.ploidy}, with attributes updated as required.
#' @author Douglas G. Scofield
#' @examples
#' 
#' data(example_genotypes)
#' attr(example_genotypes, "ploidy")
#' p1 = reduceGenalexPloidy(example_genotypes, 1)
#' 
#' @export reduceGenalexPloidy
reduceGenalexPloidy <-
function(dat, new.ploidy=1)
{
    # Would be nice to be more general, e.g., pick other than the first
    # column, or a random allele
    att <- attributes(dat)
    if (new.ploidy == att$ploidy) 
        return(dat)
    else if (new.ploidy > att$ploidy) 
        stop("new.ploidy",new.ploidy,"greater than existing ploidy",att$ploidy)
    else if (new.ploidy != 1 || att$ploidy != 2) 
        stop("can't currently handle new.ploidy other than 1, existing ploidy other than 2")
    new.col = c(1:(att$locus.columns[1]-1), att$locus.columns)
    dat = dat[, new.col]
    for (a in names(att))
        if (! a %in% c("names","locus.columns","ploidy"))
            attr(dat,a) <- att[[a]]
    attr(dat, "locus.columns") = att$locus.columns - (0:(att$n.loci - 1) * (att$ploidy - new.ploidy))
    attr(dat, "ploidy") = new.ploidy
    dat
}
