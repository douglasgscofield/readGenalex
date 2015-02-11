#' Remove specified loci from the \code{readGenalex}-format \code{data.frame}
#' 
#' Remove specified loci from the \code{readGenalex}-format \code{data.frame}
#' and updates attributes
#' 
#' 
#' @param dat An annotated \code{data.frame} created by \code{readGenalex()}
#' @param drop.loci The names of one or more loci found in \code{dat}
#' @param quiet If set to \code{TRUE}, quietly returns \code{dat} if none of
#' \code{drop.loci} are found in \code{dat}
#' @return A \code{data.frame} containing the data in \code{dat} after removing
#' loci specified by \code{drop.loci}, with attributes updated as required.
#' @author Douglas G. Scofield
#' @examples
#' 
#' data(example_genotypes)
#' newdat = dropGenalexLoci(example_genotypes, "loc3")
#' 
#' @export dropGenalexLoci
dropGenalexLoci <-
function(dat, drop.loci, quiet=FALSE)
{
    if (missing(drop.loci) || is.null(drop.loci)) return(dat)
    locus.names <- attr(dat, "locus.names")
    if (! all(drop.loci %in% locus.names))  
        if (any(drop.loci %in% locus.names)) # at least one matches
          drop.loci <- drop.loci[drop.loci %in% locus.names]
        else
          if (quiet) 
              return(dat) 
          else 
              stop("locus not present")
    att <- attributes(dat)
    dat <- dat[,-computeGenalexColumns(dat, drop.loci)]
    for (a in names(att))
        if (! a %in% c("names","n.loci","locus.names","locus.columns"))
            attr(dat,a) <- att[[a]]
    locus.names <- locus.names[! locus.names %in% drop.loci]
    attr(dat, "n.loci") <- length(locus.names)
    attr(dat, "locus.names") <- locus.names
    attr(dat, "locus.columns") <- which(names(dat) %in% locus.names)
    dat
}
