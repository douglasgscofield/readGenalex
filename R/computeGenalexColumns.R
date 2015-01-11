computeGenalexColumns <-
function(dat, locus, ploidy=NULL)
{
    if (is.null(ploidy)) ploidy <- attr(dat,"ploidy")
    as.vector(sapply(attr(dat, "locus.columns")[attr(dat, "locus.names") %in% locus],
                     function(x) x:(x+ploidy-1)))
}
