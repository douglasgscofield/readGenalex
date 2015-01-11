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
