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
