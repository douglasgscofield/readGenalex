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
