getGenalexLocus <-
function(dat, locus, pop=NULL)
{
    is.genalex(dat)
    if (! is.null(pop)) {
        pop.column <- attr(dat, "pop.title")
        dat <- subset(dat, dat[[pop.column]] %in% pop)
    }
    dat[, computeGenalexColumns(dat,locus)]
}
