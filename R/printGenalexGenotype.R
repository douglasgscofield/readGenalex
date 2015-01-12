printGenalexGenotype <-
function(dat, rows, callout.locus=NULL, 
                                   sep=" ", allele.sep="/", 
                                   callout.char="*", label=NULL)
{
    cols <- names(dat)
    ploidy <- attr(dat, "ploidy")
    for (row in rows) {
        cat(paste(sep=sep, as.character(dat[row,1]), as.character(dat[row,2])))
        if (! is.null(label))
            cat("", label)
        full.gt <- ""
        for (col in seq(from=3, to=length(cols), by=ploidy)) {
            gt <- paste(collapse=allele.sep, dat[row,col:(col+ploidy-1)])
            if (cols[col] %in% callout.locus) 
                gt <- paste(sep="", callout.char, gt, callout.char)
            full.gt <- paste(sep=sep, collapse=sep, full.gt, gt)
        }
        cat(full.gt,"\n")
    }
}
