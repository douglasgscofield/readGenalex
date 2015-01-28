writeGenalex <-
function(x, file, sep = "\t", na = "", eol = "\n")
{
    DNAME <- deparse(substitute(x))
    if (! is.genalex(x))
        stop(DNAME, " must be a readGenalex-format data.frame")
    if (file == "")
        file <- stdout()
    else if (is.character(file)) {
        file <- file(description=file, open="wt")
        on.exit(close(file))
    }
    else if (! isOpen(file, "w")) {
        open(file, "w")
        on.exit(close(file))
    }
    if (! inherits(file, "connection"))
        stop("'file' must be a character string or connection")
    a <- attributes(x)
    # TODO: handle something like quote= ?
    x <- as.data.frame(lapply(x, as.character))  # recast everything as character
    x[is.na(x)] <- na
    if (! is.null(extra <- a$extra.columns)) {
        extra <- as.data.frame(lapply(extra, as.character))  # recast everything as character
        extra[is.na(extra)] <- na
    }
    # header line 1
    cat(file = file, sep = sep, a$n.loci, a$n.samples, a$n.pops, a$pop.sizes)
    cat(file = file, eol);
    # header line 2
    cat(file = file, sep = sep, a$dataset.title, "", "", a$pop.labels)
    cat(file = file, eol);
    # header line 3, allele columns other than the first for each locus have a
    # blank header
    cat(file = file, sep = sep, a$sample.title, a$pop.title, 
        paste(collapse = paste(collapse = "", rep(sep, a$ploidy)), a$locus.names),
        rep("", a$ploidy - 1))  # final genotype columns also have blank name(s)
    # if extra columns, add headers for those
    if (! is.null(extra))
        cat(file = file, sep = sep, names(extra))
    cat(file = file, eol);
    # data plus extra columns
    for (i in 1:nrow(x)) {
        fields <- unlist(x[i, ])
        if (! is.null(extra))
            fields <- c(fields, extra[i, ])
        cat(file = file, sep = sep, fields)  # sample, population, and allele columns
        cat(file = file, eol)
    }
}
