readGenalex <-
function(file, sep="\t", ploidy=2)
{
    if (ploidy != 2) warning("ploidy other than 2 not tested")
    fcon <- file(description=file, open="rt")
    header <- .readGenalexHeader(fcon, sep, ploidy)
    # returns a list
    raw.data <- .readGenalexData(fcon, sep, header$data.column.names,
                                 header$n.samples, header$n.loci, 
                                 ploidy, header$extra.columns)
    close(fcon)
    dat <- .readGenalexJoinData(header, raw.data)
    attr(dat, "data.file.name") <- file
    attr(dat, "genetic.data.format") <- "genalex"
    dat
}
