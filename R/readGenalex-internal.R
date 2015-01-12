.readGenalexVersion <-
"0.4.1"
.readGenalexData <-
function(con, sep, col.names, n.samples, n.loci, ploidy, extra.columns=character(0))
{
    classes <- c("character","character",rep("character",n.loci*ploidy))
    scan.col.names = col.names
    extra.columns <- extra.columns[extra.columns != ""]
    if (length(extra.columns)) {
        classes <- c(classes, rep("character", length(extra.columns)))
        scan.col.names = c(col.names, extra.columns)
    }
    # switch to scan() so that we can handle data lines that contain more trailing column 
    # separators than data dolumns, due to what Excel does when exporting tab-delimited files
    # dat <- read.table(file=con, sep=sep, header=FALSE, nrows=n.samples, 
    #                   col.names=col.names, colClasses=classes, flush=TRUE)
    what <- sapply(classes, do.call, list(0))
    names(what) = scan.col.names
    dat <- scan(file=con, what=what, nmax=n.samples, flush=TRUE, quiet=TRUE)
    extra.dat = NULL
    if (length(extra.columns)) {
      extra.dat = dat[names(what) %in% extra.columns]
      dat = dat[! names(what) %in% extra.columns]
      extra.dat <- as.data.frame(extra.dat, stringsAsFactors=FALSE)
    }
    dat <- as.data.frame(dat, stringsAsFactors=FALSE)
    ####
    list(dat=dat, extra.columns=extra.dat)
}
.readGenalexHeader <-
function(con, sep, ploidy)
{
    dlines <- readLines(con=con, n=3, ok=FALSE)
    dlines <- lapply(dlines, function(x) unlist(strsplit(x, sep, perl=TRUE)))
    dlines[[1]] <- as.numeric(dlines[[1]])
    header <- list(n.loci=dlines[[1]][1], 
                   ploidy=ploidy,
                   n.samples=dlines[[1]][2], 
                   n.pops=dlines[[1]][3],
                   dataset.title=dlines[[2]][1],
                   sample.title=dlines[[3]][1],
                   pop.title=dlines[[3]][2])
    header$pop.labels <- dlines[[2]][4:(4+header$n.pops-1)]
    if (length(dlines[[3]]) >= 3 + header$n.loci*ploidy) {
        # extra columns beyond the genotype columns, do we want to load them?
        # if we do load them, we load them as character into a separate data.frame
        # attached to the "extra.columns" attribute
        extra.columns <- dlines[[3]][(3+header$n.loci*ploidy):length(dlines[[3]])]
        if (any(length(extra.columns) > 0))  # any of them are named
            header$extra.columns <- extra.columns
    }
    pop.sizes <- dlines[[1]][4:(4+header$n.pops-1)]
    names(pop.sizes) <- header$pop.labels
    header$pop.sizes <- pop.sizes
    header$locus.columns <- seq(from=3, to=(3+(header$n.loci-1)*ploidy), by=ploidy)
    header$locus.names <- dlines[[3]][header$locus.columns]
    header$data.column.names <- c(header$sample.title, header$pop.title,
                                  unlist(lapply(header$locus.names, 
                                                function(x) c(x, 
                                                              paste(sep=".", x, 
                                                                    seq(2, header$ploidy, 1))))))
    ####
    header
}
.readGenalexJoinData <-
function(header, raw.data)
{
    dat = raw.data$dat
    if (! is.null(raw.data$extra.columns)) {
        extra.columns = cbind(dat[,1], raw.data$extra.columns)  # add sample name to extra columns
        names(extra.columns)[1] = names(dat)[1]
    }
    names(dat) <- header$data.column.names
    header$data.column.names = NULL  # don't add as an attribute, it duplicates 'names'
    dat[[header$pop.title]] <- factor(dat[[header$pop.title]])
    # TODO: handle label in header with size 0 and missing from data?
    pop.labels.header = sort(header$pop.labels)
    pop.labels.data = sort(levels(dat[[header$pop.title]]))
    if (suppressWarnings(any(pop.labels.header != pop.labels.data))) {
        err <- pop.labels.data[! pop.labels.data %in% pop.labels.header]
        if (length(err))
          warning("population labels in data but not in header: ", paste(err, collapse=","))
        err <- pop.labels.header[! pop.labels.header %in% pop.labels.data]
        if (length(err))
          warning("population labels in header but not in data: ", paste(err, collapse=","))
        stop("fatal population label mismatch between header and data")
    }
    # TODO: handle label in header with size 0 and missing from data?
    pops.in.order <- names(header$pop.sizes)
    pop.sizes.in.data = table(dat[[header$pop.title]])[pops.in.order]
    mism = pop.sizes.in.data != header$pop.sizes
    if (any(mism)) {
        err1 <- paste(collapse=",",header$pop.labels[mism])
        err2 <- paste(paste(sep=" != ",collapse=", ",pop.sizes.in.data[mism], header$pop.sizes[mism]))
        stop("sizes of populations ",err1," do not match in header and data: ",err2)
    }
    for (nm in names(header)) attr(dat, nm) <- header[[nm]]
    if (! is.null(raw.data$extra.columns))
        attr(dat, "extra.columns") = extra.columns
    dat
}
