# Copyright (c) 2008-2014, Douglas G. Scofield douglasgscofield@gmail.com
#
# Read genotype data file in GenAlEx format. GenAlEx and its documentation are
# available from http://www.anu.edu.au/BoZo/GenAlEx/
#
# GenAlEx format is derived from its described cell formats in Excel, and is for
# now assumed to be held in a delimited file exported in this format.
#
# num.loci      num.samples num.pops    num.pop1   num.pop2     ...
# title         blank       blank       label.pop1 label.pop2   ...
# sample.header pop.header  name.loc1   blank      name.loc2    blank ...
# id.sample1    id.pop1     loc1.al1    loc1.al2   loc2.al1     loc2.al2 ...
# id.sample2    id.pop1     loc1.al1    loc1.al2   loc2.al1     loc2.al2 ...
#
# Calling readGenalex() for a file first reads the top 3 header lines, then
# reads the remainder of the file checking for consistency with the data
# description from the header lines.
#
# CHANGELOG
#
# v 0.4.1
# -------
# * Improve error messages for sample size mismatch and handle lack of extra
#   columns more gracefully
#
# v 0.4
# -----
# * Downconvert ploidy (2n to 1n) by sampling first allele of each locus with
#   reduceGenalexPloidy()
#
# v 0.3
# -----
# * Improve error messages when mismatch between group names in the header
#   and in the data
# * Handle the presence of extra columns to the right of the genotype
#   columns; load these into a separate data.frame attribute "extra.columns"

.readGenalexVersion <- "0.4"

readGenalex <- function(file, sep="\t", ploidy=2)
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

is.genalex <- function(checkdata)
{
    if (attr(checkdata,"genetic.data.format") != "genalex")
        stop("data not genalex format")
    TRUE
}

reduceGenalexPloidy <- function(dat, new.ploidy=1)
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
        if (! a %in% c("names","locus.columns","data.column.names","ploidy"))
            attr(dat,a) <- att[[a]]
    attr(dat, "locus.columns") = att$locus.columns - (0:(att$n.loci - 1) * (att$ploidy - new.ploidy))
    attr(dat, "data.column.names") = att$data.column.names[new.col]
    attr(dat, "ploidy") = new.ploidy
    dat
}

dropGenalexLoci <- function(dat, drop.loci, quiet=FALSE)
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

printGenalexGenotype <- function(dat, rows, callout.locus=NULL, 
                                   sep=" ", allele.sep="/", 
                                   callout.char="*", label=NULL)
{
    cols <- names(dat)
    ploidy <- attr(dat, "ploidy")
    for (row in rows) {
        cat(paste(sep=sep, collapse=sep, dat[row,cols[1:2]]))
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

reorderGenalexLoci <- function(dat, loci)
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

computeGenalexColumns <- function(dat, locus, ploidy=NULL)
{
    if (is.null(ploidy)) ploidy <- attr(dat,"ploidy")
    as.vector(sapply(attr(dat, "locus.columns")[attr(dat, "locus.names") %in% locus],
                     function(x) x:(x+ploidy-1)))
}

putGenalexLocus <- function(dat, locus, newdata)
{
    is.genalex(dat)
    dat[, computeGenalexColumns(dat,locus)] <- newdata
    dat
}

getGenalexLocus <- function(dat, locus, pop=NULL)
{
    is.genalex(dat)
    if (! is.null(pop)) {
        pop.column <- attr(dat, "pop.title")
        dat <- subset(dat, dat[[pop.column]] %in% pop)
    }
    dat[, computeGenalexColumns(dat,locus)]
}

.readGenalexJoinData <- function(header, raw.data)
{
    dat = raw.data$dat
    if (! is.null(raw.data$extra.columns)) {
        extra.columns = cbind(dat[,1], raw.data$extra.columns)  # add sample name to extra columns
        names(extra.columns)[1] = names(dat)[1]
    }
    names(dat) <- header$data.column.names
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

.readGenalexHeader <- function(con, sep, ploidy)
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

.readGenalexData <- function(con, sep, col.names, n.samples, n.loci, ploidy, extra.columns=character(0))
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

