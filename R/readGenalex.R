#' Read and manipulate GenAlEx-format genotype files
#'
#' A collection of R functions to read, manipulate and write genotype data in
#' GenAlEx format.  GenAlEx is a widely-used Excel plugin for manipulating and
#' analysing genotype data.  This package reads GenAlEx data that has been
#' exported from Excel as a delimited text file and creates an annotated
#' data frame of class \code{'genalex'}.  Several functions are provided for
#' accessing and printing this data.  GenAlEx and its documentation are
#' available at \url{http://biology-assets.anu.edu.au/GenAlEx}.  Descriptions
#' of the file format and of the annotations added to the class as attributes
#' are available via \code{help(readGenalex)}.
#' 
#' @references Peakall, R. and Smouse P.E. (2012) GenAlEx 6.5: genetic analysis
#' in Excel. Population genetic software for teaching and research-an update.
#' \emph{Bioinformatics} 28, 2537-2539.
#' 
#' Peakall, R. and Smouse P.E. (2006) GENALEX 6: genetic analysis in Excel.
#' Population genetic software for teaching and research. \emph{Molecular
#' Ecology Notes} 6, 288-295.
#'
#' \url{https://github.com/douglasgscofield/readGenalex}
#'
#' @seealso \link{readGenalex}
#'
#' @keywords package attribute manip file
#'
#' @docType package
#'
#' @name readGenalex-package
#'
#' @aliases readGenalex-package GenAlEx genotype
#'
NULL



#' Check to see if an object is of class 'genalex'
#' 
#' Check to see if an object is of class 'genalex' as recognised by
#' the \code{readGenalex} package.  With \code{force = TRUE},
#' the internal consistency of the data and annotations are checked.
#' 
#' @param x      An object that might be of class \code{'genalex'}
#'
#' @param force  If \code{TRUE} and \code{x} has class \code{'genalex'},
#'               force a deeper check to assure that the
#'               data and annotations are consistent with class 
#'               \code{'genalex'}
#'
#' @param verbose If \code{TRUE} and \code{force = TRUE}, describe any
#'               inconsistencies discovered between the data and
#'               annotations
#'
#' @return \code{TRUE} or \code{FALSE}
#'
#' @author Douglas G. Scofield
#'
#' @examples
#' 
#' data(Qagr_adult_genotypes)
#' is.genalex(Qagr_adult_genotypes)
#' 
#' @export
#' 
is.genalex <- function(x, force = FALSE, verbose = FALSE) {
    if (! inherits(x, 'genalex'))
        return(FALSE)
    if (force == FALSE)
        return(TRUE)
    stop("force = TRUE not yet implemented")
}



#' Convert object to class 'genalex'
#' 
#' Converts object \code{x} to a data frame of class \code{'genalex'}.
#' There are five cases:
#' \itemize{
#'   \item If \code{x} is of class \code{'genalex'}, it is simply returned.
#'   \item If \code{x} is of class \code{'genalex'} and \code{force = TRUE},
#'         \code{x} is examined for consistency between data and annotations,
#'         and any inconsistencies are recalculated based on the data.  To
#'         check whether inconsistencies exist, use 
#'         \code{is.genalex(..., force=TRUE)}; add \code{verbose=TRUE} for
#'         descriptions of the inconsistencies.
#'   \item If \code{x} is of class \code{'data.frame'}, it is examined to
#'         see if it might be a data frame created by an earlier version of
#'         the \code{readGenalex} package.  If so, it is converted to
#'         class \code{'genalex'} and returned.  Any other arguments are
#'         ignored.
#'   \item If \code{x} is of class \code{'data.frame'} but does not appear to
#'         be from an earlier version of \code{readGenalex}, it is converted
#'         to class \code{'genalex'} using a call to \code{\link{genalex}}
#'         assuming a format identical to class \code{'genalex'}, where the
#'         first column holds sample names, the second column holds population
#'         names, and the remaining columns hold genotypes.
#'   \item Any other class is an error.  Further conversions between genetic
#'         data formats may be added here as additional methods.
#' }
#' 
#' @param x      An object of class \code{'genalex'} or class
#'               \code{'data.frame'}
#'
#' @param force  If \code{TRUE}, check for consistency between data and
#'               annotations in \code{x} and recalculate any inconsistent
#'               attributes.  This option is only used if \code{x} has
#'               class \code{'genalex'}.
#'
#' @param names  A list of names to apply as accepted by \code{\link{genalex}}.
#'               If any names are not provided, they are taken from the names
#'               of the corresponding columns of \code{x}.  This option is 
#'               only used if \code{x} does not have class \code{'genalex'}.
#'
#' @param ploidy Ploidy of the genotype columns in \code{x}
#'               (\code{x[, 3:ncol(x)]}).  This option is only
#'               used if \code{x} does not have class \code{'genalex'}.
#'
#' @param \dots  Additional arguments, currently ignored
#'
#' @return \code{x} converted to a class \code{'genalex'} object
#'
#' @seealso \code{\link{is.genalex}}
#'
#' @author Douglas G. Scofield
#'
#' @examples
#' 
#' data(Qagr_adult_genotypes)
#' gt <- as.genalex(Qagr_adult_genotypes)
#' 
#' @export
#'
#' @name as.genalex
#'
NULL

as.genalex <- function(x, ...) UseMethod("as.genalex")

#' @rdname as.genalex
#'
#' @export
#'
as.genalex.genalex <- function(x, force = TRUE, ...) {
    if (is.genalex(x))  # already class genalex
        return(x)
    stop("'", deparse(substitute(x)), "' cannot be coerced to class 'genalex'")
}



#' @rdname as.genalex
#'
#' @export
#' 
as.genalex.data.frame <- function(x, names = NULL, ploidy = 2, ...) {
    if (! is.null(attr(x, "genetic.data.format")) &&
             attr(x, "genetic.data.format") == "genalex") {
        # convert earlier readGenalex format to class genalex
        if (! missing(names) || ! missing(ploidy))
            warning("args ignored, converting pre-1.0 readGenalex data frame")
        attr(x, "genetic.data.format") <- NULL
        return(structure(x, class = c('genalex', 'data.frame')))
    } else {
        this.call <- sys.call()
        if (ncol(x) <= 2)
            stop("not enough columns for class 'genalex'")
        # call genalex() to coerce data frame
        nm <- names(x)
        if (is.null(names)) names = list()
        if (is.null(names$title)) names$title <- "genalex"
        if (is.null(names$sample)) names$sample <- nm[1]
        if (is.null(names$pop)) names$pop <- nm[2]
        z <- genalex(x[, 1], x[, 2], x[, 3:ncol(x), drop=FALSE], names, ploidy)
        attr(z, "data.file.name") <- capture.output(print(this.call))
        return(z)
    }
    stop("'", deparse(substitute(x)), "' cannot be coerced to class 'genalex'")
}



#' Convert class \code{'genalex'} to data frame
#' 
#' Convert an object of class \code{'genalex'} to a data frame, optionally
#' removing all \code{'genalex'}-specific attributes.  Note that the
#' behaviour of \code{stringsAsFactors} will be applied to the data frame
#' during conversion.
#' 
#' @param x         An object to convert to class \code{'data.frame'}
#'
#' @param complete  If \code{TRUE}, also removes class 
#'                  \code{'genalex'}-specific attributes
#'
#' @param \dots     Additional arguments passed to \code{as.data.frame}
#'
#' @return \code{x} as class \code{'data.frame'}.  No attributes
#'         are removed, the class is simply changed to \code{data.frame}
#'         and \code{as.data.frame} is called, with all that may entail.
#'
#' @author Douglas G. Scofield
#'
#' @seealso \code{\link{as.data.frame}}
#'
#' @export
#' 
as.data.frame.genalex <- function(x, ..., complete = FALSE) {
    if (is.genalex(x)) {
        if (complete) x <- .clearGenalexAttributes(x)
        x <- as.data.frame(structure(x, class = c('data.frame')), ...,
                             stringsAsFactors = default.stringsAsFactors())
        return(x)
    }
    stop("'", deparse(substitute(x)), "' is not class 'genalex'")
}



#' Read GenAlEx-format genotypes file
#' 
#' Reads genotype data file in GenAlEx format into an annotated data frame of
#' class \code{'genalex'}.  Internal consistency checks that are allowed by
#' the GenAlEx format are also performed as data is read.  GenAlEx and its
#' documentation are available at
#' \url{http://biology-assets.anu.edu.au/GenAlEx}.
#' 
#' \code{readGenalex} expects a genotype data file in GenAlEx format, which
#' specifies three header lines describing the structure and content of the
#' file, followed by lines containing the genotype data, along with optionally
#' extra columns specifying additional information about the sampled
#' information for other analyses.  GenAlEx format for a collection of diploid
#' samples is the following, with columns separated by \code{sep}:
#'
#' \tabular{llllll}{
#'   N loci \tab Total N samples \tab N populations \tab N pop 1 \tab N pop 2 \tab ... \cr
#'   Dataset title \tab \emph{empty} \tab \emph{empty} \tab Name pop 1 \tab Name pop 2 \tab ... \cr
#'   Sample title \tab Pop title \tab Name locus 1 \tab \emph{empty} \tab Name locus 2 \tab ... \cr
#'   ID sample 1 \tab ID sample 1 pop \tab Loc 1 allele 1 \tab Loc 1 allele 2 \tab Loc 2 allele 1 \tab ... \cr
#'   ID sample 2 \tab ID sample 2 pop \tab Loc 1 allele 1 \tab Loc 1 allele 2 \tab Loc 2 allele 1 \tab ... \cr
#'   ...  \tab ... \tab ...  \tab ...  \tab ...  \tab ... \cr
#' }
#'
#' Calling \code{readGenalex} for a file first reads the top 3 header lines,
#' then reads the remainder of the file checking for consistency with the data
#' description from the header lines.  It attempts to cleanly ignore extra
#' delimiters that Excel might add when exporting a delimited file.
#'
#' After reading, the first two columns of the data frame containing the
#' sample and population names are stored as \code{character}, while the
#' genotype columns are stored as \code{numeric}, as that is the specified
#' type for genotype information in GenAlEx.  As such, it is an error for
#' these columns to contain non-numeric values that do not match
#' \code{na.strings}.
#'
#' Extra columns beyond the genotype columns are allowed. If these columns are
#' named, they are read along with the genotype columns and are stored as a
#' data frame in the \code{extra.columns} attribute and
#' \code{\link{writeGenalex}} will write their values in the columns 
#' immediately to the right of the genotype values.  These data are given 
#' their natural type as if read with 
#' \code{read.table(..., stringsAsFactors = FALSE)}, so that 
#' character values are not converted to factors.
#'
#' More information on GenAlEx is available at
#' \url{http://biology-assets.anu.edu.au/GenAlEx}.  In particular, genotype
#' information must be encoded numerically.
#' 
#' @param file       Delimited text file in GenAlEx format, typically exported
#'                   as tab- or comma-delimited text from Excel
#' 
#' @param sep        Column separator used when \code{file} was created 
#'                   (defaults to tab)
#' 
#' @param ploidy     The ploidy of genotypes encoded in \code{file} (defaults
#'                   to 2)
#' 
#' @param na.strings Strings encoding missing data.  Default is to include the
#'                   GenAlEx missing values ("0" and "-1") as well as ".",
#'                   "NA" and "" (empty).
#' 
#' @param \dots      Additional arguments passed to \code{\link{scan}} when
#'                   reading data
#' 
#' @return An annotated data frame of class \code{'genalex'} containing sample
#' data, with column names determined by line 3 of the input file.  Special
#' \code{attributes} of the data frame include:
#' 
#' \item{data.file.name }{The value of \code{file}}
#' \item{ploidy }{Ploidy of input data}
#' \item{n.loci }{Number of loci}
#' \item{n.samples }{Total number of samples}
#' \item{n.pops }{Number of populations}
#' \item{pop.labels }{Names of populations}
#' \item{pop.sizes }{Sizes of populations}
#' \item{dataset.title }{Dataset title}
#' \item{sample.title }{Sample title}
#' \item{pop.title }{Population title}
#' \item{locus.names }{Names of loci}
#' \item{locus.columns }{Numeric column position of allele 1 of each locus in
#'   the data frame}
#' \item{extra.columns }{\code{data.frame} containing any extra columns given
#'   in \code{file} to the right of the genotype columns.  Row order is the 
#'   same as for the genotype data.  Data are given their natural types using
#'   \code{type.convert(..., as.is = TRUE)}, so that characters are 
#'   not converted to factors.  If no extra columns were found, this 
#'   attribute does not exist.}
#' \item{genetic.data.format }{\code{"genalex"}, not present in package versions >= 1.0}
#' 
#' @author Douglas G. Scofield
#' 
#' @references Peakall, R. and Smouse P.E. (2012) GenAlEx 6.5: genetic analysis
#' in Excel. Population genetic software for teaching and research-an update.
#' \emph{Bioinformatics} 28, 2537-2539.
#' 
#' Peakall, R. and Smouse P.E. (2006) GENALEX 6: genetic analysis in Excel.
#' Population genetic software for teaching and research. \emph{Molecular
#' Ecology Notes} 6, 288-295.
#' 
#' @keywords file manip attribute
#' 
#' @seealso \code{\link{read.table}}, \code{\link{type.convert}}
#' 
#' @examples
#' 
#' data(Qagr_adult_genotypes)
#' head(Qagr_adult_genotypes)
#' attributes(Qagr_adult_genotypes)
#' 
#' @export
#' 
readGenalex <- function(file, sep = "\t", ploidy = 2,
                        na.strings = c("0", "-1", ".", "NA", ""),
                        ...) {
    if (ploidy != 2) warning("ploidy other than 2 not tested")
    fcon <- file(description = file, open = "rt")
    header <- .readGenalexHeader(fcon, sep, ploidy)
    # returns a list
    raw.data <- .readGenalexData(fcon, sep, header$data.column.names,
                                 header$n.samples, header$n.loci,
                                 ploidy, na.strings, header$extra.columns,
                                 ...)
    close(fcon)
    x <- .readGenalexJoinData(header, raw.data)
    attr(x, "data.file.name") <- file
    class(x) <- c('genalex', 'data.frame')
    return(x)
}



####################################
## Internal functions that do the heavy lifting


.readGenalexData <- function(con, sep, col.names, n.samples, n.loci,
                             ploidy, na.strings, extra.columns = character(0),
                             ...) {
    classes <- c("character", "character", rep("numeric", n.loci*ploidy))
    scan.col.names = col.names
    extra.columns <- extra.columns[extra.columns != ""]
    if (length(extra.columns)) {
        classes <- c(classes, rep("character", length(extra.columns)))
        scan.col.names = c(col.names, extra.columns)
    }
    # Use scan() so that we can handle data lines that contain more trailing
    # column separators than data dolumns, due to what Excel does when
    # exporting tab-delimited files
    what <- sapply(classes, do.call, list(0))
    names(what) = scan.col.names
    dat <- scan(file = con, what = what, nmax = n.samples, 
                na.strings = na.strings, flush = TRUE, quiet = TRUE, ...)
    extra.dat = NULL
    if (length(extra.columns)) {
      extra.dat = dat[names(what) %in% extra.columns]
      dat = dat[! names(what) %in% extra.columns]
      # convert types, but keep characters as characters
      extra.dat <- lapply(extra.dat, type.convert, as.is = TRUE)
      extra.dat <- as.data.frame(extra.dat, stringsAsFactors = FALSE)
    }
    dat <- as.data.frame(dat, stringsAsFactors = FALSE)
    list(dat = dat, extra.columns = extra.dat)
}



.calculateLocusColumns <- function(n.loci, ploidy) {
    seq(from = 3, to = (3+(n.loci-1)*ploidy), by = ploidy)
}



.readGenalexHeader <- function(con, sep, ploidy) {
    dlines <- readLines(con = con, n = 3, ok = FALSE)
    dlines <- lapply(dlines, function(x) unlist(strsplit(x, sep, perl = TRUE)))
    dlines[[1]] <- as.numeric(dlines[[1]])
    header <- list(n.loci = dlines[[1]][1], 
                   ploidy = ploidy,
                   n.samples = dlines[[1]][2], 
                   n.pops = dlines[[1]][3],
                   dataset.title = dlines[[2]][1],
                   sample.title = dlines[[3]][1],
                   pop.title = dlines[[3]][2])
    header$pop.labels <- dlines[[2]][4:(4+header$n.pops-1)]
    if (length(dlines[[3]]) >= 3 + header$n.loci*ploidy) {
        # extra columns beyond the genotype columns, do we want to load them?
        # if we do load them, we load them initially as character into a 
        # separate data.frame attached to the "extra.columns" attribute
        extra.columns <- 
            dlines[[3]][(3+header$n.loci*ploidy):length(dlines[[3]])]
        if (any(length(extra.columns) > 0))  # any of them are named
            header$extra.columns <- extra.columns
    }
    pop.sizes <- dlines[[1]][4:(4+header$n.pops-1)]
    names(pop.sizes) <- header$pop.labels
    header$pop.sizes <- pop.sizes
    header$locus.columns <- .calculateLocusColumns(header$n.loci, ploidy)
    header$locus.names <- dlines[[3]][header$locus.columns]
    header
}



.readGenalexJoinData <- function(header, raw.data) {
    dat <- raw.data$dat
    if (! is.null(raw.data$extra.columns)) {
        # add sample name to extra columns
        extra.columns <- cbind(dat[,1], raw.data$extra.columns)
        names(extra.columns)[1] <- names(dat)[1]
    }
    f <- function(x) {
        c(x, if (header$ploidy > 1)
                 paste(sep = ".", x, seq(2, header$ploidy, 1))
             else NULL)
    }
    data.column.names <- c(header$sample.title, header$pop.title,
                           unlist(lapply(header$locus.names, f)))
    names(dat) <- data.column.names
    # TODO: handle label in header with size 0 and missing from data?
    pop.labels.header <- sort(header$pop.labels)
    pop.labels.data <- sort(levels(factor(dat[[header$pop.title]])))
    if (suppressWarnings(any(pop.labels.header != pop.labels.data))) {
        err <- pop.labels.data[! pop.labels.data %in% pop.labels.header]
        if (length(err))
          warning("population labels in data but not  header: ", 
                  paste(err, collapse = ","))
        err <- pop.labels.header[! pop.labels.header %in% pop.labels.data]
        if (length(err))
          warning("population labels in header but not in data: ", 
                  paste(err, collapse = ","))
        stop("fatal population label mismatch between header and data")
    }
    # TODO: handle label in header with size 0 and missing from data?
    pops.in.order <- names(header$pop.sizes)
    pop.sizes.in.data <- table(dat[[header$pop.title]])[pops.in.order]
    mism <- pop.sizes.in.data != header$pop.sizes
    if (any(mism)) {
        err1 <- paste(collapse = ",",header$pop.labels[mism])
        err2 <- paste(paste(sep = " != ", collapse = ", ", pop.sizes.in.data[mism],
                            header$pop.sizes[mism]))
        stop("sizes of populations ", err1,
             " do not match in header and data: ", err2)
    }
    for (nm in names(header))
        attr(dat, nm) <- header[[nm]]
    if (! is.null(raw.data$extra.columns))
        attr(dat, "extra.columns") <- extra.columns
    dat
}



#' Combine class \code{genalex} data sets
#'
#' Combine class \code{genalex} data sets onto one larger class \code{genalex}
#' data set.  Population names and sizes are adjusted accordingly.  The
#' data sets must have the same locus names and ploidy; the order of the loci
#' may differ, and the final data set will have the locus order of the first.
#' Sample names must be unique across all data sets.  Data set title and
#' sample and population column headers are taken from the first data set
#' unless supplied in the \code{names} argument.  If one data set contains
#' extra columns, all must contain extra columns, and these are combined along
#' with the rest of the data.
#' 
#' @param \dots   Class \code{genalex} data sets.  If only one data set
#'                is supplied, it is returned unmodified.
#'
#' @param names   List of names: \code{title} for data set title,
#'                \code{sample} for sample column header, and
#'                \code{pop} for population column header
#'
#' @param deparse.level Not used (yet)
#'
#' @return Annotated data frame of class \code{'genalex'}.  If \code{names}
#' or any of its fields are not provided, the names of the first argument
#' are used.  The \code{data.file.name} attribute is a character
#' representation of the call to \code{rbind}.
#'
#' @note If one of the arguments is class \code{'data.frame'}, then this
#' function will \emph{not} be called, instead the \code{rbind.data.frame}
#' method of base R will be called silently and will return an object of
#' class \code{'data.frame'}.  This occurs because objects of class
#' \code{'genalex'} also have class \code{'data.frame'}.  This occurs
#' during method dispatch for \code{rbind}, so it is not a condition that
#' can be checked by this function.  Assure that data frames have been
#' converted to class \code{'genalex'} prior to calling this function
#' by using \code{as.genalex}.
#'
#' @author Douglas G. Scofield
#'
#' @seealso \code{\link{genalex}}, \code{\link{rbind}}, \code{\link{as.genalex}}
#'
#' @examples
#'
#' gt1 <- data.frame(a = 11:13, a.2 = 14:16, b = 101:103, b.2 = 104:106)
#' x1 <- genalex(1:3, "snurf", gt1)
#' gt2 <- data.frame(a = 21:23, a.2 = 24:26, b = 201:203, b.2 = 204:206)
#' x2 <- genalex(4:6, "snirf", gt2)
#' x <- rbind(x1, x2)
#' x
#' attributes(x)
#'
#' @export
#'
rbind.genalex <- function(..., names, deparse.level = 1) {
    # dummy up a call for data.file.name
    this.call <- sys.call()
    a <- paste(collapse = ", ",
               unlist(lapply(as.list(substitute(list(...)))[-1L],
                             as.character)))
    if (! missing(names))
        a <- paste(sep = ", ", a, 
                   paste("names =", deparse(substitute(names))))
    this.call <- paste(sep="", as.character(this.call[[1]]), "(", a, ")")
    # verify args
    if (deparse.level != 1)
        .NotYetUsed("deparse.level")
    dots <- list(...)
    if (! all(sapply(dots, is.genalex)))
        stop("all arguments must be class 'genalex'")
    if (length(dots) == 1)
        return(dots[[1]])
    dat.1 <- dots[[1]]
    att.1 <- attributes(dat.1)
    if (! all(sapply(dots, function(x) {
                               is.null(attr(x, "extra.columns")) == 
                               is.null(att.1$extra.columns)})))
        stop("all arguments must either have or lack extra columns")
    if (! all(sapply(dots, function(x) attr(x, "ploidy") == att.1$ploidy)))
        stop("all arguments must have the same ploidy")
    # make sure genotypes and extra columns are consistent
    locn.1 <- att.1$locus.names
    equal.but.order <- function(a, b) all(a %in% b) && all(b %in% a)
    extra.columns <- if (is.null(att.1$extra.columns)) NULL 
        else list(attr(dots[[1]], "extra.columns"))
    for (i in 2:length(dots)) {
        if (! equal.but.order(locn.1, attr(dots[[i]], "locus.names")))
            stop("all arguments must contain the same loci")
        else if (! all(locn.1 == attr(dots[[i]], "locus.names")))
            dots[[i]] <- reorderLoci(dots[[i]], locn.1)
        if (! is.null(extra.columns))
            extra.columns[[i]] <- attr(dots[[i]], "extra.columns")
    }
    alldat <- do.call(rbind, lapply(dots, as.data.frame))
    allextra <- if (is.null(extra.columns)) NULL 
        else do.call(rbind, lapply(extra.columns, as.data.frame))
    x <- genalex(alldat[, 1], alldat[, 2], alldat[, 3:ncol(alldat)],
                 ploidy = att.1$ploidy, extra.columns = allextra)
    attr(x, "dataset.title") <- if (missing(names) || is.null(names$title))
        att.1$dataset.title else names$title
    attr(x, "sample.title") <- if (missing(names) || is.null(names$sample))
        att.1$sample.title else names$sample
    attr(x, "pop.title") <- if (missing(names) || is.null(names$pop))
        att.1$pop.title else names$pop
    attr(x, "data.file.name") <- this.call  # note different from genalex()
    return(x)
}



#' Create new object of class \code{'genalex'} from constituent data
#'
#' Create a new object of class \code{'genalex'} given sample and
#' population names and genotype data.  Titles for the dataset, sample
#' and population columns, and loci may be provided via the \code{names}
#' argument.
#' 
#' @param samples    Sample names, must be unique and length must
#'                   match the number of rows in \code{genotypes}
#'
#' @param pops       Population names.  If \code{pops} is shorter than
#'                   the number of samples, it will be expanded following
#'                   the rules described in \code{\link{data.frame}}.
#' 
#'
#' @param genotypes  Genotype values, must be numeric
#'
#' @param names      List of names: \code{title} for data set title,
#'                   \code{sample} for sample column header,
#'                   \code{pop} for population column header, and
#'                   \code{loci} for names of loci.  If \code{loci}
#'                   is missing, the corresponding \code{genotype}
#'                   column names are used.
#'
#' @param ploidy     Ploidy of \code{genotypes}
#'
#' @param extra.columns Extra data columns, see \code{\link{readGenalex}}
#'
#' @return Annotated data frame of class \code{'genalex'}.  If \code{names}
#' or any of its fields are not provided, default names are used.  The 
#' \code{data.file.name} attribute is a character representation of the call
#' to \code{genalex}.
#'
#' @author Douglas G. Scofield
#'
#' @seealso \code{\link{readGenalex}}, \code{\link{data.frame}}
#'
#' @examples
#'
#' gt <- data.frame(a = 11:13, a.2 = 14:16, b = 101:103, b.2 = 104:106)
#' nms <- list(title = "Example")
#' x <- genalex(1:3, "snurf", gt, nms)
#' x
#' attributes(x)
#'
#' @export
#'
genalex <- function(samples, pops, genotypes, names = NULL, ploidy = 2,
                    extra.columns = NULL) {
    this.call <- sys.call()
    genotypes <- as.data.frame(genotypes)
    if (length(samples) != nrow(genotypes))
        stop("'samples' and 'genotypes' must have the same length")
    if (anyDuplicated(samples))
        stop("sample names must be unique")
    n.loci <- ncol(genotypes) / ploidy
    if (as.integer(n.loci) != n.loci)
        stop("'genotypes' must have a number of columns dividable by ploidy")
    dat <- data.frame(samples = samples, pops = pops, stringsAsFactors = FALSE)
    if (! all(sapply(genotypes, function(x) all(is.numeric(x) | is.na(x)))))
        stop("genotype data must be numeric")
    dat <- cbind(dat, genotypes)
    if (! is.null(extra.columns))
        extra.columns <- as.data.frame(extra.columns)
    pop.sizes <- sapply(split(dat$pops, dat$pops), length)
    header <- list(n.loci = n.loci,
                   ploidy = ploidy,
                   n.samples = nrow(dat),
                   n.pops = length(pop.sizes),
                   pop.labels = names(pop.sizes),
                   pop.sizes = pop.sizes,
                   locus.columns = .calculateLocusColumns(n.loci, ploidy))
    header$dataset.title <- if (missing(names) || is.null(names$title))
        "genalex" else names$title
    header$sample.title <- if (missing(names) || is.null(names$sample))
        "sample" else names$sample
    header$pop.title <- if (missing(names) || is.null(names$pop))
        "pop" else names$pop
    header$locus.names <- if (missing(names) || is.null(names$loci))
        names(dat)[header$locus.columns] else names$loci
    x <- .readGenalexJoinData(header, list(dat = dat, 
                                           extra.columns = extra.columns))
    attr(x, "data.file.name") <- capture.output(print(this.call))
    class(x) <- c('genalex', 'data.frame')
    return(x)
}



#' Write GenAlEx-format genotypes to a text file
#' 
#' Writes genotype data encoded in an annotated data frame of class
#' \code{'genalex'} to a GenAlEx-format text file.  Extra data columns are
#' included immediately to the right of genotype columns.  GenAlEx and its
#' documentation are available at
#' \url{http://biology-assets.anu.edu.au/GenAlEx}.
#' 
#' This function writes genotypes and associated information within an
#' annotated data frame of class \code{'genalex'} to a text file in GenAlEx
#' format. More information is available in the description for
#' \code{\link{readGenalex}}, and at the GenAlEx website at
#' \url{http://biology-assets.anu.edu.au/GenAlEx}.
#' 
#' Doing \code{writeGenalex(readGenalex("file.txt"), "file-write.txt")} won't
#' necessarily produce an output file identical to the input file.  Three
#' areas for which this will likely be true are:
#' 
#' \enumerate{
#'    \item Names on columns for alleles other than the first in a locus,
#'          which are ignored by \code{readGenalex}, converted to a simple
#'          concatenation of locus name and allele number in the resulting
#'          class \code{'genalex'}, and are left out of the output of
#'          \code{writeGenalex}.
#'    \item Locations of additional data columns beyond the genotype columns,
#'          which \code{readGenalex} will collect wherever there are named
#'          columns to the right of the genotype columns, and which
#'          \code{writeGenalex} will write immediately to the right of the
#'          genotype columns.  The same column names are used when writing
#'          as were present when reading.
#'    \item Missing data will be coded with the values in \code{na} and
#'          \code{na.character}, regardless of the coding used when the data
#'          were read.
#' }
#'
#' @param x     Annotated data frame of class \code{'genalex'}
#'
#' @param file  File name or connection for writing.  If given as \code{""},
#'              \code{stdout()} is used.
#'
#' @param quote Logical value (\code{TRUE} or \code{FALSE}).  If \code{TRUE},
#'              all character data are surrounded by double quotes, and all
#'              header fields except for counts are quoted if they exist.
#'              Note that genotype data will not be quoted, as they are
#'              numeric values.  Data in the extra columns will be quoted,
#'              unless some have been made numeric since being read.  If
#'              \code{FALSE}, nothing is quoted.
#'
#' @param sep   Column separator for output (defaults to \code{"\t"}).
#'
#' @param eol   End-of-line character used for output (defaults to \code{"\n"}).
#'
#' @param na    The string to use when writing missing values in genotype
#'              data.  Defaults to \code{"0"}.
#'
#' @param na.character The string to use when writing missing values in
#'              character data.  Defaults to \code{""}.
#'
#' @return No value is returned.
#'
#' @author Douglas G. Scofield
#'
#' @seealso \code{\link{readGenalex}}
#'
#' @references Peakall, R. and Smouse P.E. (2012) GenAlEx 6.5: genetic analysis
#' in Excel. Population genetic software for teaching and research-an update.
#' \emph{Bioinformatics} 28, 2537-2539.
#'
#' Peakall, R. and Smouse P.E. (2006) GENALEX 6: genetic analysis in Excel.
#' Population genetic software for teaching and research. \emph{Molecular
#' Ecology Notes} 6, 288-295.
#'
#' @keywords file manip attribute
#'
#' @examples
#'
#' data(Qagr_adult_genotypes)
#' # lots of output to terminal
#' writeGenalex(Qagr_adult_genotypes, file = "")
#'
#' @export
#'
writeGenalex <- function(x, file, quote = FALSE, sep = "\t", eol = "\n",
                         na = "0", na.character = "") {
    DNAME <- deparse(substitute(x))
    if (! is.genalex(x))
        stop(DNAME, " must be class 'genalex'")
    if (file == "")
        file <- stdout()
    else if (is.character(file)) {
        file <- file(description = file, open = "wt")
        on.exit(close(file))
    }
    else if (! isOpen(file, "w")) {
        open(file, "w")
        on.exit(close(file))
    }
    if (! inherits(file, "connection"))
        stop("'file' must be a character string or connection")
    a <- attributes(x)
    # quote function
    qu <- function(x) if (quote) paste(sep="", "\"", x, "\"") else x
    # coerce two left columns to character, and apply na.character
    x[, 1] <- as.character(x[, 1])
    x[, 2] <- as.character(x[, 2])
    x[, 1:2][is.na(x[, 1:2])] <- na.character  # character columns
    x[, 1] <- qu(x[, 1])
    x[, 2] <- qu(x[, 2])
    # now genotype columns
    x[, 3:ncol(x)][is.na(x[, 3:ncol(x)])] <- na
    # ... and extra columns, by type
    if (! is.null(extra <- a$extra.columns)) {
        for (i in 1:ncol(extra)) {
            if (is.numeric(extra[, i])) {
                extra[, i][is.na(extra[, i])] <- na
            } else {
                extra[, i] <- as.character(extra[, i])
                extra[, i][is.na(extra[, i])] <- na.character
                extra[, i] <- qu(extra[, i])
            }
        }
    }
    # header line 1
    cat(file = file, sep = sep, a$n.loci, a$n.samples, a$n.pops, a$pop.sizes)
    cat(file = file, eol)
    # header line 2
    cat(file = file, sep = sep, qu(a$dataset.title), "", "", qu(a$pop.labels))
    cat(file = file, eol)
    # header line 3, allele columns other than the first for each locus have a
    # blank header
    cat(file = file, sep = sep, qu(a$sample.title), qu(a$pop.title),
        paste(collapse = paste(collapse = "", rep(sep, a$ploidy)),
              qu(a$locus.names)),
        rep("", a$ploidy - 1))  # all but first locus column have blank names
    # if extra columns, add headers for those
    if (! is.null(extra))
        cat(file = file, sep = sep, qu(names(extra)))
    cat(file = file, eol)
    # data plus extra columns
    for (i in 1:nrow(x)) {
        cat(file = file, paste(collapse = sep, x[i, ]))
        if (! is.null(extra))
            cat(file = file, paste(collapse = sep, extra[i, ]))
        cat(file = file, eol)
    }
}



#' Summarise contents of class 'genalex' data frame
#' 
#' This prints a few lines summarising the data set title, sample size,
#' population sizes, ploidy, number of loci, and locus names, followed
#' by a summary of the data frame and a summary of the extra columns,
#' if present.
#' 
#' @param  object An annotated data frame of class \code{'genalex'}
#' 
#' @param  ...    Additional arguments passed to \code{'summary'}
#'                for the data frame
#' 
#' @return Result of \code{summary.data.frame(object)}
#'
#' @author Douglas G. Scofield
#'
#' @examples
#' 
#' data(Qagr_adult_genotypes)
#' summary(Qagr_adult_genotypes)
#' 
#' @export
#'
summary.genalex <- function(object, ...) {
    stopifnot(is.genalex(object))
    a <- attributes(object)
    cat("Dataset title:", a$dataset.title, "\n")
    cat("Number of samples:", nrow(object), "\n")
    cat("Number of populations:", a$n.pops, "\n")
    cat("Population sizes:\n")
    print(a$pop.sizes)
    cat("\nPloidy:", a$ploidy, "\n")
    cat("Number of loci:", a$n.loci, "\n")
    cat("Locus names:\n")
    print(a$locus.names)
    cat("\nSummary of genotypes:\n")
    print(oo <- NextMethod(object, ...))  # this may not be interesting here
    if (! is.null(a$extra.columns)) {
        cat("\nSummary of extra.columns data frame:\n")
        print(summary(a$extra.columns))
    }
    invisible(oo)
}


 
#' Print selected genotypes
#' 
#' Print selected genotypes, optionally calling out a locus
#' 
#' @param x    An annotated data frame of class \code{'genalex'}
#' 
#' @param rows The specific rows of \code{x} to print, default is
#'        all rows
#' 
#' @param callout.locus One or more loci on \code{x} to be surrounded by
#'        \code{callout.char} when printed
#' 
#' @param sep    Separator character to be used between loci
#' 
#' @param allele.sep Separator character to be used between alleles
#' 
#' @param callout.char Character which surrounds loci specified by
#'        \code{callout.locus}
#' 
#' @param label  Label to be included between the sample and population ID
#'        columns and the genotype columns in output
#'
#' @param \dots  Additional arguments, currently ignored
#'
#' @return No specific return value, used for its side effect of printing
#'         genotypes.
#'
#' @author Douglas G. Scofield
#'
#' @examples
#' 
#' data(Qagr_adult_genotypes)
#' printGenotype(Qagr_adult_genotypes, rows = 6:8, callout.locus = "1c08")
#' 
#' @export
#'
#' @name printGenotype
#'
NULL

printGenotype <- function(x, ...) UseMethod("printGenotype")

#' @rdname printGenotype
#'
#' @export
#'
printGenotype.genalex <- function(x, rows = 1:nrow(x), callout.locus = NULL,
                                  sep = " ", allele.sep = "/", callout.char = "*", 
                                  label = NULL, ...) {
    stopifnot(is.genalex(x))
    cols <- names(x)
    ploidy <- attr(x, "ploidy")
    for (row in rows) {
        cat(paste(sep = sep, as.character(x[row, 1]), 
                  as.character(x[row, 2])))
        if (! is.null(label))
            cat("", label)
        full.gt <- ""
        for (col in seq(from = 3, to = length(cols), by = ploidy)) {
            gt <- paste(collapse = allele.sep, x[row, col:(col+ploidy-1)])
            if (cols[col] %in% callout.locus) 
                gt <- paste(sep = "", callout.char, gt, callout.char)
            full.gt <- paste(sep = sep, collapse = sep, full.gt, gt)
        }
        cat(sep = "", full.gt, "\n")
    }
}



#' Determine numeric column positions occupied by named loci
#' 
#' Determine the numeric column positions occupied by named loci in an
#' object of class \code{'genalex'}.  This is mostly used as
#' a utility routine by other functions in the \code{readGenalex} package.
#' 
#' @param x      An annotated data frame created by \code{readGenalex}
#' 
#' @param locus  The names of one or more loci found in \code{x}
#' 
#' @param \dots  Additional arguments, currently ignored
#' 
#' @return A vector of column positions occupied by genotype data for loci
#'         named in \code{locus}.
#' 
#' @author Douglas G. Scofield
#' 
#' @examples
#' 
#' data(Qagr_adult_genotypes)
#' getLocusColumns(Qagr_adult_genotypes, c("0c19", "0m05"))
#' 
#' @export
#' 
#' @name getLocusColumns
#'
NULL

getLocusColumns <- function(x, ...) UseMethod("getLocusColumns")

#' @rdname getLocusColumns
#'
#' @export
#' 
getLocusColumns.genalex <- function(x, locus, ...) {
    as.vector(sapply(attr(x, "locus.columns")[attr(x, "locus.names") %in% locus],
                     function(y) y:(y + attr(x, "ploidy") - 1)))
}



#' Reorder class \code{'genalex'} genotype columns
#' 
#' Reorder the genotype columns of a class \code{'genalex'} object by locus.
#' 
#' @param x    An annotated data frame of class \code{'genalex'}
#' 
#' @param loci The names of loci found in \code{x}, in the desired new 
#'             order.  All loci in \code{x} must be named, and no loci
#'             may be duplicated.  The order of
#'             the alleles within each locus is preserved.
#'
#' @param \dots  Additional arguments, currently ignored
#' 
#' @return A data frame of class \code{'genalex'} containing the same 
#' genotype data from \code{x} reordered according to \code{loci}.
#' 
#' @author Douglas G. Scofield
#' 
#' @examples
#' 
#' data(Qagr_adult_genotypes)
#' # reverse loci
#' loci <- rev(attr(Qagr_adult_genotypes, "locus.names"))
#' reord = reorderLoci(Qagr_adult_genotypes, rev(loci))
#' 
#' @export
#' 
#' @name reorderLoci
#'
NULL

reorderLoci <- function(x, ...) UseMethod("reorderLoci")

#' @rdname reorderLoci
#'
#' @export
#' 
reorderLoci.genalex <- function(x, loci, ...) {
    existing.loci <- attr(x, "locus.names")
    if (! (all(existing.loci %in% loci) && all(loci %in% existing.loci))) 
        stop("reorder list must contain all existing loci")
    if (length(loci) != length(existing.loci))
        stop("loci must appear only once")
    newdata <- x[,1:2]
    for (locus in loci) {
        newdata <- cbind(newdata, getLocus(x, locus))
    }
    names.newdata <- names(newdata)
    attributes(newdata) <- attributes(x)
    names(newdata) <- names.newdata
    attr(newdata, "locus.names") <- loci
    newdata
}



.clearGenalexAttributes <- function(x) {
    attr(x, "data.file.name") <- NULL
    attr(x, "ploidy") <- NULL
    attr(x, "n.loci") <- NULL
    attr(x, "n.samples") <- NULL
    attr(x, "n.pops") <- NULL
    attr(x, "pop.labels") <- NULL
    attr(x, "pop.sizes") <- NULL
    attr(x, "dataset.title") <- NULL
    attr(x, "sample.title") <- NULL
    attr(x, "pop.title") <- NULL
    attr(x, "locus.names") <- NULL
    attr(x, "locus.columns") <- NULL
    attr(x, "extra.columns") <- NULL
    return(x)
}



#' Return genotype data for specified loci
#' 
#' Return genotype data for specified loci in an object of class
#' \code{'data.frame'}, optionally restricted to samples from specific
#' populations.
#' 
#' 
#' @param x     An annotated data frame of class \code{'genalex'}
#' 
#' @param locus The names of one or more loci found in \code{x}
#' 
#' @param pop   If supplied, return only data for samples from the specified
#'              populations
#'
#' @param \dots  Additional arguments, currently ignored
#' 
#' @return An object of class \code{'data.frame'} containing genotype data 
#' from \code{x} for loci specified in \code{code}, optionally restricted
#' to samples from populations specified in \code{pop}.
#' 
#' @author Douglas G. Scofield
#' 
#' @examples
#' 
#' data(Qagr_pericarp_genotypes)
#' nm <- attr(Qagr_pericarp_genotypes, "locus.names")
#' # get the first locus
#' loc1 <- getLocus(Qagr_pericarp_genotypes, nm[1])
#' # get the second locus of the second population
#' po <- attr(Qagr_pericarp_genotypes, "pop.labels")
#' loc2.pop2 <- getLocus(Qagr_pericarp_genotypes, nm[2], po[2])
#' 
#' @export
#' 
#' @name getLocus
#' 
NULL

getLocus <- function(x, ...) UseMethod("getLocus")

#' @rdname getLocus
#' 
#' @export
#' 
getLocus.genalex <- function(x, locus, pop = NULL, ...) {
    cols <- getLocusColumns(x, locus)
    if (! is.null(pop)) {
        pop.column <- attr(x, "pop.title")
        x <- subset(x, x[[pop.column]] %in% pop)
    }
    x <- as.data.frame(x[, cols, drop=FALSE])
    x <- .clearGenalexAttributes(x)
    return(x)
}



#' Replace genotype data in data frame of class \code{'genalex'}
#' 
#' Replace genotype data for specified loci in a data frame of class
#' \code{'genalex'}.
#' 
#' @param x       A data frame of class \code{'genalex'}
#' 
#' @param locus   The names of one or more loci found in \code{x}
#' 
#' @param newdata New genotype data for loci specified in \code{locus}.  Must
#'                have the same number of rows as \code{x}
#'
#' @param \dots  Additional arguments, currently ignored
#' 
#' @return A data frame of class \code{'genalex'} containing genotype data
#' from \code{x} with data for loci specified in \code{locus} replaced
#' with data from \code{newdata}.
#' 
#' @author Douglas G. Scofield
#' 
#' @export
#' 
#' @name replaceLocus
#' 
NULL

replaceLocus <- function(x, ...) UseMethod("replaceLocus")

#' @rdname replaceLocus
#' 
#' @export
#' 
replaceLocus.genalex <- function(x, locus, newdata, ...) {
    x[, getLocusColumns(x, locus)] <- newdata
    x
}



#' Remove specified loci from data frame of class \code{'genalex'}
#' 
#' Remove specified loci from the data frame of class \code{'genalex'}
#' and updates attributes
#' 
#' @param x          An annotated data frame of class \code{'genalex'}
#' 
#' @param drop.locus The names of one or more loci found in \code{x}
#' 
#' @param quiet      If \code{FALSE}, produce an error if any of
#'                   \code{drop.locus} are not found in \code{x}.  If
#'                   \code{TRUE}, apply whichever of \code{drop.locus}
#'                   are found in \code{x} and return the result.
#'
#' @param \dots  Additional arguments, currently ignored
#' 
#' @return A data frame containing the data in \code{x} after removing
#' loci specified by \code{drop.locus}, with attributes updated as
#' required.
#' 
#' @author Douglas G. Scofield
#' 
#' @examples
#' 
#' data(Qagr_adult_genotypes)
#' newdat <- dropLocus(Qagr_adult_genotypes, "Oe09")
#' 
#' @export
#' 
#' @name dropLocus
#' 
NULL

dropLocus <- function(x, ...) UseMethod("dropLocus")

#' @rdname dropLocus
#' 
#' @export
#' 
dropLocus.genalex <- function(x, drop.locus, quiet = FALSE, ...) {
    if (missing(drop.locus) || is.null(drop.locus))
        return(x)
    locus.names <- attr(x, "locus.names")
    if (! all(drop.locus %in% locus.names)) {
        if (! quiet || ! any(drop.locus %in% locus.names))
            stop("locus not present")
        drop.locus <- drop.locus[drop.locus %in% locus.names]
        if (length(drop.locus) == 0)
            return(x)
    }
    att <- attributes(x)
    x <- x[, -getLocusColumns(x, drop.locus)]
    for (a in names(att))
        if (! a %in% c("names", "n.loci", "locus.names", "locus.columns"))
            attr(x, a) <- att[[a]]
    locus.names <- locus.names[! locus.names %in% drop.locus]
    attr(x, "n.loci") <- length(locus.names)
    attr(x, "locus.names") <- locus.names
    attr(x, "locus.columns") <- which(names(x) %in% locus.names)
    x
}



#' Reduce the ploidy of an object of class \code{'genalex'}
#' 
#' Reduce the ploidy of an object of class \code{'genalex'}.
#' Currently restricted to reducing the ploidy of diploid data to haploid by
#' selecting only the first allele of each locus.
#' 
#' This function reduced the ploidy of a data frame of class \code{'genalex'}
#' data frame by selecting the first allele of each locus.  Occasionally
#' haploid data is encoded in GenAlEx datasets by using homozygous diploid
#' loci, and this is a useful function for making these truly haploid.
#' 
#' @param x          An annotated data frame of class \code{'genalex'}
#' 
#' @param new.ploidy The desired new ploidy.  Currently, the only usefully
#'                   accepted value is 1, with ploidy of \code{x} being 2;
#'                   a ploidy matching the current ploidy of \code{x} 
#'                   silently returns \code{x}.
#'
#' @param \dots  Additional arguments, currently ignored
#' 
#' @return A data frame of class \code{'genalex'} containing genotype data
#' from \code{x} reduced to the specified \code{new.ploidy}, with 
#' attributes updated as required.
#' 
#' @author Douglas G. Scofield
#' 
#' @examples
#' 
#' data(Qagr_adult_genotypes)
#' attr(Qagr_adult_genotypes, "ploidy")
#' p1 <- reducePloidy(Qagr_adult_genotypes, 1)
#' 
#' @export
#' 
#' @name reducePloidy
#' 
NULL

reducePloidy <- function(x, ...) UseMethod("reducePloidy")

#' @rdname reducePloidy
#' 
#' @export
#' 
reducePloidy.genalex <- function(x, new.ploidy = 1, ...) {
    # Would be nice to be more general, e.g., pick other than the first
    # column, or a random allele
    att <- attributes(x)
    if (new.ploidy == att$ploidy) 
        return(x)
    else if (new.ploidy > att$ploidy) 
        stop("new ploidy ", new.ploidy, " greater than existing ploidy ",
             att$ploidy)
    else if (new.ploidy != 1 || att$ploidy != 2) 
        stop("can't currently handle new.ploidy other than 1,",
             " existing ploidy other than 2")
    new.col <- c(1:(att$locus.columns[1]-1), att$locus.columns)
    x <- x[, new.col]
    for (a in names(att))
        if (! a %in% c("names","locus.columns","ploidy"))
            attr(x, a) <- att[[a]]
    attr(x, "locus.columns") <- att$locus.columns -
                                 (0:(att$n.loci - 1) * 
                                   (att$ploidy - new.ploidy))
    attr(x, "ploidy") <- new.ploidy
    x
}
