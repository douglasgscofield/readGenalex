#' Read and manipulate GenAlEx-format genotype files
#'
#' A collection of R functions to read, manipulate and write genotype data in
#' GenAlEx format.  GenAlEx is a widely-used Excel plugin for manipulating and
#' analysing genotype data.  This package reads GenAlEx data that has been
#' exported from Excel as a delimited text file and creates an annotated
#' data frame of class \code{'genalex'}.  Several functions are provided for
#' accessing and printing this data.  GenAlEx and its documentation are
#' available at \url{http://biology-assets.anu.edu.au/GenAlEx}.
#'
#' Read and manipulate GenAlEx-format data files.
#' \href{http://biology-assets.anu.edu.au/GenAlExGenAlEx}{GenAlEx} is a 
#' widely-used plugin for Excel that manipulates and analyses genotype data.
#' This package is designed to read GenAlEx-format genotype files that have 
#' been exported from Excel as delimited text.  Descriptions of the file 
#' format and of the annotations added to the class as attributes
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



#' Check to see if a data frame is of class 'genalex'
#' 
#' Check to see if a data frame is of class 'genalex' as recognised by
#' the \code{readGenalex} package.
#' 
#' @param x      An data frame that might also be of class \code{'genalex'}
#'
#' @return \code{TRUE} or \code{FALSE}
#'
#' @author Douglas G. Scofield
#'
#' @examples
#' 
#' data(Qagr_genotypes)
#' is.genalex(Qagr_adult_genotypes)
#' 
#' @export is.genalex
#' 
is.genalex <- function(x) {
    if (inherits(x, 'genalex'))
        TRUE
    else FALSE
}



#' Convert pre-1.0 readGenalex data frames to class 'genalex'
#' 
#' Converts a data frame to a class \code{'genalex'} object.  The
#' following cases are handled:
#'
#' \itemize{
#'   \item If the data frame is a pre-1.0 data frame, it is converted
#'   to class \code{'genalex'}
#'
#'   \item If the data frame is already of class \code{'genalex'}, it
#'   is returned
#'
#'   \item If \code{x} is of any other format, it is an error
#' }
#' 
#' @param x  An object to convert to class \code{'genalex'}, possibly
#'           of an earlier package \code{readGenalex} format
#'
#' @return \code{x} as an S3 class \code{'genalex'} object, or
#'         a fatal error
#'
#' @author Douglas G. Scofield
#'
#' @examples
#' 
#' data(Qagr_genotypes)
#' gt <- as.genalex(Qagr_adult_genotypes)
#' 
#' @export as.genalex
#' 
as.genalex <- function(x) {
    if (is.genalex(x)) { # already class genalex
        return(x)
    } else if (! is.null(attr(x, "genetic.data.format")) &&
             attr(x, "genetic.data.format") == "genalex") {
        # convert earlier readGenalex format to class genalex
        attr(x, "genetic.data.format") <- NULL
        return(structure(x, class = c('genalex', 'data.frame')))
    }
    stop("'", deparse(substitute(x)), "' cannot be coerced to class 'genalex'")
}



#' Read GenAlEx-format genotypes file
#' 
#' Reads genotype data file in GenAlEx format into an annotated
#' data frame of class \code{'genalex'}.  Internal consistency checks that are allowed by the
#' GenAlEx format are also performed as data is read.  GenAlEx and its
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
#' delimiters that Excel might add when exporting a delimited file.  Extra
#' columns beyond the genotype columns are allowed.
#'
#' More information on GenAlEx is available at
#' \url{http://biology-assets.anu.edu.au/GenAlEx}.  In particular, there are
#' other GenAlEx specifications regarding e.g., encoding of genotypes that are
#' expected here but not described.
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
#' @return An annotated data frame of class \code{'genalex'} containing sample data, with column
#' names determined by line 3 of the input file.  Special \code{attributes}
#' of the \code{data.frame} include
#' 
#' \item{data.file.name }{The value of \code{file}}
#' \item{genetic.data.format }{\code{"genalex"}}
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
#' \item{locus.columns }{Numeric column positions of allele 1 of each locus in
#'   the \code{data.frame}}
#' \item{extra.columns }{\code{data.frame} containing any extra columns given
#'   in \code{file} to the right of the genotype columns.  Row order is the 
#'   same as the genotype \code{data.frame}.  If no extra columns were found,
#'   this attribute does not exist.}
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
#' @examples
#' 
#' data(Qagr_genotypes)
#' head(Qagr_adult_genotypes)
#' attributes(Qagr_adult_genotypes)
#' 
#' @export readGenalex
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
    dat <- .readGenalexJoinData(header, raw.data)
    attr(dat, "data.file.name") <- file
    class(dat) <- c('genalex', 'data.frame')
    dat
}



####################################
## Internal functions and variables


.readGenalexData <- function(con, sep, col.names, n.samples, n.loci,
                             ploidy, na.strings, extra.columns = character(0),
                             ...) {
    classes <- c("character", "character", rep("character", n.loci*ploidy))
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
      extra.dat <- as.data.frame(extra.dat, stringsAsFactors = FALSE)
    }
    dat <- as.data.frame(dat, stringsAsFactors = FALSE)
    list(dat = dat, extra.columns = extra.dat)
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
        # if we do load them, we load them as character into a separate
        # data.frame attached to the "extra.columns" attribute
        extra.columns <- 
            dlines[[3]][(3+header$n.loci*ploidy):length(dlines[[3]])]
        if (any(length(extra.columns) > 0))  # any of them are named
            header$extra.columns <- extra.columns
    }
    pop.sizes <- dlines[[1]][4:(4+header$n.pops-1)]
    names(pop.sizes) <- header$pop.labels
    header$pop.sizes <- pop.sizes
    header$locus.columns <- seq(from = 3, to = (3+(header$n.loci-1)*ploidy),
                                by = ploidy)
    header$locus.names <- dlines[[3]][header$locus.columns]
    f <- function(x) c(x, paste(sep = ".", x, seq(2, header$ploidy, 1)))
    header$data.column.names <- c(header$sample.title, header$pop.title,
                                  unlist(lapply(header$locus.names, f)))
    header
}



.readGenalexJoinData <- function(header, raw.data) {
    dat <- raw.data$dat
    if (! is.null(raw.data$extra.columns)) {
        # add sample name to extra columns
        extra.columns <- cbind(dat[,1], raw.data$extra.columns)
        names(extra.columns)[1] <- names(dat)[1]
    }
    names(dat) <- header$data.column.names
    # don't add as an attribute, it duplicates 'names'
    header$data.column.names <- NULL
    dat[[header$pop.title]] <- factor(dat[[header$pop.title]])
    # TODO: handle label in header with size 0 and missing from data?
    pop.labels.header <- sort(header$pop.labels)
    pop.labels.data <- sort(levels(dat[[header$pop.title]]))
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



#' Write GenAlEx-format genotypes to a text file
#'
#' Writes genotype data encoded in an annotated data frame of class \code{'genalex'}
#' to a GenAlEx-format text file.  Extra data
#' columns are included immediately to the right of genotype columns.  GenAlEx
#' and its documentation are available at
#' \url{http://biology-assets.anu.edu.au/GenAlEx}.
#'
#' This function writes genotypes and associated information within an
#' annotated data frame of class \code{'genalex'} to a text file in GenAlEx format. More
#' information is available in the description for \code{\link{readGenalex}},
#' and at the GenAlEx website at
#' \url{http://biology-assets.anu.edu.au/GenAlEx}.
#'
#' Doing \code{writeGenalex(readGenalex("file.txt"), "file-write.txt")} won't
#' necessarily produce an output file identical to the input file.  Two areas
#' for which this will likely be true are:
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
#' }
#'
#' @param x     Annotated data frame of class \code{'genalex'}
#'
#' @param file  File name or connection for writing.  If given as \code{""},
#'              \code{stdout()} is used.
#'
#' @param quote Logical value (\code{TRUE} or \code{FALSE}).  If \code{TRUE},
#'              all data are surrounded by double quotes, and all header fields
#'              except for counts are quoted if they exist.  If \code{FALSE},
#'              nothing is quoted.
#'
#' @param sep   Column separator for output (defaults to \code{"\t"}).
#'
#' @param eol   End-of-line character used for output (defaults to \code{"\n"}).
#'
#' @param na    The string to use when writing missing values in the data.
#'              Defaults to \code{"0"}.
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
#' data(Qagr_genotypes)
#' # lots of output to terminal
#' writeGenalex(Qagr_adult_genotypes, file = "")
#'
#' @export writeGenalex
#'
#
# TODO maybe: handle something like quote= ?
#
writeGenalex <- function(x, file, quote = FALSE, sep = "\t", eol = "\n",
                         na = "0") {
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
    # recast everything as character, and apply NA string
    x <- as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)
    x[is.na(x)] <- na
    if (! is.null(extra <- a$extra.columns)) {
        extra <- as.data.frame(lapply(extra, as.character),
                               stringsAsFactors = FALSE)
        extra[is.na(extra)] <- na
    }
    # quote function
    qu <- function(x) if (quote) paste(sep="", "\"", x, "\"") else x
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
        fields <- unlist(x[i, ])
        if (! is.null(extra))
            fields <- c(fields, extra[i, ])
        cat(file = file, sep = sep, qu(fields))
        cat(file = file, eol)
    }
}



#' Print selected genotypes
#' 
#' Print selected genotypes, optionally calling out a locus
#' 
#' @param  dat    An annotated data frame of class \code{'genalex'}
#' 
#' @param  rows   The specific rows of \code{dat} to print
#' 
#' @param  callout.locus One or more loci on \code{dat} to be surrounded by
#'                \code{callout.char} when printed
#' 
#' @param  sep    Separator character to be used between loci
#' 
#' @param  allele.sep Separator character to be used between alleles
#' 
#' @param  callout.char Character which surrounds loci specified by
#'         \code{callout.locus}
#' 
#' @param  label  Label to be included between the sample and population ID
#'         columns and the genotype columns in output
#'
#' @return No specific return value, used for its side effect of printing
#'         genotypes.
#'
#' @author Douglas G. Scofield
#'
#' @examples
#' 
#' data(Qagr_genotypes)
#' printGenalexGenotype(Qagr_adult_genotypes, rows = 6:8, callout.locus = "1c08")
#' 
#' @export printGenalexGenotype
#'
printGenalexGenotype <- function(dat, rows, callout.locus = NULL, sep = " ",
                                 allele.sep = "/", callout.char = "*", 
                                 label = NULL) {
    stopifnot(is.genalex(dat))
    cols <- names(dat)
    ploidy <- attr(dat, "ploidy")
    for (row in rows) {
        cat(paste(sep = sep, as.character(dat[row,1]), 
                  as.character(dat[row,2])))
        if (! is.null(label))
            cat("", label)
        full.gt <- ""
        for (col in seq(from = 3, to = length(cols), by = ploidy)) {
            gt <- paste(collapse = allele.sep, dat[row,col:(col+ploidy-1)])
            if (cols[col] %in% callout.locus) 
                gt <- paste(sep = "", callout.char, gt, callout.char)
            full.gt <- paste(sep = sep, collapse = sep, full.gt, gt)
        }
        cat(full.gt,"\n")
    }
}



#' Determine numeric column positions occupied by named loci
#' 
#' Determine the numeric column positions occupied by named loci in a
#' date frame produced by \code{readGenalex}.  This is mostly used as
#' a utility routine by other functions in the \code{readGenalex} package.
#' 
#' @param dat    An annotated data frame created by \code{readGenalex}
#' 
#' @param locus  The names of one or more loci found in \code{dat}
#' 
#' @param ploidy Ploidy of data in \code{dat}, if not supplied is extracted
#'               from the \code{ploidy} attribute of \code{dat}
#' 
#' @return A vector of column positions occupied by genotype data for loci
#'         named in \code{locus}.
#' 
#' @author Douglas G. Scofield
#' 
#' @examples
#' 
#' data(Qagr_genotypes)
#' computeGenalexColumns(Qagr_adult_genotypes, c("0c19", "0m05"))
#' 
#' @export computeGenalexColumns
#' 
computeGenalexColumns <- function(dat, locus, ploidy = NULL) {
    if (is.null(ploidy)) ploidy <- attr(dat,"ploidy")
    as.vector(sapply(attr(dat, "locus.columns")[attr(dat, "locus.names") %in% locus],
                     function(x) x:(x+ploidy-1)))
}



#' Reorder a data frame of class \code{'genalex'}
#' 
#' Reorder the genotype columns of a class \code{'genalex'} data frame by 
#' locus.
#' 
#' @param dat  An annotated data frame of class \code{'genalex'}
#' 
#' @param loci The names of loci found in \code{dat}, in the desired new 
#'             order.  All loci in \code{dat} must be named.  The order of
#'             the alleles within each locus is preserved.
#' 
#' @return A data frame of class \code{'genalex'} containing the same 
#' genotype data from \code{dat} reordered according to \code{loci}.
#' 
#' @author Douglas G. Scofield
#' 
#' @examples
#' 
#' data(Qagr_genotypes)
#' # reverse loci
#' loci <- rev(attr(Qagr_adult_genotypes, "locus.names"))
#' reord = reorderGenalexLoci(Qagr_adult_genotypes, rev(loci))
#' 
#' @export reorderGenalexLoci
#' 
reorderGenalexLoci <- function(dat, loci) {
    dat <- as.genalex(dat)
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



#' Return genotype data for specified loci from class \code{'genalex'}
#' 
#' Return genotype data for specified loci in the data frame of class
#' \code{'genalex'}, optionally restricted to samples from specific
#' populations.
#' 
#' 
#' @param dat   An annotated data frame of class \code{'genalex'}
#' 
#' @param locus The names of one or more loci found in \code{dat}
#' 
#' @param pop   If supplied, return only data for samples from the specified
#'              populations
#' 
#' @return A data frame of class \code{'genalex'} containing genotype data 
#' from \code{dat} for loci specified in \code{code}, optionally restricted
#' to samples from populations specified in \code{pop}.
#' 
#' @author Douglas G. Scofield
#' 
#' @examples
#' 
#' data(Qagr_genotypes)
#' nm <- attr(Qagr_peric_genotypes, "locus.names")
#' # get the first locus
#' loc1 <- getGenalexLocus(Qagr_peric_genotypes, nm[1])
#' # get the second locus of the second population
#' po <- attr(Qagr_peric_genotypes, "pop.labels")
#' loc2.pop2 <- getGenalexLocus(Qagr_peric_genotypes, nm[2], po[2])
#' 
#' @export getGenalexLocus
#' 
getGenalexLocus <- function(dat, locus, pop = NULL) {
    is.genalex(dat)
    cols <- computeGenalexColumns(dat,locus)
    if (! is.null(pop)) {
        pop.column <- attr(dat, "pop.title")
        dat <- subset(dat, dat[[pop.column]] %in% pop)
    }
    dat[, cols]
}



#' Replace genotype data in data frame of class \code{'genalex'}
#' 
#' Replace genotype data for specified loci in a data frame of class
#' \code{'genalex'}.
#' 
#' @param dat   A data frame of class \code{'genalex'}
#' 
#' @param locus   The names of one or more loci found in \code{dat}
#' 
#' @param newdata New genotype data for loci specified in \code{locus}.  Must
#'                have the same number of rows as \code{dat}
#' 
#' @return A data frame of class \code{'genalex'} containing genotype data
#' from \code{dat} with data for loci specified in \code{locus} replaced
#' with data from \code{newdata}.
#' 
#' @author Douglas G. Scofield
#' 
#' @export putGenalexLocus
#' 
putGenalexLocus <- function(dat, locus, newdata) {
    dat <- as.genalex(dat)
    dat[, computeGenalexColumns(dat,locus)] <- newdata
    dat
}



#' Remove specified loci from data frame of class \code{'genalex'}
#' 
#' Remove specified loci from the data frame of class \code{'genalex'}
#' and updates attributes
#' 
#' @param dat       An annotated data frame of class \code{'genalex'}
#' 
#' @param drop.loci The names of one or more loci found in \code{dat}
#' 
#' @param quiet     If set to \code{TRUE}, quietly returns \code{dat} 
#'                  if none of \code{drop.loci} are found in \code{dat}
#' 
#' @return A data frame containing the data in \code{dat} after removing
#' loci specified by \code{drop.loci}, with attributes updated as required.
#' 
#' @author Douglas G. Scofield
#' 
#' @examples
#' 
#' data(Qagr_genotypes)
#' newdat <- dropGenalexLoci(Qagr_adult_genotypes, "Oe09")
#' 
#' @export dropGenalexLoci
#' 
dropGenalexLoci <- function(dat, drop.loci, quiet = FALSE) {
    dat <- as.genalex(dat)
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



#' Reduce the ploidy of a data frame of class \code{'genalex'}
#' 
#' Reduce the ploidy of a data frame of class \code{'genalex'}.
#' Currently restricted to reducing the ploidy of diploid data to haploid by
#' selecting only the first allele of each locus.
#' 
#' This function reduced the ploidy of a data frame of class \code{'genalex'}
#' data frame by selecting the first allele of each locus.  Occasionally
#' haploid data is encoded in GenAlEx datasets by using homozygous diploid
#' loci, and this is a useful function for making these truly haploid.
#' 
#' @param dat        An annotated data frame of class \code{'genalex'}
#' 
#' @param new.ploidy The desired new ploidy.  Currently, the only usefully
#'                   accepted value is 1, with ploidy of \code{dat} being 2;
#'                   a ploidy matching the current ploidy of \code{dat} 
#'                   silently returns \code{dat}.
#' 
#' @return A data frame of class \code{'genalex'} containing genotype data
#' from \code{dat} reduced to the specified \code{new.ploidy}, with 
#' attributes updated as required.
#' 
#' @author Douglas G. Scofield
#' 
#' @examples
#' 
#' data(Qagr_genotypes)
#' attr(Qagr_adult_genotypes, "ploidy")
#' p1 <- reduceGenalexPloidy(Qagr_adult_genotypes, 1)
#' 
#' @export reduceGenalexPloidy
#' 
reduceGenalexPloidy <- function(dat, new.ploidy = 1) {
    dat <- as.genalex(dat)
    # Would be nice to be more general, e.g., pick other than the first
    # column, or a random allele
    att <- attributes(dat)
    if (new.ploidy == att$ploidy) 
        return(dat)
    else if (new.ploidy > att$ploidy) 
        stop("new ploidy ", new.ploidy, " greater than existing ploidy ",
             att$ploidy)
    else if (new.ploidy != 1 || att$ploidy != 2) 
        stop("can't currently handle new.ploidy other than 1,",
             " existing ploidy other than 2")
    new.col <- c(1:(att$locus.columns[1]-1), att$locus.columns)
    dat <- dat[, new.col]
    for (a in names(att))
        if (! a %in% c("names","locus.columns","ploidy"))
            attr(dat,a) <- att[[a]]
    attr(dat, "locus.columns") <- att$locus.columns -
                                  (0:(att$n.loci - 1) * 
                                   (att$ploidy - new.ploidy))
    attr(dat, "ploidy") <- new.ploidy
    dat
}



#' Quercus agrifolia adult and pericarp microsatellite genotypes
#'
#' This data set contains two annotated data frames of class \code{'genalex'}.
#'
#' \itemize{
#'   \item \code{Qagr_adult_genotypes} contains 10-locus diploid microsatellite
#'   genotypes of 262 adult coast live oak (\emph{Quercus agrifolia}) trees from
#'   Sedgwick Reserve, Santa Barbara County, California, USA.
#'
#'   \item \code{Qagr_peric_genotypes} contains 10-locus diploid microsatellite
#'   genotypes of 568 pericarps (outer seed coats) from coast live oak acorns
#'   collected from 17 acorn woodpecker (\emph{Melanerpes formicivorus})
#'   granaries, also at Sedgwick Reserve.
#' }
#'
#' These data have been analysed in several published papers, and are also
#' publicly available at the Dryad Digital Repository.  If using these data,
#' please cite the original paper as well as the data, as shown below.
#'
#' @format Annotated data frames of class \code{'genalex'}
#' 
#' @references Scofield DG, Smouse PE, Karubian J, Sork VL (2012) Use of alpha,
#' beta, and gamma diversity measures to characterize seed dispersal by 
#' animals. The American Naturalist 180(6): 719-732. 
#' \url{http://dx.doi.org/10.1086/668202}
#' 
#' Scofield DG, Smouse PE, Karubian J, Sork VL (2012) Data from: Use of alpha,
#' beta, and gamma diversity measures to characterize seed dispersal by animals.
#' Dryad Digital Repository. \url{http://dx.doi.org/10.5061/dryad.40kq7}
#'
#' \url{https://github.com/douglasgscofield/readGenalex}
#'
#' @source \url{http://datadryad.org/resource/doi:10.5061/dryad.40kq7}
#'
#' @keywords datasets
#' 
#' @docType data
#' 
#' @name Qagr_genotypes
#' @name Qagr_adult_genotypes
#' @name Qagr_peric_genotypes
#' 
NULL


