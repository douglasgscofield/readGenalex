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
#' as tab- or comma-delimited text from Excel
#'
#' @param sep        Column separator used when \code{file} was created
#' (defaults to tab)
#'
#' @param ploidy     The ploidy of genotypes encoded in \code{file} (defaults
#' to 2)
#'
#' @param na.strings Strings encoding missing data.  Default is to include the
#' GenAlEx missing values ("0" and "-1") as well as ".", "NA" and "" (empty).
#'
#' @param \dots      Additional arguments passed to \code{\link{scan}} when
#' reading data
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
#' gt.file <- system.file("extdata/Qagr_pericarp_genotypes.txt",
#'                        package = "readGenalex")
#' gt <- readGenalex(gt.file)
#' head(gt)
#' names(attributes(gt))
#'
#' @export
#'
readGenalex <- function(file, sep = "\t", ploidy = 2,
                        na.strings = c("0", "-1", ".", "NA", ""),
                        ...) {
    if (ploidy != 2) warning("ploidy other than 2 poorly tested")
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



#' Read GenAlEx-format genotypes from an Excel worksheet
#'
#' Reads genotype data file in GenAlEx format from an Excel worksheet
#' into an annotated data frame of class \code{'genalex'}.  Both
#' \code{.xls} and \code{.xlsx} formats may be read.  The same consistency
#' checks are performed as for \code{\link{readGenalex}}.  This function
#' uses the function \code{\link[XLConnect]{readWorksheetFromFile}} from the
#' \href{http://cran.r-project.org/web/packages/XLConnect/index.html}{XLConnect}
#' package to read the Excel file.  Strings representing \code{NA} values are
#' strictly those allowed by GenAlEx itself, 0 and -1.
#'
#' This function is provided as a convenience to the user.  If there are
#' incompatibilities when reading GenAlEx-format data with this function,
#' it is recommended to export the worksheet from Excel as tab-delimited
#' text and read it with \code{\link{readGenalex}}.
#'
#' @param file       Excel workbook file from which to read the worksheet
#'
#' @param worksheet  Worksheet specification in a format accepted by
#' \code{\link[XLConnect]{readWorksheetFromFile}}, specifically the
#' worksheet name, as a character string, or an integer value indicating
#' the worksheet position in the workbook, e.g., 1, 2, etc.
#'
#' @param ploidy     The ploidy of genotypes encoded in \code{worksheet}
#' (defaults to 2)
#'
#' @return An annotated data frame of class \code{'genalex'}.  The format is
#' described in more detail in \code{\link{readGenalex}}.  The
#' \code{"data.file.name"} attribute is set to the value
#' \code{"file(worksheet)"}.
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
#' @seealso \code{\link{readGenalex}}, \code{\link[XLConnect]{readWorksheetFromFile}}
#'
#' @examples
#'
#' xl.file <- system.file("extdata/Qagr_genotypes.xlsx",
#'                        package = "readGenalex")
#' gt <- readGenalexExcel(xl.file, worksheet = "Qagr_pericarp_genotypes")
#' head(gt)
#' names(attributes(gt))
#'
#' @export
#'
readGenalexExcel <- function(file, worksheet, ploidy = 2) {
    dat <- XLConnect::readWorksheetFromFile(file, sheet = worksheet,
                                            header = FALSE)
    # replace NA cells (empty in the Excel file) with empty strings
    dat[is.na(dat)] <- ""
    dlines <- list()
    dlines[[1]] <- unlist(dat[1, , drop=TRUE])
    dlines[[2]] <- unlist(dat[2, , drop=TRUE])
    dlines[[3]] <- unlist(dat[3, , drop=TRUE])
    dlines <- lapply(dlines, unname)
    header <- .parseGenalexHeader(dlines, ploidy)
    dat <- dat[-(1:3), ]
    rownames(dat) <- NULL
    raw.data <- .readGenalexData(con = NULL,
                                 data.strings = dat,
                                 col.names = header$data.column.names,
                                 n.samples = header$n.samples,
                                 n.loci = header$n.loci,
                                 ploidy = ploidy,
                                 extra.columns = header$extra.columns)
    x <- .readGenalexJoinData(header, raw.data)
    attr(x, "data.file.name") <- paste0(file, "(", worksheet, ")")
    class(x) <- c('genalex', 'data.frame')
    return(x)
}



####################################
## Internal functions that do the heavy lifting

# .readGenalexData: Read genotype data entries from a file, via the already
# open connection 'con', or from a data.frame filled with characters passed in
# via 'data.strings'.  For now the data.frame form is used only by
# readGenalexExcel().

.readGenalexExcel.na.strings <- c("0", "-1")

.readGenalexData <- function(con = NULL, sep, col.names, n.samples, n.loci,
                             ploidy, na.strings, extra.columns = character(0),
                             data.strings,
                             ...) {
    classes <- c("character", "character", rep("numeric", n.loci * ploidy))
    scan.col.names = col.names
    extra.columns <- extra.columns[extra.columns != ""]
    if (length(extra.columns)) {
        classes <- c(classes, rep("character", length(extra.columns)))
        scan.col.names = c(col.names, extra.columns)
    }
    what <- sapply(classes, do.call, list(0))
    names(what) = scan.col.names
    dat <- NULL
    if (! is.null(con) && missing(data.strings)) {
        # Use scan() so that we can handle data lines that contain more
        # trailing column separators than data dolumns, due to what Excel does
        # when exporting tab-delimited files
        dat <- scan(file = con, what = what, nmax = n.samples, sep = sep,
                    na.strings = na.strings, flush = TRUE, quiet = TRUE, ...)
    } else if (is.null(con) && ! missing(data.strings) &&
               class(data.strings) == "data.frame") {
        # convert column classes using classes
        if (length(scan.col.names) != ncol(data.strings))
            stop("inconsistency with data.strings, ", length(scan.col.names),
                 " names but ", ncol(data.strings), " data columns")
        for (i in seq(along = scan.col.names)) {
            d <- data.strings[, i]
            is.na(d[d %in% .readGenalexExcel.na.strings]) <- TRUE
            data.strings[, i] <- as(d, classes[i])
        }
        names(data.strings) <- scan.col.names
        # read only n.samples lines
        dat <- data.strings[1:n.samples, ]
    }
    if (is.null(dat)) stop("no clear source for genotype data")
    extra.dat = NULL
    if (length(extra.columns)) {
      extra.dat = dat[scan.col.names %in% extra.columns]
      dat = dat[! scan.col.names %in% extra.columns]
      # convert types, but keep characters as characters
      extra.dat <- lapply(extra.dat, type.convert, as.is = TRUE)
      extra.dat <- as.data.frame(extra.dat, stringsAsFactors = FALSE)
    }
    dat <- as.data.frame(dat, stringsAsFactors = FALSE)
    list(dat = dat, extra.columns = extra.dat)
}



.calculateLocusColumns <- function(n.loci, ploidy) {
    seq(from = 3, to = (3 + (n.loci - 1) * ploidy), by = ploidy)
}



.readGenalexHeader <- function(con, sep, ploidy) {
    dlines <- readLines(con = con, n = 3, ok = FALSE)
    dlines <- lapply(dlines, function(x) unlist(strsplit(x, sep, perl = TRUE)))
    .parseGenalexHeader(dlines, ploidy)
}



.createDataColumnNames <- function(header) {
    f <- function(x) {
        c(x, if (header$ploidy > 1)
                 paste(sep = ".", x, seq(2, header$ploidy, 1))
             else NULL)
    }
    c(header$sample.title, header$pop.title,
      unlist(lapply(header$locus.names, f)))
}



# split this out so we can use it to parse Excel worksheets, too
.parseGenalexHeader <- function(dlines, ploidy) {
    dlines[[1]] <- as.numeric(dlines[[1]])
    header <- list(n.loci = dlines[[1]][1],
                   ploidy = ploidy,
                   n.samples = dlines[[1]][2],
                   n.pops = dlines[[1]][3],
                   dataset.title = dlines[[2]][1],
                   sample.title = dlines[[3]][1],
                   pop.title = dlines[[3]][2])
    header$pop.labels <- dlines[[2]][4:(4 + header$n.pops - 1)]
    if (length(dlines[[3]]) >= 3 + header$n.loci * ploidy) {
        # extra columns beyond the genotype columns, do we want to load them?
        # if we do load them, we load them initially as character into a
        # separate data.frame attached to the "extra.columns" attribute
        extra.columns <-
            dlines[[3]][(3 + header$n.loci * ploidy):length(dlines[[3]])]
        if (any(length(extra.columns) > 0))  # any of them are named
            header$extra.columns <- extra.columns
    }
    pop.sizes <- dlines[[1]][4:(4 + header$n.pops - 1)]
    names(pop.sizes) <- header$pop.labels
    header$pop.sizes <- pop.sizes
    header$locus.columns <- .calculateLocusColumns(header$n.loci, ploidy)
    header$locus.names <- dlines[[3]][header$locus.columns]
    header$data.column.names <- .createDataColumnNames(header)
    header
}



.readGenalexJoinData <- function(header, raw.data) {
    dat <- raw.data$dat
    if (! is.null(raw.data$extra.columns)) {
        # add sample name to extra columns
        extra.columns <- cbind(dat[,1], raw.data$extra.columns)
        names(extra.columns)[1] <- names(dat)[1]
    }
    names(dat) <- .createDataColumnNames(header)
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
    header$data.column.names <- NULL  # duplicates names
    for (nm in names(header))
        attr(dat, nm) <- header[[nm]]
    if (! is.null(raw.data$extra.columns))
        attr(dat, "extra.columns") <- extra.columns
    dat
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

