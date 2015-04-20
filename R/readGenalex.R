#' Check to see if an object is of class 'genalex'
#'
#' Check to see if an object is of class 'genalex' as recognised by
#' the \code{readGenalex} package.  With \code{force = TRUE},
#' the internal consistency of the data and annotations are checked.
#'
#' Two attributes that may commonly be found to differ from inferred
#' values are \code{"dataset.title"} and \code{"data.file.name"},
#' if there have been manipulations to the class.  These attributes
#' cannot be removed from a valid object of class \code{'genalex'},
#' but they can be set to the empty string (\code{""}) to avoid this
#' check.  Alternatively, the option \code{skip.strings = TRUE} may
#' be set to not check these specific attributes for consistency.
#' Extra data columns are checked first for existence, then for contents,
#' then for matching row names.  The option \code{skip.extra = TRUE}
#' may be set to not check extra data columns.
#'
#' @param x      An object that might be of class \code{'genalex'}
#'
#' @param force  If \code{TRUE} and \code{x} has class \code{'genalex'},
#' force a deeper check to assure that the data and annotations are
#' consistent with class \code{'genalex'}
#'
#' @param skip.strings  If \code{TRUE} and \code{force = TRUE},
#' do not check the attributes \code{"dataset.title"} and
#' \code{"data.file.name"} for consistency.
#'
#' @param skip.extra  If \code{TRUE} and \code{force = TRUE}, do not
#' check the extra data columns (attribute \code{"extra.columns"} for
#' consistency.
#'
#' @param verbose If \code{TRUE} and \code{force = TRUE}, indicate any
#' inconsistencies discovered between the data and annotations. if
#' there are no inconsistencies, nothing is printed.
#'
#' @return \code{TRUE} or \code{FALSE}
#'
#' @author Douglas G. Scofield
#'
#' @examples
#'
#' data(Qagr_adult_genotypes)
#' is.genalex(Qagr_adult_genotypes)
#' ## create an inconsistent attribute
#' attr(Qagr_adult_genotypes, "n.loci") <- 20
#' ## this doesn't detect the inconsistency
#' is.genalex(Qagr_adult_genotypes)
#' ## this detects the inconsistency, returns FALSE
#' is.genalex(Qagr_adult_genotypes, force = TRUE)
#' is.genalex(Qagr_adult_genotypes, force = TRUE, verbose = TRUE)
#'
#' @export is.genalex
#'
is.genalex <- function(x, force = FALSE, skip.strings = FALSE,
    skip.extra = FALSE, verbose = FALSE)
{
    if (! inherits(x, 'genalex'))
        return(FALSE)
    if (force == FALSE)
        return(TRUE)
    msg <- .compareGenalexAttributes(x, skip.strings = skip.strings,
                                     skip.extra = skip.extra)
    if (msg != "" && verbose)
        cat(msg, "\n")
    return(msg == "")
}



#############################################
#
# Internal functions for calculating and comparing attributes
#
.calculateGenalexAttributes <- function(x, ploidy = NULL)
{
    if (! inherits(x, 'genalex') && ! inherits(x, 'data.frame'))
        x <- as.data.frame(x)
    # names: sample pop loc1 ... loc2 ...
    ans <- list()
    # class: "genalex" "data.frame"

    # samples
    ans$n.samples <- nrow(x)
    ans$sample.title <- names(x)[1]

    # ploidy and loci
    if (! is.null(ploidy)) {
        ans$ploidy <- ploidy
    } else if (ncol(x) == 3) {
        ans$ploidy <- 1
    } else {
        # Here we look for the name of the locus being a prefix for the
        # other locus column names, with the prefix ending with *.*
        pat <- paste0("^", names(x)[3], "\\.")
        ln <- names(x)[4:ncol(x)]
        runs <- rle(grepl(pat, ln, perl = TRUE))
        ans$ploidy <- if (runs$values[1] == TRUE) (runs$lengths[1] + 1) else 1
    }
    if ((ncol(x) - 2) %% ans$ploidy)
        stop("ploidy ", ans$ploidy, 
             " inconsistent with apparent number of locus columns ",
             ncol(x) - 2)
    ans$n.loci <- (ncol(x) - 2) / ans$ploidy
    ans$locus.columns <- seq(3, ncol(x), by = ans$ploidy)
    ans$locus.names <- names(x)[ans$locus.columns]
    names(ans$locus.columns) <- ans$locus.names

    # populations
    p <- sapply(split(x[, 2], x[, 2]), length)
    ans$n.pops <- length(p)
    ans$pop.labels <- names(p)
    ans$pop.sizes <- p
    ans$pop.title <- names(x)[2]

    # extra columns, if they exist, get row names equal to sample names
    if (! is.null(ans$extra.columns))
        rownames(ans$extra.columns) <- x[, 1]

    # remaining attributes, set to empty string
    ans$dataset.title <- ""
    ans$data.file.name <- ""
    ans
}

.compare.attribute <- function(a, b, n) {
    if (is.null(a[[n]]) || is.null(b[[n]]) || any(a[[n]] != b[[n]]))
        return(n)
    else return(character(0))
}

.compare.char.attribute <- function(a, b, n, empty.ok = FALSE) {
    if (is.null(a[[n]]) || is.null(b[[n]]) || (a[[n]] != b[[n]] &&
         (! empty.ok || (a[[n]] != "" && b[[n]] != ""))))
        n
    else character(0)
}

.compareGenalexAttributes <- function(x, y = NULL, skip.strings = FALSE,
                                      skip.extra = skip.extra)
{
    # don't assume genalex for x or y, but make soft assumptions for both
    xa <- attributes(x)
    x.name <- deparse(substitute(x))
    y.name <- deparse(substitute(y))
    # If y is not supplied, instead de novo-infer attributes for x,
    # using only ploidy
    ya <- if (is.null(y)) {
            y.name <- paste("inferred attributes for", x.name)
            x.ploidy <- xa$ploidy
            if (! is.null(x.ploidy))
                .calculateGenalexAttributes(x, ploidy = x.ploidy)
            else .calculateGenalexAttributes(x)
        } else attributes(y)
    # compare
    msg <- character(0)
    msg <- c(msg, .compare.attribute(xa, ya, "n.samples"))
    msg <- c(msg, .compare.attribute(xa, ya, "sample.title"))
    msg <- c(msg, .compare.attribute(xa, ya, "ploidy"))
    msg <- c(msg, .compare.attribute(xa, ya, "n.loci"))
    msg <- c(msg, .compare.attribute(xa, ya, "locus.columns"))
    msg <- c(msg, .compare.attribute(xa, ya, "locus.names"))
    msg <- c(msg, .compare.attribute(xa, ya, "n.pops"))
    msg <- c(msg, .compare.attribute(xa, ya, "pop.labels"))
    msg <- c(msg, .compare.attribute(xa, ya, "pop.sizes"))
    msg <- c(msg, .compare.attribute(xa, ya, "pop.title"))
    if (! skip.strings) {
        msg <- c(msg, .compare.char.attribute(xa, ya, "dataset.title", TRUE))
        msg <- c(msg, .compare.char.attribute(xa, ya, "data.file.name", TRUE))
    }
    if (! skip.extra) {
        if ((! is.null(xa$extra.columns) && !is.null(y) &&
             is.null(ya$extra.columns)) ||
            (is.null(xa$extra.columns) && ! is.null(ya$extra.columns))) {
            msg <- c(msg, "extra.columns")
        } else if (! is.null(xa$extra.columns) && ! is.null(ya$extra.columns)) {
            if (! all(xa$extra.columns == ya$extra.columns))
                msg <- c(msg, "extra.columns contents")
            if (! all(rownames(xa$extra.columns) == rownames(ya$extra.columns)))
                msg <- c(msg, "extra.columns row names")
        }
    }
    if (length(msg))
        msg <- paste(x.name, "and", y.name, "attributes do not match :",
                     paste(collapse=" ", msg))
    else msg <- ""

    msg
}



#' Convert object to class 'genalex'
#'
#' Converts object \code{x} to a data frame of class \code{'genalex'}.
#' There are seven cases:
#' \itemize{
#'   \item If \code{x} is of class \code{'genalex'}, it is simply returned.
#'   \item If \code{x} is of class \code{'genalex'} and \code{force = TRUE},
#'         \code{x} is examined for consistency between data and annotations,
#'         and any inconsistencies are recalculated based on the data.  To
#'         check whether inconsistencies exist, use
#'         \code{is.genalex(..., force = TRUE)} and add \code{verbose = TRUE}
#'         for descriptions of the inconsistencies.  Ploidy is taken from
#'         \code{x} and is not reset.  Attributes that may be reset include
#'         \code{n.samples}, \code{sample.title}, \code{n.loci},
#'         \code{locus.columns}, \code{locus.names}, \code{n.pops},
#'         \code{pop.labels}, \code{pop.sizes} and \code{pop.title}.
#'   \item If \code{x} is of class \code{'data.frame'}, it is examined to
#'         see if it might be a data frame created by an earlier version of
#'         the \code{readGenalex} package.  If so, it is converted to
#'         class \code{'genalex'} and returned.  Any other arguments are
#'         ignored.
#'   \item If \code{x} is of class \code{'data.frame'} and all columns from
#'         the third to the end are of class \code{\link[genetics]{genotype}},
#'         these columns are converted to class \code{'genalex'} genotypes,
#'         with one column per allele.  Once the alleles have been split,
#'         a call to \code{\link{genalex}} establishes the correct attributes.
#'   \item If \code{x} is of class \code{'data.frame'} but does not appear to
#'         be from an earlier version of \code{readGenalex} and does not
#'         have all genotype columns of class \code{\link[genetics]{genotype}},
#'         it is converted to class \code{'genalex'} using a call to
#'         \code{\link{genalex}} assuming a format identical to class
#'         \code{'genalex'}, where the first column holds sample names, the
#'         second column holds population names, and the remaining columns
#'         hold genotypes.
#'   \item If \code{x} is of class \code{'loci'}, it is converted using
#'         \code{\link{splitGenotypes}} and \code{\link{genalex}}.  Sample
#'         names are taken from the row names.  If there
#'         is no column in \code{x} named \code{"population"}, then the
#'         \code{pop} argument must be supplied as a substitute.  Additional
#'         non-genotype columns will likely result in an error.  No attempt
#'         is currently made to use the \code{"locicol"} attribute to narrow
#'         the conversion to only locus columns.  Furthermore, only diploid
#'         genotypes are currently handled.
#'   \item Any other class is an error.  Further conversions between genetic
#'         data formats may be added as additional methods.
#' }
#' If the object is not originally of class \code{'genalex'}, the
#' \code{"data.file.name"} attribute will reflect this function call.
#'
#' @param x      An object of class \code{'genalex'} or class
#' \code{'data.frame'}
#'
#' @param sep  Character separating joined alleles, if converting from
#' object of class \code{'loci'}
#'
#' @param pop  Populations to assign to samples if there is no
#' \code{population} column in the data
#'
#' @param names  A list of names to apply as accepted by \code{\link{genalex}}.
#' If any names are not provided, they are taken from the names of the
#' corresponding columns of \code{x}.  This option is only used if \code{x}
#' does not have class \code{'genalex'}.
#'
#' @param ploidy Ploidy of the genotype columns in \code{x}
#' (\code{x[, 3:ncol(x)]}).  This option is only used if \code{x} does not
#' have class \code{'genalex'}.
#'
#' @param force  If \code{TRUE}, check for consistency between data and
#' annotations in \code{x} and recalculate and reset any inconsistent
#' attributes.  This option is only used if \code{x} has class
#' \code{'genalex'}, and is \code{FALSE} by default.
#'
#' @param \dots  Additional arguments, ignored unless \code{x} is of class
#' \code{'loci'} and if so passed to \code{\link{genalex}}
#'
#' @return \code{x} converted to a class \code{'genalex'} object
#'
#' @seealso \code{\link{is.genalex}}, \code{\link{genalex}}, \code{\link{splitGenotypes}}
#'
#' @author Douglas G. Scofield
#'
#' @examples
#'
#' data(Qagr_adult_genotypes)
#' gt <- as.genalex(Qagr_adult_genotypes)
#' cat(attr(gt, "data.file.name"), "\n")
#' gt.2 <- as.genalex(as.data.frame(Qagr_adult_genotypes))
#' cat(attr(gt.2, "data.file.name"), "\n")
#'
#' @export
#'
#' @name as.genalex
#'
NULL

as.genalex <- function(x, ...) UseMethod("as.genalex")



.reset.attribute <- function(a, b, n)
{
    if (! identical(a[[n]], b[[n]]) && all(b[[n]] != ""))
        a[[n]] <- b[[n]]
    a
}



#' @rdname as.genalex
#'
#' @export
#'
as.genalex.genalex <- function(x, force = FALSE, ...)
{
    if (! force)
        return(x)
    xa <- attributes(x)
    ya <- .calculateGenalexAttributes(x, ploidy = xa$ploidy)
    xa <- .reset.attribute(xa, ya, "n.samples")
    xa <- .reset.attribute(xa, ya, "sample.title")
    xa <- .reset.attribute(xa, ya, "n.loci")
    xa <- .reset.attribute(xa, ya, "locus.columns")
    xa <- .reset.attribute(xa, ya, "locus.names")
    xa <- .reset.attribute(xa, ya, "n.pops")
    xa <- .reset.attribute(xa, ya, "pop.labels")
    xa <- .reset.attribute(xa, ya, "pop.sizes")
    xa <- .reset.attribute(xa, ya, "pop.title")
    attributes(x) <- xa
    x
}



#' @rdname as.genalex
#'
#' @export
#'
as.genalex.data.frame <- function(x, names = NULL, ploidy = 2, ...)
{
    if (! is.null(attr(x, "genetic.data.format")) &&
             attr(x, "genetic.data.format") == "genalex") {
        # convert earlier readGenalex format to class genalex
        if (! missing(names) || ! missing(ploidy))
            warning("args ignored, converting pre-1.0 readGenalex data frame")
        attr(x, "genetic.data.format") <- NULL
        return(structure(x, class = c('genalex', 'data.frame')))
    }
    this.call <- sys.call()
    if (ncol(x) <= 2)
        stop("not enough columns for class 'genalex'")
    if (all(sapply(x[, 3:ncol(x), drop=FALSE], 
                   function(.x) inherits(.x, 'genotype')))) {
        # contains class 'genotype' columns, convert to genalex genotypes
        stop("class 'genotype' genotypes not yet handled")
        #z <- .convert_genotype(x)

        # Function to convert basic factor matrix from x[, 3:ncol(x)] to
        # genalex genotypes.  Export that function for the user, too.
        # Arguments: ploidy (verified), sep.

        # Bring it all back together.

    } else {
        # call genalex() to coerce data frame
        nm <- names(x)
        if (is.null(names)) names = list()
        if (is.null(names$title)) names$title <- "genalex"
        if (is.null(names$sample)) names$sample <- nm[1]
        if (is.null(names$pop)) names$pop <- nm[2]
        z <- genalex(x[, 1], x[, 2], x[, 3:ncol(x), drop=FALSE], names, ploidy)
    }
    attr(z, "data.file.name") <- capture.output(print(this.call))
    z
}



#' @rdname as.genalex
#'
#' @export
#'
as.genalex.loci <- function(x, sep = "/", pop, ...)
{
    if (! any(names(x), "population") && missing(pop))
        stop("no 'population' column or 'pop' argument")
    if (! is.null(x$population)) {
        pop.names <- x$population
        x$population <- NULL
    } else pop.names <- pop
    sample.names <- rownames(x)
    dat <- splitGenotypes(x, sep)
    genalex(samples = sample.names, pops = pop.names, genotypes = dat, ...)
}



#' @rdname as.genalex
#'
#' @export
#'
as.genalex.default <- function(x, ...)
{
    stop("'", deparse(substitute(x)), 
         "' cannot be coerced to class 'genalex', perhaps to a data.frame first?")
}



#' Convert class \code{'genalex'} to data frame
#'
#' Convert an object of class \code{'genalex'} to a data frame, optionally
#' removing all \code{'genalex'}-specific attributes.  Note that the
#' behaviour of \code{stringsAsFactors} will be used to determine whether
#' to convert \code{character} columns in the data frame to factors during
#' conversion.  Note also that any extra columns are not affected by this
#' conversion, as they are already stored in a data frame.
#'
#' @param x         An object to convert to class \code{'data.frame'}
#'
#' @param complete  If \code{TRUE}, also removes class
#' \code{'genalex'}-specific attributes
#'
#' @param stringsAsFactors  Should \code{character} vectors be converted
#' to factors?  This could affect sample and population names.
#'
#' @param \dots     Additional arguments passed to \code{as.data.frame}
#'
#' @return \code{x} as class \code{'data.frame'}.  With the default
#' \code{complete = FALSE}, no attributes are removed, and the class is
#' simply changed to \code{data.frame} and \code{as.data.frame} is
#' called.  With \code{complete = TRUE}, all \code{'genalex'}-specific
#' attributes are removed.
#'
#' @author Douglas G. Scofield
#'
#' @seealso \code{\link{as.data.frame}}, \code{\link{data.frame}}, \code{\link{as.genalex}}
#'
#' @examples
#'
#' data(Qagr_adult_genotypes)
#' ## leave genalex-specific attributes in place
#' dat <- as.data.frame(Qagr_adult_genotypes)
#' ## remove genalex-specific attributes
#' dat.clean <- as.data.frame(Qagr_adult_genotypes, complete = TRUE)
#' ## both should result in an identical data frame, though the
#' ## data.file.name attribute will be different.
#'
#' @export
#'
as.data.frame.genalex <- function(x, ..., complete = FALSE,
    stringsAsFactors = default.stringsAsFactors())
{
    if (is.genalex(x)) {
        if (complete)
            x <- .clearGenalexAttributes(x)
        x <- as.data.frame(structure(x, class = c('data.frame')), ...,
                             stringsAsFactors = stringsAsFactors)
        return(x)
    }
    stop("'", deparse(substitute(x)), "' is not class 'genalex'")
}



#' Combine class \code{'genalex'} data sets by adding rows
#'
#' Combine class \code{'genalex'} data sets onto one larger class
#' \code{'genalex'}data set.  Population names and sizes are adjusted
#' accordingly.  The data sets must have the same locus names and ploidy, but
#' the order of the loci may differ, and the final data set will have the
#' locus order of the first.  Sample names must be unique across all data
#' sets.  Data set title and sample and population column headers are taken
#' from the first data set unless supplied in the \code{names} argument.  If
#' one data set contains extra columns, all must contain extra columns, and
#' these are combined along with the rest of the data.
#'
#' @param \dots   Class \code{'genalex'} data sets.  If only one data set
#' is supplied, it is returned unmodified.
#'
#' @param names   List containing names: \code{title} for data set title,
#' \code{sample} for sample column header, and \code{pop} for population
#' column header.  If \code{names} or any of its fields are not provided,
#' the names of the first argument to \code{\dots} are used.
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
#' class \code{'data.frame'} with a structure that is probably not what
#' you intended.  Objects of class \code{'genalex'} also have
#' class \code{'data.frame'}.  The selection of which method to use, in
#' this case \code{rbind.data.frame}, occurs during method dispatch for
#' \code{rbind}, so it is not a condition that can be checked by this
#' function.  To prevent this, assure that data frames have been converted
#' to class \code{'genalex'} prior to calling this function by using
#' \code{as.genalex}, and if there are doubts as to the class of any of
#' the \code{\dots} arguments, use \code{is.genalex} to check the class
#' of the returned value.
#'
#' @author Douglas G. Scofield
#'
#' @seealso \code{\link{genalex}}, \code{\link{rbind}}, \code{\link{as.genalex}}, \code{\link{is.genalex}}, \code{\link{cbind.genalex}}
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
rbind.genalex <- function(..., names = NULL, deparse.level = 1)
{
    # dummy up a call for data.file.name
    this.call <- sys.call()
    a <- paste(collapse = ", ",
               unlist(lapply(as.list(substitute(list(...)))[-1L],
                             as.character)))
    if (! missing(names) && !is.null(names))
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
    att.1 <- attributes(dots[[1]])
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
    x
}



#' Combine loci from class \code{'genalex'} data sets
#'
#' Combine loci from class \code{'genalex'} data sets into one larger class
#' \code{'genalex'} data set by adding loci.  Locus counts are adjusted
#' accordingly.  The data sets must have the same ploidy, sample names
#' and sample membership in populations.  The order of the samples may
#' differ, and the final data set will have the sample order of the first.
#' Locus names must be unique across all data sets; if any locus names are
#' duplicated, the genotypes are checked to assure they contain the exact
#' same data and if so, the duplicate locus columns are ignored.  Data set
#' title and sample and population column headers are taken from the first
#' data set unless supplied in the \code{names} argument.  As subsequent
#' data sets are added, they are compared against the aggregate data set
#' assembled thus far, so a data set might have duplicate columns versus
#' the aggregate that are not duplicates versus the first data set.  See
#' Details for how extra columns are handled.
#'
#' Extra columns are taken from the first data set.  If the first data set
#' does not have extra columns but others do, extra columns are assembled
#' from data sets that have them.  Columns with the same name in extra
#' columns \emph{are dropped without checking contents}.  Columns with
#' different names are added with \code{\link{cbind}} to the right of the
#' existing set of extra columns.
#'
#' Data sets must have the same number of rows, this is a necessary
#' consequence of containing the same samples.  Unlike the default
#' \code{cbind} method, values will not be recycled to create matching
#' row counts.
#'
#' @param \dots   All arguments must be class \code{'genalex'} data sets.
#' If only one data set is supplied, it is returned unmodified.
#'
#' @param names   List of names: \code{title} for data set title,
#' \code{sample} for sample column header, and \code{pop} for population
#' column header.  If \code{names} or any of its fields are not provided,
#' the names of the first argument to \code{\dots} are used.
#'
#' @param deparse.level Not used (yet)
#'
#' @return Annotated data frame of class \code{'genalex'}.  If \code{names}
#' or any of its fields are not provided, the names of the first argument
#' are used.  The \code{data.file.name} attribute is a character
#' representation of the call to \code{cbind}.
#'
#' @note If one of the arguments is not of class \code{'genalex'}, then
#' this function will \emph{not} be called, instead \code{cbind.data.frame}
#' or perhaps another \code{cbind} method of base R will be called silently
#' and will return an object that is not of class \code{'genalex'}.  If
#' this occurs, none of the special processing for class \code{'genalex'}
#' objects will be applied and the result is probably not what you
#' intended.  If there is a chance you have mixed objects of different
#' classes while calling this function, assure that the return value is
#' class \code{'genalex'} by using \code{is.genalex}.
#'
#' If you in fact want to add genotype data from a data frame or matrix
#' to an object of class \code{'genalex'}, then use \code{addLocus}.
#'
#' @author Douglas G. Scofield
#'
# @seealso \code{\link{genalex}}, \code{\link{cbind}}, \code{\link{rbind.genalex}}, \code{\link{addLocus.genalex}}
#' @seealso \code{\link{genalex}}, \code{\link{cbind}}, \code{\link{rbind.genalex}}
#'
#' @examples
#'
#' gt1 <- data.frame(a = 11:13, a.2 = 14:16, b = 101:103, b.2 = 104:106)
#' x1 <- genalex(1:3, "snurf", gt1)
#' gt2 <- data.frame(c = 21:23, c.2 = 24:26, d = 201:203, d.2 = 204:206)
#' x2 <- genalex(1:3, "snurf", gt2)
#' x <- cbind(x1, x2)
#' x
#' attributes(x)
#'
#' @export
#'
cbind.genalex <- function(..., names = NULL, deparse.level = 1)
{
    # dummy up a call for data.file.name
    this.call <- sys.call()
    a <- paste(collapse = ", ",
               unlist(lapply(as.list(substitute(list(...)))[-1L],
                             as.character)))
    if (! missing(names) && !is.null(names))
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
    dot.1 <- dots[[1]]
    att.1 <- attributes(dot.1)
    loc.1 <- att.1$locus.columns  # already have names
    ploidy.1 <- att.1$ploidy
    # check number of rows
    for (i in 2:length(dots)) {
        if (! is.genalex(dots[[i]]))
            stop("the ", i, "-th argument is not of class 'genalex'")
        if (nrow(dots[[i]]) != nrow(dot.1))
            stop("number of rows in the ", i, "-th data set ",
                 nrow(dots[[i]]), " does not match the ", nrow(dot.1),
                 " rows in the first")
    }
    # check sample names
    equal.but.order <- function(a, b) all(a %in% b) && all(b %in% a)
    samp.1 <- dot.1[, 1]
    if (anyDuplicated(samp.1))
        stop("duplicate sample names found in first argument")
    for (i in 2:length(dots)) {
        att.i <- attributes(dots[[i]])
        if (ploidy.1 != att.i$ploidy)
            stop("all arguments must have same ploidy")
        if (anyDuplicated(dots[[i]][, 1]))
            stop("duplicate sample names found in argument ", i)
        if (! equal.but.order(samp.1, dots[[i]][, 1]))
            stop("all arguments must contain the same samples")
        m <- match(samp.1, dots[[i]][, 1])
        # reorder rows
        dots[[i]] <- dots[[i]][m, ]
        if (! is.null(att.i$extra.columns))
            att.i$extra.columns <- att.i$extra.columns[m, ]
        # check sample membership in populations
        if (! all(dot.1[, 2] == dots[[i]][, 2]))
            stop("population membership for samples in argument ", i,
                 " do not match those in the first argument")
        # check for locus name matches
        loc.i <- att.i$locus.columns  # already have names
        m <- match(names(loc.i), names(loc.1))
        for (n in m[! is.na(m)]) {
            nm <- names(loc.1)[n]
            g.1 <- dot.1[, getLocusColumns(dot.1, nm)]
            g.i <- dots[[i]][, getLocusColumns(dots[[i]], nm)]
            if (! isTRUE(all.equal(g.1, g.i)))
                stop("genotypes for duplicated locus ", nm, " in argument ",
                     i, " do not match those in argument 1")
        }
        if (any(is.na(m))) {  # at least one novel locus
            new.loci <- if (any(! is.na(m))) # indices of new loci
                (1:att.i$n.loci)[which(is.na(m))]
            else 1:att.i$n.loci
            new.loci.idx <- .calculateSingleLocusColumns(new.loci, att.i$ploidy)
            # add new loci and recalculate genalex object
            new.data <- as.data.frame(dots[[i]][, new.loci.idx], complete = TRUE)
            # don't do cbind, this preserved attributes
            new.col.idx <- seq(ncol(dot.1) + 1, ncol(dot.1) + ncol(new.data))
            dot.1[, new.col.idx] <- new.data
            dot.1 <- as.genalex(dot.1, force = TRUE)  # refresh attributes
            att.1 <- attributes(dot.1)
            loc.1 <- att.1$locus.columns
        }

        # Extra columns: match sample names vs. dot.1, and add any
        # columns whose names don't match.  Do not verify contents of
        # duplicated columns match, just drop from.
        ext.1 <- att.1$extra.columns
        ext.i <- att.i$extra.columns
        if (! is.null(ext.i)) {
            if (! is.null(ext.1)) {
                # calculate indices of novel columns
                new.ext.idx <- which(is.na(match(names(ext.i), names(ext.1))))
                newcols <- ext.i[, new.ext.idx, drop = FALSE]
                ext.1 <- cbind(ext.1, newcols, stringsAsFactors = FALSE)
            } else ext.1 <- ext.i
            extra(dot.1) <- ext.1
            att.1 <- attributes(dot.1)
        }
    }
    x <- dot.1

    # update data attributes attributes
    # update names()-related fields
    attr(x, "dataset.title") <- if (missing(names) || is.null(names$title))
        att.1$dataset.title else names$title
    attr(x, "sample.title") <- if (missing(names) || is.null(names$sample))
        att.1$sample.title else names$sample
    attr(x, "pop.title") <- if (missing(names) || is.null(names$pop))
        att.1$pop.title else names$pop
    attr(x, "data.file.name") <- this.call  # note different from genalex()
    x
}



# Calculate columns of all alleles of each locus, by locus
# index i.locus, for which first locus is 1, second is 2, etc.
.calculateSingleLocusColumns <- function(i.locus, ploidy)
{
    cols <- c()
    for (l in i.locus) {
        start <- 3 + (l - 1) * ploidy
        cols <- c(cols, seq(from = start, to = (start + ploidy - 1)))
    }
    cols
}



# Calculate columns of first allele of each locus, for all loci
.calculateLocusColumns <- function(n.loci, ploidy)
{
    seq(from = 3, to = (3 + (n.loci - 1) * ploidy), by = ploidy)
}



#' Create new object of class \code{'genalex'} from constituent data
#'
#' Create a new object of class \code{'genalex'} given sample and
#' population names and genotype data.  Titles for the dataset, sample
#' and population columns, and loci may be provided via the \code{names}
#' argument.
#'
#' @param samples    Sample names, must be unique and length must
#' match the number of rows in \code{genotypes}
#'
#' @param pops       Population names.  If \code{pops} is shorter than
#' the number of samples, it will be expanded following the rules
#' described in \code{\link{data.frame}}.
#'
#' @param genotypes  Genotype values, must be numeric
#'
#' @param names      List of names: \code{title} for data set title,
#' \code{sample} for sample column header, \code{pop} for population
#' column header, and \code{loci} for names of loci.  If \code{loci}
#' is missing, the corresponding \code{genotype} column names are used.
#' If \code{loci} is present but not the same length as the number of
#' loci, an error is produces
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
    extra.columns = NULL)
{
    this.call <- sys.call()
    samples <- as.character(samples)
    pops <- as.character(pops)
    genotypes <- as.data.frame(genotypes)
    if (length(samples) != nrow(genotypes))
        stop("'samples' and 'genotypes' must have the same length")
    if (anyDuplicated(samples))
        stop("sample names must be unique")
    n.loci <- ncol(genotypes) / ploidy
    if (as.integer(n.loci) != n.loci)
        stop("'genotypes' must have a number of columns divisible by ploidy")
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
    if (missing(names) || is.null(names$loci))
        header$locus.names <- names(dat)[header$locus.columns]
    else if (length(names$loci) == n.loci)
        header$locus.names <- names$loci
    else stop("names$loci must be the same length as the number of loci")
    names(header$locus.columns) <- header$locus.names
    x <- .readGenalexJoinData(header, list(dat = dat,
                                           extra.columns = extra.columns))
    attr(x, "data.file.name") <- capture.output(print(this.call))
    structure(x, class = c('genalex', 'data.frame'))
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
summary.genalex <- function(object, ...)
{
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
    sep = " ", allele.sep = "/", callout.char = "*", label = NULL, ...)
{
    cols <- names(x)
    ploidy <- attr(x, "ploidy")
    for (row in rows) {
        cat(paste(sep = sep, as.character(x[row, 1]),
                  as.character(x[row, 2])))
        if (! is.null(label))
            cat(sep = "", sep, label)
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
#' named in \code{locus}.
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
getLocusColumns.genalex <- function(x, locus, ...)
{
    loccol <- attr(x, "locus.columns")
    ploidy <- attr(x, "ploidy")
    if (! all(locus %in% names(loccol)))
        stop(deparse(substitute(locus)), " contains loci not in ",
             deparse(substitute(x)))
    as.vector(sapply(loccol[locus],
                     function(y) y:(y + ploidy - 1)))
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
reorderLoci.genalex <- function(x, loci, ...)
{
    existing.loci <- attr(x, "locus.names")
    if (! (all(existing.loci %in% loci) && all(loci %in% existing.loci)))
        stop("reorder list must contain all existing loci")
    if (length(loci) != length(existing.loci))
        stop("loci must appear only once")
    newdata <- x[,1:2]
    for (locus in loci) {
        # must use cbind.data.frame here
        newdata <- cbind.data.frame(newdata, getLocus(x, locus))
    }
    names.newdata <- names(newdata)
    attributes(newdata) <- attributes(x)
    names(newdata) <- names.newdata
    attr(newdata, "locus.names") <- loci
    names(attr(newdata, "locus.columns")) <- loci
    newdata
}



.clearGenalexAttributes <- function(x)
{
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
    x
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
#' populations
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
getLocus.genalex <- function(x, locus, pop = NULL, ...)
{
    cols <- getLocusColumns(x, locus)
    if (! is.null(pop)) {
        pop.column <- attr(x, "pop.title")
        x <- subset(x, x[[pop.column]] %in% pop)
    }
    x <- as.data.frame(x[, cols, drop=FALSE])
    .clearGenalexAttributes(x)
}



#' Add genotype data to an object of class \code{'genalex'}
#'
#' Add genotype genotype data for new loci to an object of class
#' \code{'genalex'}.  \code{newdata} is coerced to a data frame if
#' it is not already and then added as columns on the right.  The
#' new genotype data must have the same ploidy and the same number
#' of samples as that in \code{x}.  It is an error to duplicate
#' locus names in \code{x}.  If you want safe merging of data sets
#' with potential overlap in genotypes, use \code{\link{cbind.genalex}}.
#'
#' @param x       An annotated data frame of class \code{'genalex'}
#'
#' @param newdata The names of one or more loci found in \code{x},
#' it is coerced to a data frame if it is not one
#'
#' @param \dots  Additional arguments, currently ignored
#'
#' @return The object of class \code{'genalex'} in \code{x} with
#' genotype data from \code{newdata} as columns added on the right
#'
#' @author Douglas G. Scofield
#'
#' @examples
#'
#' data(Qagr_pericarp_genotypes)
#' dat <- head(Qagr_pericarp_genotypes, 6)
#' ## Dummy up a new locus
#' newdat <- data.frame(xx = sample(30, 6, TRUE), xx.2 = sample(30, 6, TRUE))
#' dat <- addLocus(dat, newdat)
#' dat
#'
#' @export
#'
#' @name addLocus
#'
NULL

# TODO: need to set up unit tests

addLocus <- function(x, ...) UseMethod("addLocus")

#' @rdname addLocus
#'
#' @export
#'
addLocus.genalex <- function(x, newdata, ...)
{
    x.name <- deparse(substitute(x))
    newdata.name <- deparse(substitute(newdata))
    if (! is.data.frame(newdata))
        newdata <- as.data.frame(newdata)
    if (nrow(newdata) != nrow(x))
        stop(x.name, " and ", newdata.name,
             " must have the same number of samples (rows)")
    if (! all(sapply(newdata, is.numeric)))
        stop(newdata.name, " genotype data must be numeric")
    ploidy <- attr(x, "ploidy")
    if (ncol(newdata) %% ploidy)
        stop(x.name, " and ", newdata.name, " appear not to match ploidy")
    if (any(names(newdata) %in% names(x)[c(1, 2)]))
        stop(newdata.name, " column names conflict with column names in ",
             x.name)
    nd.n.loci <- ncol(newdata) / ploidy
    nd.loc.col <- seq(1, by = ploidy, length.out = nd.n.loci)
    loc.names <- names(newdata)[nd.loc.col]
    loc.ov <- loc.names[loc.names %in% attr(x, "locus.names")]
    if (length(loc.ov))
        stop(x.name, " already contains loci: ", paste(collapse = " ", loc.ov))
    r.col <- seq(ncol(x) + 1, ncol(x) + ncol(newdata))
    x[, r.col] <- newdata  # adds on right without needing any cbind stuff
    n.loci <- attr(x, "n.loci") + nd.n.loci
    locus.names <- c(attr(x, "locus.names"), loc.names)
    locus.columns <- .calculateLocusColumns(n.loci, ploidy)
    names(locus.columns) <- locus.names
    attr(x, "n.loci") <- n.loci
    attr(x, "locus.names") <- locus.names
    attr(x, "locus.columns") <- locus.columns
    x
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
replaceLocus.genalex <- function(x, locus, newdata, ...)
{
    stopifnot(nrow(x) == nrow(newdata))
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
dropLocus.genalex <- function(x, drop.locus, quiet = FALSE, ...)
{
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
    locus.columns <- setNames(which(names(x) %in% locus.names), locus.names)
    attr(x, "locus.columns") <- locus.columns
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
reducePloidy.genalex <- function(x, new.ploidy = 1, ...)
{
    # Would be nice to be more general, e.g., pick other than the first
    # column, or a random allele
    att <- attributes(x)
    if (new.ploidy == att$ploidy)
        return(x)
    else if (new.ploidy > att$ploidy)
        stop("new ploidy ", new.ploidy, " greater than existing ploidy ",
             att$ploidy)
    else if (new.ploidy != 1 || att$ploidy != 2)
        stop("can't currently handle new.ploidy other than 1, existing ploidy other than 2")
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



#' Return or set the extra data columns of an object of class \code{'genalex'}
#'
#' Return or set the extra data columns of an object of class
#' \code{'genalex'}.  When getting extra data columns, if there are
#' none, \code{NULL} is returned.  When setting, this is more than a
#' shortcut for \code{attr(x, "extra.columns") <- value}.  It checks
#' that both \code{value} and \code{x} have the same number of rows,
#' generating an error if not, and it sets the row names of
#' \code{value} to be equal to the sample names in \code{x} before
#' assigning \code{value} to the \code{"extra.columns"} attribute.
#'
#' @usage
#' extra(x, ...)
#' extra(x) <- value
#'
#' @param x      An annotated data frame of class \code{'genalex'}
#' or of class \code{'loci'} converted from class \code{'genalex'}
#' with \code{\link{as.loci}}
#'
#' @param value  When setting extra columns, a data frame or object
#' that can be coerced to a data frame.  This is done using
#' \code{stringsAsFactors = FALSE}.  \code{value} must have the
#' same number of rows as \code{x}.
#'
#' @param \dots  Additional arguments, currently ignored
#'
#' @return A data frame containing extra columns from \code{x}, or
#' \code{NULL} if extra columns do not exist.  If setting, an object
#' of class \code{'genalex'} with its \code{"extra.columns"} attribute
#' set to \code{value}, after its rownames are adjusted as described
#' above.
#'
#' @author Douglas G. Scofield
#'
#' @examples
#'
#' data(Qagr_adult_genotypes)
#' x1 <- attr(Qagr_adult_genotypes, "extra.columns")
#' x2 <- extra(Qagr_adult_genotypes)
#' ## there are no extra columns, so both should be NULL
#' if (! isTRUE(all.equal(x1, x2)) || ! is.null(x1) || ! is.null(x2))
#'     cat("something went wrong")
#'
#' @export
#'
#' @name extra
#' @aliases extra<- extra<-.genalex
#'
NULL

extra <- function(x, ...) UseMethod("extra")

#' @export
#'
`extra<-` <- function(x, value) UseMethod("extra<-")


#' @export
#'
extra.genalex <- function(x, ...)
{
    attr(x, "extra.columns")
}



#' @export
#'
extra.data.frame <- function(x, ...)
{
    extra.genalex(x, ...)
}



#' @export
#'
`extra<-.genalex` <- function(x, value)
{
    if (! is.data.frame(value))
        value <- as.data.frame(value, stringsAsFactors = FALSE)
    rownames(value) <- x[, 1]
    attr(x, "extra.columns") <- value
    x
}



#' @export
#'
`extra<-.data.frame` <- function(x, value)
{
    `extra<-.genalex`(x, value)
}



#' Return genotypes for specified populations
#'
#' Return genotype data for specified populations from an object of class
#' \code{'genalex'}, in an object of class \code{'genalex'}.
#'
#'
#' @param x     An annotated data frame of class \code{'genalex'}
#'
#' @param pop   The names of one or more populations for which \code{x}
#' contains genotypes
#'
#' @param \dots  Additional arguments, currently ignored
#'
#' @return An object of class \code{'genalex'} containing genotype data
#' from \code{x} for populations specified in \code{code}.  Attributes
#' are re-calculated so that the return value is a valid object of class
#' \code{'genalex'}, and the \code{"data.file.name"} attribute is updated
#' to reflect the action of this function.
#'
#' @author Douglas G. Scofield
#'
#' @examples
#'
#' data(Qagr_pericarp_genotypes)
#' nm <- getPopulation(Qagr_pericarp_genotypes, "L0031")
#' nm
#'
#' @export
#'
#' @name getPopulation
#'
NULL

getPopulation <- function(x, ...) UseMethod("getPopulation")

#' @rdname getPopulation
#'
#' @export
#'
getPopulation.genalex <- function(x, pop, ...)
{
    NAME.pop <- deparse(substitute(pop))
    pop.column <- attr(x, "pop.title")
    subx <- as.genalex(x[x[[pop.column]] %in% pop, , drop = FALSE], force = TRUE)
    attr(subx, "data.file.name") <- paste0(attr(subx, "data.file.name"), "[",
                                           pop.column, "==", NAME.pop, ",]")
    subx
}



#' Return ploidy for object of class \code{'genalex'}
#'
#' Return ploidy for object of class \code{'genalex'}
#'
#'
#' @param x     An annotated data frame of class \code{'genalex'}
#'
#' @param \dots  Additional arguments, currently ignored
#'
#' @return The numeric value of the \code{"ploidy"} attribute of \code{x}
#'
#' @author Douglas G. Scofield
#'
#' @examples
#'
#' data(Qagr_pericarp_genotypes)
#' ploidy(Qagr_pericarp_genotypes)
#'
#' @export
#'
#' @name ploidy
#'
NULL

ploidy <- function(x, ...) UseMethod("ploidy")

#' @rdname ploidy
#'
#' @export
#'
ploidy.genalex <- function(x, ...)
{
    attr(x, "ploidy")
}


