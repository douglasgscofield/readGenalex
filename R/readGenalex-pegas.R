#' @include readGenalex.R
#' @include readGenalex-readwrite.R
#' @include readGenalex-genetics.R
# for collation order
NULL

#' Convert class \code{'genalex'} object to data frame of class \code{'loci'} from package \code{'pegas'}
#'
#' Converts an object of class \code{'genalex'} to a data frame of class
#' \code{'loci'} from the
#' \href{http://cran.r-project.org/web/packages/pegas/index.html}{pegas}
#' package.  This data frame is similar to one of class \code{'genalex'},
#' in that it mixes genetic and other data in the same data frame, but
#' its conversion of multiple allele columns to single genotype columns
#' is similar to the result of the \code{\link{as.genetics}} function
#' of this package.
#'
#' Like class \code{'genalex'}, class \code{'loci'} can encode genotypes of
#' any ploidy.  Once a class \code{'genalex'} object is converted to class
#' \code{'loci'}, it may be further converted to other data structures for
#' analysis with
#' \href{http://cran.r-project.org/web/packages/pegas/index.html}{pegas}
#' and
#' \href{http://cran.r-project.org/web/packages/adegenet/index.html}{adegenet}.
#'
#' The specific changes that occur to an object of
#' class \code{'genalex'} for it to become an object of class \code{'loci'}:
#' \itemize{
#'    \item Row names are set from sample names
#'    \item The first column of sample names is removed
#'    \item The population column is renamed \code{"population"}, with
#'          its original name retained in the \code{"pop.title"} attribute.
#'          It is also encoded as a factor.
#'    \item The individual alleles of a locus are merged into a single
#'          column, with alleles separated by \code{"/"}
#'          (\code{phased = FALSE}) or \code{"|"} (\code{phased = TRUE}).
#'          These columns are encoded as factors.
#'    \item The \code{"locus.columns"} attribute is updated to reflect that
#'          all alleles at a locus are now joined into a single column
#'    \item A new attribute \code{"locicol"} required by class \code{'loci'}
#'          is added, with a value identical to the \code{"locus.columns"}
#'          attribute
#'    \item The \code{class} is changed from \code{c('genalex', 'data.frame')}
#'          to \code{c('loci', 'data.frame')}
#' }
#'
#' Because of the removal of the sample name column and the additional allele
#' columns, the number of columns will be reduced by 1 plus the number of loci.
#' For further details of the structure of class \code{\link[pegas]{loci}}, see
#' \url{http://ape-package.ird.fr/pegas/DefinitionDataClassesPegas.pdf}.
#'
#' Because class \code{'loci'} can readily encode additional columns, the
#' extra columns of a class \code{'genalex'} object can be bound with
#' \code{cbind} as additional columns.
#'
#' This is a specialised wrapper around the function
#' \code{\link[pegas]{as.loci.data.frame}} from the
#' \href{http://cran.r-project.org/web/packages/pegas/index.html}{pegas}
#' package.
#'
#' @param x       Annotated data frame of class \code{'genalex'}
#'
#' @param phased  Still some details to work out.  Default \code{FALSE}.  If
#' \code{FALSE}, assumes alleles in \code{x} are unphased so that a genotype
#' of \code{101/107} is identical to a genotype of \code{107/101}.  This
#' results in the use of \code{"/"} as the allele separator.  If \code{TRUE},
#' uses \code{"|"} as the allele separator, and assumes alleles are phased so
#' that a genotype of \code{101|107} is different from a genotype of
#' \code{107/101}.
#'
#' @param check.annotation  If \code{TRUE}, the annotations for the dataset
#' are checked using \code{is.genalex(x, force = TRUE, skip.strings = TRUE)}
#' prior to conversion.  If that returns \code{FALSE}, nothing is converted
#' and an error is generated.
#'
#' @param \dots   Additional arguments, currently ignored
#'
#' @return \code{x} as an object of class \code{'loci'}, which is a data
#' frame with the genotype of each locus encoded as factors.  Additional
#' changes apply, see Details.
#'
#' @author Douglas G. Scofield
#'
#' @seealso  \code{\link[pegas]{as.loci}}, \code{\link{joinGenotypes}}
#'
#' @examples
#'
#' suppressPackageStartupMessages(require(pegas))
#' data(Qagr_pericarp_genotypes)
#' dd <- as.genalex(head(Qagr_pericarp_genotypes, 15), force = TRUE)
#' as.loci(dd)
#' str(as.loci(dd, phased = TRUE))
#'
#' @export
#'
as.loci.genalex <- function(x, phased = FALSE, check.annotation = TRUE, ...)
{
    if (! requireNamespace("pegas", quietly = TRUE))
        stop("Please install package 'pegas' to use this function")
    sep <- if (phased) "|" else "/"
    x.name <- deparse(substitute(x.name))
    if (check.annotation && ! is.genalex(x, force = TRUE, skip.strings = TRUE))
        stop(x.name, " class 'genalex' annotations are inconsistent, not converting")
    x.attr <- attributes(x)
    x <- joinGenotypes(x, sep = sep)  # x now data.frame
    # remove sample names column
    rownames(x) <- x[, 1]
    x[, 1] <- NULL
    attr(x, "locus.columns") <- attr(x, "locus.columns") - 1
    # make the genotypes factors
    for (col in attr(x, "locus.columns"))
        x[, col] <- as.factor(x[, col])
    # convert to class 'loci'
    x <- pegas::as.loci(x, allele.sep = "/|", col.pop = 1,
                        col.loci = attr(x, "locus.columns"))
    attr(x, "data.file.name") <- paste0("as.loci(", x.attr$data.file.name, ")")
    x
}



# See R/readGenalex.R for as.genalex.loci



#' Join genotypes encoded as separate alleles into single genotypes like class \code{'genotype'} or class \code{'loci'}
#'
#' @param  x   An object of class \code{'genalex'}, a data frame, or an
#' object that can be coerced to a data frame which contains genotypes
#' with alleles in separate columns, to be joined into character
#' genotypes of the form \code{"101/107"}, where the \code{"/"} is
#' determined by \code{sep}.
#'
#' @param  sep     Separator between alleles
#'
#' @param  loci    Columns or column names indicating loci to join.  When
#' called for an object of class \code{'genalex'}, all loci are joined.
#'
#' @param  ploidy  Ploidy of the indicated loci.  When called for an
#' object of class \code{'genalex'}, ploidy is determined from attributes.
#'
#' @param \dots   Additional arguments, currently ignored
#'
#' @return A data frame with allele columns replaced by genotype columns
#'
#' @export
#'
#' @name joinGenotypes
#'
NULL

joinGenotypes <- function(x, ...) UseMethod("joinGenotypes")

#' @rdname joinGenotypes
#'
#' @export
#'
joinGenotypes.genalex <- function(x, sep = "/", ...)
{
    x.attr <- attributes(x)
    x <- as.data.frame(x)
    x <- joinGenotypes.default(x, loci = x.attr$locus.columns,
                               ploidy = x.attr$ploidy, sep = sep, ...)
    x.attr$names <- c(x.attr$names[1:2], x.attr$locus.names)
    x.attr$locus.columns <- setNames(.calculateLocusColumns(x.attr$n.loci, 1),
                                     x.attr$locus.names)
    x.attr$data.file.name <- paste0("joinGenotypes(", x.attr$data.file.name, ")")
    x.attr$class <- 'data.frame'
    attributes(x) <- x.attr
    x
}



#' @rdname joinGenotypes
#'
#' @export
#'
joinGenotypes.default <- function(x, loci, ploidy, sep = "/", ...)
{
    if (! is.data.frame(x))
        x <- as.data.frame(x)
    if (missing(loci) || missing(ploidy))
        stop("both 'loci' and 'ploidy' must be supplied")
    if (is.character(loci))
        loci <- match(loci, names(x))
    locus.names <- names(x)[loci]
    drop.cols <- c()
    for (col in loci) {
        allele.cols <- seq(col, (col + ploidy - 1), 1)
        m <- x[, allele.cols, drop = FALSE]
        m <- apply(m, 1, paste, collapse = sep)
        drop.cols <- c(drop.cols, allele.cols[-1])
        x[, allele.cols[1]] <- m
    }
    x[, -drop.cols]
}

