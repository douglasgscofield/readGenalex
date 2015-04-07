#' @include readGenalex.R
#' @include readGenalex-readwrite.R
# for collation order
NULL

#' Convert class \code{'genalex'} object to data frame encoding loci using classes from package \code{'genetics'}
#'
#' Converts an object of class \code{'genalex'} to a data frame in which the
#' genotype columns for each locus have been converted to genotypes
#' encoded with class \code{\link[genetics]{genotype}} as provided by the
#' \href{http://cran.r-project.org/web/packages/genetics/index.html}{genetics}
#' package.  All class {'genalex'} attributes are retained except for
#' class \code{'genalex'} itself; see Details.  Only conversion of diploid
#' data are supported, a inherent limitation of the
#' \href{http://cran.r-project.org/web/packages/genetics/index.html}{genetics}
#' package.
#'
#' A class \code{\link[genetics]{genotype}} object is a special type
#' of factor, and in contrast to class \code{'genalex'} a diploid genotype
#' is encoded as a single level of a factor and thus is in a single column
#' of a data frame. For diploid data, the number of columns in the resulting
#' data frame will be reduced by the number of loci.  The sample name and
#' population columns are retained, as are the names of the loci.
#'
#' If either allele of a genotype is missing, the entire genotype is treated
#' as missing in class \code{\link[genetics]{genotype}}.
#'
#' This is a specialised wrapper around the functions
#' \code{\link[genetics]{makeGenotypes}} (\code{phased = FALSE}) and
#' \code{\link[genetics]{makeHaplotypes}} (\code{phased = TRUE}) from the
#' \href{http://cran.r-project.org/web/packages/genetics/index.html}{genetics}
#' package.
#'
#' @param x       Annotated data frame of class \code{'genalex'}
#'
#' @param phased  Default \code{FALSE}.  If \code{FALSE}, assumes alleles
#' in \code{x} are unphased so that a genotype of \code{101/107} is
#' identical to a genotype of \code{107/101}.  This results in the
#' application of class \code{\link[genetics]{genotype}} to the
#' genotype data, using \code{\link[genetics]{makeGenotypes}}.  Default
#' ordering for alleles is applied, which for class
#' \code{\link[genetics]{genotype}} is to be sorted by frequency; for
#' more information, see \code{\link[genetics]{genotype}}
#' If \code{TRUE}, assumes alleles are phased so that a genotype of
#' \code{101/107} is different from a genotype of \code{107/101}.
#' This results in the application of class \code{\link[genetics]{haplotype}}
#' using \code{\link[genetics]{makeHaplotypes}}.  In this case, alleles are
#' ordered as they are present in the genotype.
#'
#' @param sep     \emph{Ignored, package }\code{genetics}\emph{ applies it
#' inconsistently so we cannot reliably apply its effects.}  If it were ever
#' to work, it would be the haracter to separate alleles in a locus, passed
#' to \code{\link[genetics]{makeGenotypes}} or
#' \code{\link[genetics]{makeHaplotypes}}
#'
#' @param check.annotation  If \code{TRUE}, the annotations for the dataset
#' are checked using \code{is.genalex(x, force = TRUE, skip.strings = TRUE)}
#' prior to conversion.  If that returns \code{FALSE}, nothing is converted
#' and an error is generated.
#'
#' @param \dots   Additional arguments, currently ignored
#'
#' @return \code{x} as a data frame with the genotype of each locus encoded
#' using  class \code{\link[genetics]{genotype}}.  The class
#' \code{'genalex'} is removed while many of the attributes are retained:
#'
#' \item{data.file.name }{The original value, wrapped with
#'   \code{"as.genetics(...)"}}
#' \item{ploidy }{Retained}
#' \item{n.loci }{Retained}
#' \item{n.samples }{Retained}
#' \item{n.pops }{Retained}
#' \item{pop.labels }{Retained}
#' \item{pop.sizes }{Retained}
#' \item{dataset.title }{Retained}
#' \item{sample.title }{Retained}
#' \item{pop.title }{Retained}
#' \item{locus.names }{Retained}
#' \item{locus.columns }{Modified to indicate the change to a single column
#'   per locus}
#' \item{extra.columns }{Retained}
#'
#' @author Douglas G. Scofield
#'
#' @seealso  \code{\link[genetics]{genotype}}, \code{\link[genetics]{haplotype}}, \code{\link[genetics]{makeGenotypes}}, \code{\link[genetics]{makeHaplotypes}}
#'
#' @examples
#'
#' data(Qagr_pericarp_genotypes)
#' dd <- as.genalex(head(Qagr_pericarp_genotypes, 40), force = TRUE)
#' as.genetics(dd)
#'
#' @export
#'
#' @name as.genetics
#'
NULL

as.genetics <- function(x, ...) UseMethod("as.genetics")


#' @rdname as.genetics
#'
#' @export
#'
as.genetics.genalex <- function(x, phased = FALSE, sep = "/",
    check.annotation = TRUE, ...)
{
    if (! requireNamespace("genetics", quietly = TRUE))
        stop("Please install package 'genetics' to use this function")
    #if (! is.genalex(x))  # unnecessary, we are an S3 method now
    #    stop(x.name, " must be class 'genalex'")
    if (! missing(sep) && sep != "/") {
        warning("'sep' is applied inconsistently by package 'genetics' so is ignored here")
        sep <- "/"
    }
    x.name <- deparse(substitute(x.name))
    ploidy <- attr(x, "ploidy")
    n.loci <- attr(x, "n.loci")
    if (ploidy != 2)
        stop("class 'genotype' can only encode diploid data")
    if (check.annotation && ! is.genalex(x, force = TRUE, skip.strings = TRUE))
        stop(x.name, " class 'genalex' annotations are inconsistent, not converting")
    # calculate columns for 'convert' argument
    lst.convert <- lapply(1:n.loci, function(.x)
                          .calculateSingleLocusColumns(.x, ploidy))
    # store attibutes
    x.attr <- attributes(x)
    x <- if (phased) genetics::makeHaplotypes(x, convert = lst.convert)
         else genetics::makeGenotypes(x, convert = lst.convert)
    # modify names and restore original locus column names
    x.attr$names <- c(x.attr$names[1:2], x.attr$locus.names)
    x.attr$locus.columns <- setNames(.calculateLocusColumns(n.loci, 1),
                                     x.attr$locus.names)
    x.attr$data.file.name <- paste0("as.genetics(", x.attr$data.file.name, ")")
    x.attr$class <- 'data.frame'
    attributes(x) <- x.attr  # includes 'names', so resets column names as well
    x
}



#' Split genotypes encoded like class 'genotype' into separate alleles
#'
#' @param  x   A data frame or object that can be coerced to a data frame
#' which contains genotypes of the form \code{101/107}, where the
#' \code{"/"} is determined by \code{sep}
#'
#' @param  sep Separator between alleles
#'
#' @export
#'
splitGenotypes <- function(x, sep = "/")
{
    if (! is.data.frame(x))
        x <- as.data.frame(x)
    do.split <- function(.x, collapse.if.gt.2) {
        ans <- strsplit(as.character(.x), sep, fixed = TRUE)
        if (! collapse.if.gt.2 && any(sapply(ans, length)) > 2) {
            w <- .x[which(sapply(ans, length) > 2)]
            stop("more than two alleles identified: ",
                 paste(collapse = " ", w))
        }
        ans <- sapply(ans, function(.y) if (length(.y) <= 2) .y
                      else paste(collapse = sep, .y))
    }
    rnm <- rownames(x)
    cnm <- lapply(names(x), do.split, TRUE)
    # any that are not length 2, create "a", "a.2"
    cnm <- unlist(lapply(cnm, function(.y) if (length(.y) == 2) .y
                         else .createAlleleColumnNames(.y, 2)))
    # now split genotypes
    x <- lapply(x, do.split, FALSE)
    x.rows <- sapply(x, function(.x) if (is.null(y<-nrow(.x))) 1 else y)
    if (any(x.rows < 2))
        stop("loci not diploid: ", paste(collapse = " ", 
                                         names(x.rows)[which(x.rows < 2)]))
    # this will automatically assign NA to any genotypes with missing alleles
    getcol <- function(.l, .i) lapply(.l, function(.x) .x[.i, ])
    c.1 <- getcol(x, 1)
    c.2 <- getcol(x, 2)
    if (length(c.1) < 1 || length(c.1) != length(c.2))
        stop("some kind of inconsistency with columns in split alleles")
    ans <- c()
    for (i in seq(along = c.1))
        ans <- cbind(ans, c.1[[i]], c.2[[i]])
    storage.mode(ans) <- "integer"
    rownames(ans) <- rnm
    colnames(ans) <- cnm
    as.data.frame(ans)
}
#
