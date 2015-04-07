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
#' class \code{'genalex'} itself; see Details.  Only conversion of haploid
#' and diploid data are supported, as this is what is supported by the
#' \href{http://cran.r-project.org/web/packages/genetics/index.html}{genetics}
#' package (RIGHT??)
#'
#' A class \code{\link[genetics]{genotype}} object is a special type
#' of factor, and in contrast to class \code{'genalex'} a diploid genotype
#' is encoded as a single level of a factor and thus is in a single column
#' of a data frame. For diploid data, the number of columns in the resulting
#' data frame will be reduced by the number of loci.  The sample name and
#' population columns are retained, as are the names of the loci.
#'
#' Handling of missing data
#'
#' Handling of haploid data
#' 
#' Handling of extra columns
#'
#' Why does 'genotype' reverse the alleles??
#'
#' sep = "|" is not working!!
#' 
#'
#' This is a specialised wrapper around the functions 
#' \code{\link[genetics]{makeGenotypes}} (\code{phased = FALSE}) and
#' \code{\link[genetics]{makeHaplotypes}} (\code{phased = FALSE}) from the
#' \href{http://cran.r-project.org/web/packages/genetics/index.html}{genetics}
#' package.
#'
#' @param x       Annotated data frame of class \code{'genalex'}
#'
#' @param phased  Default \code{FALSE}.  If \code{TRUE}, assumes alleles
#' in \code{x} are phased and encodes them using class 
#' \code{\link[genetics]{haplotype}} using
#' \code{\link[genetics]{makeHaplotypes}} rather than as class
#' \code{\link[genetics]{genotype}} using
#' \code{\link[genetics]{makeGenotypes}}.  For class
#' \code{\link[genetics]{haplotype}}, a genotype of \code{101/107} is 
#' different from \code{107/101}, while these are the same for class
#' \code{\link[genetics]{genotype}}.
#'
#' @param sep     Character to separate alleles in a locus, passed to
#' \code{\link[genetics]{makeGenotypes}} or 
#' \code{\link[genetics]{makeHaplotypes}}
#' 
#' @param check.annotation  If \code{TRUE}, the annotations for the dataset
#' are checked using \code{"is.genalex(x, force = TRUE, skip.strings = TRUE)"}
#' prior to conversion.  If that returns \code{FALSE}, nothing is converted
#' and an error is generated.
#'
#' @param \dots   Additional arguments, currently ignored
#'
#' @return \code{x} as a data frame with the genotype of each locus encoded
#' using  class \code{\link[genetics]{genotype}}.  The class
#' \code{'genalex'} is removed while many of the attributes are retained:
#'
#' \item{data.file.name }{Its original value, wrapped with
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
#'   per locus, if required}
#' \item{extra.columns }{Retained}
#'
#' @author Douglas G. Scofield
#'
#' @seealso  class \code{\link[genetics]{genotype}}, \code{\link[genetics]{makeGenotypes}}, \code{\link[genetics]{makeHaplotypes}}
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
    if (! requireNamespace("genetics", quietly = TRUE)) {
        stop("Please install package 'genetics' to use this function",
             call. = FALSE)
    }
    #if (! is.genalex(x))  # unnecessary, we are an S3 method now
    #    stop(x.name, " must be class 'genalex'")
    x.name <- deparse(substitute(x.name))
    ploidy <- attr(x, "ploidy")
    n.loci <- attr(x, "n.loci")
    if (ploidy > 2)
        stop("class 'genotype' can only encode haploid or diploid data")
    if (check.annotation && ! is.genalex(x, force = TRUE, skip.strings = TRUE))
        stop(x.name, " class 'genalex' annotations are inconsistent, not writing")
    # calculate columns for 'convert' argument
    lst.convert <- lapply(1:n.loci, function(.x) 
                          .calculateSingleLocusColumns(.x, ploidy))
    # store attibutes
    x.attr <- attributes(x)
    x <- if (phased)
        genetics::makeHaplotypes(x, convert = lst.convert, sep = sep)
    else genetics::makeGenotypes(x, convert = lst.convert, sep = sep)
    # restore locus column names
    x.attr$names <- c(x.attr$names[1:2], x.attr$locus.names)
    # change locus.columns
    x.attr$locus.columns <- setNames(.calculateLocusColumns(n.loci, 1),
                                     x.attr$locus.names)
    x.attr$data.file.name <- paste0("as.genetics(", x.attr$data.file.name, ")")
    x.attr$class <- 'data.frame'
    attributes(x) <- x.attr  # this resets column names as well
    x
}




# also need a genotype -> genalex converter


