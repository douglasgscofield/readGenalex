#' Print selected genotypes
#' 
#' Print selected genotypes, optionally calling out a locus
#' 
#' 
#' @param  dat    An annotated \code{data.frame} created by \code{readGenalex()}
#' @param  rows   The specific rows of \code{dat} to print
#' @param  callout.locus One or more loci on \code{dat} to be surrounded by
#'                \code{callout.char} when printed
#' @param  sep    Separator character to be used between loci
#' @param  allele.sep Separator character to be used between alleles
#' @param  callout.char Character which surrounds loci specified by
#'         \code{callout.locus}
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
#' data(example_genotypes)
#' printGenalexGenotype(example_genotypes, rows=6:8, callout.locus="loc5")
#' 
#' @export printGenalexGenotype
#'
printGenalexGenotype <-
function(dat, rows, callout.locus=NULL, 
                                   sep=" ", allele.sep="/", 
                                   callout.char="*", label=NULL)
{
    cols <- names(dat)
    ploidy <- attr(dat, "ploidy")
    for (row in rows) {
        cat(paste(sep=sep, as.character(dat[row,1]), as.character(dat[row,2])))
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
