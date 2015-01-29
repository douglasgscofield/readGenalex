#' Replace genotype data in a \code{readGenalex}-format \code{data.frame}
#' 
#' Replace genotype data for specified loci in a \code{readGenalex}-format
#' \code{data.frame}.
#' 
#' 
#' @param dat An annotated \code{data.frame} created by \code{readGenalex()}
#' @param locus The names of one or more loci found in \code{dat}
#' @param newdata New genotype data for loci specified in \code{locus}.  Must
#' have the same number of rows as \code{dat}
#' @return A \code{data.frame} containing genotype data from \code{dat} with
#' data for loci specified in \code{locus} replaced with data from
#' \code{newdata}.
#' @author Douglas G. Scofield
#' @export putGenalexLocus
putGenalexLocus <-
function(dat, locus, newdata)
{
    is.genalex(dat)
    dat[, computeGenalexColumns(dat,locus)] <- newdata
    dat
}
