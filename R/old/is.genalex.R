#' Check to see if \code{data.frame} was created by the \code{readGenalex}
#' package
#' 
#' Check to see if \code{data.frame} was created by the \code{readGenalex}
#' package.  It specifically checks that the \code{genetic.data.format}
#' attribute of the input \code{data.frame} is \code{"genalex"}.
#' 
#' 
#' @param checkdata An annotated \code{data.frame} that might be created by
#' \code{\link{readGenalex}}
#' @param quiet If \code{TRUE}, then the function quietly returns \code{TRUE}
#' or \code{FALSE}.  If \code{FALSE}, then \code{stop()} will be called for a
#' non-\code{readGenalex} \code{data.frame}.
#' @return \code{TRUE} or \code{FALSE} if \code{quiet=TRUE}, or \code{TRUE} or
#' \code{stop()} if \code{quiet=FALSE}
#' @author Douglas G. Scofield
#' @examples
#' 
#' data(example_genotypes)
#' is.genalex(example_genotypes)
#' 
#' @export is.genalex
is.genalex <-
function(checkdata, quiet=TRUE)
{
    if (is.null(attr(checkdata,"genetic.data.format")) ||
        attr(checkdata,"genetic.data.format") != "genalex") {
        if (quiet)
            FALSE
        else stop("data not genalex format")
    } else
        TRUE
}
