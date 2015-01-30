#' Write GenAlEx-format genotypes to a text file
#' 
#' Writes genotype data encoded in an annotated \code{data.frame} via the
#' \code{readGenalex} package to a GenAlEx-format text file.  Extra data
#' columns are included immediately to the right of genotype columns.  GenAlEx
#' and its documentation are available at
#' \url{http://biology-assets.anu.edu.au/GenAlEx}.
#' 
#' This function writes genotypes and associated information within an
#' annotated \code{data.frame} to a text file in GenAlEx format. More
#' information is available in the description for \code{\link{readGenalex}},
#' and at the GenAlEx website at
#' \url{http://biology-assets.anu.edu.au/GenAlEx}.
#' 
#' Doing \code{writeGenalex(readGenalex("file.txt"), "file-write.txt")} won't
#' necessarily produce an output file identical to the input file.  Two areas
#' for which this will likely be true are: \enumerate{ \item Names on columns
#' for alleles other than the first in a locus, which are ignored by
#' \code{readGenalex}, converted to a simple concatenation of locus name and
#' allele number in the resulting \code{data.frame}, and are left out of the
#' output of \code{writeGenalex}.  \item Locations of additional data columns
#' beyond the genotype columns, which \code{readGenalex} will collect wherever
#' there are named columns to the right of the genotype columns, and which
#' \code{writeGenalex} will write immediately to the right of the genotype
#' columns.  The same column names are used when writing as were present when
#' reading. }
#' 
#' @param x Annotated \code{data.frame} created via the \code{readGenalex}
#' package.
#' @param file File name or connection for writing.  If given as \code{""},
#' \code{stdout()} is used.
#' @param sep Column separator for output (defaults to \code{"\t"}).
#' @param na The string to use when writing missing values in the data
#' (defaults to \code{""}).
#' @param eol End-of-line character used for output (defaults to \code{"\n"}).
#' @return No value is returned.
#' @author Douglas G. Scofield
#' @seealso \code{\link{readGenalex}}
#' @references Peakall, R. and Smouse P.E. (2012) GenAlEx 6.5: genetic analysis
#' in Excel. Population genetic software for teaching and research-an update.
#' \emph{Bioinformatics} 28, 2537-2539.
#' 
#' Peakall, R. and Smouse P.E. (2006) GENALEX 6: genetic analysis in Excel.
#' Population genetic software for teaching and research. \emph{Molecular
#' Ecology Notes} 6, 288-295.
#' @keywords file manip attribute
#' @examples
#' 
#' data(example_genotypes)
#' writeGenalex(example_genotypes, file = "")
#' 
#' @export writeGenalex
#' 
#
# TODO maybe: handle something like quote= ?
#
writeGenalex <-
function(x, file, sep = "\t", na = "", eol = "\n")
{
    DNAME <- deparse(substitute(x))
    if (! is.genalex(x))
        stop(DNAME, " must be a readGenalex-format data.frame")
    if (file == "")
        file <- stdout()
    else if (is.character(file)) {
        file <- file(description=file, open="wt")
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
    # header line 1
    cat(file = file, sep = sep, a$n.loci, a$n.samples, a$n.pops, a$pop.sizes)
    cat(file = file, eol);
    # header line 2
    cat(file = file, sep = sep, a$dataset.title, "", "", a$pop.labels)
    cat(file = file, eol);
    # header line 3, allele columns other than the first for each locus have a
    # blank header
    cat(file = file, sep = sep, a$sample.title, a$pop.title, 
        paste(collapse = paste(collapse = "", rep(sep, a$ploidy)), 
              a$locus.names),
        rep("", a$ploidy - 1))  # all but first locus column have blank names
    # if extra columns, add headers for those
    if (! is.null(extra))
        cat(file = file, sep = sep, names(extra))
    cat(file = file, eol);
    # data plus extra columns
    for (i in 1:nrow(x)) {
        fields <- unlist(x[i, ])
        if (! is.null(extra))
            fields <- c(fields, extra[i, ])
        cat(file = file, sep = sep, fields)  
        cat(file = file, eol)
    }
}
