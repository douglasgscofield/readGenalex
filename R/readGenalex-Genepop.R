#' @include readGenalex.R
#' @include readGenalex-readwrite.R
# for collation order
NULL



# Calculate the number of digits required per allele for Genepop output.
# At least 2 are required
.calculateGenepopAlleleDigits <- function(x)
{
    gt <- as.data.frame(x)[, 3:ncol(x)]
    gt[is.na(gt)] <- 0
    max(max(nchar(sapply(gt, as.character))), 2)
}



#' Write GenAlEx-format genotypes to a text file in Genepop format
#'
#' Writes genotype data encoded in an annotated data frame of class
#' \code{'genalex'} to a Genepop-format text file.  
#' included immediately to the right of genotype columns.  Genepop
#' and its documentation are available at
#' \url{http://kimura.univ-montp2.fr/~rousset/} and
#' \url{http://kimura.univ-montp2.fr/~rousset/Genepop.pdf}.
#'
#' Although the GenAlEx and Genepop formats encode nearly the same data,
#' there are flexibilities and limitations on both sides.  For example,
#' Genepop allows for a maximum of 3 digits to encode each allele, and
#' only haploid or diploid data, while allowing for some mixing of 
#' diploid and haploid encodings.  GenAlEx allows for more digits per 
#' allele and higher ploidy, although numeric data is still required,
#' and only allows for a single ploidy per data file.  Perhaps future
#' extensions to this function will allow for additional haploid data
#' to be written.
#'
#' An attempt is made to produce output with useful whitespace.  The
#' locus names and each set of individual genotypes are aligned at a
#' column determined by the longest sample name.
#'
#' GenAlEx format components that are attributes of class \code{'genalex'}
#' are translated to components in the Genepop file as so:
#'
#' \enumerate{
#'    \item The dataset title from the \code{"dataset.title"} attribute is
#'          written as the first line
#'    \item The locus names from the \code{"locus.names"} attribute are
#'          written comma-separated as the second line
#'    \item For each population listed in the \code{"pop.labels"} attribute,
#'          a single line containing \code{Pop popname} is written
#'    \item For each sample from a population, the sample identifier is
#'          written, then a space, comma and space, then the individual
#'          genotypes encoded as two or three digits per allele.  Alleles
#'          for each locus are concatenated, and successive loci are
#'          separated by a single space.
#'    \item Populations are written in the order in which they appear in
#'          the \code{"pop.labels"} attribute
#'    \item Missing alleles are coded as zeroes \code{"00"} or \code{"000"}
#'    \item The same number of digits are used for encoding each allele of
#'          all loci
#'    \item For Genepop ploidy is indicated by the number of digits used for
#'          encoding each locus.  Two or three digits for the entire locus
#'          indicates haploidy, while four or six digits indicates diploidy
#'          with each allele using two or three digits, respectively.
#'    \item For Genepop there is no provision for encoding additional
#'          non-genetic data
#' }
#' 
#'
#' @param x     Annotated data frame of class \code{'genalex'}
#'
#' @param file  File name or connection for writing.  If given as \code{""},
#' \code{stdout()} is used.
#'
#' @param eol   End-of-line character used for output (defaults to \code{"\n"}).
#'
#' @param check.annotation  If \code{TRUE}, the annotations for the dataset
#' are checked using \code{"is.genalex(x, force = TRUE, skip.strings = TRUE)"}.
#' If that returns \code{FALSE}, nothing is written and an error is generated.
#'
#' @return No value is returned.
#'
#' @author Douglas G. Scofield
#'
#' @seealso \code{\link{writeGenalex}}
#'
#' @references Rousset, F. (2008)  GENEPOP '007: a complete reimplementation
#' of the GENEPOP software for Windows and Linux.
#' \emph{Molecular Ecology Resources} 8, 103-106.
#' \url{http://onlinelibrary.wiley.com/doi/10.1111/j.1471-8286.2007.01931.x/abstract}.
#'
#' @keywords file manip attribute
#'
#' @examples
#'
#' data(Qagr_pericarp_genotypes)
#' # lots of output to terminal
#' dd <- as.genalex(head(Qagr_pericarp_genotypes, 40), force = TRUE)
#' writeGenepop(dd, file = "")
#'
#' @export
#'
writeGenepop <- function(x, file, eol = "\n", check.annotation = TRUE)
{
    DNAME <- deparse(substitute(x))
    if (! is.genalex(x))
        stop(DNAME, " must be class 'genalex'")
    ploidy <- attr(x, "ploidy")
    n.loci <- attr(x, "n.loci")
    if (ploidy > 2)
        stop("Genepop format can only encode haploid or diploid data")
    if (check.annotation && ! is.genalex(x, force = TRUE, skip.strings = TRUE))
        stop(DNAME, " class 'genalex' annotations are inconsistent, not writing")
    allele.digits <- .calculateGenepopAlleleDigits(x)
    if (allele.digits > 3)
        stop("One or more ", DNAME, " alleles require more than three",
             " characters to represent, which is not consistent with",
             " Genepop format")
    if (file == "")
        file <- stdout()
    else if (is.character(file)) {
        file <- file(description = file, open = "wt")
        on.exit(close(file))
    } else if (! isOpen(file, "w")) {
        open(file, "w")
        on.exit(close(file))
    }
    if (! inherits(file, "connection"))
        stop("'file' must be a character string or connection")
    # first line
    cat(file = file, sep = "", attr(x, "dataset.title"), eol)
    # second line, offset by the longest sample name and each locus name,
    # if possible, aligned with its genotype column
    offset <- max(nchar(x[, 1])) + 3
    offset.spaces <- paste(collapse = "", rep(" ", offset))
    locus.names <- attr(x, "locus.names")
    genotype.columns <- seq(offset + 1, by = (ploidy * allele.digits + 1),
                            length.out = n.loci)
    cat(file = file, sep = "", offset.spaces)
    this.c <- offset + 1
    for (i in seq(along = genotype.columns)) {
        gt.c <- genotype.columns[i]
        if (gt.c == this.c) {
            cat(file = file, sep = "", locus.names[i])
        } else if (gt.c < this.c) {
            cat(file = file, sep = "", locus.names[i])
        } else if (gt.c > this.c) {
            cat(file = file, sep = "", 
                paste(collapse = "", rep(" ", gt.c - this.c)),
                locus.names[i])
            this.c <- this.c + (gt.c - this.c)
        }
        if (i != length(genotype.columns))
            cat(file = file, sep = "", ",")
        this.c <- this.c + nchar(locus.names[i]) + 1
    }
    cat(file = file, sep = "", eol)
    ##
    allele.format <- paste0("%0", allele.digits, "d")
    # now, for each population, we have a Pop line and the population name
    for (pop in attr(x, "pop.labels")) {
        cat(file = file, sep = "", "Pop ", pop, eol) # Pop line
        # Individual genoytpes
        # popdata <- getPopulation(x, pop)  #TODO
        popdata <- x[x[, 2] == pop, , drop = FALSE]
        gt <- as.data.frame(popdata)[, 3:ncol(x)]
        gt[is.na(gt)] <- 0
        gt <- sapply(gt, function(.x) sprintf(allele.format, .x)) # alleles
        m <- c()
        for (i in seq(1, by = ploidy, length.out = n.loci)) {
            if (ploidy == 1)
                m <- cbind(m, gt[, i])
            else if (ploidy == 2)
                m <- cbind(m, paste0(gt[, i], gt[, i + 1]))
            else stop(ploidy, " is an invalid ploidy here")
        }
        m <- apply(m, 1, function(.x) paste(collapse = " ", .x))
        samples <- popdata[, 1]  # sample names
        for (i in seq(along = samples)) {
            # calculate buffer of spaces to keep things lined up
            spc <- substring(offset.spaces, nchar(samples[i]) + 4)
            cat(file = file, sep = "", samples[i], " , ", spc, m[i], eol)
        }
    }
}
