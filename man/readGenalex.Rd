\name{readGenalex}
\alias{readGenalex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read GenAlEx-format genotypes file
}
\description{
Reads genotype data file in GenAlEx format into an annotated \code{data.frame}.  Internal consistency checks that are allowed by the GenAlEx format are also performed as data is read.  GenAlEx and its documentation are available at \url{http://biology-assets.anu.edu.au/GenAlEx}.
}
\usage{
readGenalex(file, sep = "\t", ploidy = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
Delimited text file in GenAlEx format, typically exported as tab- or comma-delimited text from Excel
}
  \item{sep}{
Column separator used when \code{file} was created (defaults to tab)
}
  \item{ploidy}{
The ploidy of genotypes encoded in \code{file} (defaults to 2)
}
}
\details{
\code{readGenalex()} expects a genotype data file in GenAlEx format, which specifies three header lines describing the structure and content of the file, followed by lines containing the genotype data, along with optionally extra columns specifying additional information about the sampled information for other analyses.  GenAlEx format for a collection of diploid samples is the following, with columns separated by \code{sep}:
\tabular{llllll}{
N loci        \tab Total N samples \tab N populations  \tab N pop 1        \tab N pop 2        \tab ... \cr
Dataset title \tab \emph{empty}    \tab \emph{empty}   \tab Name pop 1     \tab Name pop 2     \tab ... \cr
Sample title  \tab Pop title       \tab Name locus 1   \tab \emph{empty}   \tab Name locus 2   \tab ... \cr
ID sample 1   \tab ID sample 1 pop \tab Loc 1 allele 1 \tab Loc 1 allele 2 \tab Loc 2 allele 1 \tab ... \cr
ID sample 2   \tab ID sample 2 pop \tab Loc 1 allele 1 \tab Loc 1 allele 2 \tab Loc 2 allele 1 \tab ... \cr
...           \tab ...             \tab ...            \tab ...            \tab ...            \tab ... \cr
}
Calling \code{readGenalex()} for a file first reads the top 3 header lines, then reads the remainder of the file checking for consistency with the data description from the header lines.  It attempts to cleanly ignore extra delimiters that Excel might add when exporting a delimited file.  Extra columns beyond the genotype columns are allowed.

More information on GenAlEx is available \url{http://biology-assets.anu.edu.au/GenAlEx}.  In particular, there are other GenAlEx specifications regarding e.g., encoding of genotypes that are expected here but not described.
}
\value{
An annotated \code{data.frame} containing sample data, with column names determined by line 3 of the input file.  Special \code{attributes()} of the \code{data.frame} include
  \item{data.file.name      }{The value of \code{file}}
  \item{genetic.data.format }{\code{"genalex"}}
  \item{ploidy              }{Ploidy of input data}
  \item{n.loci              }{Number of loci}
  \item{n.samples           }{Total number of samples}
  \item{n.pops              }{Number of populations}
  \item{pop.labels          }{Names of populations}
  \item{pop.sizes           }{Sizes of populations}
  \item{dataset.title       }{Dataset title}
  \item{sample.title        }{Sample title}
  \item{pop.title           }{Population title}
  \item{locus.columns       }{Numeric column positions of allele 1 of each locus in the \code{data.frame}}
  \item{extra.columns       }{\code{data.frame} containing any extra columns given in \code{file} to the right of the genotype columns.  Row order is the same as the genotype \code{data.frame}.  If no extra columns were found, this attribute does not exist.}
}
\references{
Peakall, R. and Smouse P.E. (2012) GenAlEx 6.5: genetic analysis in Excel. Population genetic software for teaching and research-an update. \emph{Bioinformatics} 28, 2537-2539.

Peakall, R. and Smouse P.E. (2006) GENALEX 6: genetic analysis in Excel. Population genetic software for teaching and research. \emph{Molecular Ecology Notes} 6, 288-295. 
}
\author{
Douglas G. Scofield
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(example_genotypes)
example_genotypes
attributes(example_genotypes)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
\keyword{ manip }
\keyword{ attribute }
