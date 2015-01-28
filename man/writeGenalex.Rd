\name{writeGenalex}
\alias{writeGenalex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write GenAlEx-format genotypes to a text file
}
\description{
Writes genotype data encoded in an annotated \code{data.frame} via the \code{readGenalex} package to a GenAlEx-format text file.  Extra data columns are included immediately to the right of genotype columns.  GenAlEx and its documentation are available at \url{http://biology-assets.anu.edu.au/GenAlEx}.
}
\usage{
writeGenalex(x, file, sep = "\t", na = "", eol = "\n")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Annotated \code{data.frame} created via the \code{readGenalex} package.
  }
  \item{file}{
    File name or connection for writing.  If given as \code{""}, \code{stdout()} is used.
  }
  \item{sep}{
    Column separator for output (defaults to \code{"\t"}).
  }
  \item{na}{
    The string to use when writing missing values in the data (defaults to \code{""}).
  }
  \item{eol}{
    End-of-line character used for output (defaults to \code{"\n"}).
  }
}
\value{
  No value is returned.
}
\details{
This function writes genotypes and associated information within an annotated
\code{data.frame} to a text file in GenAlEx format. More information is
available in the description for \code{\link{readGenalex}}, and at the GenAlEx
website at \url{http://biology-assets.anu.edu.au/GenAlEx}.
}
\references{
Peakall, R. and Smouse P.E. (2012) GenAlEx 6.5: genetic analysis in Excel. Population genetic software for teaching and research-an update. \emph{Bioinformatics} 28, 2537-2539.

Peakall, R. and Smouse P.E. (2006) GENALEX 6: genetic analysis in Excel. Population genetic software for teaching and research. \emph{Molecular Ecology Notes} 6, 288-295. 
}
\author{
Douglas G. Scofield
}
\seealso{
  \code{\link{readGenalex}}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
\dontrun{
data(example_genotypes)
writeGenalex(example_genotypes, file = "")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
\keyword{ manip }
\keyword{ attribute }
