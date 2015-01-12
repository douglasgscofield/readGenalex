\name{computeGenalexColumns}
\alias{computeGenalexColumns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine numeric column positions occupied by named loci
}
\description{
Determine the numeric column positions occupied by named loci in a \code{date.frame} produced by \code{readGenalex()}.  This is mostly used as a utility routine by other functions in the \code{readGenalex} package.
}
\usage{
computeGenalexColumns(dat, locus, ploidy = NULL)
}
\arguments{
  \item{dat}{
An annotated \code{data.frame} created by \code{readGenalex()}
}
  \item{locus}{
The names of one or more loci found in \code{dat}
}
  \item{ploidy}{
Ploidy of data in \code{dat}, if not supplied is extracted from the \code{ploidy} attribute of \code{dat}
}
}
\value{
A vector of column positions occupied by genotype data for loci named in \code{locus}.
}
\author{
Douglas G. Scofield
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(example_genotypes)
computeGenalexColumns(example_genotypes, c("loc2","loc4"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
