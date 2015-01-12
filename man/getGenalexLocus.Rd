\name{getGenalexLocus}
\alias{getGenalexLocus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return genotype data for specified loci in the \code{readGenalex}-format \code{data.frame}
}
\description{
Return genotype data for specified loci in the \code{readGenalex}-format \code{data.frame}, optionally restricted to samples from specific populations.
}
\usage{
getGenalexLocus(dat, locus, pop = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
An annotated \code{data.frame} created by \code{readGenalex()}
}
  \item{locus}{
The names of one or more loci found in \code{dat}
}
  \item{pop}{
If supplied, return only data for samples from the specified populations
}
}
\value{
A \code{data.frame} containing genotype data from \code{dat} for loci specified in \code{code}, optionally restricted to samples from populations specified in \code{pop}.
}
\author{
Douglas G. Scofield
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(example_genotypes)
loc1 = getGenalexLocus(example_genotypes, "loc1")
loc2.pop2 = getGenalexLocus(example_genotypes, "loc2", "pop2")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
