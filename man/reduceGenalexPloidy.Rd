\name{reduceGenalexPloidy}
\alias{reduceGenalexPloidy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reduce the ploidy of a \code{readGenalex}-format \code{data.frame}
}
\description{
Reduce the ploidy of a \code{readGenalex}-format \code{data.frame}.  Currently restricted to reducing the ploidy of diploid data to haploid by selecting only the first allele of each locus.
}
\usage{
reduceGenalexPloidy(dat, new.ploidy = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
An annotated \code{data.frame} created by \code{readGenalex()}
}
  \item{new.ploidy}{
The desired new ploidy.  Currently, the only usefully accepted value is \code{1}; a ploidy matching the current plody of \code{dat} silently returns \code{dat}.
%%     ~~Describe \code{new.ploidy} here~~
}
}
\details{
This function reduced the ploidy of a \code{readGenalex}-format \code{data.frame} by selecting the first allele of each locus.  Occasionally haploid data is encoded in GenAlEx datasets by using homozygous diploid loci, and this is a useful function for making these truly haploid.
}
\value{
A \code{data.frame} containing genotype data from \code{dat} reduced to the specified \code{new.ploidy}, with attributes updated as required.
}
\author{
Douglas G. Scofield
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(example_genotypes)
p1 = reduceGenalexPloidy(example_genotypes, 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
