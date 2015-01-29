\name{dropGenalexLoci}
\alias{dropGenalexLoci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove specified loci from the \code{readGenalex}-format \code{data.frame}
}
\description{
Remove specified loci from the \code{readGenalex}-format \code{data.frame} and updates attributes
}
\usage{
dropGenalexLoci(dat, drop.loci, quiet = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
An annotated \code{data.frame} created by \code{readGenalex()}
}
  \item{drop.loci}{
The names of one or more loci found in \code{dat}
}
  \item{quiet}{
If set to \code{TRUE}, quietly returns \code{dat} if none of \code{drop.loci} are found in \code{dat}
}
}
\value{
A \code{data.frame} containing the data in \code{dat} after removing loci specified by \code{drop.loci}, with attributes updated as required.
}
\author{
Douglas G. Scofield
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(example_genotypes)
newdat = dropGenalexLoci(example_genotypes, "loc3")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
