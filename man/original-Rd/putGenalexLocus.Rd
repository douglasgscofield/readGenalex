\name{putGenalexLocus}
\alias{putGenalexLocus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Replace genotype data in a \code{readGenalex}-format \code{data.frame}
}
\description{
Replace genotype data for specified loci in a \code{readGenalex}-format \code{data.frame}.
}
\usage{
putGenalexLocus(dat, locus, newdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
An annotated \code{data.frame} created by \code{readGenalex()}
}
  \item{locus}{
The names of one or more loci found in \code{dat}
}
  \item{newdata}{
New genotype data for loci specified in \code{locus}.  Must have the same number of rows as \code{dat}
}
}
\value{
A \code{data.frame} containing genotype data from \code{dat} with data for loci specified in \code{locus} replaced with data from \code{newdata}.
}
\author{
Douglas G. Scofield
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
