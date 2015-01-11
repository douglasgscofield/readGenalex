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
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
An annotated \code{data.frame} created by \code{readGenalex()}
}
  \item{locus}{
The names of one or more loci found in \code{dat}
%%     ~~Describe \code{locus} here~~
}
  \item{ploidy}{
Ploidy of data in \code{dat}, if not supplied is extracted from the \code{ploidy} attribute of \code{dat}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of column positions occupied by genotype data for loci named in \code{locus}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Douglas G. Scofield
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
