\name{reorderGenalexLoci}
\alias{reorderGenalexLoci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reorder a \code{readGenalex}-format \code{data.frame}
}
\description{
Reorder the genotype columns of a \code{readGenalex}-format \code{data.frame} by locus.
}
\usage{
reorderGenalexLoci(dat, loci)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
An annotated \code{data.frame} created by \code{readGenalex()}
}
  \item{loci}{
The names of loci found in \code{dat}, in the desired new order.  All loci in \code{dat} must be named.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A \code{data.frame} containing the same genotype data from \code{dat} reordered according to \code{loci}.
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
