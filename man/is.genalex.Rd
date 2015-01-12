\name{is.genalex}
\alias{is.genalex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check to see if \code{data.frame} was created by the \code{readGenalex} package
}
\description{
Check to see if \code{data.frame} was created by the \code{readGenalex} package.  It specifically checks that the \code{genetic.data.format} attribute of the input \code{data.frame} is \code{"genalex"}.
}
\usage{
is.genalex(checkdata, quiet = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{checkdata}{
An annotated \code{data.frame} that might be created by \code{readGenalex()}
}
  \item{quiet}{
If \code{TRUE}, then the function quietly returns \code{TRUE} or \code{FALSE}.  If \code{FALSE}, then \code{stop()} will be called for a non-\code{readGenalex} \code{data.frame}.
}
}
\value{
\code{TRUE} or \code{FALSE} if \code{quiet=TRUE}, or \code{TRUE} or \code{stop()} if \code{quiet=FALSE}
}
\author{
Douglas G. Scofield
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(example_genotypes)
is.genalex(example_genotypes)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
