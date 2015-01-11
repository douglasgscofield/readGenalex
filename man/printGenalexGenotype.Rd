\name{printGenalexGenotype}
\alias{printGenalexGenotype}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print selected genotypes
}
\description{
Print selected genotypes, optionally calling out a locus
}
\usage{
printGenalexGenotype(dat, rows, callout.locus = NULL, sep = " ", allele.sep = "/", 
                     callout.char = "*", label = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
An annotated \code{data.frame} created by \code{readGenalex()}
}
  \item{rows}{
The specific rows of \code{dat} to print
}
  \item{callout.locus}{
One or more loci on \code{dat} to be surrounded by \code{callout.char} when printed
}
  \item{sep}{
Separator character to be used between loci
}
  \item{allele.sep}{
Separator character to be used between alleles
}
  \item{callout.char}{
Character which surrounds loci specified by \code{callout.locus}
%%     ~~Describe \code{callout.char} here~~
}
  \item{label}{
Label to be included between the sample and population ID columns and the genotype columns in output
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
No specific return value, used for its side effect of printing genotypes.
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
