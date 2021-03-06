% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readGenalex.R
\name{genalex}
\alias{genalex}
\title{Create new object of class genalex from constituent data}
\usage{
genalex(samples, pops, genotypes, names = NULL, ploidy = 2,
  extra.columns = NULL)
}
\arguments{
\item{samples}{Sample names, must be unique and length must
match the number of rows in \code{genotypes}}

\item{pops}{Population names.  If \code{pops} is shorter than
the number of samples, it will be expanded following the rules
described in \code{\link{data.frame}}.}

\item{genotypes}{Genotype values, must be numeric}

\item{names}{List of names: \code{title} for data set title,
\code{sample} for sample column header, \code{pop} for population
column header, and \code{loci} for names of loci.  If \code{loci}
is missing, the corresponding \code{genotype} column names are used.
If \code{loci} is present but not the same length as the number of
loci, an error is produced}

\item{ploidy}{Ploidy of \code{genotypes}}

\item{extra.columns}{Extra data columns, see \code{\link{readGenalex}}}
}
\value{
Annotated data frame of class \code{genalex}.  If \code{names}
or any of its fields are not provided, default names are used.  The
\code{data.file.name} attribute is a character representation of the call
to \code{genalex}.
}
\description{
Create a new object of class \code{genalex} given sample and
population names and genotype data.  Titles for the dataset, sample
and population columns, and loci may be provided via the \code{names}
argument.
}
\examples{

gt <- data.frame(a = 11:13, a.2 = 14:16, b = 101:103, b.2 = 104:106)
nms <- list(title = "Example")
x <- genalex(1:3, "snurf", gt, nms)
x
attributes(x)

}
\author{
Douglas G. Scofield
}
\seealso{
\code{\link{readGenalex}}, \code{\link{data.frame}}
}

