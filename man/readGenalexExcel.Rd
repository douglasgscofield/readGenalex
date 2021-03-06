% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readGenalex-readwrite.R
\name{readGenalexExcel}
\alias{readGenalexExcel}
\title{Read GenAlEx-format genotypes from an Excel worksheet}
\usage{
readGenalexExcel(file, worksheet, ploidy = 2)
}
\arguments{
\item{file}{Excel workbook file from which to read the worksheet}

\item{worksheet}{Worksheet specification in a format accepted by
\code{\link[XLConnect]{readWorksheetFromFile}}, specifically the
worksheet name, as a character string, or an integer value indicating
the worksheet position in the workbook, e.g., 1, 2, etc.}

\item{ploidy}{The ploidy of genotypes encoded in \code{worksheet}
(defaults to 2)}
}
\value{
An annotated data frame of class \code{genalex}.  The format is
described in more detail in \code{\link{readGenalex}}.  The
\code{"data.file.name"} attribute is set to the value
\code{"file(worksheet)"}.
}
\description{
Reads genotype data file in GenAlEx format from an Excel worksheet
into an annotated data frame of class \code{genalex}.  Both
\code{.xls} and \code{.xlsx} formats may be read.  The same consistency
checks are performed as for \code{\link{readGenalex}}.  This function
uses the function \code{\link[XLConnect]{readWorksheetFromFile}} from the
\href{http://cran.r-project.org/web/packages/XLConnect/index.html}{XLConnect}
package to read the Excel file.  Strings representing \code{NA} values are
strictly those allowed by GenAlEx itself, 0 and -1.
}
\details{
This function is provided as a convenience to the user.  If there are
incompatibilities when reading GenAlEx-format data with this function,
it is recommended to export the worksheet from Excel as tab-delimited
text and read it with \code{\link{readGenalex}}.
}
\examples{

\dontrun{
xl.file <- system.file("extdata/Qagr_genotypes.xlsx",
                       package = "readGenalex")
## this could take ~5 sec or longer
gt <- readGenalexExcel(xl.file, worksheet = "Qagr_pericarp_genotypes")
head(gt)
names(attributes(gt))
}

}
\author{
Douglas G. Scofield
}
\references{
Peakall, R. and Smouse P.E. (2012) GenAlEx 6.5: genetic analysis
in Excel. Population genetic software for teaching and research-an update.
\emph{Bioinformatics} 28, 2537-2539.

Peakall, R. and Smouse P.E. (2006) GENALEX 6: genetic analysis in Excel.
Population genetic software for teaching and research. \emph{Molecular
Ecology Notes} 6, 288-295.
}
\seealso{
\code{\link{readGenalex}}, \code{\link[XLConnect]{readWorksheetFromFile}}
}
\keyword{attribute}
\keyword{file}
\keyword{manip}

