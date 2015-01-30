#' Read and manipulate GenAlEx-format genotype files
#' 
#' A collection of R functions to read and manipulate genotype data in GenAlEx
#' format.  GenAlEx is a widely-used Excel plugin for manipulating and
#' analysing genotype data.  This package reads GenAlEx data that has been
#' exported from Excel as a delimited text file and creates an annotated
#' \code{data.frame}.  Several functions are provided for accessing and
#' printing this data.  GenAlEx and its documentation are available at
#' \url{http://biology-assets.anu.edu.au/GenAlEx}.
#' 
#' Read and manipulate GenAlEx-format data files.
#' \href{http://biology-assets.anu.edu.au/GenAlExGenAlEx}{GenAlEx} is a 
#' widely-used' plugin for Excel that manipulates and analyses genotype data.
#' This package is designed to read GenAlEx-format genotype files that have 
#' been exported from Excel as delimited text.  Descriptions of the file 
#' format and of the annotations added to the \code{data.frame} as attributes
#' are available via \code{help(readGenalex)}.
#' 
#' @references Peakall, R. and Smouse P.E. (2012) GenAlEx 6.5: genetic analysis
#' in Excel. Population genetic software for teaching and research-an update.
#' \emph{Bioinformatics} 28, 2537-2539.
#' 
#' Peakall, R. and Smouse P.E. (2006) GENALEX 6: genetic analysis in Excel.
#' Population genetic software for teaching and research. \emph{Molecular
#' Ecology Notes} 6, 288-295.
#'
#' \url{https://github.com/douglasgscofield/readGenalex}
#'
#' @seealso \link{readGenalex}
#'
#' @keywords package attribute manip file
#'
#' @docType package
#'
#' @name readGenalex-package
#'
#' @aliases readGenalex-package GenAlEx genotype
#'
NULL



#' Example genotypes for demonstrating readGenalex package functions
#' 
#' This data set contains 16 hypothetical genotypes used for demonstrating the
#' operation of various functions in the \code{readGenalex} package.  It is a
#' \code{data.frame} created by \code{readGenalex()} and has additional
#' attributes describing the structure of the data set.
#' 
#' @format Data frame in \code{readGenalex} format containing 16 samples 
#'         from two populations, with each sample having a completely 
#'         hypothetical 5-locus genotype.
#' 
#' @source  Simulation
#'
#' @keywords datasets
#' 
#' @docType data
#' 
#' @name example_genotypes
#' 
NULL
