#' Deprecated functions in the \code{readGenalex} package
#' 
#' These functions are provided for compatibility with older version of
#' the \code{readGenalex} package.  They may eventually be completely removed.
#'
#' @rdname readGenalex-deprecated
#'
#' @name readGenalex-deprecated
#'
#' @param ... Parameters to be passed to the current version of the function
#'
#' @docType package
#'
#' @export  printGenalexGenotype computeGenalexColumns reorderGenalexLoci getGenalexLocus putGenalexLocus dropGenalexLoci reduceGenalexPloidy
#'
#' @aliases  printGenalexGenotype computeGenalexColumns reorderGenalexLoci getGenalexLocus putGenalexLocus dropGenalexLoci reduceGenalexPloidy
#'
#' @section Details:
#'
#' \tabular{rl}{
#'   \code{printGenalexGenotype} \tab now a synonym for \code{\link{printGenotype}}\cr
#'   \code{computeGenalexColumns} \tab now a synonym for \code{\link{getLocusColumns}}\cr
#'   \code{reorderGenalexLoci} \tab now a synonym for \code{\link{reorderLoci}}\cr
#'   \code{getGenalexLocus} \tab now a synonym for \code{\link{getLocus}}\cr
#'   \code{putGenalexLocus} \tab now a synonym for \code{\link{replaceLocus}}\cr
#'   \code{dropGenalexLoci} \tab now a synonym for \code{\link{dropLocus}}\cr
#'   \code{reduceGenalexPloidy} \tab now a synonym for \code{\link{reducePloidy}}\cr
#' }
#'  
printGenalexGenotype <- function(...) {
    .Deprecated("printGenotype", package = "readGenalex")
    printGenotype(...)
}
computeGenalexColumns <- function(...) {
    .Deprecated("getLocusColumns", package = "readGenalex")
    getLocusColumns(...)
}
reorderGenalexLoci <- function(...) {
    .Deprecated("reorderLoci", package = "readGenalex")
    reorderLoci(...)
}
getGenalexLocus <- function(...) {
    .Deprecated("getLocus", package = "readGenalex")
    getLocus(...)
}
putGenalexLocus <- function(...) {
    .Deprecated("replaceLocus", package = "readGenalex")
    replaceLocus(...)
}
dropGenalexLoci <- function(...) {
    .Deprecated("dropLocus", package = "readGenalex")
    dropLocus(...)
}
reduceGenalexPloidy <- function(...) {
    .Deprecated("reducePloidy", package = "readGenalex")
    reducePloidy(...)
}
