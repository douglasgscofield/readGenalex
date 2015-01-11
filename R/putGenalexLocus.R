putGenalexLocus <-
function(dat, locus, newdata)
{
    is.genalex(dat)
    dat[, computeGenalexColumns(dat,locus)] <- newdata
    dat
}
