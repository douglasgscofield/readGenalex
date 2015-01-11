is.genalex <-
function(checkdata, quiet=TRUE)
{
    if (attr(checkdata,"genetic.data.format") != "genalex") {
        if (quiet)
            FALSE
        else stop("data not genalex format")
    } else
        TRUE
}
