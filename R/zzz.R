## .First.lib <- function(lib,pkg) {
##   require(rms)
##   require(cmprsk)
##   library.dynam("QHScrnomo",pkg,lib)
##   cat("QHScrnomo- 2.0.2 has been loaded\n")
## }


##' @export

.onLoad <- function(lib, pkg) {
    ## require(rms)
    ## require(cmprsk)
    ## library.dynam("QHScrnomo",pkg,lib)
    cat("QHScrnomo- 2.0.3 has been loaded\n")
}

##'
##'
##'
##' @export

.Last.lib <- function(lib) {
    library.dynam.unload("QHScrnomo", lib)
}
