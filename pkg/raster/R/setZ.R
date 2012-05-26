# Robert J. Hijmans
# June 2011
# Version 1.0
# Licence GPL v3



setZ <- function(x, z, name='time') {
	z <- unlist(z)
	stopifnot(length(z) == nlayers(x))
	x@z <- list(z)
	names(z) <- name[1]
	x
}


getZ <- function(x) {
	return(unlist(x@z[1]))
}




