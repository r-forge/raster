# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : November 2009
# Version 0.9
# Licence GPL v3


blockSize <- function(x) {
	size <- 10  # for now
	nb <- ceiling(x@nrows / size)
	return(data.frame(size, nb))
}

