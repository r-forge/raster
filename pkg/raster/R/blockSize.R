# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : November 2009
# Version 0.9
# Licence GPL v3


blockSize <- function(x, size) {
	bs = .chunksize()
	if (missing(size)) {
		size <- min(nrow(x), max(1, floor(bs / ncol(x))))
	}
	nb <- ceiling(x@nrows / size)
	rows <- (0:(nb-1))*size + 1
	return(list(size=size, rows=rows, n=nb))
}

