# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : November 2009
# Version 0.9
# Licence GPL v3


blockSize <- function(x, chunksize, n=1) {
	n = max(n, 1)
	if (missing(chunksize)) {
		bs = .chunksize()  / n
	} else {
		bs = chunksize
	}
	size <- min(nrow(x), max(1, floor(bs / ncol(x))))
	nb <- ceiling(x@nrows / size)
	rows <- (0:(nb-1))*size + 1
	return(list(size=size, rows=rows, n=nb))
}

