# Author: Robert J. Hijmans
# Date : September 2011
# Version 1.0
# Licence GPL v3



.focalWeights <- function(x, w=rep(1,9)/9, filename='', ...) {

	out <- raster(x)
	filename <- trim(filename)

	if (canProcessInMemory(out)) {
		v <- .Call('focal3c', as.double(values(x)), as.double(w), as.integer(dim(out)), NAOK = TRUE, PACKAGE='raster')
		out <- setValues(out, v)
		if (filename  != '') {
			out <- writeRaster(out, filename, ...)
		}
	} else {

		if (filename == '') {
			filename <- rasterTmpFile()
		}
		out <- writeStart(out, filename)
		tr <- blockSize(out, minblocks=3, minrows=3)
		nc <- ncol(out)
		nc1 <- 1:nc

		v <- getValues(x, row=1, nrows=tr$nrows[1]+1)
		v <- .Call('focal3c', as.double(v), as.double(w), as.integer(c(tr$nrows[1]+1, nc)), NAOK = TRUE, PACKAGE='raster')
		out <- writeValues(out, v, 1)
		for (i in 2:(tr$n-1)) {
			v <- getValues(x, row=tr$row[i]-1, nrows=tr$nrows[i]+2)
			v <- .Call('focal3c', as.double(v), as.double(w), as.integer(c(tr$nrows[i]+2, nc)), NAOK = TRUE, PACKAGE='raster')
			out <- writeValues(out, v[-nc1], tr$row[i])
		}
		i <- tr$n
		v <- getValues(x, row=tr$row[i]-1, nrows=tr$nrows[i]+1)
		v <- .Call('focal3c', as.double(v), as.double(w), as.integer(c(tr$nrows[i]+1, nc)), NAOK = TRUE, PACKAGE='raster')
		out <- writeValues(out, v[-nc1], tr$row[i])

		out <- writeStop(out)
	}
	return(out)
}

