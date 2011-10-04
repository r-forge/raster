# Author: Robert J. Hijmans
# Date : February 2011
# Version 1.0
# Licence GPL v3


terrain <- function(x, filename='', out, unit='radians', neighbors=8, ...) {
	
	stopifnot(is.character(filename))
	filename <- trim(filename)

	stopifnot(projection(x) != "NA")

	unit <- trim(tolower(unit))
	
	out <- trim(tolower(out))
	stopifnot(out %in% c('slope', 'aspect', 'tri', 'tpi', 'roughness'))
	
	un <- as.integer(1)
	if (out == 'slope' | out == 'aspect') {
		stopifnot(unit %in% c('degrees', 'radians'))
		if (unit=='degrees') {
			un <- as.integer(0)
		}
		stopifnot(neighbors %in% c(4, 8))
	}


	if (out == 'tri') {
		opt <- 1
	} else if (out == 'tpi') {
		opt <- 2
	} else if (out == 'roughness') {
		opt <- 3
	} else if (out == 'slope') {
		if (neighbors == 4) {
			opt <- 4
		} else {
			opt <- 5
		}
	} else if (out == 'aspect') {
		if (neighbors == 4) {
			opt <- 6
		} else {
			opt <- 7
		}
	} else {
		stop('"out" is not a valid output variable')
	}
	opt <- as.integer(opt)

	out <- raster(x)
	filename <- trim(filename)
	rs <- as.double(res(out))
	
	if (canProcessInMemory(out)) {
		v <- .Call('terrain', as.double(values(x)), as.integer(dim(out)), rs, un, opt, NAOK=TRUE, PACKAGE='raster')
		out <- setValues(out, v)
		if (filename  != '') {
			out <- writeRaster(out, filename, ...)
		}
	} else {

		if (filename == '') {
			filename <- rasterTmpFile()
		}
		out <- writeStart(out, filename, ...)
		tr <- blockSize(out, minblocks=3, minrows=3)
		nc <- ncol(out)
		nc1 <- 1:nc

		v <- getValues(x, row=1, nrows=tr$nrows[1]+1)
		v <- .Call('terrain', as.double(v), as.integer(c(tr$nrows[1]+1, nc)), rs, un, opt, NAOK=TRUE, PACKAGE='raster')
		out <- writeValues(out, v, 1)
		for (i in 2:(tr$n-1)) {
			v <- getValues(x, row=tr$row[i]-1, nrows=tr$nrows[i]+2)
			v <- .Call('terrain', as.double(v), as.integer(c(tr$nrows[i]+2, nc)), rs, un, opt, NAOK=TRUE, PACKAGE='raster')
			out <- writeValues(out, v[-nc1], tr$row[i])
		}
		i <- tr$n
		v <- getValues(x, row=tr$row[i]-1, nrows=tr$nrows[i]+1)
		v <- .Call('terrain', as.double(v), as.integer(c(tr$nrows[i]+1, nc)), rs, un, opt, NAOK=TRUE, PACKAGE='raster')
		out <- writeValues(out, v[-nc1], tr$row[i])

		out <- writeStop(out)
	}
	return(out)
}


# x <- terrain(utm, out='tri')

 