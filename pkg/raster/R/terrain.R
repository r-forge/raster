# Author: Robert J. Hijmans
# Date : February 2011
# Version 1.0
# Licence GPL v3


terrain <- function(x, filename='', out='', unit='radians', neighbors=8, ...) {
	
	stopifnot(is.character(filename))
	filename <- trim(filename)

	stopifnot(projection(x) != "NA")

	unit <- trim(tolower(unit))
	
	out <- trim(tolower(out))
	i <- which(! out %in% c('tri', 'tpi', 'roughness','slope', 'aspect'))
	if (length(i) > 0) {
		stop('invalid value in "out"')
	}
	stopifnot(length(out) > 0 ) 
	
	un <- as.integer(1)
	if ('slope' %in% out | 'aspect' %in% out) {
		stopifnot(unit %in% c('degrees', 'radians'))
		if (unit=='degrees') {
			un <- as.integer(0)
		}
		stopifnot(neighbors %in% c(4, 8))
	}


	opt <- rep(0, 7)
	if ('tri' %in% out) {
		opt[1] <- 1
	} 
	if ('tpi' %in% out) {
		opt[2] <- 1
	} 
	if ('roughness' %in% out) {
		opt[3] <- 1
	}
	if ('slope' %in% out) {
		if (neighbors == 4) {
			opt[4] <- 1
		} else {
			opt[5] <- 1
		}
	}
	if ('aspect' %in% out) {
		if (neighbors == 4) {
			opt[6] <- 1
		} else {
			opt[7] <- 1
		}
	} 
	opt <- as.integer(opt)
	nl <- sum(opt)
	
	if (nl == 1) {
		out <- raster(x)
	} else {
		out <- brick(x, values=FALSE, nl=nl)
	}
	layerNames(out) <- c('tri', 'tpi', 'roughness','slope', 'slope', 'aspect', 'aspect')[as.logical(opt)]
	filename <- trim(filename)
	rs <- as.double(res(out))
	
	if (canProcessInMemory(out)) {
		v <- .Call('terrain', as.double(values(x)), as.integer(dim(out)), rs, un, opt, NAOK=TRUE, PACKAGE='raster')
		out <- setValues(out, v)
		if (filename  != '') {
			out <- writeRaster(out, filename, ...)
		}
	} else {

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

 