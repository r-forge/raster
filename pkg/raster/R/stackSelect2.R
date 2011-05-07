# Author: Robert J. Hijmans
# Date:  May 2011
# Version 1
# Licence GPL v3


setMethod('stackSelect', signature(x='RasterStackBrick', y='RasterStackBrick'), 
function(x, y, recycle=FALSE, filename='', na.rm=FALSE, ...) {

	nlx <- nlayers(x)
	nly <- nlayers(y)
	filename <- trim(filename)
	out <- brick(x, values=FALSE)	
	compare(out, y)

	if (recycle) {
		stopifnot(nly > 1)
		stopifnot(nlx > nly)
		stopifnot(nlx %% nly == 0)
		nl <- nlx+nlx+nly
		nr <- nlx / nly
		id <- as.integer( (rep(1:nr, each=nly)-1) * nly )
		
	} else {
		if (nly == 1) {
			out <- raster(out)
		} else {
			out@data@nlayers <- nlayers(y)
		}
		nl <- nlx+nly
		id <- 0
	}

	ib <- (nlx+1):(nlx+nly)

	if (canProcessInMemory(x, nl)) {
	
		y <- round(getValues(y))
		y[y < 1 | y > nly] <- NA
		x <- cbind(getValues(x), y)
		n <- apply(x, 1, function(z) z[z[ib]+id] )
		out <- setValues(out, t(n))
		if (filename != "") {
			out <- writeRaster(out, filename=filename, ...)
		}
		
	} else {
	
		if (filename == '') { filename <- rasterTmpFile() }
	
		out <- writeStart(out, filename=filename, ...)
		tr <- blockSize(out, n=nlx+nly)
		pb <- pbCreate(tr$n, type=.progress(...))

		for (i in 1:tr$n) {
			j <- round(getValues(y, row=tr$row[i], nrows=tr$nrows[i]))
			j[j < 1 | j > nly] <- NA
			v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			v <- cbind(v, j)
			n <- apply(v, 1, function(z) z[z[ib]+id] )
			out <- writeValues(out, t(n), tr$row[i])
			pbStep(pb) 
		}
		out <- writeStop(out)
		pbClose(pb)
	}
	return(out)
} )
		
