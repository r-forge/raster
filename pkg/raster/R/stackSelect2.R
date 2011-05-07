# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date:  May 2011
# Version 1
# Licence GPL v3


setMethod('stackSelect', signature(x='RasterStackBrick', y='RasterStackBrick'), 
function(x, y, recycle=FALSE, filename='', na.rm=FALSE, ...) {

	nlx <- nlayers(x)
	nly <- nlayers(y)
	filename <- trim(filename)

	if (recursive) {
		stopifnot(nly > 1)
		stopifnot(nlx > nly)
		stopifnot(nlx %% nly == 0)
		
		nrounds <- nlx / nly
		out <- brick(x, values=FALSE)	
		compare(out, y)
	
		ib <- (nlx+1):(nlx+nly)
	
		if (canProcessInMemory(x, nlx+nlx+nly)) {
	
			y <- round(getValues(y))
			y[y < 1 | y > nly] <- NA
			m <- matrix(nr=ncell(out), nc=nlx)
			x <- cbind(getValues(x), y)
			for (r in 1:nrounds) {
				ia <- ((r-1)*nly+1)
				ia <- ia:(ia+nly)
				n <- apply(x, 1, function(x) x[ia][ x[ib]] )
				m[,ia] <- t(n)
			}
			out <- setValues(out, m)
			if (filename != "") {
				out <- writeRaster(out, filename=filename, ...)
			}
			return(out)
		}
	
		if (filename == '') { filename <- rasterTmpFile() } 
	
		out <- writeStart(out, filename=filename, ...)
		tr <- blockSize(out, n=nlx+nly)
		pb <- pbCreate(tr$n, type=.progress(...))

		for (i in 1:tr$n) {
			j <- round(getValues(y, row=tr$row[i], nrows=tr$nrows[i]))
			j[j < 1 | j > nly] <- NA
			v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			v <- cbind(v, j)
			m <- matrix(nr=nrow(v), nc=nrounds)
			for (r in 1:nrounds) {
				ia <- ((r-1)*nly+1)
				ia <- ia:(ia+nly)
				n <- apply(x, 1, function(x) x[ia][ x[ib]] )
				m[,ia] <- t(n)
			}	
			out <- writeValues(out, m, tr$row[i])
			pbStep(pb) 
		}
		out <- writeStop(out)
		pbClose(pb)
		return(out)
		
	} else {
	
		if (nly == 1) {
			out <- raster(x)
		} else {
			out <- brick(x, values=FALSE)	
			out@data@nlayers <- nlayers(y)
		}
		compare(out, y)
	
		ib <- (nlx+1):(nlx+nly)
	
		if (canProcessInMemory(x, nlx+nly+(2*nrounds))) {
		
			y <- round(getValues(y))
			y[y < 1 | y > nly] <- NA
			x <- cbind(getValues(x), y)
			m <- apply(x, 1, function(x)x[1:nlx][x[ib]] )
			out <- setValues(out, m)
			if (filename != "") {
				out <- writeRaster(out, filename=filename, ...)
			}
			return(out)
		}
	
		if (filename == '') { filename <- rasterTmpFile() } 
	
		out <- writeStart(out, filename=filename, ...)
		tr <- blockSize(out, n=nlx+nly)
		pb <- pbCreate(tr$n, type=.progress(...))

		for (i in 1:tr$n) {
			j <- round(getValues(y, row=tr$row[i], nrows=tr$nrows[i]))
			j[j < 1 | j > nly] <- NA
			v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			v <- cbind(v, j)
			m <- apply(x, 1, function(x)x[1:nlx][x[ib]] )
			out <- writeValues(out, m, tr$row[i])
			pbStep(pb) 
		}
		out <- writeStop(out)
		pbClose(pb)
		return(out)
	}
} )
	

