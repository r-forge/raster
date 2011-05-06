# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date:  March 2011
# Version 1
# Licence GPL v3


setMethod('stackSelect', signature(x='RasterStackBrick', y='RasterStackBrick'), 
function(x, y, fun=sum, filename='', na.rm=FALSE, ...) {

	nlx <- nlayers(x)
	nly <- nlayers(y)
	stopifnot(nlx > nly)
	stopifnot(nlx %% nly == 0)
	filename <- trim(filename)

	nrounds <- nlx / nly
	if (nrounds == 1) {
		out <- raster(x)
	} else {
		out <- brick(x)	
		out@data@nlayers <- as.integer(nrounds)
	}
	compare(out, y)
	
	
	if (canProcessInMemory(x, nlx+nly+(2*nrounds))) {
	
		i <- round(getValues(y))
		i[i < 1 | i > nly] <- NA
		m <- matrix(nr=ncell(out), nc=nrounds)
		n <- matrix(nr=ncell(out), nc=nly)
		
		x <- as.matrix(getValues(x))
		for (r in 1:nrounds) {
			sc <- ((r-1)*nly+1)
			xx <- x[, sc:(sc+nly-1), drop=FALSE]
			a <- cbind(1:nrow(xx), i)
			for (p in 1:nly) {
				n[,p] <- xx[a[,c(1, (1+p))]]
			}
			m[,r] <- apply(n, 1, fun, na.rm=na.rm)
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
		v <- as.matrix(getValues(x, row=tr$row[i], nrows=tr$nrows[i]))
		
		n <- matrix(nr=nrow(v), nc=nly)
		m <- matrix(nr=nrow(v), nc=nrounds)
		for (r in 1:nrounds) {
			sc <- ((r-1)*nly+1)
			xx <- v[, sc:(sc+nly-1), drop=FALSE]
			a <- cbind(1:nrow(xx), j)
			for (p in 1:nly) {
				n[,p] <- xx[a[,c(1, (1+p))]]
			}
			m[,r] <- apply(n, 1, fun, na.rm=na.rm)			
		}
		out <- writeValues(out, m, tr$row[i])
		pbStep(pb) 
	}

	out <- writeStop(out)
	pbClose(pb)
	return(out)
} )
	

