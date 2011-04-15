# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3


.calcFilter2 <- function(rows, colnrs, res, filter, fun, na.rm) {
    for (i in 1:length(res)) {	
		d <- rows[, colnrs[i, ]]
		res[i] <- fun(d * filter, na.rm=na.rm)
	}
	return(res)
}


.focalFilterPad <- function(x, filter, fun=sum, filename="", na.rm=FALSE, padValue=NA, ...) {
	if (!is.matrix(filter)) { stop('filter must be a matrix') }
	ngb <- dim(filter)
	if (prod(ngb) == 0) { stop('ncol and nrow of filter must be > 0') }

	ngbgrid <- raster(x)

	limcol <- floor(ngb[2] / 2)
	colnrs <- (-limcol+1):(ncol(ngbgrid)+limcol)
	colnrs <- .embed(colnrs, ngb[2]) + limcol 
#	colnrs[colnrs > ncol(ngbgrid) | colnrs < 0] <- 0

	limrow <- floor(ngb[1] / 2)
	ngbdata <- matrix(padValue, ncol=ncol(x)+2*limcol, nrow=nrow(filter)) 
	colrange <- (limcol+1):(ncol(ngbdata)-limcol)

	for (i in 1:limrow) {
		ngbdata[i+limrow+1, colrange] <- getValues(x, i)
	}
	
#	ngbdata[(limrow+1):nrow(ngbdata), colrange] <- matrix(getValues(x, 1, limrow), ncol=ncol(x))
	
	res <- rep(NA, ncol(ngbgrid))
	
	filename <- trim(filename)
	if (!canProcessInMemory(ngbgrid, 2) && filename == '') {
		filename <- rasterTmpFile()			
	}
	
	if (filename == '') {
		v <- matrix(NA, ncol=nrow(ngbgrid), nrow=ncol(ngbgrid))
	} else {
		v <- vector(length=0)
		ngbgrid <- writeStart(ngbgrid, filename=filename, ...)
	}

	pb <- pbCreate(nrow(ngbgrid), type=.progress(...))

	
	lastrow <- nrow(filter)
	rrows <- 1:(lastrow-1)
	
	for (r in 1:nrow(ngbgrid)) {
		rr <- r + limrow
		if (rr <= nrow(ngbgrid)) {
			ngbdata[rrows,] <- ngbdata[rrows+1,]
			ngbdata[lastrow, colrange] <- getValues(x, rr)
		} else {
			ngbdata[rrows,] <- ngbdata[rrows+1,]
			ngbdata[lastrow, ] <- padValue
		}
		
		ngbvals <- .calcFilter2(ngbdata, colnrs, res, filter, fun=fun, na.rm=na.rm)
		if (filename != "") {
			ngbgrid <- writeValues(ngbgrid, ngbvals, r)
		} else {
			v[,r] <- ngbvals
		}
		pbStep(pb, r)
	}
	pbClose(pb)
	
	if (filename == "") { 
		ngbgrid <- setValues(ngbgrid, as.vector(v)) 
	} else {
		ngbgrid <- writeStop(ngbgrid)
	}
	return(ngbgrid)
}
	
