# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3


..calcFilter2 <- function(rows, colnrs, res, filter, fun, na.rm) {
# not used
    for (i in 1:length(res)) {	
		d <- rows[, colnrs[i, ]]
		res[i] <- fun(d * filter, na.rm=na.rm)
	}
	return(res)
}



focalFilter <- function(x, filter, fun=sum, filename="", na.rm=FALSE, pad=TRUE, padValue=NA, ...) {
	if (!pad) {
		return(.focalFilterNoPad(x=x, filter=filter, fun=fun, filename=filename, na.rm=na.rm, ...))
	}

	if (!is.matrix(filter)) { stop('filter must be a matrix') }
	ngb <- dim(filter)
	if (prod(ngb) == 0) { stop('ncol and nrow of filter must be > 0') }
	if (min(ngb%%2) == 0) { stop('filter must have uneven sides') }	

	ngbgrid <- raster(x)
	glob <- .isGlobalLatLon(x)
	
	limcol <- floor(ngb[2] / 2)
	colnrs <- (-limcol+1):(ncol(ngbgrid)+limcol)
	colnrs <- .embed(colnrs, ngb[2]) + limcol 
	if (glob) {
		padfc <- 1:limcol
		fc <- padfc + limcol
		lc <- (ncol(ngbgrid)+1):(ncol(ngbgrid)+limcol)
		padlc <- lc + limcol
	}
	
	limrow <- floor(ngb[1] / 2)
	ngbdata <- matrix(padValue, ncol=ncol(x)+2*limcol, nrow=nrow(filter), byrow=TRUE) 
	colrange <- (limcol+1):(ncol(ngbdata)-limcol)

	fr <- (nrow(ngbdata)-limrow+1):nrow(ngbdata)
	ngbdata[fr, colrange] <- matrix(getValues(x, 1, limrow), nrow=limrow, byrow=TRUE)
	if (glob) {
		ngbdata[fr, padfc] <- ngbdata[fr, lc]
		ngbdata[fr, padlc] <- ngbdata[fr, fc]				
	}
	
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
			if (glob) {
				ngbdata[lastrow, padfc] <- ngbdata[lastrow, lc]
				ngbdata[lastrow, padlc] <- ngbdata[lastrow, fc]				
			}
		} else {
			ngbdata[rrows,] <- ngbdata[rrows+1,]
			ngbdata[lastrow, ] <- padValue
		}
		
		d <- matrix(as.vector(ngbdata[, t(colnrs)]), nrow=length(filter)) * as.vector(filter)
		ngbvals <- apply(d, 2, FUN=fun, na.rm=na.rm)
		
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

