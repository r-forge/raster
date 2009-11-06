# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  November 2009
# Version 0.9
# Licence GPL v3


focalNA <- function(raster, fun=mean, ngb=3, recursive=FALSE, filename="", ...) {
	ngb <- .checkngb(ngb)
	ngbgrid <- raster(raster)

	# first create an empty matrix with nrows = ngb and ncols = raster@ncols
	res <- vector(length=length(ncol(ngbgrid)))
	limcol <- floor(ngb[2] / 2)
	colnrs <- (-limcol+1):(ncol(ngbgrid)+limcol)
	colnrs <- .embed(colnrs, ngb[2])
	colnrs[colnrs > ncol(ngbgrid) | colnrs < 0] <- 0

	limrow <- floor(ngb[1] / 2)
	midrow <- ceiling(ngb[1] / 2)
	ngbdata <- matrix(NA, nrow=0, ncol=ncol(ngbgrid))
# add all rows needed for first ngb, minus 1 that will be read in first loop	


	for (r in 1:limrow) {
		rowdata <- getValues(raster, r)
		ngbdata <- rbind(ngbdata, rowdata)
	}

	res <- vector(length=ncol(ngbdata))

	keepgoing <- FALSE
	
	filename <- trim(filename)
	finalfilename <- filename
	if (filename != '' & recursive) {filename <- rasterTmpFile()}
	if (!canProcessInMemory(ngbgrid, 2) && filename == '') {
		filename <- rasterTmpFile()
		if (getOption('verbose')) { cat('writing raster to:', filename)	}						
	}

	if (filename == '') {
		v <- matrix(NA, ncol=nrow(ngbgrid), nrow=ncol(ngbgrid))
	} else {
		v <- vector(length=0)
	}
	
	pb <- pbCreate(nrow(ngbgrid), type=.progress(...))

	for (r in 1:nrow(ngbgrid)) {		
		rr <- r + limrow
		if (rr <= nrow(ngbgrid)) {
			rowdata <- getValues(raster, rr)
			if (dim(ngbdata)[1] == ngb[1]) {
				ngbdata <- rbind(ngbdata[2:ngb[1],], rowdata)
			} else {
				ngbdata <- rbind(ngbdata, rowdata)			
			}
		} else {
			ngbdata <- ngbdata[-1, ,drop=FALSE]
		}
		
		vals <- ngbdata[midrow,]
		if (sum(is.na(vals)) > 0) {
			ngbvals <- .calcNGB(ngbdata, colnrs, res, fun, keepdata=TRUE)
			vals[is.na(vals)] <- ngbvals[is.na(vals)]
			if (sum(is.na(vals)) > 0) { keepGoing <- TRUE }
		}
		
		if (filename != "") {
			ngbgrid <- setValues(ngbgrid, vals, r)
			ngbgrid <- writeRaster(ngbgrid, filename=filename, ...)
		} else {
			v[,r] <- vals
		}
		pbStep(pb, r)
	}
	pbClose(pb)

	if (filename == "") { 
		ngbgrid <- setValues(ngbgrid, as.vector(v)) 
	}
	
	if (recursive & keepGoing) {
		ngbgrid <- focalNA(ngbgrid, fun=fun, ngb=ngb, filename="", recursive=TRUE, ...) 
	} else {
		ngbgrid <- saveAs(ngbgrid, finalfilename, ...)
		return(ngbgrid)
	}
}

