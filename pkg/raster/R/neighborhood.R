# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3


.calcNGB <- function(rows, colnrs, res, fun, keepdata) {
	res[] <- NA
    for (i in 1:dim(rows)[2]) {
		d <- as.vector(rows[, colnrs[i, ]])
		if (keepdata) {
			d <- na.omit(d)
		}
		res[i] <- fun(d)
	}	
	return(res)
}



neighborhood <- function(raster, fun=mean, filename="", ngb=3, keepdata=TRUE, overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1) {
	ngb <- as.integer(round(ngb))
	if (ngb < 3) { stop("ngb should be 3 or larger") } 
	if (ngb %% 2 == 0) { stop("only odd neighborhoods are supported") }
	lim <- floor(ngb / 2)

	filename <- trim(filename)
	ngbgrid <- raster(raster, filename=filename)
	dataType(ngbgrid) <- datatype

# first create an empty matrix with nrows = ngb and ncols = raster@ncols
	res <- vector(length=length(ncol(ngbgrid)))
	lim <- floor(ngb / 2)
	colnrs <- (-lim+1):(ncol(ngbgrid)+lim)
	colnrs <- embed(colnrs, ngb)
	colnrs[colnrs > ncol(ngbgrid) | colnrs < 0] <- 0

	ngbdata <- matrix(NA, nrow=0, ncol=ncol(ngbgrid))
# add all rows needed for first ngb, minus 1 that will be read in first loop	
	for (r in 1:lim) {
		if (dataContent(raster)=='all') {
			rowdata <- valuesRow(raster, r)
		} else {	
			rowdata <- values(readRow(raster, r))
		}
		ngbdata <- rbind(ngbdata, rowdata)
	}

	res <- vector(length=ncol(ngbdata))

	v <- vector(length=0)
	starttime <- proc.time()

	for (r in 1:nrow(ngbgrid)) {		
		rr <- r + lim
		if (rr <= nrow(ngbgrid)) {
			if (dataContent(raster)=='all') {
				rowdata <- valuesRow(raster, r)
			} else {	
				rowdata <- values(readRow(raster, r))
			}
			if (dim(ngbdata)[1] == ngb) {
				ngbdata <- rbind(ngbdata[2:ngb,], rowdata)
			} else {
				ngbdata <- rbind(ngbdata, rowdata)			
			}
		} else {
			ngbdata <- ngbdata[-1, ,drop=FALSE]
		}

		
		ngbvals <- .calcNGB(ngbdata, colnrs, res, fun, keepdata)
		if (filename != "") {
			ngbgrid <- setValues(ngbgrid, ngbvals, r)
			ngbgrid <- writeRaster(ngbgrid, overwrite=overwrite, filetype=filetype)
		} else {
			v <- c(v, ngbvals)
		}
#		if (r %in% track) { .showTrack(r, ngbgrid@nrows, track, starttime) }
	}
	if (filename == "") { 
		ngbgrid <- setValues(ngbgrid, v) 
	}
	return(ngbgrid)
}
	
