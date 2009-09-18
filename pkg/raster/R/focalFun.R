# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3

.embed <- function(x, dimension) {
    n <- length(x)
    m <- n - dimension + 1
    data <- x[1:m + rep.int(1:dimension, rep.int(m, dimension)) - 1]
	dim(data) <- c(m, dimension)
	return(data)
}


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


focal <- function(raster, fun=mean, filename="", ngb=3, keepdata=TRUE, ...) {
	
	datatype <- .datatype(...)
	filetype <- .filetype(...)
	overwrite <- .overwrite(...)
	track <- .track(...)

	ngb <- as.integer(round(ngb))
	if (length(ngb) == 1) {
		ngb <- c(ngb, ngb)
	} else if (length(ngb) > 2) {
		stop('ngb should be a single value or two values')
	}
	if (min(ngb) < 3) { stop("ngb should be 3 or larger") } 
	
#	if (ngb[1] %% 2 == 0 | ngb[2] %% 2 == 0) { stop("only odd neighborhoods are supported") }

	filename <- trim(filename)
	ngbgrid <- raster(raster, filename=filename)
	dataType(ngbgrid) <- datatype

# first create an empty matrix with nrows = ngb and ncols = raster@ncols
	res <- vector(length=length(ncol(ngbgrid)))
	limcol <- floor(ngb[2] / 2)
	colnrs <- (-limcol+1):(ncol(ngbgrid)+limcol)
	colnrs <- .embed(colnrs, ngb[2])
	colnrs[colnrs > ncol(ngbgrid) | colnrs < 0] <- 0

	limrow <- floor(ngb[1] / 2)
	ngbdata <- matrix(NA, nrow=0, ncol=ncol(ngbgrid))
# add all rows needed for first ngb, minus 1 that will be read in first loop	
	for (r in 1:limrow) {
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
		rr <- r + limrow
		if (rr <= nrow(ngbgrid)) {
			if (dataContent(raster)=='all') {
				rowdata <- valuesRow(raster, rr)
			} else {	
				rowdata <- values(readRow(raster, rr))
			}
			if (dim(ngbdata)[1] == ngb[1]) {
				ngbdata <- rbind(ngbdata[2:ngb[1],], rowdata)
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
		if (r %in% track) { .showTrack(r, ngbgrid@nrows, track, starttime) }
	}
	if (filename == "") { 
		ngbgrid <- setValues(ngbgrid, v) 
	}
	return(ngbgrid)
}
	
