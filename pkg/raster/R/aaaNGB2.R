
.getNGBindex <- function(rows, colnrs) {
	index <- vector()
	for (i in 1:dim(rows)[2]) {
		v <- as.vector(rows[, colnrs[i, ]])
		index <- c(index, rep(i, times=length(v)))
	}
	return(index)
}


.getNGBvalues <- function(rows, colnrs) {
	values <- vector()
	for (i in 1:dim(rows)[2]) {
		v <- as.vector(rows[, colnrs[i, ]])
		values <- c(values, v)
	}
	return(values)
}



.ngb2 <- function(raster, fun=mean, ngb=3, keepdata=TRUE, filename="", ...) {
	
	ngb <- .checkngb(ngb)
	
	ngbgrid <- raster(raster)

# to do: if proj is latlon & between -180 to 180, then use cells from other side..
#	global <- .isGlobalLatLon(raster)
#	if (global) {}
	
	# first create an empty matrix with nrows = ngb and ncols = raster@ncols
	
	limcol <- floor(ngb[2] / 2)
	colnrs <- (-limcol+1):(ncol(ngbgrid)+limcol)
	colnrs <- .embed(colnrs, ngb[2])
	colnrs[colnrs > ncol(ngbgrid) | colnrs < 0] <- 0

	ngbdata <- matrix(NA, nrow=ngb[2], ncol=ncol(ngbgrid))
	index <- .getNGBindex(ngbdata, colnrs) 	
	
	
	if (keepdata) { 
		test <- try(fun(na.omit(NA)))
		if (class(test) == 'try-error') {
			fun <- function(x) {
				x <- na.omit(x)
				if (length(x) == 0) { return(NA)
				} else { return(fun(x)) 
				}
			}
		} else {
			fun <- function(x){ fun(na.omit(x)) }
		}
	}
		
	limrow <- floor(ngb[1] / 2)
	ngbdata <- matrix(NA, nrow=0, ncol=ncol(ngbgrid))
# add all rows needed for first ngb, minus 1 that will be read in first loop	
	for (r in 1:limrow) {
		rowdata <- getValues(raster, r)
		ngbdata <- rbind(ngbdata, rowdata)
	}


	filename <- trim(filename)
	if (!canProcessInMemory(ngbgrid, 2) && filename == '') {
		filename <- rasterTmpFile()
		if (getOption('verbose')) { cat('writing raster to:', filename)	}						
	}

	if (filename == '') {
		v <- matrix(NA, ncol=nrow(ngbgrid), nrow=ncol(ngbgrid))
	} else {
		v <- vector(length=0)
	}
	
	
	res <- vector(length=ncol(ngbdata))
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
		
		v <- .getNGBvalues(ngbdata, colnrs)
		ngbvals <- tapply(v, index, fun)
		
		if (filename != "") {
			ngbgrid <- setValues(ngbgrid, ngbvals, r)
			ngbgrid <- writeRaster(ngbgrid, filename=filename, ...)
		} else {
			v[,r] <- ngbvals
		}
		pbStep(pb, r)
	}
	pbClose(pb)

	if (filename == "") { 
		ngbgrid <- setValues(ngbgrid, as.vector(v)) 
	}
	return(ngbgrid)
}
	
	