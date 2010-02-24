

edge <- function(raster, filename="", type='both', keepclasses=FALSE, ...) {
	
	if (! type %in% c('both', 'inner', 'outer')) stop()
	
	ngb <- c(3,3)
	
	ngbgrid <- raster(raster)

# to do: if proj is latlon & between -180 to 180, then use cells from other side..
#	global <- .isGlobalLatLon(raster)
#	if (global) {}
	
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
		rowdata <- getValues(raster, r)
		if (! keepclasses) {
			rowdata[!is.na(rowdata)] <- 1
		} else {
			rowdata = round(rowdata)
			rowdata[rowdata < 1] <- 0
		}
		rowdata[is.na(rowdata)] <- 0
		ngbdata <- rbind(ngbdata, rowdata)
	}

	res <- vector(length=ncol(ngbdata))

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
	
	pb <- pbCreate(nrow(ngbgrid), type=.progress(...))

	fun = function(x) length(unique(x))!=1
	for (r in 1:nrow(ngbgrid)) {		
		rr <- r + limrow
		if (rr <= nrow(ngbgrid)) {
			rowdata <- getValues(raster, rr)
			if (! keepclasses) {
				rowdata[!is.na(rowdata)] <- 1
			} else {
				rowdata = round(rowdata)
				rowdata[rowdata < 1] <- 0
			}
			rowdata[is.na(rowdata)] <- 0
			if (dim(ngbdata)[1] == ngb[1]) {
				ngbdata <- rbind(ngbdata[2:ngb[1],], rowdata)
			} else {
				ngbdata <- rbind(ngbdata, rowdata)			
			}
		} else {
			ngbdata <- ngbdata[-1, ,drop=FALSE]
		}
		
		ngbvals <- .calcNGB(ngbdata, colnrs, res, fun, keepdata=FALSE) 
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

	
#ee = edge(r, progress='text')
