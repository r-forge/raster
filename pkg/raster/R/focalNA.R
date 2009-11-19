# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  November 2009
# Version 0.9
# Licence GPL v3


focalNA <- function(raster, fun=mean, ngb=3, recursive=FALSE, iterator=0, filename="", ...) {
	allNA <- TRUE
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

	keepGoing <- FALSE
	
	filename <- trim(filename)
    if (recursive) {
		fn <- ''
		ovwr <- .overwrite(...)
		if (filename != '' & file.exists(filename) & !ovwr) {
			stop('file exists, use overwrite=TRUE to overwrite it')
		}
	} else {
		fn <- filename
	}
	
	if (!canProcessInMemory(ngbgrid, 4) && fn == '') {
		fn <- rasterTmpFile()
		if (getOption('verbose')) { cat('writing raster to:', fn)	}						
	}

	if (fn == '') {
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
			nas <- sum(is.na(vals))
			if (nas > 0) { keepGoing <- TRUE }
			if (allNA) {
				if (nas < ncol(ngbgrid)) {
					allNA <- FALSE
				}
			}
		} else { 
			allNA <- FALSE
		}
		
		if (fn != "") {
			ngbgrid <- setValues(ngbgrid, vals, r)
			ngbgrid <- writeRaster(ngbgrid, filename=fn, ...)
		} else {
			v[,r] <- vals
		}
		pbStep(pb, r)
	}
	pbClose(pb)

	if (fn == "") { 
		ngbgrid <- setValues(ngbgrid, as.vector(v)) 
	}
	# perhaps useful to avoid clogging up ram when used recursively?
	rm(v)
	rm(raster)
	
	if (recursive) {
		if (allNA) {
			stop('all values are NA')
		}
		#test <- try (iter <- iter + 1, silent=TRUE )
		#if (class(test) == "try-error") { iter <- 1 }
		if (iterator >= 0) {
			iterator <- 1 + iterator
			cat('iteration', iterator, '\n')
			flush.console()
		}
		if (keepGoing) {
			ngbgrid <- focalNA(ngbgrid, fun=fun, ngb=ngb, recursive=TRUE, filename=filename, iterator=iterator, ...) 
		} else {
			if (filename != '') {
				ngbgrid <- saveAs(ngbgrid, filename=filename, ...)
			}
			return(ngbgrid)
		}
	} else {
		return(ngbgrid)
	}
}

