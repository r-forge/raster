# Authors: Robert J. Hijmans 
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3

setMethod('merge', signature(x='RasterLayer', y='RasterLayer'), 
function(x,y,..., tolerance=0.05, filename="", format, overwrite, progress){ 
	
	if (missing(format)) {
		format <- .filetype()
	} 
	if (missing(overwrite)) {
		overwrite <- .overwrite()
	}
	if (missing(progress)) {
		progress <- .progress()
	}

	dots <- list(...)
	rasters <- c(x, y)
	if (length(dots) > 0) {
		for (i in 1:length(dots)) {
			if (class(dots[[i]]) == 'RasterLayer') {
				rasters <- c(rasters, dots[[i]])
			}
		}
	}

	compare(rasters, bb=FALSE, rowcol=FALSE, orig=TRUE, res=TRUE, tolerance=tolerance)
	bb <- unionExtent(rasters)
	outraster <- raster(rasters[[1]], filename)
	outraster <- setExtent(outraster, bb, keepres=TRUE, snap=FALSE)

	if (all(sapply(rasters, dataContent) == 'all')) {
		outraster[] = NA
		for (i in 1:length(rasters)) {
			cells = cellsFromExtent(outraster, extent(rasters[[i]]) )
			x = cbind(cells, outraster[cells], values(rasters[[i]]))
			x = subset(x, is.na(x[,2]))
			outraster[x[,1]] = x[,3]
		}	
		return(outraster)
	}
	
	isInt <- TRUE
	for (i in 1:length(rasters)) {
		dtype <- .shortDataType(rasters[[i]]@file@datanotation)
		if (dtype != 'INT') {
			isInt <- FALSE
		}
	}
	if (isInt) { 
		datatype <- 'INT4S'
	} else { 
		datatype <- 'FLT4S'
	}
	
	rowcol <- matrix(0, ncol=3, nrow=length(rasters))
	for (i in 1:length(rasters)) {
		xy1 <- xyFromCell(rasters[[i]], 1) # first row/col on old raster[[i]]
		xy2 <- xyFromCell(rasters[[i]], ncell(rasters[[i]]) ) #last row/col on old raster[[i]]
		rowcol[i,1] <- rowFromY(outraster, xy1[2]) #start row on new raster
		rowcol[i,2] <- rowFromY(outraster, xy2[2]) #end row
		rowcol[i,3] <- colFromX(outraster, xy1[1]) #start col
	}
	
	
	if (!canProcessInMemory(x, 2) && filename == '') {
		filename <- rasterTmpFile()
								
	}

	if (filename == "") {
		v <- matrix(NA, ncol=nrow(x), nrow=ncol(x))
	} else {
		outraster = writeStart(outraster, filename, ...)
	}
	
	pb <- pbCreate(nrow(outraster), type=progress)
	
	ds = sapply(rasters, dataSource)
	dc = sapply(rasters, dataContent)
	
	for (r in 1:nrow(outraster)) {
		rd <- as.vector(matrix(NA, nrow=1, ncol=ncol(outraster))) 
		for (i in length(rasters):1) {  #reverse order so that the first raster covers the second etc.
			if (r >= rowcol[i,1] & r <= rowcol[i,2]) { 
				if (ds[i] == 'disk' | dc[i] == 'all') {
					d <- getValues(rasters[[i]], r + 1 - rowcol[i,1]) 
				} else {
					d <- vector(length=ncol(rasters[[i]]))
					d[] <- NA
				}	
				id2 <- seq(1:ncol(rasters[[i]])) + rowcol[i,3] - 1
				d <- cbind(id2, d)
				d <- na.omit(d)
				rd[d[,1]] <- d[,2]
			}		
		}
		
		if (filename != '') {
			writeValues(outraster, rd, r)
		} else {
			v[,r] <- rd
		}
		pbStep(pb, r)
	}
	pbClose(pb)

	if (filename == "") { 
		outraster <- setValues(outraster, as.vector(v))
	} else {
		outraster <- writeStop(outraster)
	}
	return(outraster)
}
)


