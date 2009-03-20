# Authors: Robert J. Hijmans 
# International Rice Research Institute
#contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.8
# Licence GPL v3

setMethod('merge', signature(x='RasterLayer', y='RasterLayer'), 
function(x,y,...,tolerance=0.05, filename="", overwrite=FALSE, filetype='raster', track=-1 ){ 
	
	rasters <- list(...)
	if (length(rasters) > 0) {
		for (i in 1:length(rasters)) {
			if (class(rasters[[i]]) != 'RasterLayer') {
				print(class(rasters[[i]]))
				stop('only supply RasterLayer objects as ... arguments')
			}
		}
	}
	rasters <- c(x, y, rasters)
	
			
	compare(rasters, bb=FALSE, rowcol=FALSE, orig=TRUE, res=TRUE, tolerance=tolerance)
	bb <- unionBbox(rasters)
	outraster <- raster(rasters[[1]], filename)
#	bndbox <- newBbox(bb[1,1], bb[1,2], bb[2,1], bb[2,2])
	outraster <- setExtent(outraster, bb, keepres=TRUE, snap=FALSE)

	isint <- TRUE
	for (i in 1:length(rasters)) {
		dtype <- .shortDataType(rasters[[i]]@file@datanotation)
		if (dtype != 'INT') {
			isInt <- FALSE
		}
	}
	if (isInt) { 
		dataType(outraster) <- 'INT4S'
	}
	
	rowcol <- matrix(0, ncol=3, nrow=length(rasters))
	for (i in 1:length(rasters)) {
		xy1 <- xyFromCell(rasters[[i]], 1) # first row/col on old raster[[i]]
		xy2 <- xyFromCell(rasters[[i]], ncell(rasters[[i]]) ) #last row/col on old raster[[i]]
		rowcol[i,1] <- rowFromY(outraster, xy1[2]) #start row on new raster
		rowcol[i,2] <- rowFromY(outraster, xy2[2]) #end row
		rowcol[i,3] <- colFromX(outraster, xy1[1]) #start col
	}
	
	v <- vector(length=0)
	
	if (!canProcessInMemory(x, 2) && filename == '') {
		filename <- tempfile()
		filename(outraster) <- filename
		if (options('verbose')[[1]]) { cat('writing raster to:', filename(raster))	}						
	}

	starttime <- proc.time()
	
	for (r in 1:nrow(outraster)) {
		rd <- as.vector(matrix(NA, nrow=1, ncol=ncol(outraster))) 
		for (i in length(rasters):1) {  #reverse order so that the first raster covers the second etc.
			if (r >= rowcol[i,1] & r <= rowcol[i,2]) { 
				if (dataSource(rasters[[i]]) == 'disk') {
					rasters[[i]] <- readRow(rasters[[i]], r + 1 - rowcol[i,1]) 
					d <- values(rasters[[i]])
				} else if (dataContent(rasters[[i]]) == 'all') {
					d <- valuesRow(rasters[[i]], r + 1 - rowcol[i,1]) 
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
		
		if (outraster@file@name != '') {
			outraster <- setValues(outraster, rd, r)
			outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype)
		} else {
			v <- c(v, rd)
		}

		if (r %in% track) { .showTrack(r, outraster@nrows, track, starttime) }
		
	}
	if (outraster@file@name == "") { 
		outraster <- setValues(outraster, v) 
	}
	return(outraster)
}
)


