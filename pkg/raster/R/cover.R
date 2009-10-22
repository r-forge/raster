# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("cover")) {
	setGeneric("cover", function(x, y, ...)
		standardGeneric("cover"))
}	

setMethod('cover', signature(x='RasterLayer', y='RasterLayer'), 
	function(x, y, ..., datatype=NULL) {

	rasters <- .makeRasterList(x, y, ...)
	compare(rasters)
		
	outRaster <- raster(x)

	if (is.null(datatype)) {
# check for boolean data?
		isInt <- TRUE
		for (i in 1:length(rasters)) {
			dtype <- .shortDataType(rasters[[i]]@file@datanotation)
			if (dtype != 'INT') {
				isInt <- FALSE
			}
		}
		if (isInt) { 
			datatype  <- 'INT4S'
		} else { 
			datatype <- 'FLT4S'
		}
	}
	.setDataType(outRaster) <- datatype
		
	filename <- .filename(...)	
	if (!canProcessInMemory(x, 4) && filename == '') {
		filename <- rasterTmpFile()
		.setFilename(outRaster) <- filename
		if (getOption('verbose')) { cat('writing raster to:', filename(raster))	}						
	}
	

	v <- vector(length=0)
	pb <- pbCreate(nrow(outRaster), type=.progress(...))
	
	for (r in 1:nrow(outRaster)) {
		v1 <- getValues(rasters[[1]], r)
		for (j in 2:length(rasters)) {
			v2 <- getValues(rasters[[j]], r)
			v1[is.na(v1)] <- v2[is.na(v1)] 
		}	
		if (filename == "") {
			v <- c(v, v1)
		} else {
			outRaster <- setValues(outRaster, v1, r)
			outRaster <- writeRaster(outRaster, filename=filename, datatype=datatype, ...)
		}
		pbStep(pb, r) 
	}
	pbClose(pb)

	if (outRaster@file@name == "") {
		outRaster <- setValues(outRaster, v)
	}
	return(outRaster)
}
)
