# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("cover")) {
	setGeneric("cover", function(x, y, ...)
		standardGeneric("cover"))
}	

setMethod('cover', signature(x='RasterLayer', y='RasterLayer'), 
	function(x, y, ..., filename="", filetype, datatype,  overwrite, track) {

	if (missing(filetype)) { filetype <- .filetype(...)	} 
	if (missing(overwrite)) { overwrite <- .overwrite(...) }
	if (missing(track)) { track <- .track(...) }

	outRaster <- raster(x, filename)

	rasters <- c(x, y)
	obs <- list(...)
	if (isTRUE(length(obs) > 0)) {
		for (i in 1:length(obs)) {
			if (extends(class(obs[[i]]), "RasterLayer")) {
				if (!(dataContent(obs[[i]]) != 'all'  &  dataSource(obs[[i]]) == 'ram' )) {
					rasters <- c(rasters, obs[[i]])
				}
			}
		}
	}
	
	if (length(rasters) < 2) { 
		if (length(rasters) == 1) {
			warning('Only a single useable RasterLayer')
			return(rasters[[1]])
		} else {
			stop('No useable RasterLayers')		
		}
	}
	compare(rasters)
		

	if (!missing(datatype)) {
		dataType(outRaster) <- datatype
	} else {
		dtp <-  getOption('rasterDatatype')
		if (!is.null(dtp)) {
			dataType(outRaster) <- dtp
		} else {
			isInt <- TRUE
			for (i in 1:length(rasters)) {
				dtype <- .shortDataType(rasters[[i]]@file@datanotation)
				if (dtype != 'INT') {
					isInt <- FALSE
				}
			}
			if (isInt) { 
				dataType(outRaster) <- 'INT4S'
			} else { 
				dataType(outRaster) <- 'FLT4S'
			}
		}
	}
	
		
	if (!canProcessInMemory(x, 4) && filename == '') {
		filename <- rasterTmpFile()
		filename(outRaster) <- filename
		if (getOption('verbose')) { cat('writing raster to:', filename(raster))	}						
	}
	starttime <- proc.time()

	v <- vector(length=0)
	for (r in 1:nrow(outRaster)) {
		v1 <- valuesRow(rasters[[1]], r)
		for (j in 2:length(rasters)) {
			v2 <- valuesRow(rasters[[j]], r)
			v1[is.na(v1)] <- v2[is.na(v1)] 
		}	
		if (filename == "") {
			v <- c(v, v1)
		} else {
			outRaster <- setValues(outRaster, v1, r)
			outRaster <- writeRaster(outRaster, filetype=filetype, overwrite=overwrite)
		}
		if (r %in% track) { .showTrack(r, outRaster@nrows, track, starttime) }
	}
	if (outRaster@file@name == "") {
		outRaster <- setValues(outRaster, v)
	}
	return(outRaster)
}
)
