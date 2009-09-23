# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  August 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	

setMethod('predict', signature(object='RasterStackBrick'), 
	function(object, model, filename="", datatype='FLT4S', filetype = 'raster', overwrite=FALSE, track=-1, vartype=NA, ...) {
		predrast <- raster(object)
		filename(predrast) <- filename
		dataType(predrast) <- datatype
		
		dataclasses <- attr(model$terms, "dataClasses")
		f <- names( which(dataclasses == 'factor') )
		if (length(f) > 0) { 
			haveFactor <- TRUE 
		} else {
			haveFactor <- FALSE
		}
		
		if (!canProcessInMemory(predrast) && filename == '') {
			filename <- rasterTmpFile()
			filename(outRaster) <- filename
			if (getOption('verbose')) { cat('writing raster to:', filename(outRaster))	}						
		} 
		v <- vector()

		for (r in 1:nrow(object)) {
			rowvals <- as.data.frame(valuesRow(object, r))
			names(rowvals) <- layerNames(object)
			if (haveFactor) {
				for (i in 1:length(f)) {
					rowvals[,f[i]] <- as.factor(rowvals[,f[i]])
				}
			}
			predv <- as.vector( predict(model, rowvals, ...) )
			if (filename == '') {
				v <- c(v, predv)
			} else {
				predrast <- setValues(predrast, predv, r)
				predrast <- writeRaster(predrast, filetype=filetype, overwrite=overwrite)
			}
		}
		if (filename == '') {
			predrast <- setValues(predrast, v)
		}
		return(predrast)
	}
)



setMethod('predict', signature(object='RasterLayer'), 
	function(object, model, filename="", datatype='FLT4S', filetype = 'raster', overwrite=FALSE, track=-1,  ...) {
		predrast <- raster(object)
		filename(predrast) <- filename
		dataType(predrast) <- datatype

		if (!canProcessInMemory(predrast) && filename == '') {
			filename <- rasterTmpFile()
			filename(outRaster) <- filename
			if (getOption('verbose')) { cat('writing raster to:', filename(outRaster))	}						
		} 
		v <- vector()

		arow <- 1:ncol(object)
		for (r in 1:nrow(object)) {
			xy <- as.data.frame(xyFromCell(object, arow + (r-1) * ncol(object)) )
			predv <- as.vector( predict(model, xy, ...) )
			if (filename == '') {
				v <- c(v, predv)
			} else {
				predrast <- setValues(predrast, predv, r)			
				predrast <- writeRaster(predrast, filetype=filetype, overwrite=overwrite)
			}
		}	
		if (filename == '') {
			predrast <- setValues(predrast, v)
		}
		return(predrast)
	}
)

