# Author: Robert J. Hijmans, r.hijmans@gmail.com
# University of California, Davis
# Date :  August 2009
# Version 0.8
# Licence GPL v3

if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	

setMethod('predict', signature(object='RasterStack'), 
	function(object, model, filename="", datatype='FLT4S', filetype = 'raster', overwrite=FALSE, track=-1, vartype=NA, ...) {
		predrast <- raster(object)
		filename(predrast) <- filename
		dataType(predrast) <- datatype
		
		dataclasses <- attr(model$terms, "dataClasses")
		f <- which(dataclasses == 'factor')
		if (length(f) > 0) { 
			haveFactor <- TRUE 
		} else {
			haveFactor <- FALSE
		}
		
		if (dataContent(object) == 'all') {
			rowvals <- data.frame( values(object, names=TRUE))
			if (haveFactor) {
				for (i in 1:length(f)) {
					rowvals[,f[i]] <- as.factor(rowvals[,f[i]])
				}
			}
			
			predv <- predict(model, rowvals, ...)
			predrast <- setValues(predrast, predv)
			if (filename(predrast) != "") {
				predrast <- writeRaster(predrast)
			}
			return(predrast)
		} else {
			for (r in 1:nrow(object)) {
				object <- readRow(object, r)
				rowvals <- data.frame( values(object, names=TRUE))		
				if (haveFactor) {
					for (i in 1:length(f)) {
						rowvals[,f[i]] <- as.factor(rowvals[,f[i]])
					}
				}
				predv <- predict(model, rowvals, ...)
				predrast <- setValues(predrast, predv, r)
				predrast <- writeRaster(predrast, filetype=filetype, overwrite=overwrite)
			}
		}	
		return(predrast)
	}
)


	