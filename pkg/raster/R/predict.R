# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  August 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	

setMethod('predict', signature(object='RasterStackBrick'), 
	function(object, model, filename="", ...) {
		predrast <- raster(object)
		datatype <- .datatype(...)
		filetype <- .filetype(...)
		overwrite <- .overwrite(...)

		filename(predrast) <- filename
		dataType(predrast) <- datatype
			
		v <- (attr(model$terms, "factors"))
		varnames <- attr(v, "dimnames")[[2]]
		stacknames <- layerNames(object)
		
		if (length( varnames[(varnames %in% stacknames)] ) != length(varnames)) {
			stop('variable in model that is not in Raster* object: \nIn model:', varnames, '\n', 'In Raster object:', stacknames)
		}

		if ( length( unique(stacknames[(stacknames %in% varnames)] )) != length(stacknames[(stacknames %in% varnames)] )) {
			stop('duplicate names in Raster* object: ', stacknames)
		}
		
			
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

		starttime <- proc.time()
		pb <- .setProgressBar(nrow(object), type=.progress(...))

		for (r in 1:nrow(object)) {
			rowvals <- getValues(object, r)
			rowvals <- as.data.frame(rowvals)
			colnames(rowvals) <- layerNames(object)
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
			.doProgressBar(pb, r) 
		}
		.closeProgressBar(pb, starttime)
		
		if (filename == '') {
			predrast <- setValues(predrast, v)
		}
		return(predrast)
	}
)



setMethod('predict', signature(object='RasterLayer'), 
	function(object, model, filename="",  ...) {
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
		starttime <- proc.time()
		pb <- .setProgressBar(nrow(object), type=.progress(...))

		for (r in 1:nrow(object)) {
			xy <- as.data.frame(xyFromCell(object, arow + (r-1) * ncol(object)) )
			predv <- as.vector( predict(model, xy, ...) )
			if (filename == '') {
				v <- c(v, predv)
			} else {
				predrast <- setValues(predrast, predv, r)			
				predrast <- writeRaster(predrast, filetype=filetype, overwrite=overwrite)
			}
			.doProgressBar(pb, r) 
		}
		.closeProgressBar(pb, starttime)
		if (filename == '') {
			predrast <- setValues(predrast, v)
		}
		return(predrast)
	}
)

