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
		filename <- trim(filename)
			
		v <- (attr(model$terms, "factors"))
		varnames <- attr(v, "dimnames")[[2]]
		stacknames <- layerNames(object)
		
		if (length( varnames[(varnames %in% stacknames)] ) != length(varnames)) {
			stop('variable in model that is not in Raster* object: \nIn model: ', paste(varnames, collapse='; '), '\n', 'In Raster object: ', paste(stacknames, collapse='; '))
		}

		if ( length( unique(stacknames[(stacknames %in% varnames)] )) != length(stacknames[(stacknames %in% varnames)] )) {
			stop('duplicate names in Raster* object: ', stacknames)
		}
		
			
		dataclasses <- attr(model$terms, "dataClasses")[-1]
		f <- names( which(dataclasses == 'factor') )
		if (length(f) > 0) { 
			haveFactor <- TRUE 
		} else {
			haveFactor <- FALSE
		}
		
		if (!canProcessInMemory(predrast) && filename == '') {
			filename <- rasterTmpFile()
			if (getOption('verbose')) { cat('writing raster to:', filename)	}						
		} 

		if (filename == '') {
			v <- matrix(NA, ncol=nrow(predrast), nrow=ncol(predrast))
		} 

		napred <- rep(NA, ncol(predrast))
		
		pb <- pbCreate(nrow(object), type=.progress(...))

		for (r in 1:nrow(object)) {
			rowvals <- getValues(object, r)
			rowvals <- as.data.frame(rowvals)
			colnames(rowvals) <- layerNames(object)
			if (haveFactor) {
				for (i in 1:length(f)) {
					rowvals[,f[i]] <- as.factor(rowvals[,f[i]])
				}
			}
			
			# patch for predict.gam
			# perhaps speeds it up too?
			if ( sum(!is.na(rowvals)) == 0 ) {
				predv <- napred
			} else {
			
				predv <- as.vector( as.numeric ( predict(model, rowvals, ...) ))
			
				if (length(predv) != nrow(rowvals)) {
				# perhaps no prediction for rows with NA ? 
				# this was a problem with predict.rf, now fixed
					rowvals <- na.omit( cbind(1:nrow(rowvals), rowvals) )
					indices <- rowvals[,1]
					if (length(indices) == length(predv)) {
						pr <- 1:ncol(object)
						pr[] <- NA
						pr[indices] <- predv
						predv <- pr
					} else {
						stop('length of predict vector does not match lenght of input vectors; please submit a bug report')
					}
				}
			}
			
			if (filename == '') {
				v[,r] <- predv
			} else {
				predrast <- setValues(predrast, predv, r)
				predrast <- writeRaster(predrast, filename=filename, ...)
			}
			pbStep(pb, r) 
		}
		pbClose(pb)
		
		if (filename == '') {
			predrast <- setValues(predrast, as.vector(v))
		}
		return(predrast)
	}
)



setMethod('predict', signature(object='RasterLayer'), 
	function(object, model, filename="",  ...) {
		predrast <- raster(object)
		filename <- trim(filename)

		if (!canProcessInMemory(predrast) && filename == '') {
			filename <- rasterTmpFile()
			filename(outRaster) <- filename
			if (getOption('verbose')) { cat('writing raster to:', filename)	}						
		} 
		if (filename == '') {
			v <- matrix(NA, ncol=nrow(predrast), nrow=ncol(predrast))
		} 

		arow <- 1:ncol(object)
		
		pb <- pbCreate(nrow(object), type=.progress(...))

		for (r in 1:nrow(object)) {
			xy <- as.data.frame(xyFromCell(object, arow + (r-1) * ncol(object)) )
			predv <- as.vector( predict(model, xy, ...) )
			if (filename == '') {
				v[,r] <- predv
			} else {
				predrast <- setValues(predrast, predv, r)			
				predrast <- writeRaster(predrast, filename=filename, ...)
			}
			pbStep(pb, r) 
		}
		pbClose(pb)
		if (filename == '') {
			predrast <- setValues(predrast, as.vector(v))
		}
		return(predrast)
	}
)

