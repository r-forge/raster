# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  August 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	

setMethod('predict', signature(object='Raster'), 
	function(object, model, filename="", xy=FALSE, ...) {
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
		xyOnly <- FALSE
		if (xy) { 
			arow <- 1:ncol(object) 
			if (class(object) == 'RasterStack') {
				if (nlayers(object)==0) { xyOnly <- TRUE }
			} else {
				if (dataSource(object) == 'ram') {
					if (dataContent(object) != 'all') {
						xyOnly <- TRUE 
					}
				}				
			}
		} else {
			if (class(object) == 'RasterStack') {
				if (nlayers(object)==0) { stop('empty RasterStack') }
			} else {
				if (dataSource(object) == 'ram') {
					if (dataContent(object) != 'all') {
						{ stop('No values associated with this Raster object') }
					}
				}				
			}
		}
		lns <- layerNames(object)
		
		pb <- pbCreate(nrow(object), type=.progress(...))
		
		#print(xy)
		#print(xyOnly)
		
		for (r in 1:nrow(object)) {
			if (xyOnly) {
				xy <- xyFromCell(predrast, arow + (r-1) * ncol(predrast)) 
				rowvals <- data.frame(x=xy[,1], y=xy[,2])
			} else {
				rowvals <- as.data.frame( getValues(object, r) )
				colnames(rowvals) <- lns
				if (haveFactor) {
					for (i in 1:length(f)) {
						rowvals[,f[i]] <- as.factor(rowvals[,f[i]])
					}
				}
				if (xy) {
					xy <- xyFromCell(predrast, arow + (r-1) * ncol(predrast)) 
					rowvals <- cbind(x=xy[,1], y=xy[,2], rowvals)
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


