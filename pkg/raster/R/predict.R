
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
			indices <- 1:ncell(predrast)
			rowvals <- data.frame( na.omit(cbind( indices, values(object, names=TRUE)) ) )
			if (haveFactor) {
				for (i in 1:length(f)) {
					rowvals[,f[i]] <- as.factor(rowvals[,f[i]])
				}
			}
			
			predv <- indices
			predv[] <- NA
			predv[indices] <- predict(model, rowvals[,-1], ...)
			predrast <- setValues(predrast, predv)
			if (filename(predrast) != "") {
				predrast <- writeRaster(predrast)
			}
			return(predrast)
		} else {
			myIdVar <- 1:ncol(object)
			predv <- 1:ncol(object)
			for (r in 1:nrow(object)) {
				object <- readRow(object, r)
				
				rowvals <- na.omit( cbind(myIdVar, values(object, names=TRUE)) )
				if (haveFactor) {
					for (i in 1:length(f)) {
						rowvals[,f[i]] <- as.factor(rowvals[,f[i]])
					}
				}

				indices <- rowvals[,1]
				predv[] <- NA
				if (length(indices) > 0) {
					predv[indices] <- predict(model, data.frame(rowvals[,-1]), ...)
				}
				predrast <- setValues(predrast, predv, r)
				predrast <- writeRaster(predrast, filetype=filetype, overwrite=overwrite)
			}
		}	
		return(predrast)
	}
)


	