
if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	

setMethod('predict', signature(object='RasterStack'), 
	function(object, model, filename="", datatype='INT4S', filetype = 'raster', overwrite=FALSE, track=-1, ...) {
		predrast <- raster(object)
		filename(predrast) <- filename
		dataType(predrast) <- datatype
		myIdVar <- 1:ncol(object)
		predv <- 1:ncol(object)
		for (r in 1:nrow(object)) {
			object <- readRow(object, r)
			rowvals <- na.omit( cbind(myIdVar, values(object, names=TRUE)) )
			indices <- rowvals[,1]
			predv[] <- NA
			if (length(indices) > 0) {
				pred <- predict(model, rowvals[,-1], ...)
				predv[indices] <- pred
			}
			predrast <- setValues(predrast, predv, r)
			predrast <- writeRaster(predrast, filetype=filetype, overwrite=overwrite)
		}
		return(predrast)
	}
)


	