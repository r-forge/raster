# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3



setMethod("max", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){
		obs <- list(...)
		if (length(obs) == 0) {
			# this is for a RasterStack
			return(setRaster(x, values=apply(as.matrix(.getRasterValues(x)), 1, max, na.rm=na.rm)))
		} else {
			v <- .getRasterValues(x)
			for (i in 1:length(obs)) {
				v <- apply(cbind(v, .getAllTypeOfValues(x, obs[[i]], i)), 1, max, na.rm=na.rm)
			}
			return(setRaster(x, values=v))
		}
	}
)


setMethod("min", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){
		obs <- list(...)
		if (length(obs) == 0) {
			return(setRaster(x, values=apply(as.matrix(.getRasterValues(x)), 1, min, na.rm=na.rm)))
		} else {
			v <- .getRasterValues(x)
			for (i in 1:length(obs)) {
				v <- apply(cbind(v, .getAllTypeOfValues(x, obs[[i]], i)), 1, min, na.rm=na.rm)
			}
			return(setRaster(x, values=v))
		}
	}
)


setMethod("sum", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){
		obs <- list(...)
		if (length(obs) == 0) {
			return(setRaster(x, values=rowSums(as.matrix(.getRasterValues(x)), na.rm)))
		} else {
			v <- .getRasterValues(x)
			if (!(is.null(dim(v)))) {
				v <- rowSums(as.matrix(.getRasterValues(x)), na.rm=na.rm)
			} 
			for (i in 1:length(obs)) {
				vv <- .getAllTypeOfValues(x, obs[[i]], i)
				v <- rowSums(cbind(v, vv), na.rm=na.rm)
			}
		return(setRaster(x, values=v))
		}
	}
)


#todo "any", "all" 
	
	
setMethod("mean", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){
		obs <- list(...)
		if (length(obs) == 0) {
			return(setRaster(x, values=rowMeans(as.matrix(.getRasterValues(x)), na.rm)))
		} else {
			v <- .getRasterValues(x)
			if (!(is.null(dim(v)))) {
				v <- rowMeans(as.matrix(.getRasterValues(x)), na.rm=na.rm)
			} 
			for (i in 1:length(obs)) {
				vv <- .getAllTypeOfValues(x, obs[[i]], i)
				v <- rowMeans(cbind(v, vv), na.rm=na.rm)
			}
		return(setRaster(x, values=v))
		}
	}
)

	
	
	
setMethod("range", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){
		return(max(x, ..., na.rm=na.rm) - min(x, ..., na.rm=na.rm))
	}
)	



#if (!isGeneric("median")) {
#	setGeneric("median", function(x, na.rm=FALSE)
#		standardGeneric("median"))
#}


#setMethod('median', signature(x='Raster'), 
#	function(x, na.rm=FALSE){
#		if (dataContent(x) == 'all') {
#			return(median(values(x), na.rm=na.rm))
#		} else {
# needs to be improved for large files. Make frequency table row by row.....
#			return(median(values(readAll(x)), na.rm=na.rm))
#		}
#	}
#)


#if (!isGeneric("rmedian")) {
#	setGeneric("rmedian", function(x, ..., na.rm=FALSE)
#		standardGeneric("rmedian"))
#}

#setMethod('rmedian', signature(x='Raster'), 
#	function(x, ..., na.rm=FALSE){
#		obs <- list(...)
#		if (length(obs) == 0) {
#			return(setRaster(x, values=apply(as.matrix(.getRasterValues(x)), 1, median, na.rm=na.rm)))
#		} else {
#			stk <- stack(c(x,obs))
#			v <- vector()
#			for (r in 1:nrow(stk)) {
#				v <- c(v, apply(values(readRow(stk, r)), 1, median, na.rm=na.rm)) 
#			}
#			return(setRaster(x, values=v))
#		}
#	}
#)

