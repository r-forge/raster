# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : February 2010
# Version 0.9
# Licence GPL v3


	
if (!isGeneric("is.factor")) {
	setGeneric("is.factor", function(x)
		standardGeneric("is.factor"))
}	

setMethod('is.factor', signature(x='Raster'), 
	function(x) {
		return(x@data@isfactor)
	}
)

setMethod('is.factor', signature(x='RasterStack'), 
	function(x) {
		sapply(x@layers, function(x) x@data@isfactor)
	}
)


if (!isGeneric("labels")) {
	setGeneric("labels", function(x)
		standardGeneric("labels"))
}	

setMethod('labels', signature(x='Raster'), 
	function(x) {
		return(x@data@attributes)
	}
)

setMethod('labels', signature(x='RasterStack'), 
	function(x) {
		sapply(x@layers, function(x) x@data@attributes) 
	}
)


if (!isGeneric("labels<-")) {
	setGeneric("labels<-", function(x, value)
		standardGeneric("labels<-"))
}	


setMethod('labels<-', signature(x='RasterLayer', value='list'), 
	function(x, value) {
		if (length(value) != 1) {
			stop('lenght(value) != 1')
		}
		x@data@attributes <- value
		return(x)
	}
)

setMethod('labels<-', signature(x='RasterBrick', value='list'), 
	function(x, value) {
		if (length(value) != nlayers(x)) {
			stop('lenght(value) != nlayers(x)')
		}
		x@data@attributes <- value
		return(x)
	}
)



if (!isGeneric("asFactor")) {
	setGeneric("asFactor", function(x, ...)
		standardGeneric("asFactor"))
}

setMethod('asFactor', signature(x='ANY'), 
	function(x, ...) {
		return(factor(x, ...))
	}
)

setMethod('asFactor', signature(x='RasterLayer'), 
	function(x, values=NULL, ...) {
		x@data@isfactor = TRUE
		if (is.null(values) ) {
			x <- round(x)
			x@data@atttributes <- list(data.frame(VALUE=unique(x)))
		} else {
			x@data@attributes <- values
		}			
		return(x)
	}
)

setMethod('asFactor', signature(x='RasterBrick'), 
	function(x, values=NULL, ...) {
		x@data@isfactor = TRUE
		if (is.null(values) ) {
			x <- round(x)
			x@data@atttributes <- list(data.frame(VALUE=unique(x)))
		} else {
			x@data@atttributes <- values
		}			
		return(x)
	}
)

