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
	setGeneric("labels", function(object, ...)
		standardGeneric("labels"))
}	

setMethod('labels', signature(object='Raster'), 
	function(object, ...) {
		return(object@data@attributes)
	}
)

setMethod('labels', signature(object='RasterStack'), 
	function(object, ...) {
		sapply(object@layers, function(x) x@data@attributes) 
	}
)


if (!isGeneric("labels<-")) {
	setGeneric("labels<-", function(object, values)
		standardGeneric("labels<-"))
}	


setMethod('labels<-', signature(object='RasterLayer', values='list'), 
	function(object, values) {
		if (length(values) != 1) {
			stop('length(values) != 1')
		}
		object@data@attributes <- values
		return(object)
	}
)

setMethod('labels<-', signature(object='RasterBrick', values='list'), 
	function(object, values) {
		if (length(values) != nlayers(object)) {
			stop('length(values) != nlayers(object)')
		}
		object@data@attributes <- values
		return(object)
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
		x@data@isfactor <- TRUE
		if (is.null(values) ) {
			#x <- round(x) #this makes slot isfactor FALSE again
			x@data@attributes <- list(data.frame(VALUE=unique(x)))
		} else {
			x@data@attributes <- values
		}	
		return(x)
	}
)

.asFactor <- function(x, values){

		return(x)
}		

setMethod('asFactor', signature(x='RasterBrick'), 
	function(x, values=NULL, ...) {
		x@data@isfactor <- TRUE
		if (is.null(values) ) {
			#x <- round(x) #this makes slot isfactor FALSE again
			x@data@attributes <- list(data.frame(VALUE=unique(x)))
		} else {
			x@data@attributes <- values
		}			
		return(x)
	}
)

