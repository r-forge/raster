# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : November 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("xyValues")) {
	setGeneric("xyValues", function(object, xy, ...)
		standardGeneric("xyValues"))
}	


setMethod("xyValues", signature(object='Raster', xy='SpatialPoints'), 
	function(object, xy, method='simple', buffer=NULL, fun=NULL, na.rm=TRUE,...) { 
		callGeneric(object, coordinates(xy),  method, buffer, fun, na.rm, ...)
	}	
)


setMethod("xyValues", signature(object='Raster', xy='data.frame'), 
	function(object, xy, method='simple', buffer=NULL, fun=NULL, na.rm=TRUE,...) { 
		callGeneric(object, as.matrix(xy), method, buffer, fun, na.rm, ...)
	}	
)


setMethod("xyValues", signature(object='Raster', xy='vector'), 
	function(object, xy, method='simple', buffer=NULL, fun=NULL, na.rm=TRUE, ...) { 
		if (length(xy) == 2) {
			callGeneric(object, matrix(xy, ncol=2), method, buffer, fun, na.rm,  ...)
		} else {
			stop('xy coordinates should be a two-column matrix or data.frame, or a vector of two numbers.')
		}
	} )

	
setMethod("xyValues", signature(object='RasterLayer', xy='matrix'), 
	function(object, xy, method='simple', buffer=NULL, fun=NULL, na.rm=TRUE, ...) { 

		if (dim(xy)[2] != 2) {
			stop('xy has wrong dimensions; it should have 2 columns' )
		}

		if (! is.null(buffer)) {
			if (method != 'simple') { warning('method argument is ignored when a buffer is used') }
			return( .xyvBuf(object, xy, buffer, fun, na.rm=na.rm) )
		}

		if (method=='bilinear') {
			return(.bilinearValue(object, xy))
		} else if (method=='simple') {
			cells <- cellFromXY(object, xy)
			return(.readCells(object, cells))
		} else {
			stop('invalid method argument. Should be simple or bilinear.')
		}
	}	
)	


setMethod("xyValues", signature(object='RasterStack', xy='matrix'), 
	function(object, xy, method='simple', buffer=NULL, fun=NULL, na.rm=TRUE, ...) { 
		.xyvStackBrick(object, xy, method, buffer, fun, na.rm, ...)
} )

setMethod("xyValues", signature(object='RasterBrick', xy='matrix'), 
	function(object, xy, method='simple', buffer=NULL, fun=NULL, na.rm=TRUE, ...) { 
		.xyvStackBrick(object, xy, method, buffer, fun, na.rm, ...)
} )


.xyvStackBrick <- function(object, xy, method='simple', buffer=NULL, fun=NULL, na.rm=TRUE, ...) { 

		dots <- list(...)
		layer <- dots$layer
		n <- dots$nl
		nls <- nlayers(object)
	
		if (is.null(layer)) { layer <- 1 } 
		if (is.null(n)) { n <- nls } 
		layer <- min(max(1, round(layer)), nls)
		n <- min(max(1, round(n)), nls-layer+1)
	
		if (dim(xy)[2] != 2) {
			stop('xy has wrong dimensions; there should be 2 columns only' )
		}
		
		if (! is.null(buffer)) {
			if (method != 'simple') { warning('method argument is ignored when a buffer is used') }
			return( .xyvBuf(object, xy, buffer, fun, na.rm, layer=layer, n=n) )
		}

		if (method == 'bilinear') {
			result <- .bilinearValue(object, xy, layer=layer, n=n)
			return(result)		
	
		} else if (method=='simple') {
		
			cells <- cellFromXY(object, xy)
			return( cellValues(object, cells, layer=layer, n=n) )
			
		} else {
			stop('invalid method argument. Should be simple or bilinear.')
		}
	}


