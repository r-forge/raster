# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : November 2008
# Version 0.9
# Licence GPL v3



if (!isGeneric("xyValues")) {
	setGeneric("xyValues", function(object, xyCoords, ...)
		standardGeneric("xyValues"))
}	


setMethod("xyValues", signature(object='Raster', xyCoords='SpatialPoints'), 
	function(object, xyCoords, method='simple') { 
		callGeneric(object, coordinates(xyCoords), method=method)
	}	
)


setMethod("xyValues", signature(object='Raster', xyCoords='data.frame'), 
	function(object, xyCoords, method='simple') { 
		callGeneric(object, as.matrix(xyCoords), method=method)
	}	
)


setMethod("xyValues", signature(object='Raster', xyCoords='vector'), 
	function(object, xyCoords, method='simple') { 
		if (length(xyCoords) != 2) {
			stop('xyCoords should be a two column matrix or a vector of length 2')
		}
		callGeneric(object, matrix(xyCoords, ncol=2), method=method)
	}
)

	
setMethod("xyValues", signature(object='RasterLayer', xyCoords='matrix'), 
	function(object, xyCoords, method='simple') { 
		if (dim(xyCoords)[2] != 2) {
			stop('xyCoords has wrong dimensions; it should have 2 columns' )
		}
		if (method=='bilinear') {
			return(.bilinearValue(object, xyCoords))
		} else if (method=='simple') {
			cells <- cellFromXY(object, xyCoords)
			return(.readCells(object, cells))
		} else {
			stop('invalid method argument. Should be simple or bilinear.')
		}
	}	
)	


setMethod("xyValues", signature(object='RasterStack', xyCoords='matrix'), 
	function(object, xyCoords, method='simple') { 
		if (dim(xyCoords)[2] != 2) {
			stop('xyCoords has wrong dimensions; there should be 2 columns only' )
		}
		
		if (method == 'bilinear') {
			for (i in seq(nlayers(object))) {
				r <- raster(object, i)
				v <- .bilinearValue(r, xyCoords)
				if (i == 1) {
					result <- v
				} else {
					result <- cbind(result, v)
				}
			}
			if (!(is.null(dim(result)))) {
				colnames(result) <- object@data@colnames
			}	
			return(result)		
	
		} else if (method=='simple') {
			cells <- cellFromXY(object, xyCoords)
			return(.stackReadCells(object, cells))
		} else {
			stop('invalid method argument. Should be simple or bilinear.')
		}
	}
)



setMethod("xyValues", signature(object='RasterBrick', xyCoords='matrix'), 
	function(object, xyCoords, method='simple') { 
		if (dim(xyCoords)[2] != 2) {
			stop('xyCoords has wrong dimensions; there should be 2 columns only' )
		}
		
		if (method == 'bilinear') {
			for (i in seq(nlayers(object))) {
				r <- raster(object, i)
				v <- .bilinearValue(r, xyCoords)
				if (i == 1) {
					result <- v
				} else {
					result <- cbind(result, v)
				}
			}
			if (!(is.null(dim(result)))) {
				colnames(result) <- object@data@colnames
			}	
			return(result)		
	
		} else if (method=='simple') {
			cells <- cellFromXY(object, xyCoords)
			return(.brickReadCells(object, cells))
		} else {
			stop('invalid method argument. Should be simple or bilinear.')
		}
	}
)



