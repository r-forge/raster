# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2008
# Version 0.9
# Licence GPL v3



if (!isGeneric("stack")) {
	setGeneric("stack", function(x, ...)
		standardGeneric("stack"))
}	

setMethod("stack", signature(x='missing'), 
function(x) {
	return(new("RasterStack"))
	}
)

setMethod("stack", signature(x='Raster'), 
function(x, ..., bands=NULL) {
	rlist <- c(x, list(...))
	return(stack(rlist, bands))	
} )



setMethod("stack", signature(x='character'), 
function(x, ..., bands=NULL, xvar='', yvar='', zvar='', time='') {
    if (xvar != '' | yvar != '' | zvar != '' | is.numeric(time)) {
		return(.stackCDF(x, xvar, yvar, zvar, time))
	} else {
		rlist <- c(x, list(...))
		return(stack(rlist, bands))
	}
} )


setMethod("stack", signature(x='list'), 
function(x, bands=NULL) {
	j <- 0
	r <- list()
	for (i in 1:length(x)) {
		j <- j + 1
		if (is.character(x[[i]])) {
			if (is.null(bands)) {
				r[j] <- raster(x[[i]])
			} else if (bands[[i]] > 0) {
					r[j] <- raster(x[[i]], bands[[i]])
			} else {
				r[j] <- raster(x[[i]], 1)
				bds <- nbands(r[[j]])
				if (bds > 1) {
					for (b in 2:bds) {
						j <- j + 1
						r[j] <- raster(x[[i]], band=b)
					}
				}
			}
		} else if (extends(class(x[[i]]), "Raster")) {
			r[j] <- x[[i]]
		} else {
			stop("Arguments should be Raster* objects or filenames")
		}	
	}
	return(addLayer(new("RasterStack"), r))
} )


setMethod("stack", signature(x='SpatialGrid'), 
	function(x) {
		stk <- new("RasterStack")
		stk <- setExtent(stk, extent(x))
		projection(stk) <- x@proj4string
		rowcol(stk) <- c(x@grid@cells.dim[2], x@grid@cells.dim[1])

		if (class(x)=='SpatialGridDataFrame') {
			stk <- setValues(stk, as.matrix(x@data))
			rs <- as(stk, 'RasterLayer')
			stk <- setValues(stk, as.matrix(x@data))
			for (i in 1:ncol(x@data)) {
				stk@layers[i] <- rs
			}
		}
		return(stk)
	}
)
	

setMethod("stack", signature(x='SpatialPixels'), 
	function(x) {
		x <- as(x, 'SpatialGridDataFrame')
		return(stack(x))
	}
)