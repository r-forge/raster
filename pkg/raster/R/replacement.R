# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,8
# Licence GPL v3


'projection<-' <- function(x, value) {
	return( setProjection(x, value) )
}

'ncol<-' <- function(x, value) {
	return( setRowCol(x, ncols=value) )
}	

'nrow<-' <- function(x, value) {
	return( setRowCol(x, nrows=value) )
}	


setMethod("[", "RasterLayer",
	function(x,i,j,...,drop=FALSE) {
		if  (!missing(j)) {	stop("incorrect number of dimensions") }
		if (dataContent(x) == 'nodata') {
			if (ncells(x) < 1000000) {
				if (dataSource(x) == 'disk') {
					x <- readAll(x)
				} else {
					stop('no data associated with this RasterLayer object')
				}
			} else {
				stop('Large raster, no data in memory, use readAll() first')
			}	
		}
		return(values(x)[i]) 
	}
)


setReplaceMethod("[", "RasterLayer",  
	function(x, i, j, value) {
		if  (!missing(j)) {	stop("incorrect number of dimensions") }
		if (dataContent(x) == 'nodata') {
			if (ncells(x) < 1000000) {
				if (dataSource(x) == 'disk') {
					x <- readAll(x)
				} else {
					stop('no data associated with this RasterLayer object')
				}
			} else {
				stop('Large raster, no data in memory, use readAll() first')
			}	
		}
		x@data@values[i] <- value
		x <- setFilename(x, "")
		x@data@source <- 'ram'
		return(x)
	}
)


setMethod("[", "RasterStack",
	function(x,i,j,layer,...,drop=FALSE) {
		if  (!missing(layer)) {	stop("incorrect number of dimensions") }
		if  (missing(j)) {	return(values(x)[i, ]) 
		} else {
			v <- valuesRow(x,j)
			return(v[i])
		}
	}
)


setMethod("[[", c("RasterLayer", "ANY", "missing"), 
	function(x, i, j, ...) {
		return(values(x)[i])
	}
)
 

 setReplaceMethod("[[", c("RasterLayer", "ANY", "missing", "ANY"), 
	function(x, i, j, value) {
# if ...
		x@data@values[i] <- value
		return(x)
	}	
)

