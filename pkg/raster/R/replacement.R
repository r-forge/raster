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

setReplaceMethod("[", c("RasterLayer", "ANY", "missing", "ANY"), 
	function(x, i, j, value) {
# if ...
		x@data@values[i] <- value
		return(x)
	}
)

setMethod("[", "RasterLayer",
	function(x,i,j,...,drop=FALSE) {
# if ...
		return(values(x)[i])
	}
)


setMethod("[[", c("RasterLayer", "ANY", "missing"), 
	function(x, i, j, ...) {
# if ...
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

