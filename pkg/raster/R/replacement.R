# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3

'filename<-' <- function(x, value) {
	return( setFilename(x, value) )
}

'datatype<-' <- function(x, value) {
	return( setDatatype(x, value) )
}

'projection<-' <- function(x, value) {
	return( setProjection(x, value) )
}

'ncol<-' <- function(x, value) {
	return( setRowCol(x, ncols=value) )
}	

'nrow<-' <- function(x, value) {
	return( setRowCol(x, nrows=value) )
}	

'resolution<-' <- function(x, value) {
	if (length(value) == 1) {
		return( setRes(x, xres=value, yres=value) )
	} else {
		return( setRes(x, xres=value[1], yres=value[2]) )
	}
}

'xmin<-' <- function(x, value) {
	return(changeBbox(x, xmn=value))
}

'xmax<-' <- function(x, value) {
	return(changeBbox(x, xmx=value))
}

'ymin<-' <- function(x, value) {
	return(changeBbox(x, ymn=value))
}

'ymax<-' <- function(x, value) {
	return(changeBbox(x, ymx=value))
}


.getColValues <- function(r, colnr) {
	firstcol <- 1:nrow(r) * ncol(r) - ncol(r) 
	cells <- colnr + firstcol 
	return(values(r)[cells])
}



setMethod("[", "RasterLayer",
	function(x,i,j,...,drop=FALSE) {
		if (!missing(j)) { stop("incorrect number of dimensions") }
# consider row, sparse....		
		if (dataContent(x) != 'all') {
			if (dataSource(x) != 'disk') {
				stop('no data associated with this RasterLayer object')
			} else {
				return(cellValues(x, i))
			}	
		} else {
			return(values(x)[i]) 
		}	
	}
)


setReplaceMethod("[", "RasterLayer",  
	function(x, i, j, value) {
		if  (!missing(j)) {	
			stop("incorrect number of dimensions") 
		}
		if  (missing(i)) {	
			if (length(value) == ncell(x)) {
				return(setValues(x, value))
			} else if (length(value) == 1) {
				return( setValues(x, rep(value, times=ncell(x))) )
			} else {
				stop('length of replacement values should be 1 or ncell')
			}
		}
		if (class(i) == "RasterLayer") {
			i <- as.logical( .getRasterValues(i) ) 
		}
# what about data rows ?		
		if (dataContent(x) == 'nodata') {
			if (ncell(x) < 1000000) {
				if (dataSource(x) == 'disk') {
					x <- readAll(x)
				} else {
					x <- setValues(x, rep(NA, times=ncell(x)))
				}
			} else {
				stop('Large raster with no data in memory, use readAll() first')
			}	
		}
		x@data@values[i] <- value
		x@data@source <- 'ram'
		x <- setFilename(x, "")
		x <- setMinMax(x)
		return(x)
	}
)


setMethod("[[", c("RasterLayer","ANY","ANY"),
# i = row
# j = col
	function(x,i,j,...,drop=FALSE) {
		if (dataContent(x) == 'nodata') {
			if (ncell(x) < 1000000) {
				if (dataSource(x) == 'disk') {
					x <- readAll(x)
				} else {
					stop('no data associated with this RasterLayer object')
				}
			} else {
				stop('Large raster, no data in memory, use readAll() first')
			}	
		}
		return( matrix(values(x), nrow(x), ncol(x), byrow=T)[i,j] )
	}
)


setReplaceMethod("[[", "RasterLayer",  
	function(x, i, j, value) {
		if (!missing(i)) {
			if (class(i) == "RasterLayer") {
				i <- as.logical( .getRasterValues(i) ) 
			}
		}
		if (!missing(j)) {
			if (class(j) == "RasterLayer") {
				j <- as.logical( .getRasterValues(i) ) 
			}
		}
		
		if (dataContent(x) == 'nodata') {
			if (ncell(x) < 1000000) {
				if (dataSource(x) == 'disk') {
					x <- readAll(x)
				} else {
					x <- setValues(x, rep(NA, times=ncell(x)))
				}
			} else {
				stop('Large raster with no data in memory, use readAll() first')
			}	
		}
		v <- matrix(values(x), nrow(x), ncol(x), byrow=T)
		x <- clearValues(x)
		v[i,j] <- value
		x <- setValues(x, as.vector(t(v)))
		x <- setFilename(x, "")
		x <- setMinMax(x)
		return(x)
	}
)


