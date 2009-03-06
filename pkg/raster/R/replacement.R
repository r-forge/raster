# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3



setMethod("[", c("RasterLayer","ANY", "ANY"),
	function(x,i,j,...,drop=TRUE) {
		if (dataContent(x) != 'all') {
			if (dataSource(x) != 'disk') {
				stop('no data associated with this RasterLayer object')
			} else {
				if (.CanProcessInMemory(x, 1)) {
					x <- readAll(x)
				}
			}
		}
		
		argsn <- nargs() - length(list(...)) - !missing(drop)
		if (dataContent(x) == 'all') {
			if ( missing(j) && argsn == 2) {
				callNextMethod( matrix(values(x), nrow(x), ncol(x), byrow=T), i=i, drop=drop )
			} else {
				callNextMethod( matrix(values(x), nrow(x), ncol(x), byrow=T), i=i, j=j, drop=drop )
			}
		} else {
			if ( missing(j) ) {
				if ( argsn == 2 ) {
					return(cellValues(x, i))
				} else {
					cells <- cellsFromRow(x, i)
					return(cellValues(x, cells))
				} 
			} else if (missing(i)) {
				cells <- cellsFromCol(x, j)
				return(cellValues(x, cells))
			} else {
		# bound to fail in most cases:
				cells <- cellFromRowCol(x, i, j)
				return(cellValues(x, cells))
			}
		}
	}
)




setReplaceMethod("[", c("RasterLayer","missing", "missing"),
	function(x, i, j, value) {
		if (length(value) == ncell(x)) {
			return(setValues(x, value))
		} else if (length(value) == 1) {
			return( setValues(x, rep(value, times=ncell(x))) )
		} else {
			stop('length of replacement values should be 1 or ncell')
		}
	}
)

setReplaceMethod("[", c("RasterLayer","ANY", "missing"),
	function(x, i, j, value) {
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


